
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-deprecations #-}


{- |

Module      :  Main
Portability :  unportable

Probably not at all portable. Makes use of functions from the
@unistd.h@ header file.

TODO:

Tidy this up, shift some of the logic out to another module.
-}


module Main 
where

import Control.Arrow              (left)
import Control.Exception          (bracket, throw, assert)
import Control.Monad              (when)
import Control.Monad.Error.Class  (Error(..), MonadError(..) )
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Maybe  (MaybeT(..))


import Data.ConfigFile            (emptyCP, readfile
                                  , optionxform, get  )
import Data.Either.Utils          (eitherToMonadError)
import Data.Monoid
import Data.Maybe                 (fromJust)
import Data.Time.Clock.POSIX      (getPOSIXTime)

import Foreign.C.Types            (CInt(..))
import Foreign.C.Error            (throwErrnoIfMinus1_)

import Network.BSD                (getHostName)

import System.Directory           (setCurrentDirectory, makeAbsolute)
import System.Environment         (withArgs)
import System.IO                  (Handle, hClose, stderr,
                                  hPutStrLn, hPutStr)
import System.IO.Error            (modifyIOError)
import qualified System.Posix.Files as F
import System.Posix.IO            (exclusive, defaultFileFlags, openFd
                                  , OpenMode(..), fdToHandle )
import System.Posix.User          (getUserEntryForName, userID, userGroupID )
import System.Posix.Types         (CGid(..), CUid(..), UserID
                                  , GroupID )
import System.Random              (getStdRandom, randomR)


import ConfigLocation             (configFileLocn)
import CmdArgs                    (AttCmdArgs(..), withCmdArgs)
import EmailAddress               (EmailAddress(..), validateFromString)
import qualified DeliveryHeaders as DH
import DeliveryHeaders            (Addr(..))



-- * C functions and wrappers around them

-- | unistd funcs:
--
-- @
-- #include <unistd.h>
--
--       int setresuid(uid_t ruid, uid_t euid, uid_t suid);
--       int setresgid(gid_t rgid, gid_t egid, gid_t sgid);
-- @
foreign import ccall "setresgid" setresgid_c :: CGid -> CGid -> CGid -> IO CInt
foreign import ccall "setresuid" setresuid_c :: CUid -> CUid -> CUid -> IO CInt

-- | wrapper around C func @setresgid@.
setResGid :: CGid -> CGid -> CGid -> IO ()
setResGid r e s = throwErrnoIfMinus1_ "setResGid" $ setresgid_c r e s

-- | wrapper around C func @setresuid@.
setResUid :: CUid -> CUid -> CUid -> IO ()
setResUid r e s = throwErrnoIfMinus1_ "setResUid" $ setresuid_c r e s

-- * low-level, Posix-specific functions

-- | opens w/ mode 0644, but gives error if exists
--
-- i.e. @-rw-r--r--@
openIfNExist :: String -> IO Handle
openIfNExist !filePath = do
  let mode = foldl F.unionFileModes F.nullFileMode 
              [F.ownerReadMode, F.ownerWriteMode, F.groupReadMode, F.otherReadMode]
      openFileFlags = defaultFileFlags { exclusive = True }
  !fd <- openFd filePath WriteOnly (Just mode) openFileFlags
  !hdl <- fdToHandle fd
  return hdl

-- | opens and closes a file created with openIfNExist,
-- and executes the action f on it in between.
--
withMailFile :: String -> (Handle -> IO a) -> IO a
withMailFile !filePath !f = do
  let open =  openIfNExist filePath
  let close = hClose

  bracket open close f


-- | get the uid and gid for a username
getUserIDs :: String -> IO (UserID, GroupID)
getUserIDs userName = do
  userEntry <- getUserEntryForName userName
  let uid = userID userEntry
      gid = userGroupID userEntry 
  return (uid, gid)


-- | Return an allegedly unique filename; useful to add new mail files in a maildir. Name is of format <time> <random num> <hostname>.
--
-- from https://hackage.haskell.org/package/imm-0.5.1.0/
getUniqueName :: IO String
getUniqueName = do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)
    return . concat $ [time, ".", rand, ".", hostname]


-- * utility functions

-- | force either
forceEitherMsg :: Either err t -> (err -> String) -> t
forceEitherMsg x f = case
  x of
    Left err -> throw $ userError $ f err
    Right val -> val

-- | emit warning to stderr
warning :: String -> IO ()
warning str =
  hPutStrLn stderr $ "attomail: warning: " <> str

-- | for use with Either
mkError :: Error a => (t -> String) -> Either t b -> Either a b
mkError f = left (strMsg . f)


-- * Program config    

-- | Configuration, as read from the config file.
data Config = Config { mailDir :: String, userName :: String }
  deriving (Show, Eq)

-- | @getConfig filename@ reads the config file at @filename@.
getConfig :: String -> IO Config
getConfig confFile = do
  cp <- readfile ( emptyCP { optionxform = id } ) (confFile:: String)
  cp <- return $ let f err = "Error reading config file '" <> confFile <> "': " 
                             <> show err
                 in forceEitherMsg cp f

  let mailDir = let f err = "Error getting option 'mailDir': " <> show err
                 in  forceEitherMsg (get cp "DEFAULT" "mailDir") f
  let userName = let f err = "Error getting option 'userName': " <> show err
                 in  forceEitherMsg (get cp "DEFAULT" "userName") f
  return $ Config mailDir userName

-- * deal with addresses

-- | the 'Addr' type is simply something that _might_ be an address,
-- this validates it.
validateAddr
  :: Maybe Addr -> Either String (Maybe EmailAddress.EmailAddress)
validateAddr addr = 
  runMaybeT $ do
    (Addr addrStr) <- MaybeT $ return addr
    lift $ EmailAddress.validateFromString addrStr    


-- | wrapper around validateAddr. Pass in a func that
-- takes in a (presumably bad) address and an error message, and spits
-- out a string
--
-- And then, the Maybe/Either result of validation will
-- get turned into an appropriate MonadError action we can just
-- execute in IO.
handleAddr
  :: (MonadError e m, Error e) =>
     (Addr -> String -> String)
     -> Maybe Addr -> m (Maybe EmailAddress.EmailAddress)
handleAddr f addr = 
  eitherToMonadError $ mkError (f $ fromJust addr) $ validateAddr addr

-- * whopping big monolithic funcs

-- | deliver mail to the maildir directory mailDir,
-- owned by userName, with command-line args cmdArgs.
--
-- Creates a unique file name w/ getUniqueName. If something creates
-- the file in between generating the name and delivering mail,
-- an IOError will get thrown. 
--
-- AttCmdArgs should never have 0 recipients
deliverMail :: FilePath -> String -> AttCmdArgs -> IO ()
deliverMail mailDir userName cmdArgs = do
  let (!AttCmdArgs !fromAddress !_nm !recipients) = cmdArgs

  !fromAddress <- flip handleAddr fromAddress (\badAddr err -> 
                          "bad from address " <> show (unAddr badAddr) <>
                           ", err was: " <> show err)
  fromAddress <- return $ (Addr . show) <$> fromAddress

  (uid, gid) <- flip modifyIOError 
                 (getUserIDs userName)
                 (\ioErr -> userError $ "couldn't get gid and uid for user '"
                    <> userName <> "', err was: " <> show ioErr)

  setResGid gid gid gid
  setResUid uid uid uid

  flip modifyIOError
        (setCurrentDirectory =<< makeAbsolute mailDir)
        (\ioErr -> userError $ "couldn't change to mail dir '" 
                    <> mailDir <> "', err was: " <> show ioErr)

  fileName <- getUniqueName
  tm <- DH.getMailTime

  withMailFile fileName $ \outHdl -> do
    --print "in withMailFile"
    mesgConts <- getContents
    when (length recipients > 1) $
      warning "multiple recipients found, ignoring all but first"
    -- if the command-line parser works correctly, there should be
    -- no possibility of an empty list
    !_ <- return $! assert (not (null recipients))
    let toAddr = head recipients
        possHeadered = DH.addHeaders tm mesgConts fromAddress toAddr 
        res2X = eitherToMonadError $ flip mkError possHeadered (\err -> 
                    "error parsing stdin as mail message. err was: "
                    <> show err <> "\n"
                    <> "fileconts: " <> show mesgConts)

    headeredMesg <- res2X                    

    -- putStrLn $ "res: " <> show res
    !_ <- hPutStr outHdl headeredMesg
    --print "done withMailFile"
    return ()



-- | process args as for sendmail.
--
-- @
--   sendmail [flags] [receipients] < message
--    -f = envelop sender address
--    -F = full name of sender
--    -bm = read mail from stdin (default)
--    -bp, -bs = we ignore these
-- @
main :: IO ()
main = do
  --args <- getArgs
  --hPutStrLn stderr $ "args: " ++ show args
  (Config !mailDir !userName) <- flip modifyIOError
                                (getConfig ConfigLocation.configFileLocn)
                                (\ioErr -> userError $ 
                                   "couldn't open config"
                                   <> "file, error was: " <> show ioErr)
  withCmdArgs (deliverMail mailDir userName)

-- * testing

-- | here for testing purposes  
test :: IO AttCmdArgs  
test =
  withArgs [
              "-f", "auditor@auditrix"
             , "-F", "Joe Bloggs"
             , "-bm"
             , "john@john.com"
           ] 
           (withCmdArgs return)


