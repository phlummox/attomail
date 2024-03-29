
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

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

import Control.Arrow
import Control.Exception          (bracket, throw)
import Control.Monad              (when)
import Control.Monad.Except

import Data.Char                  (isSpace)
import Data.List                  (foldl')
import Data.Monoid
import Data.Time.Clock.POSIX      (getPOSIXTime)

import Foreign.C.Types            (CInt(..))
import Foreign.C.Error            (throwErrnoIfMinus1_)

import Network.BSD                (getHostName)

import System.Directory           (setCurrentDirectory, makeAbsolute)

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

import Text.ConfigParser
import Text.Parsec                ( satisfy, many1 )
import Text.Parsec.Text           ( Parser )

import ConfigLocation             (configFileLocn)
import CmdArgs                    (AttCmdArgs(..), withCmdArgs)
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
openIfNExist :: FilePath -> IO Handle
openIfNExist !filePath = do
  let mode = foldl' F.unionFileModes F.nullFileMode
              [F.ownerReadMode, F.ownerWriteMode, F.groupReadMode, F.otherReadMode]
      openFileFlags = defaultFileFlags { exclusive = True }
  !fd <- openFd filePath WriteOnly (Just mode) openFileFlags
  !hdl <- fdToHandle fd
  return hdl

-- | opens and closes a file created with openIfNExist,
-- and executes the action f on it in between.
--
withMailFile :: FilePath -> (Handle -> IO a) -> IO a
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
getUniqueName :: IO FilePath
getUniqueName = do
    time     <- show <$> getPOSIXTime
    hostname <- getHostName
    rand     <- show <$> (getStdRandom $ randomR (1,100000) :: IO Int)
    return $ mconcat [time, ".", rand, ".", hostname]


-- * utility functions

-- | force either
forceEitherMsg :: Either err t -> (err -> String) -> t
forceEitherMsg x f = case
  x of
    Left err -> throw $ userError $ f err
    Right val -> val

-- vendored in from MissingH
eitherToMonadError :: MonadError e m => Either e a -> m a
eitherToMonadError (Left x) = throwError x
eitherToMonadError (Right x) = return x

-- | emit warning to stderr
warning :: String -> IO ()
warning str =
  hPutStrLn stderr $ "attomail: warning: " <> str

-- * Program config

-- | Configuration, as read from the config file.
data Config = Config { mailDir :: !String, userName :: !String }
  deriving (Show, Eq)


cp :: ConfigParser (Maybe String, Maybe String)
cp = configParser (Nothing,Nothing) [parseMailDir, parseUserName]
  where
    parseMailDir = ConfigOption {
          key = "mailDir"
        , required = True
        , parser = string'
        , action = \x -> Control.Arrow.first (const $ Just x)
        }
    parseUserName = ConfigOption {
          key = "userName"
        , required = True
        , parser = string'
        , action = \x -> Control.Arrow.second (const $ Just x)
        }

    string' :: Parser String
    string' =  many1 (satisfy (not . isSpace))



-- | @getConfig filename@ reads the config file at @filename@.
getConfig :: FilePath -> IO Config
getConfig confFile = do
  conf <- parseFromFile cp confFile
  case conf of
    Left msg ->     do warning $ "Error parsing config file "  <>
                          show confFile <> ":"
                       warning $ show msg
                       error "error"
    Right (Just mailDir, Just userName) ->
                    return $ Config mailDir userName
    _        ->     error "should be impossible"



-- * whopping big monolithic funcs

-- | deliver mail to the maildir directory mailDir,
-- owned by userName, with command-line args cmdArgs.
--
-- Creates a unique file name w/ getUniqueName. If something creates
-- the file in between generating the name and delivering mail,
-- an IOError will get thrown.
--
deliverMail :: FilePath -> String -> AttCmdArgs -> IO ()
deliverMail mailDir userName cmdArgs = do
  let (AttCmdArgs fromAddress _nm recipients) = cmdArgs

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
    mesgConts <- getContents
    when (length recipients > 1) $
      warning "multiple recipients found, ignoring all but first"
    when (null recipients) $
      warning "no recipients found, making up a bogus one"

    let toAddr :: Addr
        toAddr = case recipients of
                    [] -> Addr "foo@bar.com"
                    (x:_) -> x

    finalMesg <- case DH.addHeaders tm mesgConts fromAddress toAddr of
      Left ex -> do warning $ "err parsing stdin as mail message, but will " <>
                      "deliver anyway. err was: " <> show ex <> "\n" <>
                      "stdin conts was: " <>
                      show mesgConts
                    return mesgConts
      Right r -> return r

    hPutStr outHdl finalMesg



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
  (Config !mailDir !userName) <- flip modifyIOError
                                (getConfig ConfigLocation.configFileLocn)
                                (\ioErr -> userError $
                                   "couldn't open config"
                                   <> "file, error was: " <>
                                      show ioErr)
  withCmdArgs (deliverMail mailDir userName)



