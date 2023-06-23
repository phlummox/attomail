

{- |

Process command-line arguments, mimicking sendmail:

@
 sendmail [flags] [receipients] < message
   -f = envelop sender address
   -F = full name of sender
   -bm = read mail from stdin (default)
   -bp, -bs, various others = we ignore these
@

TODO: add some fancy validation of addresses.
-}

module CmdArgs (
    AttCmdArgs(..)
  , withCmdArgs
)
where

import Data.Monoid                  ( (<>), mconcat )
                                    -- needed - Data.Monoid
                                    -- not imported by all versions of Options

import Options.Applicative hiding   (helper, strOption)
import qualified Options.Applicative as Op
import Options.Applicative.Types    (readerAsk)

import DeliveryHeaders              ( Addr(..) )

strOption :: Mod OptionFields String -> Parser String
strOption = Op.strOption


-- | Addr option reader
addr :: ReadM (Maybe Addr)
addr = Just . Addr <$> str

-- | name option reader
name :: ReadM (Maybe String)
name = Just  <$> str

-- | combinator thing for the mode of something
--  (e.g. "-bm".) Tell user we only accept one
-- permissible mode.
mode :: Char -> ReadM ()
mode validMode = do
  x <- readerAsk
  if x == [validMode]
    then return ()
    else readerError $ "can only take mode '" <> [validMode] <> "' as arg"

-- | Data structure for command-line arguments.
data AttCmdArgs = AttCmdArgs
  {
      senderEnvelopeAddress :: Maybe Addr -- ^ Possible envelope address.
    , senderFullName :: Maybe String  -- ^ Possible sender full name
    , recipients :: [Addr] -- ^ Recipients. (Ignored.)
  }
  deriving (Eq, Show)

-- | Parser for command-line args.
--
-- TODO:
--
-- add more (ignored) options.
-- see http://www.sendmail.org/~ca/email/man/sendmail.html
attCmdArgs :: Parser AttCmdArgs
attCmdArgs = AttCmdArgs 
  <$> option addr
      ( short 'f' 
        <> value Nothing 
        <> metavar "ADDRESS" 
        <> help "Sender envelope address" )
  
  <*> ( option name
      ( short 'F' 
        <> value Nothing 
        <> metavar "NAME"
        <> help "Sender full name" )
    <* option (mode 'm')
        ( short 'b'
        <> value ()
        <> metavar "MODE"
        <> help "-bm: Read input from stdin" )
    <* switch   
        ( short 'i'
          <> help "(ignored, used only for compatibility with sendmail")
    <* many ( strOption   
        ( short 'o'
          <> help "(ignored, used only for compatibility with sendmail"))
    <* many ( strOption   
        ( short 'O'
          <> help "(ignored, used only for compatibility with sendmail"))
    <* many ( strOption   
        ( short 'B'
          <> help "(ignored, used only for compatibility with sendmail"))
    <* many ( strOption   
        ( short 'q'
          <> help "(ignored, used only for compatibility with sendmail"))
    <* switch   
        ( short 'v'
          <> help "(ignored, used only for compatibility with sendmail")
        )
  <*> some ( argument (Addr <$> str) (metavar "RECIPIENTS...") )

-- | program version
version :: String
version = "0.1.0.2"


-- | just used for testing
doStuff :: AttCmdArgs -> IO ()
doStuff x@AttCmdArgs{} = 
  print x


withCmdArgs :: (AttCmdArgs -> IO b) -> IO b
withCmdArgs f = execParser opts >>= f
  where
    --opts = info (helper <*> attCmdArgs)
    opts = info (attCmdArgs <**> Op.helper)
      ( fullDesc
        <> progDesc desc
        <> header h )

    desc = "read a message on stdin and deliver it to user in config file"
    h    = "attomail v " <> version <> "simple mail delivery to one user"  


