

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
import Version                      (getVersion)

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
    , recipients :: [Addr] -- ^ Recipients.
  }
  deriving (Eq, Show)

-- | Parser for command-line args.
--
-- see <http://www.sendmail.org/~ca/email/man/sendmail.html>
-- or something like <https://manpages.ubuntu.com/manpages/bionic/man1/mailq.1.html>
-- for meanings of ignored options.
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
          <> help "Ignored, used only for compatibility with sendmail. (Originally: 'Ignore dots alone on lines by themselves in incoming messages.')")
    <* switch
        ( short 't'
          <> help "Ignored, used only for compatibility with sendmail. (Originally: 'Read message to work out the recipients.')")
    <* many ( strOption
        ( short 'o'
          <> help "Ignored, used only for compatibility with sendmail. (Originally: set an option.)"))
    <* many ( strOption
        ( short 'O'
          <> help "Ignored, used only for compatibility with sendmail. (Originally: set an option.)"))
    <* many ( strOption
        ( short 'B'
          <> metavar "TYPE"
          <> help "Ignored, used only for compatibility with sendmail. (Originally: Set the body type to TYPE.  Current legal values are 7BIT or 8BITMIME.)"))
    <* many ( strOption
        ( short 'q'
          <> help "Ignored, used only for compatibility with sendmail. (Originally: used to specify a queue interval.)"))
    <* switch
        ( short 'v'
          <> help "Ignored, used only for compatibility with sendmail. (Originally: enable verbose output.)")
        )
  <*> many ( argument (Addr <$> str)
             (metavar "RECIPIENTS..."
             <> help "Recipients to send to. Used to construct a 'To:' address, but otherwise ignored: attomail always delivers to a config-specified mail folder."
              )
            )

versionOption :: Parser (a -> a)
versionOption =
  infoOption displayedVersion ( long "version"
                        <> help "display version"
                        )
  where
    displayedVersion = getVersion

withCmdArgs :: (AttCmdArgs -> IO b) -> IO b
withCmdArgs f = execParser opts >>= f
  where
    opts = info (attCmdArgs <**> Op.helper <**> versionOption)
      ( fullDesc
        <> progDesc desc
        <> header h )

    desc = "read a message on stdin and deliver it to user in config file"
    h    = "attomail v " <> getVersion <> " simple mail delivery to one user"


