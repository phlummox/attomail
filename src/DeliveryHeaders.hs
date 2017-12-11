
{-|
  Add delivery headers to an email message.
-}


module DeliveryHeaders (
  -- * Data types

    Addr(..)
  , MailTime

  -- * header utilities
  , isDate
  , isFrom
  , makeReceived
  , addHeaders

  -- * string utilities
  , toStr
  , rstrip

  -- * time utilities
  , getMailTime

) where

import Control.Arrow              (left)
import Control.Applicative        ( (<$>) )

import Data.ByteString.Char8      (unpack)
import Data.Char                  (isSpace)
import Data.List                  (dropWhileEnd)
import Data.Maybe                 (fromJust, isJust)
import Data.Monoid
import Data.UnixTime              (mailDateFormat, getUnixTime, formatUnixTime)

import Text.Parsec
import Text.ParserCombinators.Parsec.Rfc2822NS 
                                  (Field(..), message, GenericMessage(..) )

-- | ... actually, any string at all is considered a valid
-- "time" string we can use.
newtype MailTime = MailTime String

-- | just a newtype to distinguish address from other strings.
newtype Addr = Addr { unAddr :: String }
  deriving (Show, Eq)

toStr :: MailTime -> String
toStr (MailTime str) = str

-- | get the unix time
getMailTime :: IO MailTime
getMailTime = do
  time <- getUnixTime
  MailTime . unpack <$> formatUnixTime mailDateFormat time

-- | is this a 'Date' field?
isDate :: Field -> Bool
isDate field = case field of
  Date _ -> True
  _      -> False

-- | Is this a 'From' field?
isFrom :: Field -> Bool
isFrom field = case field of
  From _ -> True
  _      -> False

-- | make up the "@Received:@" header, given a time,
-- a possible "from" address, and a "to" address.
makeReceived :: MailTime -> Maybe Addr -> Addr -> String
makeReceived (MailTime timeStr) fromAddr toAddr = 
  let toAddrStr = unAddr toAddr
      rec = ["Received: for " <> toAddrStr <> " with local (attomail)"]
      envelope = if isJust fromAddr 
                 then [" (envelope-from " <> unAddr (fromJust fromAddr) <> ")"]
                 else []
      end = ["; " <> timeStr <> "\r\n"] -- should end w/ crlf??
  in  concat $ rec ++ envelope ++ end

-- | strip whitespace from right-hand end
rstrip :: String -> String
rstrip = dropWhileEnd isSpace 


-- | add minimal headers: a "@Received:@" header, a "@Date:@"
-- header if we haven't already been given one,
-- a "@From:@" field if we haven't already been given one.
addHeaders :: MailTime -> String -> Maybe Addr -> Addr -> Either String String
addHeaders time mesgText fromAddr toAddr = do
  (Message headers body) <- left show (parse message "stdin" mesgText)
  let received  = [makeReceived time fromAddr toAddr]
      fromStr   = case fromAddr of 
                    Nothing -> ""
                    Just a  -> unAddr a
      hasDate   = any isDate headers
      hasFrom   = any isFrom headers
      dateField = if hasDate
                  then []
                  else ["Date: " <> toStr time <> "\r\n"]
      fromField = if hasFrom
                  then []
                  else ["From: " <> fromStr <> "\r\n"]
      headBit   = [rstrip (take (length mesgText - length body) mesgText) <> "\r\n"]

      newHead = concat $ received ++ headBit ++ dateField ++ fromField ++ ["\r\n"]
  return $ newHead ++ body



