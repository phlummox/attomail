
{-|

Adapted from Text.EmailAddress by Dennis Gosnell (see
<https://hackage.haskell.org/package/emailaddress>)
but
without all the dependencies (like postgresql-simple ...). Uses the @email-validate@ package,
instead. 

-}

{-# LANGUAGE InstanceSigs #-}

module EmailAddress 
(     -- * Data Type
      EmailAddress(EmailAddress, unEmailAddress)
      -- * Create EmailAddress
    , emailAddress
    , emailAddressFromText
    , emailAddressFromString
      -- * validation
    , validate
    , validateFromText
    , validateFromString
      -- * Unsafe creation
    , unsafeEmailAddress
) where

import Text.Read (Read(readPrec), ReadPrec)

import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import Data.ByteString hiding (pack,unpack)


import qualified Text.Email.Validate as EV

-- | wrapper around our implementation - 'EV.EmailAddress'.
newtype EmailAddress = EmailAddress
    { unEmailAddress :: EV.EmailAddress }
    deriving (Eq, Ord)

-- |
-- >>> (read "\"foo@gmail.com\"") :: EmailAddress
-- "foo@gmail.com"
instance Read EmailAddress where
    readPrec :: ReadPrec EmailAddress
    readPrec = fmap EmailAddress readPrec

-- |
-- >>> import qualified Data.ByteString.Char8 as BS 
-- >>> :set -XOverloadedStrings 
-- >>> show $ unsafeEmailAddress "foo" "gmail.com"
-- "\"foo@gmail.com\""
instance Show EmailAddress where
    show :: EmailAddress -> String
    show = show . unEmailAddress


-- | Wrapper around 'EV.validate'.
--
-- >>> validate "foo@gmail.com"
-- Right "foo@gmail.com"
-- >>> import Data.Either (isLeft)
-- >>> isLeft $ validate "not an email address"
-- True
validate :: ByteString -> Either String EmailAddress
validate = fmap EmailAddress . EV.validate

-- | Wrapper around 'EV.emailAddress'.
--
-- Similar to 'validate', but returns 'Nothing' if the email address fails to
-- parse.
--
-- >>> emailAddress "foo@gmail.com"
-- Just "foo@gmail.com"
-- >>> emailAddress "not an email address"
-- Nothing
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress = fmap EmailAddress . EV.emailAddress

-- | Create an 'EmailAddress' from a 'Text' value.  See 'validate'.
validateFromText :: Text -> Either String EmailAddress
validateFromText = validate . encodeUtf8

-- | Create an 'EmailAddress' from a 'Text' value.  See 'emailAddress'.
emailAddressFromText :: Text -> Maybe EmailAddress
emailAddressFromText = emailAddress . encodeUtf8

-- | Create an 'EmailAddress' from a 'String' value.  See 'validate'.
validateFromString :: String -> Either String EmailAddress
validateFromString = validateFromText . pack

-- | Create an 'EmailAddress' from a 'String' value.  See 'emailAddress'.
emailAddressFromString :: String -> Maybe EmailAddress
emailAddressFromString = emailAddressFromText . pack


-- | Wrapper around 'EV.unsafeEmailAddress'.
--
-- Unsafely create an 'EmailAddress' from a local part and a domain part.  The
-- first argument is the local part, and the second argument is the domain
-- part.
--
-- For example, in the email address @foo\@gmail.com@, the local part is @foo@
-- and the domain part is @gmail.com@.
--
-- >>> unsafeEmailAddress "foo" "gmail.com"
-- "foo@gmail.com"
unsafeEmailAddress
    :: ByteString    -- ^ Local part
    -> ByteString    -- ^ Domain part
    -> EmailAddress
unsafeEmailAddress = (EmailAddress .) . EV.unsafeEmailAddress

