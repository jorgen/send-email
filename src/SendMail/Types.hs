module SendMail.Types where

import Data.Text
import Control.Monad.IO.Class
import Data.Either

newtype PlainTextMail = PlainTextMail Text
    deriving (Show, Eq)

newtype HtmlMail = HtmlMail Text
    deriving (Show, Eq)

newtype Address = Address Text
    deriving (Show, Eq)

newtype FromAddress = FromAddress Text
    deriving (Show, Eq)

newtype ToAddress = ToAddress Text
    deriving (Show, Eq)

newtype Subject = Subject Text
    deriving (Show, Eq)

newtype ErrorMessage = ErrorMessage Text
    deriving (Show, Eq)

newtype OkMessage = OkMessage Text
    deriving (Show, Eq)

data Email = Email
    { subject :: Subject
    , toAddress :: ToAddress
    , ccAddress :: Maybe Address
    , bccAddress :: Maybe Address
    , fromAddress :: FromAddress
    , plainMail :: PlainTextMail
    , htmlContent :: Maybe HtmlMail
    } deriving (Show, Eq)

newtype EmailSender = EmailSender (Email -> IO (Either ErrorMessage OkMessage))
