{-# LANGUAGE TemplateHaskell #-}
module SendMail.Internal.Types where

import SendMail.Types

import Data.Text as Text
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH


data MailServiceConfig = MailServiceConfig
    { service :: Text
    , mailgunDomain :: Maybe Text
    , mailgunApiKey :: Maybe Text
    , sesAccessKey  :: Maybe Text
    , sesSecretKey  :: Maybe Text
    } deriving (Show, Eq)
$(deriveFromJSON defaultOptions ''MailServiceConfig)

tPlainMail :: PlainTextMail -> Text
tPlainMail (PlainTextMail t) = t

tHtmlMail :: HtmlMail -> Text
tHtmlMail (HtmlMail t) = t

tAddress :: Address -> Text
tAddress (Address t) = t

tFromAddress :: FromAddress -> Text
tFromAddress (FromAddress t) = t

tToAddress :: ToAddress -> Text
tToAddress (ToAddress t) = t

tSubject :: Subject -> Text
tSubject (Subject t) = t

