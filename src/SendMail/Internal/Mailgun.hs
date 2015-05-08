{-# LANGUAGE OverloadedStrings #-}
module SendMail.Internal.Mailgun where

import SendMail.Types as MailTypes
import qualified SendMail.Internal.Types as IntTypes

import Network.HTTP.Conduit
import Network.HTTP.Client.MultipartFormData

import Data.Text as Text
import Data.Text.Encoding as TE
import Data.ByteString as BS

import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as S8

import Debug.Trace

newtype MailgunDomain = MailgunDomain Text
newtype MailgunApiKey = MailgunApiKey Text

getDomainText :: MailgunDomain -> Text
getDomainText (MailgunDomain a) = a

getApiKey :: MailgunApiKey -> Text
getApiKey (MailgunApiKey a) = a

data MailgunContext = MailgunContext { httpManager :: Manager
                                     , mailgunDomain ::  MailgunDomain
                                     , mailgunApiKey :: MailgunApiKey
                                     }

applyUser :: ByteString -> Request -> Request
applyUser user req =
    req { requestHeaders = authHeader : requestHeaders req }
  where
    authHeader = (CI.mk "Authorization", basic)
    basic = S8.append "Basic " (B64.encode $ BS.concat ["api:", user])


createUrlString :: MailgunContext -> Text
createUrlString mailgunContext =
    Text.concat ["https://api.mailgun.net/v3/", domain, "/messages"]
      where
          domain = getDomainText $ mailgunDomain mailgunContext

createUrl :: MonadThrow m => MailgunContext -> m Request
createUrl mailgunContext = do
    req' <- parseUrl $ Text.unpack $ createUrlString mailgunContext
    return $ applyUser (encodeUtf8 $ getApiKey $ mailgunApiKey mailgunContext) req'

addPart :: Text -> ByteString -> [Part] -> [Part]
addPart name value parts = (partBS name value) : parts

addPartMaybe :: Text -> Maybe ByteString -> [Part] -> [Part]
addPartMaybe name (Just value) parts = addPart name value parts
addPartMaybe _ _ parts = parts

maybeEncode :: Maybe Text -> Maybe ByteString
maybeEncode (Just text) = Just $ encodeUtf8 text
maybeEncode _ = Nothing

addEmailToRequest :: MonadIO m => MailTypes.Email -> Request -> m Request
addEmailToRequest email =
    formDataBody $ (addPart "from" fromAddr)
                 $ (addPart "to" toAddr)
                 $ (addPartMaybe "cc" ccAddr)
                 $ (addPartMaybe "bcc" bccAddr)
                 $ (addPart "subject" subjectBS)
                 $ (addPart "text" plainMailBS)
                 $ (addPartMaybe "html" htmlMailBS) []
    where
        fromAddr = encodeUtf8 $ IntTypes.tFromAddress $ fromAddress email
        toAddr = encodeUtf8 $ IntTypes.tToAddress $ toAddress email
        ccAddr = fmap encodeUtf8 $ fmap IntTypes.tAddress $ ccAddress email
        bccAddr = fmap encodeUtf8 $ fmap IntTypes.tAddress $ bccAddress email
        subjectBS = encodeUtf8 $ IntTypes.tSubject $ subject email
        plainMailBS = encodeUtf8 $ IntTypes.tPlainMail $ plainMail email
        htmlMailBS = fmap encodeUtf8 $ fmap IntTypes.tHtmlMail $ htmlContent email

createMailgunSender :: Manager -> IntTypes.MailServiceConfig -> Maybe MailTypes.EmailSender
createMailgunSender manager config =
    createMailgunSenderImpl manager domain apikey
    where
        domain = IntTypes.mailgunDomain config
        apikey = IntTypes.mailgunApiKey config

createMailgunSenderImpl :: Manager -> Maybe Text -> Maybe Text -> Maybe MailTypes.EmailSender
createMailgunSenderImpl manager (Just a) (Just b) = Just $ MailTypes.EmailSender $ sendEmail (MailgunContext manager (MailgunDomain a) (MailgunApiKey b))
createMailgunSenderImpl _ _ _ = Nothing

sendEmail :: MailgunContext -> MailTypes.Email -> IO (Either MailTypes.ErrorMessage MailTypes.OkMessage)
sendEmail context email = do
    request' <- createUrl context
    request <- addEmailToRequest email request'
    response <- trace ("Sending: \n" ++ show request) httpLbs request (httpManager context)
    trace (show response) return $ Left $ MailTypes.ErrorMessage "Error not implemented"

-- $(logInfo) $ logMsg (MailTypes.toAddress email) (MailTypes.subject email)

logMsg :: MailTypes.ToAddress -> MailTypes.Subject -> Text
logMsg (MailTypes.ToAddress toAddress) (MailTypes.Subject subject) = Text.concat ["Mailgun: Sending email: ", toAddress, ". Subject: ", subject]
