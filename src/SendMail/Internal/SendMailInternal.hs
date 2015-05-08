{-# LANGUAGE OverloadedStrings #-}
module SendMail.Internal.SendMailInternal where

import qualified SendMail.Types as MailTypes
import qualified SendMail.Internal.Types as IntTypes
import qualified SendMail.Internal.Mailgun as Mailgun

import Network.HTTP.Client.Conduit
import Data.ByteString
import qualified Data.Text as Text
import Data.Yaml
import Data.Aeson
import Data.HashMap.Strict as M
import Debug.Trace

privateImplGetBackend :: Show env => env -> Manager -> FilePath -> IO (Maybe MailTypes.EmailSender)
privateImplGetBackend env manager mailConfFile = do
    decodeFile mailConfFile >>= (privateImplGetBackendFromMObject env manager)

privateImplGetBackendFromContent :: Show env => env -> Manager -> ByteString -> IO (Maybe MailTypes.EmailSender)
privateImplGetBackendFromContent env manager mailConfString =
    privateImplGetBackendFromMObject env manager obj
      where
          obj = Data.Yaml.decode mailConfString :: Maybe Value

privateImplGetBackendFromMObject :: Show env => env -> Manager -> Maybe Value -> IO (Maybe MailTypes.EmailSender)
privateImplGetBackendFromMObject _ _ Nothing =
       fail $ "Invalid SendMail YAML Configuration"
privateImplGetBackendFromMObject env manager (Just (Object obj))
        | Just v <- M.lookup (Text.pack $ show env) obj = (parseMonad privateImplParseConf v) >>= (getMailSenderFromKey manager)
privateImplGetBackendFromMObject env _ x =
        fail $ "Could not find environment: " ++ show env ++ " " ++ show x

privateImplParseConf :: Value -> Parser IntTypes.MailServiceConfig
privateImplParseConf = withObject "SendMail configuration" $ \o -> do
    service       <- o .:  "service"
    mailgunDomain <- o .:? "mailgunDomain"
    mailgunApiKey <- o .:? "mailgunApiKey"
    sesAccessKey  <- o .:? "sesAccessKey"
    sesSecretKey  <- o .:? "sesSecretKey"
    return IntTypes.MailServiceConfig
            { IntTypes.service       = service
            , IntTypes.mailgunDomain = mailgunDomain
            , IntTypes.mailgunApiKey = mailgunApiKey
            , IntTypes.sesAccessKey  = sesAccessKey
            , IntTypes.sesSecretKey  = sesSecretKey
            }

getMailSenderFromKey :: Manager -> IntTypes.MailServiceConfig -> IO (Maybe MailTypes.EmailSender)
getMailSenderFromKey manager config =
    case (Text.toLower $ IntTypes.service config) of
        "mailgun" -> return $ Mailgun.createMailgunSender manager config
        _ -> return Nothing


