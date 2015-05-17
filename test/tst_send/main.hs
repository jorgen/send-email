{-# LANGUAGE OverloadedStrings #-}

import Data.Text as Text
import Data.ByteString as ByteString hiding (hPutStrLn)
import Data.ByteString.Char8 as BSC8 hiding (hPutStrLn)
import Data.Maybe
import Text.Read

import Control.Monad

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

import Network.HTTP.Conduit

import SendMail.SendMail
import SendMail.Types

data Environment = Development | Testing | Staging | Production | Unknown
    deriving(Read, Show)


defaultYaml :: ByteString
defaultYaml = "\
\Development: &defaults\n\
\   service: \"mailgun\"\n\
\   mailgunDomain: \"samples.mailgun.org\"\n\
\   mailgunApiKey: \"key-3ax6xnjp29jd6fds4gc373sgvjxteol0\"\n\
\ "

data ScriptOptions = ScriptOptions {
    from :: Text,
    to :: Text,
    env :: Text,
    config :: Maybe Text
}

defaultOptions = ScriptOptions {
    from = "LISTS <lists@limilind.com>",
    to = "",
    env = "Development",
    config = Nothing
}

options :: [ OptDescr (ScriptOptions -> IO ScriptOptions) ]
options =
    [  Option "t" ["to"]
        (ReqArg
            (\arg opt -> return $ opt {
                to = Text.pack arg
            })
            "TO")
        "email address to send to"

    , Option "f" ["from"]
        (ReqArg
            (\arg opt -> return $ opt {
                from = Text.pack arg
            })
            "FROM")
        "email adress to mark as from"
    , Option "e" ["environment"]
        (ReqArg
            (\arg opt -> return $ opt {
                env = Text.pack arg
            })
            "ENVIRONMENT")
        "environment variable to use in config"
    , Option "c" ["config"]
        (ReqArg
            (\arg opt -> return $ opt {
                config = Just $ Text.pack arg
            })
            "CONFIG_FILE")
        "yaml config file containing mail service configuration"
    ]

getScriptArgs :: IO ScriptOptions
getScriptArgs = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
    let haveUnresolvedOptions = not $ Prelude.null nonOptions
    let haveErrors = not $ Prelude.null errors
    when haveUnresolvedOptions ( do
                            hPutStrLn stderr $ "Unresolved options: " ++ Prelude.concat nonOptions
                            hPutStrLn stderr $ usageInfo "" options
                            exitWith $ ExitFailure  1)

    when haveErrors ( do
                    hPutStrLn stderr $ "Errors parsing command line options"
                    mapM (hPutStrLn stderr) errors
                    exitWith $ ExitFailure 1)

    finalOptions <- Prelude.foldl (>>=) (return defaultOptions) actions

    when (Text.null $ to finalOptions) ( do
                    hPutStrLn stderr $ "Missing to email address"
                    hPutStrLn stderr $ usageInfo "" options
                    exitWith $ ExitFailure 1)
    return finalOptions

replaceIfEmpty :: Text -> Text -> Text
replaceIfEmpty def "" = def
replaceIfEmpty _ notEmpty = notEmpty

getMailSenderFromOptions :: ScriptOptions -> Manager -> IO (Maybe EmailSender)
getMailSenderFromOptions options manager = maybe
    (getEmailSenderFromContent e manager defaultYaml)
    (getEmailSenderFromFile e manager . Text.unpack)
    conf
    where
        e = fromMaybe Development (readMaybe $ Text.unpack $ env options :: Maybe Environment)
        conf = config options

sendMail' :: Maybe EmailSender -> ScriptOptions -> IO ()
sendMail' (Just (EmailSender emailSender)) options = do
    emailSender email
    return ()
      where
          subject = Subject $ "This is a test mail"
          toAddress = ToAddress $ to options
          fromAddress = FromAddress $ from options
          plainText = PlainTextMail "Hello\n. This is a test mail from send-email\n"
          email = Email subject toAddress Nothing Nothing fromAddress plainText Nothing



sendMail _ _ =
    hPutStrLn stderr "Failed to get mailsender"

main :: IO ()
main = do
    scriptArgs <- getScriptArgs
    manager <- newManager conduitManagerSettings
    hPutStrLn stderr $ Text.unpack $ Text.concat ["Config: ", replaceIfEmpty "empty" (to scriptArgs), " : ", from scriptArgs, " : ", fromMaybe (Text.pack $ "\n" ++ (BSC8.unpack defaultYaml)) (config scriptArgs)]
    getMailSenderFromOptions scriptArgs manager >>= flip sendMail' scriptArgs



