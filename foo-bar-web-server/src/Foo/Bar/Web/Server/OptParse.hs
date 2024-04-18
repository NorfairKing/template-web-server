{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.Web.Server.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Monad.Logger
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

data Settings = Settings
  { settingPort :: !Int,
    settingLogLevel :: !LogLevel,
    settingGoogleAnalyticsTracking :: !(Maybe Text),
    settingGoogleSearchConsoleVerification :: !(Maybe Text)
  }
  deriving (Show)

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let settingPort = fromMaybe 8080 $ flagPort <|> envPort <|> mc confPort
      settingLogLevel = fromMaybe LevelWarn $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
      settingGoogleAnalyticsTracking = flagGoogleAnalyticsTracking <|> envGoogleAnalyticsTracking <|> mc confGoogleAnalyticsTracking
      settingGoogleSearchConsoleVerification = flagGoogleSearchConsoleVerification <|> envGoogleSearchConsoleVerification <|> mc confGoogleSearchConsoleVerification
  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confGoogleAnalyticsTracking :: !(Maybe Text),
    confGoogleSearchConsoleVerification :: !(Maybe Text)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "port" "Port"
          .= confPort
        <*> optionalField "log-level" "Minimal severity for log messages"
          .= confLogLevel
        <*> optionalField "google-analytics-tracking" "Google analytics tracking code"
          .= confGoogleAnalyticsTracking
        <*> optionalField "google-search-console-verification" "Google search console html element verification code"
          .= confGoogleSearchConsoleVerification

instance HasCodec LogLevel where
  codec =
    stringConstCodec
      [ (LevelDebug, "Debug"),
        (LevelInfo, "Info"),
        (LevelWarn, "Warn"),
        (LevelError, "Error")
      ]

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' "config.yaml"

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envLogLevel :: !(Maybe LogLevel),
    envGoogleAnalyticsTracking :: !(Maybe Text),
    envGoogleSearchConsoleVerification :: !(Maybe Text)
  }

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "FOO_BAR_WEB_SERVER_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
      <*> Env.var (fmap Just . Env.auto) "PORT" (mE <> Env.help "Port")
      <*> Env.var (fmap Just . Env.auto) "LOG_LEVEL" (mE <> Env.help "Minimal severity for log messages")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_ANALYTICS_TRACKING" (mE <> Env.help "Google analytics tracking code")
      <*> Env.var (fmap Just . Env.str) "GOOGLE_SEARCH_CONSOLE_VERIFICATION" (mE <> Env.help "Google search console html element verification code")
  where
    mE = Env.def Nothing

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagLogLevel :: !(Maybe LogLevel),
    flagGoogleAnalyticsTracking :: !(Maybe Text),
    flagGoogleSearchConsoleVerification :: !(Maybe Text)
  }

parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                help "Port",
                metavar "PORT"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                help "Minimal severity for log messages",
                metavar "LOG_LEVEL"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-analytics-tracking",
                help "Google analytics tracking code",
                metavar "CODE"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "google-search-console-verification",
                help "Google search console html element verification code",
                metavar "CODE"
              ]
          )
      )
