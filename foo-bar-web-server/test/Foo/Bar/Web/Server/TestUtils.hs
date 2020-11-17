module Foo.Bar.Web.Server.TestUtils where

import Control.Monad.Logger
import Foo.Bar.Web.Server.Application ()
import Foo.Bar.Web.Server.Foundation
import Foo.Bar.Web.Server.Static
import Test.Hspec
import Yesod.Test

type FooBarWebServerSpec = YesodSpec App

type FooBarWebServerExample = YesodExample App

fooBarWebServerSpec :: FooBarWebServerSpec -> Spec
fooBarWebServerSpec =
  yesodSpec $
    App
      { appLogLevel = LevelWarn,
        appStatic = fooBarWebServerStatic,
        appGoogleAnalyticsTracking = Nothing,
        appGoogleSearchConsoleVerification = Nothing
      }
