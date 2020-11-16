{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foo.Bar.Web.Server.Handler.Home where

import Foo.Bar.Web.Server.Foundation
import Foo.Bar.Web.Server.Widget
import Yesod

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")
