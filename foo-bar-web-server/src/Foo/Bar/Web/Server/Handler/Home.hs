{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foo.Bar.Web.Server.Handler.Home where

import Foo.Bar.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = defaultLayout $(widgetFile "home")
