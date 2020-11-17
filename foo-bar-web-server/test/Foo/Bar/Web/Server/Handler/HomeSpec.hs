module Foo.Bar.Web.Server.Handler.HomeSpec (spec) where

import Foo.Bar.Web.Server.Handler.TestImport

spec :: Spec
spec = fooBarWebServerSpec $ ydescribe "HomeR"
  $ yit "GETs a 200"
  $ do
    get HomeR
    statusIs 200
