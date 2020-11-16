module Foo.Bar.Web.Server.Static.TH
  ( mkStatic,
  )
where

import Foo.Bar.Web.Server.Constants
import Language.Haskell.TH
import Yesod.EmbeddedStatic

mkStatic :: Q [Dec]
mkStatic =
  mkEmbeddedStatic
    development
    "fooBarWebServerStatic"
    []
