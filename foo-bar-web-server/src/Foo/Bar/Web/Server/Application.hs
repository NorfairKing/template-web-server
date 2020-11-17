{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.Web.Server.Application where

import Foo.Bar.Web.Server.Foundation
import Foo.Bar.Web.Server.Handler

mkYesodDispatch "App" resourcesApp
