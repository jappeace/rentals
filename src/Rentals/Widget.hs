{-# LANGUAGE CPP #-}

-- | Support for hot reloading of widgets in the development environment
module Rentals.Widget
  ( widgetFile
  , isDevelopment
  )  where

import Language.Haskell.TH.Syntax (Exp, Q)
import qualified Text.Cassius.Ordered as Ordered
import qualified Text.Lucius.Ordered as Ordered
import Yesod.Default.Util
import Data.Default(def)
import Text.Julius (juliusFile, juliusFileReload)

-- | sometimes env doesn't work (supercede jobs)
--   helpful for deciding reloading
isDevelopment :: Bool
#ifdef DEVELOPMENT
isDevelopment = True
#else
isDevelopment = False
#endif

widgetFile :: String -> Q Exp
widgetFile =
  (if isDevelopment
                then widgetFileReload
                else widgetFileNoReload)
              def
