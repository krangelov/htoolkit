-----------------------------------------------------------------------------------------
{-| Module      :  Font
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Fonts.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Font
              (              
              -- * Fonts
                createFont
              , defaultFont
             
              -- * Enumerate
              , getFontNames, getFontVariants

              -- * Standard font definitions.
              , defaultFontDef
              , serifFontDef
              , sansSerifFontDef
              , nonProportionalFontDef
              , smallFontDef
              , symbolFontDef
              ) where

import Graphics.UI.Port.Font
