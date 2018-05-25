-----------------------------------------------------------------------------------------
{-| Module      :  Colors
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module defines the abstract data type which represents colors and defines a 
    large set of predefined colors. The full list of predefined colors is 
    visible here: <colors.html>
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Colors
            ( -- * Color type
              Color
            , rgbColor, colorRed, colorGreen, colorBlue
            , cmyColor, colorCyan, colorMagenta, colorYellow 
              -- * Standard colors.
            , aliceblue, antiquewhite, aqua, aquamarine, azure, beige
            , bisque, black, blanchedalmond, blue, blueviolet, brown
            , burlywood, cadetblue, chartreuse, chocolate, coral
            , cornflower, cornsilk, crimson, cyan, darkblue, darkcyan
            , darkgoldenrod, darkgray, darkgreen, darkkhaki
            , darkmagenta, darkolivegreen, darkorange, darkorchid
            , darkred, darksalmon, darkseagreen, darkslateblue
            , darkslategray, darkturquoise, darkviolet, deeppink
            , deepskyblue, dimgray, dodgerblue, firebrick
            , floralwhite, forestgreen, fuchsia, gainsboro
            , ghostwhite, gold, goldenrod, gray, green, greenyellow
            , honeydew, hotpink, indianred, indigo, ivory, khaki
            , lavender, lavenderblush, lawngreen, lemonchiffon
            , lightblue, lightcoral, lightcyan, lightgoldenrodyellow
            , lightgreen, lightgray, lightpink, lightsalmon
            , lightseagreen, lightskyblue, lightslategray
            , lightsteelblue, lightyellow, lime
            , limegreen, linen, magenta, maroon, mediumaquamarine
            , mediumblue, mediumorchid, mediumpurple, mediumseagreen
            , mediumslateblue, mediumspringgreen, mediumturquoise
            , mediumvioletred, midnightblue, mintcream, mistyrose
            , moccasin, navajowhite, navy, oldlace, olive, olivedrab
            , orange, orangered, orchid, palegoldenrod, palegreen
            , paleturquoise, palevioletred, papayawhip, peachpuff
            , peru, pink, plum, powderblue, purple, red, rosybrown
            , royalblue, saddlebrown, salmon, sandybrown, seagreen
            , seashell, sienna, silver, skyblue, slateblue, slategray
            , snow, springgreen, steelblue, teal, thistle, tomato
            , turquoise, violet, wheat, white, whitesmoke, yellow
            , yellowgreen
            -- * GUI specific colors
            , dialogColor, windowColor, textColor
            -- * Marshalling
            , CColor, fromCColor, toCColor
            ) where

import Foreign.C
import Data.Word
import Data.Bits
import GHC.Read
import Text.Read
import Text.ParserCombinators.ReadPrec
import System.IO.Unsafe(unsafePerformIO)

newtype Color = Color Word deriving Eq

instance Show Color where
    showsPrec d c
        | c == aliceblue           = showString "aliceblue"        
        | c == antiquewhite        = showString "antiquewhite"
        | c == aqua                = showString "aqua"
        | c == aquamarine          = showString "aquamarine"
        | c == azure               = showString "azure"
        | c == beige               = showString "beige"
        | c == bisque              = showString "bisque"
        | c == black               = showString "black"
        | c == blanchedalmond      = showString "blanchedalmond"
        | c == blue                = showString "blue"
        | c == blueviolet          = showString "blueviolet"
        | c == brown               = showString "brown"
        | c == burlywood           = showString "burlywood"
        | c == cadetblue           = showString "cadetblue"
        | c == chartreuse          = showString "chartreuse"
        | c == chocolate           = showString "chocolate"
        | c == coral               = showString "coral"
        | c == cornflower          = showString "cornflower"
        | c == cornsilk            = showString "cornsilk"
        | c == crimson             = showString "crimson"
        | c == cyan                = showString "cyan"
        | c == darkblue            = showString "darkblue"
        | c == darkcyan            = showString "darkcyan"
        | c == darkgoldenrod       = showString "darkgoldenrod"
        | c == darkgray            = showString "darkgray"
        | c == darkgreen           = showString "darkgreen"
        | c == darkkhaki           = showString "darkkhaki"
        | c == darkmagenta         = showString "darkmagenta"
        | c == darkolivegreen      = showString "darkolivegreen"
        | c == darkorange          = showString "darkorange"
        | c == darkorchid          = showString "darkorchid"
        | c == darkred             = showString "darkred"
        | c == darksalmon          = showString "darksalmon"
        | c == darkseagreen        = showString "darkseagreen"
        | c == darkslateblue       = showString "darkslateblue"
        | c == darkslategray       = showString "darkslategray"
        | c == darkturquoise       = showString "darkturquoise"
        | c == darkviolet          = showString "darkviolet"
        | c == deeppink            = showString "deeppink"
        | c == deepskyblue         = showString "deepskyblue"
        | c == dimgray             = showString "dimgray"
        | c == dodgerblue          = showString "dodgerblue"
        | c == firebrick           = showString "firebrick"
        | c == floralwhite         = showString "floralwhite"
        | c == forestgreen         = showString "forestgreen"
        | c == fuchsia             = showString "fuchsia"
        | c == gainsboro           = showString "gainsboro"
        | c == ghostwhite          = showString "ghostwhite"
        | c == gold                = showString "gold"
        | c == goldenrod           = showString "goldenrod"
        | c == gray                = showString "gray"
        | c == green               = showString "green"
        | c == greenyellow         = showString "greenyellow"
        | c == honeydew            = showString "honeydew"
        | c == hotpink             = showString "hotpink"
        | c == indianred           = showString "indianred"
        | c == indigo              = showString "indigo"
        | c == ivory               = showString "ivory"
        | c == khaki               = showString "khaki"
        | c == lavender            = showString "lavender"
        | c == lavenderblush       = showString "lavenderblush"
        | c == lawngreen           = showString "lawngreen"
        | c == lemonchiffon        = showString "lemonchiffon"
        | c == lightblue           = showString "lightblue"
        | c == lightcoral          = showString "lightcoral"
        | c == lightcyan           = showString "lightcyan"
        | c == lightgoldenrodyellow= showString "lightgoldenrodyellow"
        | c == lightgreen          = showString "lightgreen"
        | c == lightgray           = showString "lightgray"
        | c == lightpink           = showString "lightpink"
        | c == lightsalmon         = showString "lightsalmon"
        | c == lightseagreen       = showString "lightseagreen"
        | c == lightskyblue        = showString "lightskyblue"
        | c == lightslategray      = showString "lightslategray"
        | c == lightsteelblue      = showString "lightsteelblue"
        | c == lightyellow         = showString "lightyellow"
        | c == lime                = showString "lime"
        | c == limegreen           = showString "limegreen"
        | c == linen               = showString "linen"
        | c == magenta             = showString "magenta"
        | c == maroon              = showString "maroon"
        | c == mediumaquamarine    = showString "mediumaquamarine"
        | c == mediumblue          = showString "mediumblue"
        | c == mediumorchid        = showString "mediumorchid"
        | c == mediumpurple        = showString "mediumpurple"
        | c == mediumseagreen      = showString "mediumseagreen"
        | c == mediumslateblue     = showString "mediumslateblue"
        | c == mediumspringgreen   = showString "mediumspringgreen"
        | c == mediumturquoise     = showString "mediumturquoise"
        | c == mediumvioletred     = showString "mediumvioletred"
        | c == midnightblue        = showString "midnightblue"
        | c == mintcream           = showString "mintcream"
        | c == mistyrose           = showString "mistyrose"
        | c == moccasin            = showString "moccasin"
        | c == navajowhite         = showString "navajowhite"
        | c == navy                = showString "navy"
        | c == oldlace             = showString "oldlace"
        | c == olive               = showString "olive"
        | c == olivedrab           = showString "olivedrab"
        | c == orange              = showString "orange"
        | c == orangered           = showString "orangered"
        | c == orchid              = showString "orchid"
        | c == palegoldenrod       = showString "palegoldenrod"
        | c == palegreen           = showString "palegreen"
        | c == paleturquoise       = showString "paleturquoise"
        | c == palevioletred       = showString "palevioletred"
        | c == papayawhip          = showString "papayawhip"
        | c == peachpuff           = showString "peachpuff"
        | c == peru                = showString "peru"
        | c == pink                = showString "pink"
        | c == plum                = showString "plum"
        | c == powderblue          = showString "powderblue"
        | c == purple              = showString "purple"
        | c == red                 = showString "red"
        | c == rosybrown           = showString "rosybrown"
        | c == royalblue           = showString "royalblue"
        | c == saddlebrown         = showString "saddlebrown"
        | c == salmon              = showString "salmon"
        | c == sandybrown          = showString "sandybrown"
        | c == seagreen            = showString "seagreen"
        | c == seashell            = showString "seashell"
        | c == sienna              = showString "sienna"
        | c == silver              = showString "silver"
        | c == skyblue             = showString "skyblue"
        | c == slateblue           = showString "slateblue"
        | c == slategray           = showString "slategray"
        | c == snow                = showString "snow"
        | c == springgreen         = showString "springgreen"
        | c == steelblue           = showString "steelblue"
        | c == teal                = showString "teal"
        | c == thistle             = showString "thistle"
        | c == tomato              = showString "tomato"
        | c == turquoise           = showString "turquoise"
        | c == violet              = showString "violet"
        | c == wheat               = showString "wheat"
        | c == white               = showString "white"
        | c == whitesmoke          = showString "whitesmoke"
        | c == yellow              = showString "yellow"
        | c == yellowgreen         = showString "yellowgreen"
        | otherwise               = showParen (d > 0)
                            (showString "rgbColor " . shows (colorRed   c) .
                             showChar   ' '        . shows (colorGreen c) . 
                             showChar   ' '     . shows (colorBlue  c))

instance Read Color where
  readPrec =
      do { Ident "aliceblue"              <- lexP; return aliceblue          } +++
      do { Ident "antiquewhite"      <- lexP; return antiquewhite          } +++
      do { Ident "aqua"              <- lexP; return aqua                  } +++
      do { Ident "aquamarine"      <- lexP; return aquamarine          } +++
      do { Ident "azure"              <- lexP; return azure                  } +++
      do { Ident "beige"              <- lexP; return beige                  } +++
      do { Ident "bisque"              <- lexP; return bisque          } +++
      do { Ident "black"              <- lexP; return black                  } +++      
      do { Ident "blanchedalmond"      <- lexP; return blanchedalmond      } +++
    do { Ident "blue"              <- lexP; return blue                  } +++
    do { Ident "blueviolet"      <- lexP; return blueviolet            } +++
    do { Ident "brown"              <- lexP; return brown                  } +++
    do { Ident "burlywood"              <- lexP; return burlywood          } +++
    do { Ident "cadetblue"              <- lexP; return cadetblue          } +++
    do { Ident "chartreuse"      <- lexP; return chartreuse          } +++
    do { Ident "chocolate"              <- lexP; return chocolate          } +++
    do { Ident "coral"              <- lexP; return coral                  } +++
    do { Ident "cornflower"      <- lexP; return cornflower          } +++
    do { Ident "cornsilk"              <- lexP; return cornsilk          } +++
    do { Ident "crimson"              <- lexP; return crimson          } +++
    do { Ident "cyan"              <- lexP; return cyan                  } +++
    do { Ident "darkblue"            <- lexP; return darkblue            } +++
    do { Ident "darkcyan"            <- lexP; return darkcyan            } +++
    do { Ident "darkgoldenrod"       <- lexP; return darkgoldenrod       } +++
    do { Ident "darkgray"            <- lexP; return darkgray            } +++
    do { Ident "darkgreen"           <- lexP; return darkgreen           } +++
    do { Ident "darkkhaki"           <- lexP; return darkkhaki           } +++
    do { Ident "darkmagenta"         <- lexP; return darkmagenta         } +++
    do { Ident "darkolivegreen"      <- lexP; return darkolivegreen      } +++
    do { Ident "darkorange"          <- lexP; return darkorange          } +++
    do { Ident "darkorchid"          <- lexP; return darkorchid          } +++
    do { Ident "darkred"             <- lexP; return darkred             } +++
    do { Ident "darksalmon"          <- lexP; return darksalmon          } +++
    do { Ident "darkseagreen"        <- lexP; return darkseagreen        } +++
    do { Ident "darkslateblue"       <- lexP; return darkslateblue       } +++
    do { Ident "darkslategray"       <- lexP; return darkslategray       } +++
    do { Ident "darkturquoise"       <- lexP; return darkturquoise       } +++
    do { Ident "darkviolet"          <- lexP; return darkviolet          } +++
    do { Ident "deeppink"            <- lexP; return deeppink            } +++
    do { Ident "deepskyblue"         <- lexP; return deepskyblue         } +++
    do { Ident "dimgray"             <- lexP; return dimgray             } +++
    do { Ident "dodgerblue"          <- lexP; return dodgerblue          } +++
    do { Ident "firebrick"           <- lexP; return firebrick           } +++
    do { Ident "floralwhite"         <- lexP; return floralwhite         } +++
    do { Ident "forestgreen"         <- lexP; return forestgreen         } +++
    do { Ident "fuchsia"             <- lexP; return fuchsia             } +++
    do { Ident "gainsboro"           <- lexP; return gainsboro           } +++
    do { Ident "ghostwhite"          <- lexP; return ghostwhite          } +++
    do { Ident "gold"                <- lexP; return gold                } +++
    do { Ident "goldenrod"           <- lexP; return goldenrod           } +++
    do { Ident "gray"              <- lexP; return gray                  } +++      
    do { Ident "green"              <- lexP; return green                 } +++
    do { Ident "greenyellow"         <- lexP; return greenyellow         } +++
    do { Ident "honeydew"            <- lexP; return honeydew            } +++
    do { Ident "hotpink"             <- lexP; return hotpink             } +++
    do { Ident "indianred"           <- lexP; return indianred           } +++
    do { Ident "indigo"              <- lexP; return indigo              } +++
    do { Ident "ivory"               <- lexP; return ivory               } +++
    do { Ident "khaki"               <- lexP; return khaki               } +++
    do { Ident "lavender"            <- lexP; return lavender            } +++
    do { Ident "lavenderblush"       <- lexP; return lavenderblush       } +++
    do { Ident "lawngreen"           <- lexP; return lawngreen           } +++
    do { Ident "lemonchiffon"        <- lexP; return lemonchiffon        } +++
    do { Ident "lightblue"           <- lexP; return lightblue           } +++
    do { Ident "lightcoral"          <- lexP; return lightcoral          } +++
    do { Ident "lightcyan"           <- lexP; return lightcyan           } +++
    do { Ident "lightgoldenrodyellow"<- lexP; return lightgoldenrodyellow} +++
    do { Ident "lightgreen"          <- lexP; return lightgreen          } +++
    do { Ident "lightgray"           <- lexP; return lightgray           } +++
    do { Ident "lightpink"           <- lexP; return lightpink           } +++
    do { Ident "lightsalmon"         <- lexP; return lightsalmon         } +++
    do { Ident "lightseagreen"       <- lexP; return lightseagreen       } +++
    do { Ident "lightskyblue"        <- lexP; return lightskyblue        } +++
    do { Ident "lightslategray"      <- lexP; return lightslategray      } +++
    do { Ident "lightsteelblue"      <- lexP; return lightsteelblue      } +++
    do { Ident "lightyellow"         <- lexP; return lightyellow         } +++
    do { Ident "lime"                <- lexP; return lime                } +++
    do { Ident "limegreen"           <- lexP; return limegreen           } +++
    do { Ident "linen"               <- lexP; return linen               } +++
      do { Ident "magenta"              <- lexP; return magenta             } +++      
      do { Ident "maroon"              <- lexP; return maroon              } +++
      do { Ident "mediumaquamarine"    <- lexP; return mediumaquamarine    } +++
      do { Ident "mediumblue"          <- lexP; return mediumblue          } +++
      do { Ident "mediumorchid"        <- lexP; return mediumorchid        } +++
      do { Ident "mediumpurple"        <- lexP; return mediumpurple        } +++
      do { Ident "mediumseagreen"      <- lexP; return mediumseagreen      } +++
      do { Ident "mediumslateblue"     <- lexP; return mediumslateblue     } +++
      do { Ident "mediumspringgreen"   <- lexP; return mediumspringgreen   } +++
      do { Ident "mediumturquoise"     <- lexP; return mediumturquoise     } +++
      do { Ident "mediumvioletred"     <- lexP; return mediumvioletred     } +++
      do { Ident "midnightblue"        <- lexP; return midnightblue        } +++
      do { Ident "mintcream"           <- lexP; return mintcream           } +++
      do { Ident "mistyrose"           <- lexP; return mistyrose           } +++
      do { Ident "moccasin"            <- lexP; return moccasin            } +++
      do { Ident "navajowhite"         <- lexP; return navajowhite         } +++
      do { Ident "navy"                <- lexP; return navy                } +++
      do { Ident "oldlace"             <- lexP; return oldlace             } +++
      do { Ident "olive"               <- lexP; return olive               } +++
      do { Ident "olivedrab"           <- lexP; return olivedrab           } +++
      do { Ident "orange"              <- lexP; return orange              } +++
      do { Ident "orangered"           <- lexP; return orangered           } +++
      do { Ident "orchid"              <- lexP; return orchid              } +++
      do { Ident "palegoldenrod"       <- lexP; return palegoldenrod       } +++
      do { Ident "palegreen"           <- lexP; return palegreen           } +++
      do { Ident "paleturquoise"       <- lexP; return paleturquoise       } +++
      do { Ident "palevioletred"       <- lexP; return palevioletred       } +++
      do { Ident "papayawhip"          <- lexP; return papayawhip          } +++
      do { Ident "peachpuff"           <- lexP; return peachpuff           } +++
      do { Ident "peru"                <- lexP; return peru                } +++
      do { Ident "pink"                <- lexP; return pink                } +++
      do { Ident "plum"                <- lexP; return plum                } +++
      do { Ident "powderblue"          <- lexP; return powderblue          } +++
      do { Ident "purple"              <- lexP; return purple              } +++
      do { Ident "red"              <- lexP; return red                  } +++
      do { Ident "rosybrown"           <- lexP; return rosybrown           } +++
      do { Ident "royalblue"           <- lexP; return royalblue           } +++
      do { Ident "saddlebrown"         <- lexP; return saddlebrown         } +++
      do { Ident "salmon"              <- lexP; return salmon              } +++
      do { Ident "sandybrown"          <- lexP; return sandybrown          } +++
      do { Ident "seagreen"            <- lexP; return seagreen            } +++
      do { Ident "seashell"            <- lexP; return seashell            } +++
      do { Ident "sienna"              <- lexP; return sienna              } +++
      do { Ident "silver"              <- lexP; return silver              } +++
      do { Ident "skyblue"             <- lexP; return skyblue             } +++
      do { Ident "slateblue"           <- lexP; return slateblue           } +++
      do { Ident "slategray"           <- lexP; return slategray           } +++
      do { Ident "snow"                <- lexP; return snow                } +++
      do { Ident "springgreen"         <- lexP; return springgreen         } +++
      do { Ident "steelblue"           <- lexP; return steelblue           } +++
      do { Ident "teal"                <- lexP; return teal                } +++
      do { Ident "thistle"             <- lexP; return thistle             } +++
      do { Ident "tomato"              <- lexP; return tomato              } +++
      do { Ident "turquoise"           <- lexP; return turquoise           } +++
      do { Ident "violet"              <- lexP; return violet              } +++
      do { Ident "wheat"               <- lexP; return wheat               } +++
      do { Ident "white"               <- lexP; return white               } +++
      do { Ident "whitesmoke"          <- lexP; return whitesmoke          } +++
      do { Ident "yellow"              <- lexP; return yellow          } +++
      do { Ident "yellowgreen"         <- lexP; return yellowgreen         } +++
    parens
        ( prec 10
          ( do Ident "rgbColor" <- lexP
               r               <- step readPrec
               g               <- step readPrec
               b               <- step readPrec
               return (rgbColor r g b)
          )
        ) +++
    parens
        ( prec 10
          ( do Ident "cmyColor" <- lexP
               r               <- step readPrec
               g               <- step readPrec
               b               <- step readPrec
               return (cmyColor r g b)
           )
         )


-- | Create a color from a red\/green\/blue triple.
rgbColor :: Word8 -> Word8 -> Word8 -> Color
rgbColor r g b = Color (((fromIntegral b) * 0x10000) .|. ((fromIntegral g) * 0x100) .|. fromIntegral r)

-- | Returns a red color component
colorRed   :: Color -> Word8
colorRed   (Color c) = fromIntegral ((c              ) .&. 0xFF)

-- | Returns a green color component
colorGreen :: Color -> Word8
colorGreen (Color c) = fromIntegral ((c `div` 0x100  ) .&. 0xFF)

-- | Returns a blue color component
colorBlue  :: Color -> Word8
colorBlue  (Color c) = fromIntegral ((c `div` 0x10000) .&. 0xFF)

-- | Create a color from a cyan\/magenta\/yellow triple.
cmyColor :: Word8 -> Word8 -> Word8 -> Color
cmyColor c m y = Color ((((fromIntegral y) * 0x10000) .|. ((fromIntegral m) * 0x100) .|. fromIntegral c) `xor` 0xFFFFFF)

-- | Returns a cyan color component
colorCyan :: Color -> Word8
colorCyan (Color c) = fromIntegral ((c              ) .&. 0xFF) `xor` 0xFF

-- | Returns a magenta color component
colorMagenta :: Color -> Word8
colorMagenta (Color c) = fromIntegral ((c `div` 0x100  ) .&. 0xFF) `xor` 0xFF

-- | Returns a yellow color component
colorYellow :: Color -> Word8
colorYellow (Color c) = fromIntegral ((c `div` 0x10000) .&. 0xFF) `xor` 0xFF

-- Default colors.
aliceblue, antiquewhite, aqua, aquamarine, azure, beige,
 bisque, black, blanchedalmond, blue, blueviolet, brown,
 burlywood, cadetblue, chartreuse, chocolate, coral,
 cornflower, cornsilk, crimson, cyan, darkblue, darkcyan,
 darkgoldenrod, darkgray, darkgreen, darkkhaki,
 darkmagenta, darkolivegreen, darkorange, darkorchid,
 darkred, darksalmon, darkseagreen, darkslateblue,
 darkslategray, darkturquoise, darkviolet, deeppink,
 deepskyblue, dimgray, dodgerblue, firebrick,
 floralwhite, forestgreen, fuchsia, gainsboro,
 ghostwhite, gold, goldenrod, gray, green, greenyellow,
 honeydew, hotpink, indianred, indigo, ivory, khaki,
 lavender, lavenderblush, lawngreen, lemonchiffon,
 lightblue, lightcoral, lightcyan, lightgoldenrodyellow,
 lightgreen, lightgray, lightpink, lightsalmon, 
 lightseagreen, lightskyblue, lightslategray,
 lightsteelblue, lightyellow, lime,
 limegreen, linen, magenta, maroon, mediumaquamarine,
 mediumblue, mediumorchid, mediumpurple, mediumseagreen,
 mediumslateblue, mediumspringgreen, mediumturquoise,
 mediumvioletred, midnightblue, mintcream, mistyrose,
 moccasin, navajowhite, navy, oldlace, olive, olivedrab,
 orange, orangered, orchid, palegoldenrod, palegreen,
 paleturquoise, palevioletred, papayawhip, peachpuff,
 peru, pink, plum, powderblue, purple, red, rosybrown,
 royalblue, saddlebrown, salmon, sandybrown, seagreen,
 seashell, sienna, silver, skyblue, slateblue, slategray,
 snow, springgreen, steelblue, teal, thistle, tomato,
 turquoise, violet, wheat, white, whitesmoke, yellow,
 yellowgreen :: Color

aliceblue           = Color 0xFFF8F0
antiquewhite        = Color 0xD7EBFA
aqua                = Color 0xFFFF00
aquamarine          = Color 0xD4FF7F
azure               = Color 0xFFFFF0
beige               = Color 0xDCF5F5
bisque              = Color 0xFFE4C4
black               = Color 0x000000
blanchedalmond      = Color 0xCDEBFF
blue                = Color 0xFF0000
blueviolet          = Color 0xE22B8A
brown               = Color 0x2A2AA5
burlywood           = Color 0x87B8DE
cadetblue           = Color 0xA09E5F
chartreuse          = Color 0x00FF7F
chocolate           = Color 0x1E69D2
coral               = Color 0x507FFF
cornflower          = Color 0xED9564
cornsilk            = Color 0xDCF8FF
crimson             = Color 0x3C14DC
cyan                = Color 0xFFFF00
darkblue            = Color 0x8B0000
darkcyan            = Color 0x8B8B00
darkgoldenrod       = Color 0x0B86B8
darkgray            = Color 0xA9A9A9
darkgreen           = Color 0x006400
darkkhaki           = Color 0xBDB76B
darkmagenta         = Color 0x8B008B
darkolivegreen      = Color 0x2F6B55
darkorange          = Color 0x008CFF
darkorchid          = Color 0xCC3299
darkred             = Color 0x00008B
darksalmon          = Color 0x7A96E9
darkseagreen        = Color 0x8BBC8F
darkslateblue       = Color 0x8B3D48
darkslategray       = Color 0x4F4F2F
darkturquoise       = Color 0xD1CE00
darkviolet          = Color 0xD30094
deeppink            = Color 0x9314FF
deepskyblue         = Color 0xFFBF00
dimgray             = Color 0x696969
dodgerblue          = Color 0xFF901E
firebrick           = Color 0x2222B2
floralwhite         = Color 0xF0FAFF
forestgreen         = Color 0x228B22
fuchsia             = Color 0xFF00FF
gainsboro           = Color 0xDCDCDC
ghostwhite          = Color 0xFFF8F8
gold                = Color 0x00D7FF
goldenrod           = Color 0x20A5DA
gray                = Color 0x808080
green               = Color 0x008000
greenyellow         = Color 0x2FFFAD
honeydew            = Color 0xF0FFF0
hotpink             = Color 0xB469FF
indianred           = Color 0x5C5CCD
indigo              = Color 0x82004B
ivory               = Color 0xF0FFFF
khaki               = Color 0x8CE6F0
lavender            = Color 0xFAE6E6
lavenderblush       = Color 0xF5F0FF
lawngreen           = Color 0x00FC7C
lemonchiffon        = Color 0xCDFAFF
lightblue           = Color 0xE6D8AD
lightcoral          = Color 0x8080F0
lightcyan           = Color 0xFFFFE0
lightgoldenrodyellow= Color 0xD2FAFA
lightgreen          = Color 0x90EE90
lightgray           = Color 0xD3D3D3
lightpink           = Color 0xC1B6FF
lightsalmon         = Color 0x7AA0FF
lightseagreen       = Color 0xAAB220
lightskyblue        = Color 0xFACE87
lightslategray      = Color 0x998877
lightsteelblue      = Color 0xDEC4B0
lightyellow         = Color 0xE0FFFF
lime                = Color 0x00FF00
limegreen           = Color 0x32CD32
linen               = Color 0xE6F0FA
magenta             = Color 0xFF00FF
maroon              = Color 0x000080
mediumaquamarine    = Color 0xAACD66
mediumblue          = Color 0xCD0000
mediumorchid        = Color 0xD355BA
mediumpurple        = Color 0xDB7093
mediumseagreen      = Color 0x71B33C
mediumslateblue     = Color 0xEE687B
mediumspringgreen   = Color 0x9AFA00
mediumturquoise     = Color 0xCCD148
mediumvioletred     = Color 0x8515C7
midnightblue        = Color 0x701919
mintcream           = Color 0xFAFFF5
mistyrose           = Color 0xE1E4FF
moccasin            = Color 0xB5E4FF
navajowhite         = Color 0xADDEFF
navy                = Color 0x800000
oldlace             = Color 0xE6F5FD
olive               = Color 0x008080
olivedrab           = Color 0x238E6B
orange              = Color 0x00A5FF
orangered           = Color 0x0045FF
orchid              = Color 0xD670DA
palegoldenrod       = Color 0xAAE8EE
palegreen           = Color 0x98FB98
paleturquoise       = Color 0xEEEEAF
palevioletred       = Color 0x9370DB
papayawhip          = Color 0xD5EFFF
peachpuff           = Color 0xB9DAFF
peru                = Color 0x3F85CD
pink                = Color 0xCBC0FF
plum                = Color 0xDDA0DD
powderblue          = Color 0xE6E0B0
purple              = Color 0x800080
red                 = Color 0x0000FF
rosybrown           = Color 0x8F8FBC
royalblue           = Color 0xE16941
saddlebrown         = Color 0x13458B
salmon              = Color 0x7280FA
sandybrown          = Color 0x60A4F4
seagreen            = Color 0x578B2E
seashell            = Color 0xEEF5FF
sienna              = Color 0x2D52A0
silver              = Color 0xC0C0C0
skyblue             = Color 0xEBCE87
slateblue           = Color 0xCD5A6A
slategray           = Color 0x908070
snow                = Color 0xFAFAFF
springgreen         = Color 0x7FFF00
steelblue           = Color 0xB48246
teal                = Color 0x808000
thistle             = Color 0xD8BFD8
tomato              = Color 0x4763FF
turquoise           = Color 0xD0E040
violet              = Color 0xEE82EE
wheat               = Color 0xB3DEF5
white               = Color 0xFFFFFF
whitesmoke          = Color 0xF5F5F5
yellow              = Color 0x00FFFF
yellowgreen         = Color 0x32CD9A
 
{-# NOINLINE dialogColor #-}
-- | the default color for dialogs
dialogColor :: Color
dialogColor = fromCColor (unsafePerformIO osGetDialogColor)
foreign import ccall osGetDialogColor :: IO CColor

{-# NOINLINE windowColor #-}
-- | the default color for dialogs
windowColor :: Color
windowColor = fromCColor (unsafePerformIO osGetWindowColor)
foreign import ccall osGetWindowColor :: IO CColor

{-# NOINLINE textColor #-}
-- | the default text color
textColor :: Color
textColor = fromCColor (unsafePerformIO osGetTextColor)
foreign import ccall osGetTextColor :: IO CColor

-- marshalling

type CColor  = CUInt

toCColor :: Color -> CColor
toCColor (Color c) = fromIntegral c

fromCColor :: CColor -> Color
fromCColor cx = Color (fromIntegral cx)
