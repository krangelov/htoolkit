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
            , alpha, colorAlpha

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
    showsPrec d c0
        | c == aliceblue           = showColor "aliceblue"        
        | c == antiquewhite        = showColor "antiquewhite"
        | c == aqua                = showColor "aqua"
        | c == aquamarine          = showColor "aquamarine"
        | c == azure               = showColor "azure"
        | c == beige               = showColor "beige"
        | c == bisque              = showColor "bisque"
        | c == black               = showColor "black"
        | c == blanchedalmond      = showColor "blanchedalmond"
        | c == blue                = showColor "blue"
        | c == blueviolet          = showColor "blueviolet"
        | c == brown               = showColor "brown"
        | c == burlywood           = showColor "burlywood"
        | c == cadetblue           = showColor "cadetblue"
        | c == chartreuse          = showColor "chartreuse"
        | c == chocolate           = showColor "chocolate"
        | c == coral               = showColor "coral"
        | c == cornflower          = showColor "cornflower"
        | c == cornsilk            = showColor "cornsilk"
        | c == crimson             = showColor "crimson"
        | c == cyan                = showColor "cyan"
        | c == darkblue            = showColor "darkblue"
        | c == darkcyan            = showColor "darkcyan"
        | c == darkgoldenrod       = showColor "darkgoldenrod"
        | c == darkgray            = showColor "darkgray"
        | c == darkgreen           = showColor "darkgreen"
        | c == darkkhaki           = showColor "darkkhaki"
        | c == darkmagenta         = showColor "darkmagenta"
        | c == darkolivegreen      = showColor "darkolivegreen"
        | c == darkorange          = showColor "darkorange"
        | c == darkorchid          = showColor "darkorchid"
        | c == darkred             = showColor "darkred"
        | c == darksalmon          = showColor "darksalmon"
        | c == darkseagreen        = showColor "darkseagreen"
        | c == darkslateblue       = showColor "darkslateblue"
        | c == darkslategray       = showColor "darkslategray"
        | c == darkturquoise       = showColor "darkturquoise"
        | c == darkviolet          = showColor "darkviolet"
        | c == deeppink            = showColor "deeppink"
        | c == deepskyblue         = showColor "deepskyblue"
        | c == dimgray             = showColor "dimgray"
        | c == dodgerblue          = showColor "dodgerblue"
        | c == firebrick           = showColor "firebrick"
        | c == floralwhite         = showColor "floralwhite"
        | c == forestgreen         = showColor "forestgreen"
        | c == fuchsia             = showColor "fuchsia"
        | c == gainsboro           = showColor "gainsboro"
        | c == ghostwhite          = showColor "ghostwhite"
        | c == gold                = showColor "gold"
        | c == goldenrod           = showColor "goldenrod"
        | c == gray                = showColor "gray"
        | c == green               = showColor "green"
        | c == greenyellow         = showColor "greenyellow"
        | c == honeydew            = showColor "honeydew"
        | c == hotpink             = showColor "hotpink"
        | c == indianred           = showColor "indianred"
        | c == indigo              = showColor "indigo"
        | c == ivory               = showColor "ivory"
        | c == khaki               = showColor "khaki"
        | c == lavender            = showColor "lavender"
        | c == lavenderblush       = showColor "lavenderblush"
        | c == lawngreen           = showColor "lawngreen"
        | c == lemonchiffon        = showColor "lemonchiffon"
        | c == lightblue           = showColor "lightblue"
        | c == lightcoral          = showColor "lightcoral"
        | c == lightcyan           = showColor "lightcyan"
        | c == lightgoldenrodyellow= showColor "lightgoldenrodyellow"
        | c == lightgreen          = showColor "lightgreen"
        | c == lightgray           = showColor "lightgray"
        | c == lightpink           = showColor "lightpink"
        | c == lightsalmon         = showColor "lightsalmon"
        | c == lightseagreen       = showColor "lightseagreen"
        | c == lightskyblue        = showColor "lightskyblue"
        | c == lightslategray      = showColor "lightslategray"
        | c == lightsteelblue      = showColor "lightsteelblue"
        | c == lightyellow         = showColor "lightyellow"
        | c == lime                = showColor "lime"
        | c == limegreen           = showColor "limegreen"
        | c == linen               = showColor "linen"
        | c == magenta             = showColor "magenta"
        | c == maroon              = showColor "maroon"
        | c == mediumaquamarine    = showColor "mediumaquamarine"
        | c == mediumblue          = showColor "mediumblue"
        | c == mediumorchid        = showColor "mediumorchid"
        | c == mediumpurple        = showColor "mediumpurple"
        | c == mediumseagreen      = showColor "mediumseagreen"
        | c == mediumslateblue     = showColor "mediumslateblue"
        | c == mediumspringgreen   = showColor "mediumspringgreen"
        | c == mediumturquoise     = showColor "mediumturquoise"
        | c == mediumvioletred     = showColor "mediumvioletred"
        | c == midnightblue        = showColor "midnightblue"
        | c == mintcream           = showColor "mintcream"
        | c == mistyrose           = showColor "mistyrose"
        | c == moccasin            = showColor "moccasin"
        | c == navajowhite         = showColor "navajowhite"
        | c == navy                = showColor "navy"
        | c == oldlace             = showColor "oldlace"
        | c == olive               = showColor "olive"
        | c == olivedrab           = showColor "olivedrab"
        | c == orange              = showColor "orange"
        | c == orangered           = showColor "orangered"
        | c == orchid              = showColor "orchid"
        | c == palegoldenrod       = showColor "palegoldenrod"
        | c == palegreen           = showColor "palegreen"
        | c == paleturquoise       = showColor "paleturquoise"
        | c == palevioletred       = showColor "palevioletred"
        | c == papayawhip          = showColor "papayawhip"
        | c == peachpuff           = showColor "peachpuff"
        | c == peru                = showColor "peru"
        | c == pink                = showColor "pink"
        | c == plum                = showColor "plum"
        | c == powderblue          = showColor "powderblue"
        | c == purple              = showColor "purple"
        | c == red                 = showColor "red"
        | c == rosybrown           = showColor "rosybrown"
        | c == royalblue           = showColor "royalblue"
        | c == saddlebrown         = showColor "saddlebrown"
        | c == salmon              = showColor "salmon"
        | c == sandybrown          = showColor "sandybrown"
        | c == seagreen            = showColor "seagreen"
        | c == seashell            = showColor "seashell"
        | c == sienna              = showColor "sienna"
        | c == silver              = showColor "silver"
        | c == skyblue             = showColor "skyblue"
        | c == slateblue           = showColor "slateblue"
        | c == slategray           = showColor "slategray"
        | c == snow                = showColor "snow"
        | c == springgreen         = showColor "springgreen"
        | c == steelblue           = showColor "steelblue"
        | c == teal                = showColor "teal"
        | c == thistle             = showColor "thistle"
        | c == tomato              = showColor "tomato"
        | c == turquoise           = showColor "turquoise"
        | c == violet              = showColor "violet"
        | c == wheat               = showColor "wheat"
        | c == white               = showColor "white"
        | c == whitesmoke          = showColor "whitesmoke"
        | c == yellow              = showColor "yellow"
        | c == yellowgreen         = showColor "yellowgreen"
        | otherwise                = let showRGB = showString "rgbColor " . shows (colorRed   c) .
                                                   showChar   ' '         . shows (colorGreen c) . 
                                                   showChar   ' '         . shows (colorBlue  c)
                                     in showParen (d > 0)
                                                  (if a == 0
                                                     then showRGB
                                                     else showRGB . showString " `alpha` " . shows a)
        where
          a = colorAlpha c0
          c = alpha c0 0

          showColor name
            | a == 0    = showString name
            | otherwise = showParen (d > 0) (showString name . showString " `alpha` " . shows a)

instance Read Color where
  readPrec =
    do { Ident "aliceblue"           <- lexP; returnColor aliceblue           } +++
    do { Ident "antiquewhite"        <- lexP; returnColor antiquewhite        } +++
    do { Ident "aqua"                <- lexP; returnColor aqua                } +++
    do { Ident "aquamarine"          <- lexP; returnColor aquamarine          } +++
    do { Ident "azure"               <- lexP; returnColor azure               } +++
    do { Ident "beige"               <- lexP; returnColor beige               } +++
    do { Ident "bisque"              <- lexP; returnColor bisque              } +++
    do { Ident "black"               <- lexP; returnColor black               } +++      
    do { Ident "blanchedalmond"      <- lexP; returnColor blanchedalmond      } +++
    do { Ident "blue"                <- lexP; returnColor blue                } +++
    do { Ident "blueviolet"          <- lexP; returnColor blueviolet          } +++
    do { Ident "brown"               <- lexP; returnColor brown               } +++
    do { Ident "burlywood"           <- lexP; returnColor burlywood           } +++
    do { Ident "cadetblue"           <- lexP; returnColor cadetblue           } +++
    do { Ident "chartreuse"          <- lexP; returnColor chartreuse          } +++
    do { Ident "chocolate"           <- lexP; returnColor chocolate           } +++
    do { Ident "coral"               <- lexP; returnColor coral               } +++
    do { Ident "cornflower"          <- lexP; returnColor cornflower          } +++
    do { Ident "cornsilk"            <- lexP; returnColor cornsilk            } +++
    do { Ident "crimson"             <- lexP; returnColor crimson             } +++
    do { Ident "cyan"                <- lexP; returnColor cyan                } +++
    do { Ident "darkblue"            <- lexP; returnColor darkblue            } +++
    do { Ident "darkcyan"            <- lexP; returnColor darkcyan            } +++
    do { Ident "darkgoldenrod"       <- lexP; returnColor darkgoldenrod       } +++
    do { Ident "darkgray"            <- lexP; returnColor darkgray            } +++
    do { Ident "darkgreen"           <- lexP; returnColor darkgreen           } +++
    do { Ident "darkkhaki"           <- lexP; returnColor darkkhaki           } +++
    do { Ident "darkmagenta"         <- lexP; returnColor darkmagenta         } +++
    do { Ident "darkolivegreen"      <- lexP; returnColor darkolivegreen      } +++
    do { Ident "darkorange"          <- lexP; returnColor darkorange          } +++
    do { Ident "darkorchid"          <- lexP; returnColor darkorchid          } +++
    do { Ident "darkred"             <- lexP; returnColor darkred             } +++
    do { Ident "darksalmon"          <- lexP; returnColor darksalmon          } +++
    do { Ident "darkseagreen"        <- lexP; returnColor darkseagreen        } +++
    do { Ident "darkslateblue"       <- lexP; returnColor darkslateblue       } +++
    do { Ident "darkslategray"       <- lexP; returnColor darkslategray       } +++
    do { Ident "darkturquoise"       <- lexP; returnColor darkturquoise       } +++
    do { Ident "darkviolet"          <- lexP; returnColor darkviolet          } +++
    do { Ident "deeppink"            <- lexP; returnColor deeppink            } +++
    do { Ident "deepskyblue"         <- lexP; returnColor deepskyblue         } +++
    do { Ident "dimgray"             <- lexP; returnColor dimgray             } +++
    do { Ident "dodgerblue"          <- lexP; returnColor dodgerblue          } +++
    do { Ident "firebrick"           <- lexP; returnColor firebrick           } +++
    do { Ident "floralwhite"         <- lexP; returnColor floralwhite         } +++
    do { Ident "forestgreen"         <- lexP; returnColor forestgreen         } +++
    do { Ident "fuchsia"             <- lexP; returnColor fuchsia             } +++
    do { Ident "gainsboro"           <- lexP; returnColor gainsboro           } +++
    do { Ident "ghostwhite"          <- lexP; returnColor ghostwhite          } +++
    do { Ident "gold"                <- lexP; returnColor gold                } +++
    do { Ident "goldenrod"           <- lexP; returnColor goldenrod           } +++
    do { Ident "gray"                <- lexP; returnColor gray                } +++      
    do { Ident "green"               <- lexP; returnColor green               } +++
    do { Ident "greenyellow"         <- lexP; returnColor greenyellow         } +++
    do { Ident "honeydew"            <- lexP; returnColor honeydew            } +++
    do { Ident "hotpink"             <- lexP; returnColor hotpink             } +++
    do { Ident "indianred"           <- lexP; returnColor indianred           } +++
    do { Ident "indigo"              <- lexP; returnColor indigo              } +++
    do { Ident "ivory"               <- lexP; returnColor ivory               } +++
    do { Ident "khaki"               <- lexP; returnColor khaki               } +++
    do { Ident "lavender"            <- lexP; returnColor lavender            } +++
    do { Ident "lavenderblush"       <- lexP; returnColor lavenderblush       } +++
    do { Ident "lawngreen"           <- lexP; returnColor lawngreen           } +++
    do { Ident "lemonchiffon"        <- lexP; returnColor lemonchiffon        } +++
    do { Ident "lightblue"           <- lexP; returnColor lightblue           } +++
    do { Ident "lightcoral"          <- lexP; returnColor lightcoral          } +++
    do { Ident "lightcyan"           <- lexP; returnColor lightcyan           } +++
    do { Ident "lightgoldenrodyellow"<- lexP; returnColor lightgoldenrodyellow} +++
    do { Ident "lightgreen"          <- lexP; returnColor lightgreen          } +++
    do { Ident "lightgray"           <- lexP; returnColor lightgray           } +++
    do { Ident "lightpink"           <- lexP; returnColor lightpink           } +++
    do { Ident "lightsalmon"         <- lexP; returnColor lightsalmon         } +++
    do { Ident "lightseagreen"       <- lexP; returnColor lightseagreen       } +++
    do { Ident "lightskyblue"        <- lexP; returnColor lightskyblue        } +++
    do { Ident "lightslategray"      <- lexP; returnColor lightslategray      } +++
    do { Ident "lightsteelblue"      <- lexP; returnColor lightsteelblue      } +++
    do { Ident "lightyellow"         <- lexP; returnColor lightyellow         } +++
    do { Ident "lime"                <- lexP; returnColor lime                } +++
    do { Ident "limegreen"           <- lexP; returnColor limegreen           } +++
    do { Ident "linen"               <- lexP; returnColor linen               } +++
    do { Ident "magenta"             <- lexP; returnColor magenta             } +++      
    do { Ident "maroon"              <- lexP; returnColor maroon              } +++
    do { Ident "mediumaquamarine"    <- lexP; returnColor mediumaquamarine    } +++
    do { Ident "mediumblue"          <- lexP; returnColor mediumblue          } +++
    do { Ident "mediumorchid"        <- lexP; returnColor mediumorchid        } +++
    do { Ident "mediumpurple"        <- lexP; returnColor mediumpurple        } +++
    do { Ident "mediumseagreen"      <- lexP; returnColor mediumseagreen      } +++
    do { Ident "mediumslateblue"     <- lexP; returnColor mediumslateblue     } +++
    do { Ident "mediumspringgreen"   <- lexP; returnColor mediumspringgreen   } +++
    do { Ident "mediumturquoise"     <- lexP; returnColor mediumturquoise     } +++
    do { Ident "mediumvioletred"     <- lexP; returnColor mediumvioletred     } +++
    do { Ident "midnightblue"        <- lexP; returnColor midnightblue        } +++
    do { Ident "mintcream"           <- lexP; returnColor mintcream           } +++
    do { Ident "mistyrose"           <- lexP; returnColor mistyrose           } +++
    do { Ident "moccasin"            <- lexP; returnColor moccasin            } +++
    do { Ident "navajowhite"         <- lexP; returnColor navajowhite         } +++
    do { Ident "navy"                <- lexP; returnColor navy                } +++
    do { Ident "oldlace"             <- lexP; returnColor oldlace             } +++
    do { Ident "olive"               <- lexP; returnColor olive               } +++
    do { Ident "olivedrab"           <- lexP; returnColor olivedrab           } +++
    do { Ident "orange"              <- lexP; returnColor orange              } +++
    do { Ident "orangered"           <- lexP; returnColor orangered           } +++
    do { Ident "orchid"              <- lexP; returnColor orchid              } +++
    do { Ident "palegoldenrod"       <- lexP; returnColor palegoldenrod       } +++
    do { Ident "palegreen"           <- lexP; returnColor palegreen           } +++
    do { Ident "paleturquoise"       <- lexP; returnColor paleturquoise       } +++
    do { Ident "palevioletred"       <- lexP; returnColor palevioletred       } +++
    do { Ident "papayawhip"          <- lexP; returnColor papayawhip          } +++
    do { Ident "peachpuff"           <- lexP; returnColor peachpuff           } +++
    do { Ident "peru"                <- lexP; returnColor peru                } +++
    do { Ident "pink"                <- lexP; returnColor pink                } +++
    do { Ident "plum"                <- lexP; returnColor plum                } +++
    do { Ident "powderblue"          <- lexP; returnColor powderblue          } +++
    do { Ident "purple"              <- lexP; returnColor purple              } +++
    do { Ident "red"                 <- lexP; returnColor red                 } +++
    do { Ident "rosybrown"           <- lexP; returnColor rosybrown           } +++
    do { Ident "royalblue"           <- lexP; returnColor royalblue           } +++
    do { Ident "saddlebrown"         <- lexP; returnColor saddlebrown         } +++
    do { Ident "salmon"              <- lexP; returnColor salmon              } +++
    do { Ident "sandybrown"          <- lexP; returnColor sandybrown          } +++
    do { Ident "seagreen"            <- lexP; returnColor seagreen            } +++
    do { Ident "seashell"            <- lexP; returnColor seashell            } +++
    do { Ident "sienna"              <- lexP; returnColor sienna              } +++
    do { Ident "silver"              <- lexP; returnColor silver              } +++
    do { Ident "skyblue"             <- lexP; returnColor skyblue             } +++
    do { Ident "slateblue"           <- lexP; returnColor slateblue           } +++
    do { Ident "slategray"           <- lexP; returnColor slategray           } +++
    do { Ident "snow"                <- lexP; returnColor snow                } +++
    do { Ident "springgreen"         <- lexP; returnColor springgreen         } +++
    do { Ident "steelblue"           <- lexP; returnColor steelblue           } +++
    do { Ident "teal"                <- lexP; returnColor teal                } +++
    do { Ident "thistle"             <- lexP; returnColor thistle             } +++
    do { Ident "tomato"              <- lexP; returnColor tomato              } +++
    do { Ident "turquoise"           <- lexP; returnColor turquoise           } +++
    do { Ident "violet"              <- lexP; returnColor violet              } +++
    do { Ident "wheat"               <- lexP; returnColor wheat               } +++
    do { Ident "white"               <- lexP; returnColor white               } +++
    do { Ident "whitesmoke"          <- lexP; returnColor whitesmoke          } +++
    do { Ident "yellow"              <- lexP; returnColor yellow              } +++
    do { Ident "yellowgreen"         <- lexP; returnColor yellowgreen         } +++
    parens
        ( prec 10
          ( do Ident "rgbColor" <- lexP
               r                <- step readPrec
               g                <- step readPrec
               b                <- step readPrec
               returnColor (rgbColor r g b)
          )
        ) +++
    parens
        ( prec 10
          ( do Ident "cmyColor" <- lexP
               r                <- step readPrec
               g                <- step readPrec
               b                <- step readPrec
               returnColor (cmyColor r g b)
           )
         )
    where
      returnColor col =
        do Punc "`"      <- lexP
           Ident "alpha" <- lexP
           Punc "`"      <- lexP
           a             <- step readPrec
           return (col `alpha` a)
        +++
        do return col


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

alpha :: Color -> Word8 -> Color
alpha (Color c) a = Color (((fromIntegral a) * 0x1000000) .|. (c .&. 0xFFFFFF))

colorAlpha :: Color -> Word8
colorAlpha (Color c) = fromIntegral ((c `div` 0x1000000) .&. 0xFF)

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
