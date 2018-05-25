module Main where

import qualified Data.Map as Map
import Graphics.UI.GIO

main = start "Fonts" "1.0" SDI [] $ do
	w <- window []
	set w [on paint =: onPaint w, on resize =: \_ -> repaint w]
	names <- getFontNames
	styles <- popup [] w
	sizes  <- popup [] w
	fonts  <- popup [items =: map (\name -> (name, onSetFontName w styles sizes name)) names] w
	set w [layout =: hfill fonts <<< hfill styles <<< hfill sizes, size =: Size 400 200]
	showWindow w
	where
		onSetFontName w styles popSizes name = do
			variants <- getFontVariants name 1 40
			set styles [items =: Map.foldrWithKey (\w_s sizes items -> styleToItem w_s sizes : items) [] variants]
			where
				styleToItem (weight,style) sizes =
					(variantToString weight style, onSetFontStyle popSizes w name weight style sizes)
			
		onSetFontStyle popSizes w name weight style sizes = 
			set popSizes [items =: map sizeToItem sizes]
			where
				sizeToItem size =
					(show size, onSetFontSize w (FontDef { fontName=name
					                                     , fontSize=size
					                                     , fontWeight=weight
					                                     , fontStyle=style
					                                     , fontUnderline=False
					                                     , fontStrikeOut=False
					                                     }))

		onSetFontSize :: Window -> FontDef -> IO ()
		onSetFontSize w fontdef = do
			myfont <- createFont fontdef
			set w [font =: myfont]
			repaint w
			
		onPaint w can _ _ = do
			size <- get w view
			metrics <- get can canvasFontMetrics
			width   <- get can (canvasFontStringWidth text)
			let Rect l t r b = centralRect (rectOfSize size) (Size width (fontLineHeight metrics))
			drawString (Point l (t+fontAscent metrics)) text can
			where
				text = "Hello, world!"
		
variantToString weight style = weightToString weight ++ " " ++ styleToString style
	where
		weightToString 100 = "thin"
		weightToString 200 = "ultra light"
		weightToString 300 = "light"
		weightToString 400 = "regular"
		weightToString 500 = "medium"
		weightToString 600 = "demi bold"
		weightToString 700 = "bold"
		weightToString 800 = "ultra bold"
		weightToString 900 = "black"

		styleToString Roman   = ""
		styleToString Italic  = "italic"
		styleToString Oblique = "oblique"
