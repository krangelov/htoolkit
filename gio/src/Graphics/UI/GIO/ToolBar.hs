-----------------------------------------------------------------------------------------
{-| Module      :  ToolBar
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    ToolBar.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.ToolBar
		(
		-- * Creation
		  ToolBar,    toolBar
		, ToolActionButton, toolActionButtonAt, toolActionButton, getToolButtonAction
		, ToolButton, toolButtonAt, toolButton
		, ToolCheckButton, toolCheckButtonAt, toolCheckButton
		, ToolRadioButton, toolRadioButtonAt, toolRadioButton, setToolRadioGroup
		, ToolDropDownButton, toolDropDownButtonAt, toolDropDownButton
		, ToolLine,   toolLineAt,   toolLine
             	) where

import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Menu
import Graphics.UI.GIO.Action
import qualified Graphics.UI.Port as Lib

--------------------------------------------------------------------
--  ToolBar
--------------------------------------------------------------------

data ToolBar = ToolBar {htoolbar :: WindowHandle}

toolBar :: String -> PositionType -> Int -> Int -> Int -> [Prop ToolBar] -> IO ToolBar
toolBar name place band_num band_position offset props = do
	bar <- do hwnd <- Lib.createToolBar name place band_num band_position offset
	          return (ToolBar hwnd)
	set bar props
	return bar

instance Deadly ToolBar where
  destroyWidget t = Lib.destroyToolBar (htoolbar t)
  destroy         = newStdEvent htoolbar Lib.getWindowDestroyHandler Lib.setWindowDestroyHandler Lib.setWindowDestroyDefHandler

instance Countable ToolBar where
  count = readAttr "count" (Lib.getToolBarButtonCount . htoolbar)

--------------------------------------------------------------------
--  ToolButton
--------------------------------------------------------------------

data ToolActionButton a = ToolActionButton !ToolHandle !a
hbutton (ToolActionButton handle action) = handle

toolActionButtonAt :: IsAction a => Maybe Int -> [Prop (ToolActionButton a)] -> a -> ToolBar -> IO (ToolActionButton a)
toolActionButtonAt pos props action (ToolBar hwnd) = do
	btn <- do
		hbtn <- Lib.insertToolButton (haction action) hwnd pos
		return (ToolActionButton hbtn action)
	set btn props
	return btn

toolActionButton :: IsAction a => [Prop (ToolActionButton a)] -> a -> ToolBar -> IO (ToolActionButton a)
toolActionButton = toolActionButtonAt Nothing

getToolButtonAction :: ToolActionButton a -> a
getToolButtonAction (ToolActionButton handle action) = action

instance Able a => Able (ToolActionButton a) where
  enabled = mapAttrObj getToolButtonAction enabled

instance Commanding a => Commanding (ToolActionButton a) where
  command = mapEventObj getToolButtonAction command

instance HasIcon a => HasIcon (ToolActionButton a) where
  icon  = mapAttrObj getToolButtonAction icon

instance Tipped a => Tipped (ToolActionButton a) where
  tooltip = mapAttrObj getToolButtonAction tooltip

instance Titled a => Titled (ToolActionButton a) where
  title = mapAttrObj getToolButtonAction shortTitle

instance Checked a => Checked (ToolActionButton a) where
  checked = mapAttrObj getToolButtonAction checked

instance Positioned (ToolActionButton a) where
  pos = readAttr "pos" (Lib.getToolItemPos . hbutton)

instance Deadly (ToolActionButton a) where
  destroyWidget t = Lib.destroyToolItem (hbutton t)
  destroy         = newStdEvent hbutton Lib.getToolDestroyHandler Lib.setToolDestroyHandler Lib.setToolDestroyDefHandler

--------------------------------------------------------------------
--  ToolButton
--------------------------------------------------------------------

type ToolButton = ToolActionButton Action

toolButtonAt :: Maybe Int -> [Prop ToolButton] -> ToolBar -> IO ToolButton
toolButtonAt pos props toolbar = do
	act <- action []
	toolActionButtonAt pos props act toolbar

toolButton :: [Prop ToolButton] -> ToolBar -> IO ToolButton
toolButton = toolButtonAt Nothing

--------------------------------------------------------------------
--  ToolCheckButton
--------------------------------------------------------------------

type ToolCheckButton = ToolActionButton CheckAction

toolCheckButtonAt :: Maybe Int -> [Prop ToolCheckButton] -> ToolBar -> IO ToolCheckButton
toolCheckButtonAt pos props toolbar = do
	act <- checkAction []
	toolActionButtonAt pos props act toolbar

toolCheckButton :: [Prop ToolCheckButton] -> ToolBar -> IO ToolCheckButton
toolCheckButton = toolCheckButtonAt Nothing

--------------------------------------------------------------------
--  ToolRadioButton
--------------------------------------------------------------------

type ToolRadioButton = ToolActionButton RadioAction

toolRadioButtonAt :: Maybe Int -> [Prop ToolRadioButton] -> ToolBar -> IO ToolRadioButton
toolRadioButtonAt pos props toolbar = do
	act <- radioAction []
	toolActionButtonAt pos props act toolbar

toolRadioButton :: [Prop ToolRadioButton] -> ToolBar -> IO ToolRadioButton
toolRadioButton = toolRadioButtonAt Nothing

-- | The 'setToolRadioGroup' function specifies a set of mutually exclusive options.
setToolRadioGroup :: [ToolRadioButton] -> IO ()
setToolRadioGroup items = setActionRadioGroup (map getToolButtonAction items)

--------------------------------------------------------------------
--  ToolDropDownButton
--------------------------------------------------------------------

type ToolDropDownButton = ToolActionButton DropDownAction

toolDropDownButtonAt :: Maybe Int -> Menu -> [Prop ToolDropDownButton] -> ToolBar -> IO ToolDropDownButton
toolDropDownButtonAt pos menu props toolbar = do
	act <- dropDownAction menu []
	toolActionButtonAt pos props act toolbar

toolDropDownButton :: Menu -> [Prop ToolDropDownButton] -> ToolBar -> IO ToolDropDownButton
toolDropDownButton = toolDropDownButtonAt Nothing

--------------------------------------------------------------------
--  ToolLine
--------------------------------------------------------------------

data ToolLine = ToolLine {hline :: ToolHandle}

toolLineAt :: Maybe Int -> [Prop ToolLine] -> ToolBar -> IO ToolLine
toolLineAt pos props (ToolBar hwnd) = do
	line <- do hline <- Lib.insertToolLine hwnd pos
	           return (ToolLine hline)
	set line props
	return line

toolLine :: [Prop ToolLine] -> ToolBar -> IO ToolLine
toolLine = toolLineAt Nothing

instance Positioned ToolLine where
  pos = readAttr "pos" (Lib.getToolItemPos . hline)

instance Deadly ToolLine where
  destroyWidget t = Lib.destroyToolItem (hline t)
  destroy         = newStdEvent hline Lib.getToolDestroyHandler Lib.setToolDestroyHandler Lib.setToolDestroyDefHandler
