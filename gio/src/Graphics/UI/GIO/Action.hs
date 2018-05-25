-----------------------------------------------------------------------------------------
{-| Module      :  Action
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Actions represent operations that the user can be perform, along with some information how it
    should be presented in the interface. There are functions which allows to create menu and toolbar items
    from a given action. As well as the event that is fired when the action gets activated, the following also
    gets associated with the action: a label, an accelerator, a tooltip and a toolbar label (usually shorter than label).

    Apart from regular actions, there are check actions, which can be toggled between two states and
    radio actions, of which only one in a group can be in the 'selected' state.
-}
-----------------------------------------------------------------------------------------

module Graphics.UI.GIO.Action
		( Action, action
		, CheckAction, checkAction
		, RadioAction, radioAction, setActionRadioGroup
		, DropDownAction, dropDownAction
		, IsAction, haction
		) where

import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.MenuType
import qualified Graphics.UI.Port as Lib

class IsAction a where
	haction :: a -> ActionHandle

--------------------------------------------------------------------
--  Action
--------------------------------------------------------------------

newtype Action = Action ActionHandle

action :: [Prop Action] -> IO Action
action props = do
	act <- do
		haction <- Lib.createAction
		return (Action haction)
	set act props
	return act

instance IsAction Action where
	haction (Action h) = h

instance Able Action where
  enabled = newStdAttr haction Lib.getActionEnabled Lib.setActionEnabled

instance Commanding Action where
  command = newStdEvent haction Lib.getActionCommandHandler Lib.setActionCommandHandler Lib.setActionCommandDefHandler

instance DynamicUpdate Action where
  update = newStdEvent haction Lib.getActionUpdateHandler Lib.setActionUpdateHandler Lib.setActionUpdateDefHandler

instance HasIcon Action where
  icon  = newStdAttr haction Lib.getActionBitmap Lib.setActionBitmap

instance Tipped Action where
  tooltip = newStdAttr haction Lib.getActionTip Lib.setActionTip

instance Titled Action where
  title = newStdAttr haction Lib.getActionText Lib.setActionText
  shortTitle = newStdAttr haction Lib.getActionShortText Lib.setActionShortText

instance Accelerated Action where
  accel = newStdAttr haction Lib.getActionAccel Lib.setActionAccel

instance Deadly Action where
  destroyWidget t = Lib.destroyAction (haction t)
  destroy         = newStdEvent haction Lib.getActionDestroyHandler Lib.setActionDestroyHandler Lib.setActionDestroyDefHandler

--------------------------------------------------------------------
--  CheckAction
--------------------------------------------------------------------

newtype CheckAction = CheckAction ActionHandle

checkAction :: [Prop CheckAction] -> IO CheckAction
checkAction props = do
	act <- do
		haction <- Lib.createCheckAction
		return (CheckAction haction)
	set act props
	return act

instance IsAction CheckAction where
	haction (CheckAction h) = h

instance Able CheckAction where
  enabled = newStdAttr haction Lib.getActionEnabled Lib.setActionEnabled

instance Commanding CheckAction where
  command = newStdEvent haction Lib.getActionCommandHandler Lib.setActionCommandHandler Lib.setActionCommandDefHandler

instance DynamicUpdate CheckAction where
  update = newStdEvent haction Lib.getActionUpdateHandler Lib.setActionUpdateHandler Lib.setActionUpdateDefHandler

instance HasIcon CheckAction where
  icon  = newStdAttr haction Lib.getActionBitmap Lib.setActionBitmap

instance Tipped CheckAction where
  tooltip = newStdAttr haction Lib.getActionTip Lib.setActionTip

instance Titled CheckAction where
  title = newStdAttr haction Lib.getActionText Lib.setActionText
  shortTitle = newStdAttr haction Lib.getActionShortText Lib.setActionShortText

instance Checked CheckAction where
  checked = newStdAttr haction Lib.getActionChecked Lib.setActionChecked

instance Accelerated CheckAction where
  accel = newStdAttr haction Lib.getActionAccel Lib.setActionAccel

instance Deadly CheckAction where
  destroyWidget t = Lib.destroyAction (haction t)
  destroy            = newStdEvent haction Lib.getActionDestroyHandler Lib.setActionDestroyHandler Lib.setActionDestroyDefHandler

--------------------------------------------------------------------
--  RadioAction
--------------------------------------------------------------------

newtype RadioAction = RadioAction ActionHandle

radioAction :: [Prop RadioAction] -> IO RadioAction
radioAction props = do
	act <- do
		haction <- Lib.createRadioAction
		return (RadioAction haction)
	set act props
	return act

instance IsAction RadioAction where
	haction (RadioAction h) = h

instance Able RadioAction where
  enabled = newStdAttr haction Lib.getActionEnabled Lib.setActionEnabled

instance Commanding RadioAction where
  command = newStdEvent haction Lib.getActionCommandHandler Lib.setActionCommandHandler Lib.setActionCommandDefHandler

instance DynamicUpdate RadioAction where
  update = newStdEvent haction Lib.getActionUpdateHandler Lib.setActionUpdateHandler Lib.setActionUpdateDefHandler

instance HasIcon RadioAction where
  icon  = newStdAttr haction Lib.getActionBitmap Lib.setActionBitmap

instance Tipped RadioAction where
  tooltip = newStdAttr haction Lib.getActionTip Lib.setActionTip

instance Titled RadioAction where
  title = newStdAttr haction Lib.getActionText Lib.setActionText
  shortTitle = newStdAttr haction Lib.getActionShortText Lib.setActionShortText

instance Checked RadioAction where
  checked = newStdAttr haction Lib.getActionChecked Lib.setActionChecked

instance Accelerated RadioAction where
  accel = newStdAttr haction Lib.getActionAccel Lib.setActionAccel

instance Deadly RadioAction where
  destroyWidget t = Lib.destroyAction (haction t)
  destroy         = newStdEvent haction Lib.getActionDestroyHandler Lib.setActionDestroyHandler Lib.setActionDestroyDefHandler

setActionRadioGroup :: [RadioAction] -> IO ()
setActionRadioGroup acts = Lib.setActionRadioGroup (map haction acts)

--------------------------------------------------------------------
--  DropDownAction
--------------------------------------------------------------------

newtype DropDownAction = DropDownAction ActionHandle

dropDownAction :: Menu -> [Prop DropDownAction] -> IO DropDownAction
dropDownAction menu props = do
	act <- do
		haction <- Lib.createDropDownAction (hmenu menu)
		return (DropDownAction haction)
	set act props
	return act

instance IsAction DropDownAction where
	haction (DropDownAction h) = h

instance Able DropDownAction where
  enabled = newStdAttr haction Lib.getActionEnabled Lib.setActionEnabled

instance DynamicUpdate DropDownAction where
  update = newStdEvent haction Lib.getActionUpdateHandler Lib.setActionUpdateHandler Lib.setActionUpdateDefHandler

instance HasIcon DropDownAction where
  icon  = newStdAttr haction Lib.getActionBitmap Lib.setActionBitmap

instance Tipped DropDownAction where
  tooltip = newStdAttr haction Lib.getActionTip Lib.setActionTip

instance Titled DropDownAction where
  title = newStdAttr haction Lib.getActionText Lib.setActionText

instance Deadly DropDownAction where
  destroyWidget t = Lib.destroyAction (haction t)
  destroy         = newStdEvent haction Lib.getActionDestroyHandler Lib.setActionDestroyHandler Lib.setActionDestroyDefHandler
