{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    This module contains interface to all controls supported from GIO.
    A control is a child window an application uses in conjunction with
    another window to perform simple input and output tasks.
    The controls provides the user with the means to type text, choose options,
    and execute an actions.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Controls(
                               -- * Button
                               -- | A button control is a small, rectangular child window that can be
                               -- clicked on and off. Buttons can be labeled or appear without text.
                               -- A button typically changes appearance when the user clicks it.
                                 Button, button

                               -- * Label
                               -- | A label control simply displays a text string and it can be used
                               -- to label other controls. A label control can\'t take input and can\'t provide output.
                               , Label, label

                               -- * Entry
                               -- | An entry control is a rectangular widget in which the user can enter text.
                               , Entry, entry, readOnly, password

                               -- * DateEntry
                               -- | A date entry control is a rectangular widget in which the user can enter a date.
                               , DateEntry, dateEntry

                               -- * Choice & Popup
                               -- | The choice control consists of a list box combined with a label control.
                               -- The list box of the control drops down only when the user selects 
                               -- the drop-down arrow next to the label. The currently selected item in the 
                               -- list box is displayed in the label control. When the user 
                               -- selects an item from the popup list, the 'command' event is generated. 
                               -- The application can assign a specific value to each item to identify it. 
                               -- If the value type is an instance of 'Eq', then the application can use
                               -- the 'value' attribute to select the item by its value. The popup control 
                               -- is a special kind of choice control where the value assigned to each item 
                               -- is of (IO ()) type. Each time the user selects an item, 
                               -- its IO action is executed.
                               , Choice, Popup, popup, choice

                               -- * SelectionList & ListBox
                               -- | A selection list displays a list of items, that the user can view and select.
                               -- In a single-selection list, the user can select only one item while in
                               -- a multiple-selection list, a range of items can be selected. When the user 
                               -- selects an item from the list box, it becomes highlighted and the 
                               -- 'command' event is generated. The application can assign a specific value to 
                               -- each item to identify it. If the value type is an instance of 'Eq', 
                               -- then the application can use the 'value' attribute to specify the set of 
                               -- selected items. The list box control is a special kind of 
                               -- selection list control where the value assigned to each item is of (IO ()) type. 
                               -- Each time the user selects an item, its IO action is executed.
                               , SelectionList, ListBox, selectionList, selectionCheckList, listBox

                               -- * Slider
                               -- | A slider control is a widget containing a slider and tick marks.
                               -- When the user moves the slider, using either the mouse or the direction keys,
                               -- the control generates the 'command' event to indicate the change.
                               -- Slider controls are useful when you want the user to select a discrete value 
                               -- in a given range. The slider can be vertical or horizontal.
                               , Slider, hslider, vslider
                               
                               -- * TrackBar
                               -- | An track bar control is a pair of arrow buttons that the user can click 
                               -- to increment or decrement the value value displayed in another control.
                               , TrackBar, hTrackBar, vTrackBar, increment, decrement
                               
                               -- * ProgressBar
                               -- | A progress bar control is a window that an application can use to 
                               -- indicate the progress of a lengthy operation. It consists of a rectangle 
                               -- that is gradually filled, from left to right, with the system specified color
                               -- as an operation progresses. A progress bar control has a range and a current 
                               -- position. The range represents the entire duration of the operation, and the 
                               -- current position represents the progress the application has made toward 
                               -- completing the operation.
                               , ProgressBar, hProgressBar, vProgressBar
                               
                               -- * CheckBox
                               -- | A check box consists of a square box and a label, that indicates a choice 
                               -- the user can make by selecting the check. Each CheckBox has two states: 
                               -- checked (a check mark inside the box) or cleared (no check mark). The
                               -- state is controled from the 'checked' attribute. Repeatedly clicking a 
                               -- check box toggles it from checked to cleared and back again.
                               , CheckBox, checkBox
                               
                               -- * RadioBox
                               -- | A radio button consists of a round button and a label that indicates a 
                               -- choice the user can make by selecting the button. An application typically 
                               -- uses radio buttons in a group box to permit the user to choose from a set 
                               -- of mutually exclusive options. The group is defined with 'setRadioBoxGroup'
                               -- function. When the user selects an radio button from group then the system 
                               -- automatically sets its check state to 'checked' and clears all other 
                               -- buttons within the same group.
                               , RadioBox, radioBox, setRadioBoxGroup
                               
                               -- * GroupBox
                               -- | A group box is a rectangle that surrounds a set of controls, such as 
                               -- check boxes or radio buttons, with application-defined label in its upper 
                               -- left corner. The sole purpose of a group box is to organize controls related 
                               -- by a common purpose (usually indicated by the label).
                               , GroupBox, groupBox
                               
                               -- * CompoundControl
                               , CompoundControl, compoundControl

                               -- * Notebook
                               -- | A notebook control is analogous to the dividers in a real notebook.
                               -- By using a notebook control, an application can define multiple pages for
                               -- the same area of a window or dialog box. Each page is represented from
                               -- 'NotebookPage' type and is created from 'notebookPageAt' or 'notebookPage'
                               -- function.
                               , Notebook, notebook, labelsPosition, selectedPage
                               , NotebookPage, notebookPageAt, notebookPage

                               -- * Splitter
                               , Splitter, hSplitter, vSplitter
                               ) where

import qualified Graphics.UI.Port as Port
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events
import Graphics.UI.GIO.Canvas
import Graphics.UI.GIO.Layout
import Control.Monad(when, mapM_, filterM)
import System.Time(ClockTime)

--------------------------------------------------------------------
--  Helper functions
--------------------------------------------------------------------

insertAt :: Int -> a -> [a] -> [a]
insertAt pos v [] = []
insertAt pos v (x:xs)
  | pos == 0  = v : xs
  | otherwise = x : insertAt (pos-1) v xs

removeAt :: Int -> [a] -> [a]
removeAt pos [] = []
removeAt pos (x:xs)
  | pos == 0  = xs
  | otherwise = x : removeAt (pos-1) xs
      	  
--------------------------------------------------------------------
--  Label
--------------------------------------------------------------------
-- | A simple text label.
data Label    = Label { lhandle :: !WindowHandle
                      , lparent :: !WindowHandle
                      , lfont   :: Var Font
                      }

-- | Create a text label.
label :: Container w => [Prop Label] -> w -> IO Label
label props w
  = do lab <- do hlab <- Port.createLabel (hwindow w)
                 lfont <- newVar Port.defaultFont
                 return (Label hlab (hwindow w) lfont)
       set lab props
       return lab

instance Titled Label where
  title  = newStdAttr lhandle Port.getLabelText Port.setLabelText

instance HasFont Label where
  font  = newAttr (getVar . lfont) (\w font -> Port.changeLabelFont (lhandle w) font >> setVar (lfont w) font)

instance Dimensions Label where
  frame = newStdAttr lhandle Port.getControlFrame Port.moveResizeControl
                  
instance Able Label where
  enabled = newStdAttr lhandle Port.getControlEnabled Port.setControlEnabled
  
instance Tipped Label where
  tooltip = newStdAttr lhandle Port.getControlTip Port.setControlTip

instance Control Label where
  pack = stdPack lparent lhandle Port.getLabelRequestSize

--------------------------------------------------------------------
--  Button
--------------------------------------------------------------------
-- | A standard push button.
data Button   = Button { bhandle :: !WindowHandle
                       , bparent :: !WindowHandle
                       , bfont   :: Var Font
                       }

-- | Create a button.
button :: Container w => [Prop Button] -> w -> IO Button
button props w
  = do but <- do hbut <- Port.createButton (hwindow w)
                 bfont <- newVar Port.defaultFont
                 return (Button hbut (hwindow w) bfont)
       set but props
       return but

instance Titled Button where
  title = newStdAttr bhandle Port.getButtonText Port.setButtonText

instance HasFont Button where
  font  = newAttr (getVar . bfont) (\w font -> Port.changeButtonFont (bhandle w) font >> setVar (bfont w) font)

instance Dimensions Button where
  frame = newStdAttr bhandle Port.getControlFrame Port.moveResizeControl

instance Able Button where
  enabled = newStdAttr bhandle Port.getControlEnabled Port.setControlEnabled
  
instance Tipped Button where
  tooltip = newStdAttr bhandle Port.getControlTip Port.setControlTip

instance Control Button where
  pack = stdPack bparent bhandle Port.getButtonRequestSize
             
instance Commanding Button where
  command = newStdEvent bhandle Port.getControlCommandHandler Port.setControlCommandHandler Port.setControlCommandDefHandler

--------------------------------------------------------------------
--  Entry
--------------------------------------------------------------------
-- | A standard text entry control.
data Entry   = Entry { ehandle :: !WindowHandle
                     , eparent :: !WindowHandle
                     , efont   :: Var Font
                     }

-- | Create an entry.
entry :: Container w => [Prop Entry] -> w -> IO Entry
entry props w
  = do e <- do hentry <- Port.createEdit (hwindow w)
               efont  <- newVar Port.defaultFont
               return (Entry hentry (hwindow w) efont)
       set e props
       return e

instance Titled Entry where
  title  = newStdAttr ehandle Port.getEditText Port.setEditText

instance HasFont Entry where
  font  = newAttr (getVar . efont) (\w font -> Port.changeEditBoxFont (ehandle w) font >> setVar (efont w) font)

instance Dimensions Entry where
  frame = newStdAttr ehandle Port.getControlFrame Port.moveResizeControl

instance Able Entry where
  enabled = newStdAttr ehandle Port.getControlEnabled Port.setControlEnabled
  
instance Tipped Entry where
  tooltip = newStdAttr ehandle Port.getControlTip Port.setControlTip

instance Control Entry where
  pack = stdPack eparent ehandle Port.getEditRequestSize
  
instance Valued Entry String where
  value = title
    
-- | Determines if the user can edit the text in the editable widget or not. 
readOnly :: Attr Entry Bool    
readOnly = newStdAttr ehandle Port.getEditReadOnly Port.setEditReadOnly

-- | Determines whether the entry is used to edit a password or a normal text.
password :: Attr Entry Bool
password = newStdAttr ehandle Port.getEditPassword Port.setEditPassword

--------------------------------------------------------------------
--  DateEntry
--------------------------------------------------------------------
-- | A date entry control.
data DateEntry = DateEntry
           { dehandle :: !WindowHandle
           , deparent :: !WindowHandle
           }

-- | Create an entry.
dateEntry :: Container w => [Prop DateEntry] -> w -> IO DateEntry
dateEntry props w = do
    de <- do hentry <- Port.createDateEntry (hwindow w)
             return (DateEntry hentry (hwindow w))
    set de props
    return de

instance Able DateEntry where
  enabled = newStdAttr dehandle Port.getControlEnabled Port.setControlEnabled

instance Tipped DateEntry where
  tooltip = newStdAttr dehandle Port.getControlTip Port.setControlTip

instance Control DateEntry where
  pack = stdPack deparent dehandle Port.getDateEntryRequestSize
  
instance Valued DateEntry ClockTime where
  value = newStdAttr dehandle Port.getDateEntryValue Port.setDateEntryValue

--------------------------------------------------------------------
-- Choice & Popup
--------------------------------------------------------------------}

-- | A choice control.
data Choice a = Choice { chhandle :: !WindowHandle
                       , chparent :: !WindowHandle
                       , chitems  :: Var [(String,a)]
                       }

-- | A popup selection box is a special kind of choice control. The default
-- command handler automatically calls a handler associated as a value to the
-- selected item.
type Popup = Choice (IO ())

-- | Create a popup selection box.
choice :: Container w => [Prop (Choice a)] -> w -> IO (Choice a)
choice props w
  = do p <- do hpop <- Port.createPopUp (hwindow w)
               chitems <- newVar []
               return (Choice hpop (hwindow w) chitems)
       set p props
       return p

-- | Create a popup selection box.
popup :: Container w => [Prop Popup] -> w -> IO Popup
popup props w = do
  c <- choice props w
  set c [on command =: popupCommand c]
  return c
  where 
    -- default command handler
    popupCommand :: Popup -> IO ()
    popupCommand p
      = do i  <- get p selected
           xs <- getVar (chitems p)
           when (i>=0 && i < length xs) (snd (xs!!i))  -- invoke appropiate handler

instance Countable (Choice a) where
  count = readAttr "count" (fmap length . getVar . chitems)

instance CommandItems Choice where
  items
    = newAttr (\w -> getVar (chitems w))
              (\w xs -> do Port.removeAllPopUpItems (chhandle w)
                           mapM_ (Port.appendPopUpItem (chhandle w) . fst) xs
                           setVar (chitems w) xs
                           Port.setPopUpSelection (chhandle w) 0)

  appendItem p item@(title,action) = do
    items <- takeVar (chitems p)
    Port.appendPopUpItem (chhandle p) title
    putVar (chitems p) (items++[item])

  insertItem p pos item@(title,action) = do
    items <- takeVar (chitems p)
    Port.insertPopUpItem (chhandle p) pos title
    putVar (chitems p) (insertAt pos item items)

  removeItem p pos = do
    items <- takeVar (chitems p)
    Port.removePopUpItem (chhandle p) pos
    putVar (chitems p) (removeAt pos items)

  removeAllItems p = do
    items <- takeVar (chitems p)
    Port.removeAllPopUpItems (chhandle p)
    putVar (chitems p) []

instance SingleSelect (Choice a) where
  selected = newStdAttr chhandle Port.getPopUpSelection Port.setPopUpSelection

instance Dimensions (Choice a) where
  frame = newStdAttr chhandle Port.getControlFrame Port.moveResizeControl

instance Able (Choice a) where
  enabled = newStdAttr chhandle Port.getControlEnabled Port.setControlEnabled

instance Tipped (Choice a) where
  tooltip = newStdAttr chhandle Port.getControlTip Port.setControlTip

instance Control (Choice a) where
  pack = stdPack chparent chhandle Port.getPopUpRequestSize

instance Commanding (Choice a) where
  command = newStdEvent chhandle Port.getControlCommandHandler Port.setControlCommandHandler Port.setControlCommandDefHandler

instance Eq a => Valued (Choice a) (Maybe a) where
  value = newAttr getValue setValue
    where
      getValue c = do
        i  <- get c selected
        xs <- getVar (chitems c)
        return (if i>=0 && i < length xs
                then Just (snd (xs!!i))
                else Nothing)
          
      setValue c (Just v) = do
        xs <- getVar (chitems c)
        set c [selected =: findValue v 0 xs]
      setValue c Nothing = do
        set c [selected =: -1]
        
      findValue v n [] = -1
      findValue v n ((_,v'):vs)
        | v == v'   = n
        | otherwise = findValue v (n+1) vs

--------------------------------------------------------------------
-- SelectionList & ListBox
--------------------------------------------------------------------

-- | A selection list control
data SelectionList a = SelectionList
	{ slIsMulti :: Bool
	, slhandle  :: !WindowHandle
	, slparent  :: !WindowHandle
	, slitems   :: Var [(String,a)]
	}

-- | A list box is a special kind of selection list control.  The default
-- command handler automatically calls a handler associated as a value to the
-- selected item.                       
type ListBox = SelectionList (IO ())

-- | Create a selection list box.
selectionList :: Container w => Bool -> [Prop (SelectionList a)] -> w -> IO (SelectionList a)
selectionList multi props w = do
	lb <- do hlist <- Port.createListBox (hwindow w) multi
		 slitems <- newVar []
		 return (SelectionList multi hlist (hwindow w) slitems)
	set lb props
	return lb

-- | Create a selection checklist control. A "checklist control" 
-- displays a list of items where each item in the list has a 
-- check box next to it. The checklist control is a similar to the 
-- normal list box with multi selection allowed. The difference 
-- is that the checklist uses check boxes to indicate 
-- the selected items.
selectionCheckList :: Container w => [Prop (SelectionList a)] -> w -> IO (SelectionList a)
selectionCheckList props w = do
	lb <- do hlist <- Port.createCheckListBox (hwindow w)
		 slitems <- newVar []
		 return (SelectionList True hlist (hwindow w) slitems)
	set lb props
	return lb

-- | Create a list box.
listBox :: Container w => Bool -> [Prop ListBox] -> w -> IO ListBox
listBox multi props w = do
	lb <- selectionList multi props w
	set lb [on command =: listBoxCommand lb]
	return lb
	where
		listBoxCommand :: ListBox -> IO ()
		listBoxCommand lb = do
  			i  <- Port.getListBoxCurrentItem (slhandle lb)
			xs <- getVar (slitems lb)
			when (i>=0 && i < length xs) (snd (xs!!i))  -- invoke appropiate handler

instance Countable (SelectionList a) where
  count = readAttr "count" (fmap length . getVar . slitems)

instance CommandItems SelectionList where
  items
    = newAttr (\w -> getVar (slitems w))
              (\w xs -> do Port.removeAllListBoxItems (slhandle w)
                           mapM_ (Port.appendListBoxItem (slhandle w) . fst) xs
                           setVar (slitems w) xs)
                           
  appendItem lb item@(title,action) = do
    items <- takeVar (slitems lb)
    Port.appendListBoxItem (slhandle lb) title
    putVar (slitems lb) (items++[item])
  
  insertItem lb pos item@(title,action) = do
    items <- takeVar (slitems lb)
    Port.insertListBoxItem (slhandle lb) pos title
    putVar (slitems lb) (insertAt pos item items)
  
  removeItem lb pos = do
    items <- takeVar (slitems lb)
    Port.removeListBoxItem (slhandle lb) pos
    putVar (slitems lb) (removeAt pos items)     
  
  removeAllItems lb = do
    items <- takeVar (slitems lb)
    Port.removeAllListBoxItems (slhandle lb)
    putVar (slitems lb) []


instance SingleSelect (SelectionList a) where
  selected = newStdAttr slhandle Port.getListBoxSingleSelection Port.setListBoxSingleSelection

instance MultiSelect (SelectionList a) where
  selection = newAttr getter setter
    where
      getter w = do
        items <- getVar (slitems w)
        filterM (\i -> Port.getListBoxItemSelectState (slhandle w) i) [0..length items-1]

      setter w xs = do
        items <- getVar (slitems w)
        mapM_ (\x -> Port.setListBoxItemSelectState (slhandle w) x (elem x xs)) [0..length items-1]

instance Dimensions (SelectionList a) where
  frame = newStdAttr slhandle Port.getControlFrame Port.moveResizeControl

instance Able (SelectionList a) where
  enabled = newStdAttr slhandle Port.getControlEnabled Port.setControlEnabled

instance Tipped (SelectionList a) where
  tooltip = newStdAttr slhandle Port.getControlTip Port.setControlTip

instance Control (SelectionList a) where
  pack = stdPack slparent slhandle Port.getListBoxRequestSize

instance Commanding (SelectionList a) where
  command = newStdEvent slhandle Port.getControlCommandHandler Port.setControlCommandHandler Port.setControlCommandDefHandler
  
instance Eq a => Valued (SelectionList a) [a] where
  value = newAttr getter setter
    where
      getter (SelectionList multi handle _ itemsVar)
        | multi     = do
                 xs <- getVar itemsVar
	         let getSelection _ [] = return []
	             getSelection i ((_,v):xs) = do
	                selected <- Port.getListBoxItemSelectState handle i
	                vs <- getSelection (i+1) xs
	                return (if selected then v:vs else vs)
                 getSelection 0 xs
        | otherwise = do
                 index <- Port.getListBoxSingleSelection handle
	         xs <- getVar itemsVar
	         return (if index>=0 && index < length xs
	                  then [snd (xs!!index)]
                          else [])
      setter (SelectionList multi handle _ itemsVar) vs
        | multi     = do
                xs <- getVar itemsVar
                let setSelection _ [] = return ()
                    setSelection i ((_,v):xs) = do
                      Port.setListBoxItemSelectState handle i (elem v vs)
                      setSelection (i+1) xs
                setSelection 0 xs
        | otherwise = do
        	xs <- getVar itemsVar
        	let setSelection _ [] = Port.setListBoxSingleSelection handle (-1)
        	    setSelection i ((_,v):xs)
        	      | v `elem` vs = Port.setListBoxSingleSelection handle i
        	      | otherwise   = setSelection (i+1) xs
                setSelection 0 xs
    

--------------------------------------------------------------------
--  Check box
--------------------------------------------------------------------
-- | A single check control.
data CheckBox = CheckBox
	{ chandle :: !WindowHandle
	, cparent :: !WindowHandle
	}

-- | Create a check control with a certain label.
checkBox :: Container w => [Prop CheckBox] -> w -> IO CheckBox
checkBox props w
  = do c <- do hcheck <- Port.createCheckBox (hwindow w)
               return (CheckBox hcheck (hwindow w))
       set c props
       return c

instance Titled CheckBox where
  title = newStdAttr chandle Port.getCheckBoxText  Port.setCheckBoxText

instance Checked CheckBox where
  checked = newAttr (\w   -> Port.getCheckBoxSelectState (chandle w))
                    (\w b -> do Port.setCheckBoxSelectState (chandle w) b
                                cmd <- get w (on command)
                                cmd)
   
instance Commanding CheckBox where
  command = newStdEvent chandle Port.getControlCommandHandler Port.setControlCommandHandler Port.setControlCommandDefHandler
  
instance Able CheckBox where
  enabled = newStdAttr chandle Port.getControlEnabled Port.setControlEnabled
  
instance Tipped CheckBox where
  tooltip = newStdAttr chandle Port.getControlTip Port.setControlTip

instance Control CheckBox where
  pack = stdPack cparent chandle Port.getCheckBoxRequestSize
  
instance Valued CheckBox Bool where
  value = checked

--------------------------------------------------------------------
-- Radio box
--------------------------------------------------------------------
-- | A single radio control.
data RadioBox = RadioBox
	{ rhandle :: !WindowHandle
	, rparent :: !WindowHandle
	}

-- | Create a radio control.
radioBox :: Container w => [Prop RadioBox] -> w -> IO RadioBox
radioBox props w
  = do r <- do hradio <- Port.createRadioBox (hwindow w)
               return (RadioBox hradio (hwindow w))
       set r props
       return r

instance Titled RadioBox where
  title = newStdAttr rhandle Port.getRadioBoxText Port.setRadioBoxText

instance Checked RadioBox where
  checked = newStdAttr rhandle Port.getRadioBoxSelectState Port.setRadioBoxSelectState
   
instance Commanding RadioBox where
  command = newStdEvent rhandle Port.getControlCommandHandler Port.setControlCommandHandler Port.setControlCommandDefHandler

instance Able RadioBox where
  enabled = newStdAttr rhandle Port.getControlEnabled Port.setControlEnabled
  
instance Tipped RadioBox where
  tooltip = newStdAttr rhandle Port.getControlTip Port.setControlTip

instance Control RadioBox where
  pack = stdPack rparent rhandle Port.getRadioBoxRequestSize
  
instance Valued RadioBox Bool where
  value = checked


-- | Connect a list of radio controls such that only one of them is selected
-- at any time.
setRadioBoxGroup :: [RadioBox] -> IO ()
setRadioBoxGroup items = Port.setRadioBoxGroup (map rhandle items)

--------------------------------------------------------------------
-- Slider
--------------------------------------------------------------------

-- | A slider control.
data Slider = Slider{ shandle :: !WindowHandle
                    , sparent :: !WindowHandle
                    }
                      
-- | Create a horizontal slider control.
hslider :: Container w => [Prop Slider] -> w -> IO Slider
hslider props w
  = do r <- do shandle <- Port.createHorzSlider (hwindow w)
               return (Slider shandle (hwindow w))
       set r props
       return r

-- | Create a vertical slider control.
vslider :: Container w => [Prop Slider] -> w -> IO Slider
vslider props w
  = do r <- do shandle <- Port.createVertSlider (hwindow w)
               return (Slider shandle (hwindow w))
       set r props
       return r

instance RangedSelect Slider where
   range = newAttr (\w   -> Port.getSliderRange (shandle w))
                   (\w (min,max) -> Port.setSliderRange (shandle w) min max)

   selectedPos = newStdAttr shandle Port.getSliderPosition Port.setSliderPosition

instance Commanding Slider where
  command = newStdEvent shandle Port.getControlCommandHandler Port.setControlCommandHandler Port.setControlCommandDefHandler

instance Control Slider where
  pack = stdPack sparent shandle Port.getSliderRequestSize

instance Valued Slider Int where
  value = selectedPos


--------------------------------------------------------------------
-- TrackBar
--------------------------------------------------------------------

-- | A track bar control.
data TrackBar = TrackBar
	{ tbhandle :: !WindowHandle
	, tbparent :: !WindowHandle
	}
                      
-- | Create a horizontal track bar control.
hTrackBar :: Container w => [Prop TrackBar] -> w -> IO TrackBar
hTrackBar props w
  = do tb <- do tbhandle <- Port.createHorzTrackBar (hwindow w)
                return (TrackBar tbhandle (hwindow w))
       set tb props
       return tb
       
-- | Create a vertical track bar control.
vTrackBar :: Container w => [Prop TrackBar] -> w -> IO TrackBar
vTrackBar props w
  = do tb <- do tbhandle <- Port.createVertTrackBar (hwindow w)
                return (TrackBar tbhandle (hwindow w))
       set tb props
       return tb

instance Control TrackBar where
  pack = stdPack tbparent tbhandle Port.getTrackBarRequestSize

-- | The vertical track bar control fires a 'increment' event when the up arrow is clicked.
-- The horizontal track bar control fires the same event when the right arrow is clicked.
increment :: Event TrackBar (IO ())
increment = newStdEvent tbhandle Port.getTrackBarIncrementHandler Port.setTrackBarIncrementHandler Port.setTrackBarIncrementDefHandler

-- | The vertical track bar control fires a 'decrement' event when the down arrow is clicked.
-- The horizontal track bar control fires the same event when the left arrow is clicked.
decrement :: Event TrackBar (IO ())
decrement = newStdEvent tbhandle Port.getTrackBarDecrementHandler Port.setTrackBarDecrementHandler Port.setTrackBarDecrementDefHandler

--------------------------------------------------------------------
-- ProgressBar
--------------------------------------------------------------------

-- | A progress bar.
data ProgressBar = ProgressBar
                    { pbhandle :: !WindowHandle
                    , pbparent :: !WindowHandle
                    , pbrange  :: Var (Int,Int)
                    }
                      
-- | Create a horizontal progress bar.
-- The boolean parameter specify whether the bar shows continuous or discrete values.
hProgressBar :: Container w => Bool -> [Prop ProgressBar] -> w -> IO ProgressBar
hProgressBar smooth props w
  = do r <- do pbhandle <- Port.createHorzProgressBar (hwindow w) smooth
               pbrange  <- newVar (0,100)
               Port.setProgressBarFraction pbhandle 0 100 0
               return (ProgressBar pbhandle (hwindow w) pbrange)
       set r props
       return r
       
-- | Create a vertical progress bar.
-- The boolean parameter specify whether the bar shows continuous or discrete values.
vProgressBar :: Container w => Bool -> [Prop ProgressBar] -> w -> IO ProgressBar
vProgressBar smooth props w
  = do r <- do pbhandle <- Port.createVertProgressBar (hwindow w) smooth
               pbrange  <- newVar (0,100)
               Port.setProgressBarFraction pbhandle 0 100 0
               return (ProgressBar pbhandle (hwindow w) pbrange)
       set r props
       return r

instance RangedSelect ProgressBar where
   range = newAttr (\w   -> getVar (pbrange w))
                   (\w r@(min,max) -> do
                       pos <- Port.getProgressBarFraction (pbhandle w) min max
                       Port.setProgressBarFraction (pbhandle w) min max pos
                       setVar (pbrange w) r)

   selectedPos 
         = newAttr (\w -> do
                        (min,max) <- getVar (pbrange w)
                        Port.getProgressBarFraction (pbhandle w) min max)
                   (\w pos -> do
                        (min,max) <- getVar (pbrange w)
                        Port.setProgressBarFraction (pbhandle w) min max pos)

instance Control ProgressBar where
  pack = stdPack pbparent pbhandle Port.getProgressBarRequestSize

instance Valued ProgressBar Int where
  value = selectedPos

--------------------------------------------------------------------
-- CompoundControl
--------------------------------------------------------------------
-- | A compound control.
data CompoundControl = CompoundControl
	{ cchandle     :: !WindowHandle
	, ccparent     :: !WindowHandle
	, vdomain      :: Var Size
	, vautosize    :: Var Bool
	, vpen         :: Var Pen
	, vbufferMode  :: Var BufferMode
	, vpaint       :: Var PaintFunction
	, vlayout      :: Var Layout
	}

-- | Create a compound control
compoundControl :: Container w => [Prop CompoundControl] -> w -> IO CompoundControl
compoundControl props w = do
	c <- Port.createCompoundControl (hwindow w) >>= compoundControlFromHandle (hwindow w)
	set c props
	return c

compoundControlFromHandle :: WindowHandle -> WindowHandle -> IO CompoundControl
compoundControlFromHandle hwnd hctrl
  = do c <- do vpaint     <- newVar (\_ _ _ -> return ())
               vautosize  <- newVar True
               vlayout    <- newVar empty
               vdomain    <- newVar (sz 0 0)
               vpen       <- newVar windowPen
               vbufferMode<- newVar UnBuffered
               return (CompoundControl hctrl hwnd vdomain vautosize vpen vbufferMode vpaint vlayout)
       recolorCompound c
       set c [on relayout =:: relayoutCompound]
       -- just by setting a dummy paint function, we will at least intialize the canvas properly on a repaint
       set c [on paint =: (\_ _ _ -> return ())]
       return c

relayoutCompound :: CompoundControl -> IO ()
relayoutCompound c
  = do frame  <- Port.getControlFrame (cchandle c)
       domain <- get c domain
       lay    <- getVar (vlayout c)
       needed <- getLayoutSize lay
       let d1 = maxSize domain needed
           d2 = maxSize d1 (rectSize frame)
       Port.setWindowDomainSize (cchandle c) d1
       layoutInRect (rectOfSize d2) lay
       return ()

recolorCompound :: CompoundControl -> IO ()
recolorCompound c
  = do col   <- get c color
       bgcol <- get c bgcolor
       hat   <- get c hatch
       Port.setWindowColor (cchandle c) col bgcol hat
       repaint c
       relayoutCompound c

instance Scrollable CompoundControl where
  scroll = newStdEvent cchandle Port.getWindowScrollHandler Port.setWindowScrollHandler Port.setWindowScrollDefHandler
  domain = newAttr (\c   -> getVar (vdomain c))
                   (\c x -> setVar (vdomain c) x >> relayoutCompound c)
  origin = newStdAttr cchandle Port.getWindowScrollOrigin Port.setWindowScrollOrigin

instance Dimensions CompoundControl where
  frame  = newStdAttr cchandle Port.getControlFrame Port.moveResizeControl

instance Drawn CompoundControl where
  pen = newAttr (getVar . vpen) (\w pen -> setVar (vpen w) pen >> recolorCompound w)
  
  bufferMode = newStdAttr vbufferMode getVar setVar
  
instance HasFont CompoundControl where
  font = mapAttr penFont (\pen c -> pen{penFont=c}) pen

instance Reactive CompoundControl where
  mouse       = newStdEvent cchandle Port.getWindowMouseHandler       Port.setWindowMouseHandler       Port.setWindowMouseDefHandler
  keyboard    = newStdEvent cchandle Port.getWindowKeyboardHandler    Port.setWindowKeyboardHandler    Port.setWindowKeyboardDefHandler
  contextMenu = newStdEvent cchandle Port.getWindowContextMenuHandler Port.setWindowContextMenuHandler Port.setWindowContextMenuDefHandler

instance Paint CompoundControl where
  repaint c = Port.invalidateWindow (cchandle c)
  paint     = newEvent (getVar . vpaint)
                          (\w h -> Port.setWindowPaintHandler    (cchandle w) (wndpaint w h) >> setVar (vpaint w) h)
                          (\w   -> Port.setWindowPaintDefHandler (cchandle w) >> setVar (vpaint w) (\_ _ _ -> return ()))
            where
              wndpaint c paintfun hcanvas updArea
                = do pen   <- get c pen
                     bmode <- get c bufferMode
                     withCanvas bmode pen hcanvas $ \can -> paintfun can updArea []
  paintIn c bmode f = do 
     pen   <- get c pen
     Port.drawInWindow (cchandle c) (\hcanvas -> withCanvas bmode pen hcanvas f)

instance Able CompoundControl where
  enabled = newStdAttr cchandle Port.getControlEnabled Port.setControlEnabled

instance Container CompoundControl where
  layout 
    = writeAttr "layout" (\w c -> do
       let new_lay = pack c
       old_lay  <- getVar (vlayout w)
       domain   <- get w domain
       needed   <- getLayoutSize new_lay
       let d = maxSize domain needed
       Port.setWindowDomainSize (cchandle w) d
       frame    <- Port.getControlFrame (cchandle w)
       updateControlsVisibility old_lay new_lay
       layoutInRect (rectOfSize (maxSize d (rectSize frame))) new_lay
       setVar (vlayout w) new_lay)
  autosize   = readAttr "autosize"   (\c -> return False)
  layoutSize = readAttr "layoutSize" (\c -> getVar (vlayout c) >>= getLayoutSize)
  relayout   = newStdEvent cchandle Port.getContainerReLayoutHandler Port.setContainerReLayoutHandler Port.setContainerReLayoutDefHandler
  hwindow c  = cchandle c

instance Control CompoundControl where
  pack = stdPack ccparent cchandle Port.getCompoundControlRequestSize


--------------------------------------------------------------------
-- GroupBox
--------------------------------------------------------------------
-- | A group box.
data GroupBox = GroupBox
	{ gbhandle     :: !WindowHandle
	, gbparent     :: !WindowHandle
	, gblayout     :: Var Layout
	}

-- | Create a group box
groupBox :: Container w => [Prop GroupBox] -> w -> IO GroupBox
groupBox props w
  = do c <- do gbhandle   <- Port.createGroupBox (hwindow w)
               gblayout   <- newVar empty
               return (GroupBox gbhandle (hwindow w) gblayout)
       set c [on relayout =:: relayoutGroupBox]
       set c props
       return c

relayoutGroupBox :: GroupBox -> IO ()
relayoutGroupBox c
  = do (Rect fl ft fr fb) <- Port.getControlFrame (gbhandle c)
       lay  <- getVar (gblayout c)
       (l,t,r,b) <- Port.getGroupBoxBordersSize (gbhandle c)
       layoutInRect (Rect l t (fr-fl-r) (fb-ft-b)) lay
       return ()

instance Container GroupBox where
  layout     = writeAttr "layout"  (\w c -> do
                     let new_lay = pack c
                     old_lay <- getVar (gblayout w)
                     updateControlsVisibility old_lay new_lay
                     setVar (gblayout w) new_lay
                     Port.relayoutContainer (gbhandle w))
  autosize   = readAttr "autosize"   (\c -> return True)
  layoutSize = readAttr "layoutSize" (\c -> getVar (gblayout c) >>= getLayoutSize)
  relayout   = newStdEvent gbhandle Port.getContainerReLayoutHandler Port.setContainerReLayoutHandler Port.setContainerReLayoutDefHandler
  hwindow c  = gbhandle c

instance Titled GroupBox where
  title  = newStdAttr gbhandle Port.getGroupBoxText Port.setGroupBoxText

instance Control GroupBox where
  pack c = stdPack gbparent gbhandle getGroupBoxRequestSize c
  	where
		getGroupBoxRequestSize hwnd = do
			(l,t,r,b) <- Port.getGroupBoxBordersSize hwnd
			Size w h <- getVar (gblayout c) >>= getLayoutSize
			return (Size (l+w+r) (t+h+b))



--------------------------------------------------------------------
-- Notebook
--------------------------------------------------------------------
-- | A notebook control.
data Notebook = Notebook
	{ nbhandle :: !WindowHandle
	, nbparent :: !WindowHandle
	, nbpages  :: !(Var [NotebookPage])
	}

-- | Create a notebook control.
notebook :: Container w => [Prop Notebook] -> w -> IO Notebook
notebook props w
  = do nb <- do hbook <- Port.createNotebook (hwindow w)
                refPages <- newVar []
                return (Notebook hbook (hwindow w) refPages)
       set nb props
       return nb

instance Control Notebook where
  pack w = stdPack nbparent nbhandle getNotebookRequestSize w
  		where
  			getNotebookRequestSize hwnd = do
  				outSize <- Port.getNotebookRequestSize hwnd
  				pages <- getVar (nbpages w)
  				pageSizes <- mapM (\page -> get page layoutSize) pages
  				let inSize = foldr maxSize (Size 10 10) pageSizes
  				return (addSize inSize outSize)

labelsPosition :: Attr Notebook PositionType
labelsPosition = newStdAttr nbhandle Port.getNotebookLabelsPosition  Port.setNotebookLabelsPosition

instance Countable Notebook where
  count = readAttr "count" (Port.getNotebookPageCount . nbhandle)

instance SingleSelect Notebook where
  selected = newStdAttr nbhandle Port.getNotebookSelection Port.setNotebookSelection

-- | The selected page in the notebook
selectedPage :: Attr Notebook NotebookPage
selectedPage = newAttr getPage setPage
	where
		getPage w = do
			index <- Port.getNotebookSelection (nbhandle w)
			pages <- getVar (nbpages w)
			return (pages !! index)

		setPage w p = do
			index <- Port.getNotebookPagePos (pghandle p)
			Port.setNotebookSelection (nbhandle w)index

-- | A notebook page.
data NotebookPage = NotebookPage
	{ pghandle :: !WindowHandle
	, pgparent :: !WindowHandle
	, pglayout :: !(Var Layout)
	}

-- | Create a new page at specified position in the notebook.
notebookPageAt :: Maybe Int -> [Prop NotebookPage] -> Notebook -> IO NotebookPage
notebookPageAt mb_pos props (Notebook hNotebook _ refPages)
  = do pg <- do hpage   <- Port.insertNotebookPage hNotebook mb_pos
                vlayout <- newVar empty
                return (NotebookPage hpage hNotebook vlayout)
       updateVar refPages ((:) pg)
       set pg [on relayout =: relayoutNotebookPage pg]
       set pg props
       return pg

-- | Appends a new page in the notebook.
notebookPage :: [Prop NotebookPage] -> Notebook -> IO NotebookPage
notebookPage = notebookPageAt Nothing

relayoutNotebookPage :: NotebookPage -> IO ()
relayoutNotebookPage c
  = do size <- Port.getNotebookPageSize (pghandle c)
       lay  <- getVar (pglayout c)
       layoutInRect (rectOfSize size) lay
       return ()

instance Container NotebookPage where
  layout     = writeAttr "layout"  (\w c -> do
                     let new_lay = pack c
                     old_lay <- getVar (pglayout w)
                     updateControlsVisibility old_lay new_lay                     
                     setVar (pglayout w) new_lay
                     Port.relayoutContainer (pghandle w))
  autosize   = readAttr "autosize"   (\c -> return True)
  layoutSize = readAttr "layoutSize" (\c -> getVar (pglayout c) >>= getLayoutSize)
  relayout   = newStdEvent pghandle Port.getContainerReLayoutHandler Port.setContainerReLayoutHandler Port.setContainerReLayoutDefHandler
  hwindow c  = pghandle c

instance Titled NotebookPage where
  title  = newStdAttr pghandle Port.getNotebookPageTitle Port.setNotebookPageTitle

instance Positioned NotebookPage where
  pos = readAttr "pos" (Port.getNotebookPagePos . pghandle)

instance Activity NotebookPage where
  activate  = newStdEvent pghandle Port.getWindowActivateHandler   Port.setWindowActivateHandler   Port.setWindowActivateDefHandler
  deactivate= newStdEvent pghandle Port.getWindowDeactivateHandler Port.setWindowDeactivateHandler Port.setWindowDeactivateDefHandler
  isactive = newAttr getIsActive setIsActive
  	where
  		getIsActive p = do
  			index <- Port.getNotebookPagePos   (pghandle p)
  			sel   <- Port.getNotebookSelection (pgparent p)
  			return (index == sel)

  		setIsActive p _ = do
			index <- Port.getNotebookPagePos (pghandle p)
			Port.setNotebookSelection (pgparent p)index

instance Deadly NotebookPage where
  destroyWidget w = Port.destroyNotebookPage (pghandle w)
  destroy         = newStdEvent pghandle Port.getWindowDestroyHandler Port.setWindowDestroyHandler Port.setWindowDestroyDefHandler

instance HasIcon NotebookPage where
  icon  = newStdAttr pghandle Port.getNotebookPageBitmap Port.setNotebookPageBitmap

--------------------------------------------------------------------
-- Splitter
--------------------------------------------------------------------
-- | A Splitter control.
data Splitter = Splitter
	{ splhandle :: !WindowHandle
	, splparent :: !WindowHandle
	}

-- | Create a horizontal splitter control.
hSplitter :: Container w => [Prop Splitter] -> w -> IO (Splitter, CompoundControl, CompoundControl)
hSplitter props w = do
	(hsplitter, hpane1, hpane2) <- Port.createHorzSplitter (hwindow w)
	let spl = Splitter hsplitter (hwindow w)
	pane1 <- compoundControlFromHandle hsplitter hpane1
	pane2 <- compoundControlFromHandle hsplitter hpane2
	set spl props
	return (spl,pane1,pane2)

-- | Create a horizontal splitter control.
vSplitter :: Container w => [Prop Splitter] -> w -> IO (Splitter, CompoundControl, CompoundControl)
vSplitter props w = do
	(hsplitter, hpane1, hpane2) <- Port.createVertSplitter (hwindow w)
	let spl = Splitter hsplitter (hwindow w)
	pane1 <- compoundControlFromHandle hsplitter hpane1
	pane2 <- compoundControlFromHandle hsplitter hpane2
	set spl props
	return (spl,pane1,pane2)

instance Control Splitter where
  pack = stdPack splparent splhandle Port.getSplitterRequestSize

instance RangedSelect Splitter where
   range = readAttr "range" (Port.getSplitterRange . splhandle)
   selectedPos = newStdAttr splhandle Port.getSplitterPosition Port.setSplitterPosition
