-----------------------------------------------------------------------------------------
{-| Module      :  Controls
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Defines common controls:

    * Labels

    * Button
    
    * Check box
    
    * Edit box
    
    * DateEntry
    
    * List box
    
    * Popup box
    
    * Radio item
    
    * Slider
    
    * TrackBar
    
    * ProgressBar
    
    * CompoundControl

    * GroupBox
    
    * Notebook
    
    * NotebookPage
    
    * Splitter
    
    * TreeView
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Controls
           ( 
           -- * Generic
             moveResizeControl
           , getControlFrame
           , setControlEnabled, getControlEnabled
           , setControlVisible, getControlVisible
           , setControlTip,     getControlTip
           , relayoutContainer
           -- * Label
           , createLabel, getLabelRequestSize
           , getLabelText, setLabelText
           , changeLabelFont
           -- * Button
           , createButton, getButtonRequestSize
           , getButtonText, setButtonText
           , changeButtonFont
           -- * Edit box
           , createEdit, getEditRequestSize
           , getEditText,      setEditText
           , getEditReadOnly,  setEditReadOnly
           , getEditPassword,setEditPassword
           , changeEditBoxFont
           -- * DateEntry
           , createDateEntry, getDateEntryRequestSize
           , getDateEntryValue, setDateEntryValue
           -- * Check box
           , createCheckBox, getCheckBoxRequestSize
           , getCheckBoxText,  setCheckBoxText
           , getCheckBoxSelectState
           , setCheckBoxSelectState
           -- * Radio item
           , createRadioBox, getRadioBoxRequestSize
           , getRadioBoxText,  setRadioBoxText
           , setRadioBoxSelectState
           , getRadioBoxSelectState
           , setRadioBoxGroup
           -- * Pop up box
           , createPopUp, getPopUpRequestSize
           , appendPopUpItem, insertPopUpItem
           , removePopUpItem, removeAllPopUpItems
           , getPopUpSelection, setPopUpSelection
           -- * List box
           , createListBox, createCheckListBox, getListBoxRequestSize
           , appendListBoxItem, insertListBoxItem
           , removeListBoxItem, removeAllListBoxItems
           , getListBoxSingleSelection, setListBoxSingleSelection
           , getListBoxItemSelectState, setListBoxItemSelectState
           , getListBoxCurrentItem
           -- * Slider
           , createHorzSlider, createVertSlider, getSliderRequestSize
           , getSliderRange,    setSliderRange
           , getSliderPosition, setSliderPosition
           -- * TrackBar
           , createHorzTrackBar, createVertTrackBar, getTrackBarRequestSize
           -- * ProgressBar
           , createHorzProgressBar, createVertProgressBar, getProgressBarRequestSize
           , setProgressBarFraction, getProgressBarFraction
           -- * CompoundControl
           , createCompoundControl, getCompoundControlRequestSize
           -- * GroupBox
           , createGroupBox, getGroupBoxBordersSize
           , getGroupBoxText, setGroupBoxText
           -- * Notebook
           , createNotebook, getNotebookRequestSize
           , getNotebookLabelsPosition, setNotebookLabelsPosition
           , getNotebookSelection,      setNotebookSelection
           , getNotebookPageCount
           -- * NotebookPage
           , insertNotebookPage
           , getNotebookPageTitle,  setNotebookPageTitle
           , getNotebookPageBitmap, setNotebookPageBitmap
           , getNotebookPagePos
           , destroyNotebookPage
           , getNotebookPageSize
           -- * Splitter
           , createHorzSplitter, createVertSplitter
           , getSplitterRequestSize
           , getSplitterRange
           , getSplitterPosition, setSplitterPosition
           -- * TreeView
           , createTreeView
           , TreeViewColumnType
           , treeViewColumnTypeInt, treeViewColumnTypeString, treeViewColumnTypeBool
           , addTreeViewColumn, appendTreeViewItem
           , getTreeViewRequestSize
           -- * WebView
           , createWebView
           , webViewLoadURL
           , getWebViewRequestSize
           ) where

import Foreign
import Foreign.C
import System.Time
import Control.Concurrent.MVar
import Data.Maybe(fromMaybe)
import Graphics.UI.Port.Types
import Graphics.UI.Port.Handlers  -- just for haddock
import Graphics.UI.Port.PtrMap as PtrMap

-----------------------------------------------------------------------------------------
-- Generic
-----------------------------------------------------------------------------------------}
-- | Change the position and size of a control.
moveResizeControl :: WindowHandle -> Rect -> IO ()
moveResizeControl hwnd rect
  = withCRect rect $ \x0 y0 x1 y1 ->
    osMoveResizeControl hwnd x0 y0 (x1-x0) (y1-y0)
foreign import ccall osMoveResizeControl :: WindowHandle -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Get the position and size of a control.
getControlFrame :: WindowHandle -> IO Rect
getControlFrame hwnd
  = withCRectResult $ \crect ->
    do osGetControlRect hwnd crect
foreign import ccall osGetControlRect  :: WindowHandle -> Ptr CInt -> IO ()

foreign import ccall unsafe "osSetControlEnabled" setControlEnabled :: WindowHandle -> Bool -> IO ()
foreign import ccall unsafe "osGetControlEnabled" getControlEnabled :: WindowHandle -> IO Bool

foreign import ccall unsafe "osSetControlVisible" setControlVisible :: WindowHandle -> Bool -> IO ()
foreign import ccall unsafe "osGetControlVisible" getControlVisible :: WindowHandle -> IO Bool

getControlTip :: WindowHandle -> IO String
getControlTip hwnd
  = resultCString (osGetControlTip hwnd)
foreign import ccall osGetControlTip :: WindowHandle -> IO CString

setControlTip :: WindowHandle -> String -> IO ()
setControlTip hwnd txt
  = withPortString txt (osSetControlTip hwnd)
foreign import ccall osSetControlTip :: WindowHandle -> PortString -> IO ()

foreign import ccall "osReLayoutContainer" relayoutContainer :: WindowHandle -> IO ()

-----------------------------------------------------------------------------------------
-- Label  
-----------------------------------------------------------------------------------------
-- | Create a label (i.e. just text).
foreign import ccall "osCreateLabel" createLabel :: WindowHandle -> IO WindowHandle

-- | The minimal size that the label needs.
getLabelRequestSize :: WindowHandle -> IO Size
getLabelRequestSize hwnd = withCSizeResult (osGetLabelReqSize hwnd)
foreign import ccall osGetLabelReqSize :: WindowHandle -> Ptr CInt -> IO ()

getLabelText :: WindowHandle -> IO String
getLabelText hwnd = resultPortString (osGetLabelText hwnd)
foreign import ccall osGetLabelText :: WindowHandle -> IO PortString

setLabelText :: WindowHandle -> String -> IO ()
setLabelText hwnd txt = withPortString txt (osSetLabelText hwnd)
foreign import ccall osSetLabelText :: WindowHandle -> PortString -> IO ()

changeLabelFont :: WindowHandle -> Font -> IO ()
changeLabelFont hwnd font = withCFont font (osChangeLabelFont hwnd)
foreign import ccall osChangeLabelFont :: WindowHandle -> FontHandle -> IO ()

-----------------------------------------------------------------------------------------
-- Button  
-----------------------------------------------------------------------------------------
-- | Create a button control. An event handler for button clicks can be
-- installed with 'setControlCommandHandler'.
foreign import ccall "osCreateButton" createButton :: WindowHandle -> IO WindowHandle

-- | The minimal size that the button needs.
getButtonRequestSize :: WindowHandle -> IO Size
getButtonRequestSize hwnd = withCSizeResult (osGetButtonReqSize hwnd)
foreign import ccall osGetButtonReqSize :: WindowHandle -> Ptr CInt -> IO ()

getButtonText :: WindowHandle -> IO String
getButtonText hwnd = resultPortString (osGetButtonText hwnd)
foreign import ccall osGetButtonText :: WindowHandle -> IO PortString

setButtonText :: WindowHandle -> String -> IO ()
setButtonText hwnd txt = withPortString txt (osSetButtonText hwnd)
foreign import ccall osSetButtonText :: WindowHandle -> PortString -> IO ()

changeButtonFont :: WindowHandle -> Font -> IO ()
changeButtonFont hwnd font = withCFont font (osChangeButtonFont hwnd)
foreign import ccall osChangeButtonFont :: WindowHandle -> FontHandle -> IO ()

-----------------------------------------------------------------------------------------
-- CheckBox  
-----------------------------------------------------------------------------------------
-- | Create a check box. An event handler for check box clicks can be
-- installed with 'setControlCommandHandler'.
foreign import ccall "osCreateCheckBox" createCheckBox :: WindowHandle -> IO WindowHandle

getCheckBoxRequestSize :: WindowHandle -> IO Size
getCheckBoxRequestSize hwnd = withCSizeResult (osGetCheckBoxReqSize hwnd)
foreign import ccall osGetCheckBoxReqSize :: WindowHandle -> Ptr CInt -> IO ()

getCheckBoxText :: WindowHandle -> IO String
getCheckBoxText hwnd = resultPortString (osGetCheckBoxText hwnd)
foreign import ccall osGetCheckBoxText :: WindowHandle -> IO PortString

setCheckBoxText :: WindowHandle -> String -> IO ()
setCheckBoxText hwnd txt = withPortString txt (osSetCheckBoxText hwnd)
foreign import ccall osSetCheckBoxText :: WindowHandle -> PortString -> IO ()

getCheckBoxSelectState :: WindowHandle -> IO Bool
getCheckBoxSelectState hwnd
  = do x <- osGetCheckBoxState hwnd; return (fromCBool x)
foreign import ccall osGetCheckBoxState :: WindowHandle -> IO CBool

setCheckBoxSelectState :: WindowHandle -> Bool -> IO ()
setCheckBoxSelectState hwnd x
  = osSetCheckBoxState hwnd (toCBool x)
foreign import ccall osSetCheckBoxState :: WindowHandle -> CBool -> IO ()


-----------------------------------------------------------------------------------------
-- EditBox  
-----------------------------------------------------------------------------------------
-- | Create an edit control.
foreign import ccall "osCreateEdit" createEdit :: WindowHandle -> IO WindowHandle

getEditRequestSize :: WindowHandle -> IO Size
getEditRequestSize hwnd = withCSizeResult (osGetEditReqSize hwnd)
foreign import ccall osGetEditReqSize :: WindowHandle -> Ptr CInt -> IO ()

getEditText :: WindowHandle -> IO String
getEditText hwnd
  = resultPortString (osGetEditText hwnd)
foreign import ccall osGetEditText :: WindowHandle -> IO PortString

setEditText :: WindowHandle -> String -> IO ()
setEditText hwnd txt
  = withPortString txt (osSetEditText hwnd)
foreign import ccall osSetEditText :: WindowHandle -> PortString -> IO ()

foreign import ccall "osSetEditReadOnly" setEditReadOnly :: WindowHandle -> Bool -> IO ()
foreign import ccall "osGetEditReadOnly" getEditReadOnly :: WindowHandle -> IO Bool

foreign import ccall "osSetEditPassword" setEditPassword :: WindowHandle -> Bool -> IO ()
foreign import ccall "osGetEditPassword" getEditPassword :: WindowHandle -> IO Bool

changeEditBoxFont :: WindowHandle -> Font -> IO ()
changeEditBoxFont hwnd font = withCFont font (osChangeEditBoxFont hwnd)
foreign import ccall osChangeEditBoxFont :: WindowHandle -> FontHandle -> IO ()

-----------------------------------------------------------------------------------------
-- DateEntry
-----------------------------------------------------------------------------------------
-- | Create a date entry control.
foreign import ccall "osCreateDateEntry" createDateEntry :: WindowHandle -> IO WindowHandle

getDateEntryRequestSize :: WindowHandle -> IO Size
getDateEntryRequestSize hwnd = withCSizeResult (osGetDateEntryReqSize hwnd)
foreign import ccall osGetDateEntryReqSize :: WindowHandle -> Ptr CInt -> IO ()

getDateEntryValue :: WindowHandle -> IO ClockTime
getDateEntryValue hwnd = do
  ctime <- osGetDateEntryValue hwnd
  return (TOD (fromIntegral ctime) 0)
foreign import ccall osGetDateEntryValue :: WindowHandle -> IO CInt

setDateEntryValue :: WindowHandle -> ClockTime -> IO ()
setDateEntryValue hwnd (TOD time 0) = osSetDateEntryValue hwnd (fromIntegral time)
foreign import ccall osSetDateEntryValue :: WindowHandle -> CTime -> IO ()


-----------------------------------------------------------------------------------------
--  RadioBox
-----------------------------------------------------------------------------------------
-- | Create a radio box.
-- An event handler for radio box clicks can be
-- installed with 'setControlCommandHandler'. 
foreign import ccall "osCreateRadioBox" createRadioBox :: WindowHandle -> IO WindowHandle

getRadioBoxRequestSize :: WindowHandle -> IO Size
getRadioBoxRequestSize hwnd = withCSizeResult (osGetRadioBoxReqSize hwnd)
foreign import ccall osGetRadioBoxReqSize :: WindowHandle -> Ptr CInt -> IO ()

getRadioBoxText :: WindowHandle -> IO String
getRadioBoxText hwnd = resultPortString (osGetRadioBoxText hwnd)
foreign import ccall osGetRadioBoxText :: WindowHandle -> IO PortString

setRadioBoxText :: WindowHandle -> String -> IO ()
setRadioBoxText hwnd txt = withPortString txt (osSetRadioBoxText hwnd)
foreign import ccall osSetRadioBoxText :: WindowHandle -> PortString -> IO ()

getRadioBoxSelectState :: WindowHandle -> IO Bool
getRadioBoxSelectState hwnd
  = do x <- osGetRadioBoxState hwnd; return (fromCBool x)
foreign import ccall osGetRadioBoxState :: WindowHandle -> IO CBool

setRadioBoxSelectState :: WindowHandle -> Bool -> IO ()
setRadioBoxSelectState hwnd x
  = osSetRadioBoxState hwnd (toCBool x)
foreign import ccall osSetRadioBoxState :: WindowHandle -> CBool -> IO ()

setRadioBoxGroup :: [WindowHandle] -> IO ()
setRadioBoxGroup handles = withArray0 nullHandle handles osSetRadioBoxGroup
foreign import ccall osSetRadioBoxGroup :: Ptr WindowHandle -> IO ()


-----------------------------------------------------------------------------------------
-- ListBox
-----------------------------------------------------------------------------------------
-- | Create a new list box. Takes an boolean argument that is 'True' when multiple
-- selection are allowed.
-- An event handler for list box clicks can be
-- installed with 'setControlCommandHandler'.
foreign import ccall "osCreateListBox" createListBox :: WindowHandle -> Bool -> IO WindowHandle

-- | Create a new checklist box. A "checklist box" displays a list of 
-- items where each item in the list has a check box next to it.
-- The check list box is a similar to the normal list box with multi
-- selection allowed. The difference is that the checklist box uses
-- check boxes to indicate the selected items.
foreign import ccall "osCreateCheckListBox" createCheckListBox :: WindowHandle -> IO WindowHandle

appendListBoxItem :: WindowHandle -> String -> IO ()
appendListBoxItem hwnd txt
  = withPortString txt (osAppendListBoxItem hwnd)
foreign import ccall osAppendListBoxItem :: WindowHandle -> PortString -> IO ()

insertListBoxItem :: WindowHandle -> Int -> String -> IO ()
insertListBoxItem hwnd idx txt
  = withPortString txt (osInsertListBoxItem hwnd (toCInt idx))
foreign import ccall osInsertListBoxItem :: WindowHandle -> CInt -> PortString -> IO ()

removeListBoxItem :: WindowHandle -> Int -> IO ()
removeListBoxItem hwnd idx
  = osRemoveListBoxItem hwnd (toCInt idx)
foreign import ccall osRemoveListBoxItem :: WindowHandle -> CInt -> IO ()

foreign import ccall "osRemoveAllListBoxItems" removeAllListBoxItems :: WindowHandle -> IO ()

getListBoxRequestSize :: WindowHandle -> IO Size
getListBoxRequestSize hwnd = withCSizeResult (osGetListBoxReqSize hwnd)
foreign import ccall osGetListBoxReqSize :: WindowHandle -> Ptr CInt -> IO ()

getListBoxSingleSelection :: WindowHandle -> IO Int
getListBoxSingleSelection hwnd
  = do ci <- osGetListBoxSingleSelection hwnd
       return (fromCInt ci)
foreign import ccall osGetListBoxSingleSelection :: WindowHandle -> IO CInt

setListBoxSingleSelection :: WindowHandle -> Int -> IO ()
setListBoxSingleSelection hwnd i
  = osSetListBoxSingleSelection hwnd (toCInt i)
foreign import ccall osSetListBoxSingleSelection :: WindowHandle -> CInt -> IO ()

getListBoxItemSelectState :: WindowHandle -> Int -> IO Bool
getListBoxItemSelectState hwnd i
  = do cb <- osGetListBoxItemSelectState hwnd (toCInt i)
       return (fromCBool cb)
foreign import ccall osGetListBoxItemSelectState :: WindowHandle -> CInt -> IO CBool

setListBoxItemSelectState :: WindowHandle -> Int -> Bool -> IO ()
setListBoxItemSelectState hwnd i b
  = osSetListBoxItemSelectState hwnd (toCInt i) (toCBool b)
foreign import ccall osSetListBoxItemSelectState :: WindowHandle -> CInt -> CBool -> IO ()

foreign import ccall "osGetListBoxCurrentItem" getListBoxCurrentItem :: WindowHandle -> IO Int

-----------------------------------------------------------------------------------------
--  Popup
-----------------------------------------------------------------------------------------
-- | Create a  new popup box.
-- An event handler for pop up clicks can be
-- installed with 'setControlCommandHandler'.
foreign import ccall "osCreatePopUp" createPopUp :: WindowHandle -> IO WindowHandle

appendPopUpItem :: WindowHandle -> String -> IO ()
appendPopUpItem hwnd txt
  = withPortString txt (osAppendPopUpItem hwnd)
foreign import ccall osAppendPopUpItem :: WindowHandle -> PortString -> IO ()

insertPopUpItem :: WindowHandle -> Int -> String -> IO ()
insertPopUpItem hwnd idx txt
  = withPortString txt (osInsertPopUpItem hwnd (toCInt idx))
foreign import ccall osInsertPopUpItem :: WindowHandle -> CInt -> PortString -> IO ()

removePopUpItem :: WindowHandle -> Int -> IO ()
removePopUpItem hwnd idx
  = osRemovePopUpItem hwnd (toCInt idx)
foreign import ccall osRemovePopUpItem :: WindowHandle -> CInt -> IO ()

foreign import ccall "osRemoveAllPopUpItems" removeAllPopUpItems :: WindowHandle -> IO ()

getPopUpRequestSize :: WindowHandle -> IO Size
getPopUpRequestSize hwnd = withCSizeResult (osGetPopUpReqSize hwnd)
foreign import ccall osGetPopUpReqSize :: WindowHandle -> Ptr CInt -> IO ()

getPopUpSelection :: WindowHandle -> IO Int
getPopUpSelection hwnd
  = do ci <- osGetPopUpSelection hwnd
       return (fromCInt ci)
foreign import ccall osGetPopUpSelection :: WindowHandle -> IO CInt

setPopUpSelection :: WindowHandle -> Int -> IO ()
setPopUpSelection hwnd i
  = osSetPopUpSelection hwnd (toCInt i)
foreign import ccall osSetPopUpSelection :: WindowHandle -> CInt -> IO ()

-----------------------------------------------------------------------------------------
-- Slider
-----------------------------------------------------------------------------------------

-- | Create a new horizontal slider control.
foreign import ccall "osCreateHorzSlider" createHorzSlider :: WindowHandle -> IO WindowHandle

-- | Create a new vertical slider control.
foreign import ccall "osCreateVertSlider" createVertSlider :: WindowHandle -> IO WindowHandle

getSliderRequestSize :: WindowHandle -> IO Size
getSliderRequestSize hwnd = withCSizeResult (osGetSliderReqSize hwnd)
foreign import ccall osGetSliderReqSize :: WindowHandle -> Ptr CInt -> IO ()

setSliderRange :: WindowHandle -> Int -> Int -> IO ()
setSliderRange hwnd min max = osSetSliderRange hwnd (toCInt min) (toCInt max)
foreign import ccall osSetSliderRange :: WindowHandle -> CInt -> CInt -> IO ()

getSliderRange :: WindowHandle -> IO (Int, Int)
getSliderRange hwnd = do
    ptr <- mallocArray 2
    pos <- osGetSliderRange hwnd ptr (ptr `advancePtr` 1)
    min <- peekElemOff ptr 0
    max <- peekElemOff ptr 1
    return (fromCInt min, fromCInt max)
foreign import ccall osGetSliderRange :: WindowHandle -> Ptr CInt -> Ptr CInt -> IO ()

setSliderPosition :: WindowHandle -> Int -> IO ()
setSliderPosition hwnd pos = osSetSliderPosition hwnd (toCInt pos)
foreign import ccall osSetSliderPosition :: WindowHandle -> CInt -> IO ()

getSliderPosition :: WindowHandle -> IO Int
getSliderPosition hwnd
  = do pos <- osGetSliderPosition hwnd
       return (fromCInt pos)
foreign import ccall osGetSliderPosition :: WindowHandle -> IO CInt

-----------------------------------------------------------------------------------------
-- TrackBar
-----------------------------------------------------------------------------------------

-- | Create a new horizontal track bar control.
foreign import ccall "osCreateHorzTrackBar" createHorzTrackBar :: WindowHandle -> IO WindowHandle

-- | Create a new vertical track bar control.
foreign import ccall "osCreateVertTrackBar" createVertTrackBar :: WindowHandle -> IO WindowHandle

getTrackBarRequestSize :: WindowHandle -> IO Size
getTrackBarRequestSize hwnd = withCSizeResult (osGetTrackBarReqSize hwnd)
foreign import ccall osGetTrackBarReqSize :: WindowHandle -> Ptr CInt -> IO ()

-----------------------------------------------------------------------------------------
-- ProgressBar
-----------------------------------------------------------------------------------------

-- | Create a new horizontal progress bar.
-- The boolean parameter specify whether the bar shows continuous or discrete values.
foreign import ccall "osCreateHorzProgressBar" createHorzProgressBar :: WindowHandle -> Bool -> IO WindowHandle

-- | Create a new vertical progress bar.
-- The boolean parameter specify whether the bar shows continuous or discrete values.
foreign import ccall "osCreateVertProgressBar" createVertProgressBar :: WindowHandle -> Bool -> IO WindowHandle

getProgressBarRequestSize :: WindowHandle -> IO Size
getProgressBarRequestSize hwnd = withCSizeResult (osGetProgressBarReqSize hwnd)
foreign import ccall osGetProgressBarReqSize :: WindowHandle -> Ptr CInt -> IO ()

setProgressBarFraction :: WindowHandle -> Int -> Int -> Int -> IO ()
setProgressBarFraction hwnd min max pos = osSetProgressBarFraction hwnd (toCInt min) (toCInt max) (toCInt pos)
foreign import ccall osSetProgressBarFraction :: WindowHandle -> CInt -> CInt -> CInt -> IO ()

getProgressBarFraction :: WindowHandle -> Int -> Int -> IO Int
getProgressBarFraction hwnd min max = do
    pos <- osGetProgressBarFraction hwnd (toCInt min) (toCInt max)
    return (fromCInt pos)
foreign import ccall osGetProgressBarFraction :: WindowHandle -> CInt -> CInt -> IO CInt

-----------------------------------------------------------------------------------------
-- CompoundControl
-----------------------------------------------------------------------------------------

-- | Create a new compound control
foreign import ccall "osCreateCompoundControl" createCompoundControl :: WindowHandle -> IO WindowHandle

getCompoundControlRequestSize :: WindowHandle -> IO Size
getCompoundControlRequestSize hwnd = withCSizeResult (osGetCompoundControlReqSize hwnd)
foreign import ccall osGetCompoundControlReqSize :: WindowHandle -> Ptr CInt -> IO ()

-----------------------------------------------------------------------------------------
-- GroupBox
-----------------------------------------------------------------------------------------

-- | Create a new group box
foreign import ccall "osCreateGroupBox" createGroupBox :: WindowHandle -> IO WindowHandle

getGroupBoxBordersSize :: WindowHandle -> IO (Int,Int,Int,Int)
getGroupBoxBordersSize hwnd = allocaArray 4 $ \cborders -> do
    osGetGroupBoxBordersSize hwnd cborders
    [l,t,r,b] <- peekArray 4 cborders
    return (fromCInt l,fromCInt t,fromCInt r,fromCInt b)
foreign import ccall osGetGroupBoxBordersSize :: WindowHandle -> Ptr CInt -> IO ()

getGroupBoxText :: WindowHandle -> IO String
getGroupBoxText hwnd = resultPortString (osGetGroupBoxText hwnd)
foreign import ccall osGetGroupBoxText :: WindowHandle -> IO PortString

setGroupBoxText :: WindowHandle -> String -> IO ()
setGroupBoxText hwnd txt = withPortString txt (osSetGroupBoxText hwnd)
foreign import ccall osSetGroupBoxText :: WindowHandle -> PortString -> IO ()

-----------------------------------------------------------------------------------------
-- Notebook
-----------------------------------------------------------------------------------------

-- | Create a new notebook control.
foreign import ccall "osCreateNotebook" createNotebook :: WindowHandle -> IO WindowHandle

getNotebookRequestSize :: WindowHandle -> IO Size
getNotebookRequestSize hwnd = withCSizeResult (osGetNotebookReqSize hwnd)
foreign import ccall osGetNotebookReqSize :: WindowHandle -> Ptr CInt -> IO ()

setNotebookLabelsPosition :: WindowHandle -> PositionType -> IO ()
setNotebookLabelsPosition hwnd position = osSetNotebookLabelsPosition hwnd (toCPositionType position)
foreign import ccall osSetNotebookLabelsPosition :: WindowHandle -> CInt -> IO ()

getNotebookLabelsPosition :: WindowHandle -> IO PositionType
getNotebookLabelsPosition hwnd = fmap fromCPositionType (osGetNotebookLabelsPosition hwnd)
foreign import ccall osGetNotebookLabelsPosition :: WindowHandle -> IO CInt

getNotebookSelection :: WindowHandle -> IO Int
getNotebookSelection hwnd
  = do ci <- osGetNotebookSelection hwnd
       return (fromCInt ci)
foreign import ccall osGetNotebookSelection :: WindowHandle -> IO CInt

setNotebookSelection :: WindowHandle -> Int -> IO ()
setNotebookSelection hwnd i
  = osSetNotebookSelection hwnd (toCInt i)
foreign import ccall osSetNotebookSelection :: WindowHandle -> CInt -> IO ()

foreign import ccall "osGetNotebookPageCount" getNotebookPageCount :: WindowHandle -> IO Int

insertNotebookPage :: WindowHandle -> Maybe Int -> IO WindowHandle
insertNotebookPage hwnd mb_pos = osInsertNotebookPage hwnd (fromMaybe (-1) mb_pos)
foreign import ccall osInsertNotebookPage :: WindowHandle -> Int -> IO WindowHandle

-- | Get the label of the notebook page.
getNotebookPageTitle :: WindowHandle -> IO String
getNotebookPageTitle hwnd = resultPortString (osGetNotebookPageTitle hwnd)
foreign import ccall osGetNotebookPageTitle :: WindowHandle -> IO PortString

-- | Set the label of the notebook page.
setNotebookPageTitle :: WindowHandle -> String -> IO ()
setNotebookPageTitle hwnd title = withPortString title (osSetNotebookPageTitle hwnd)
foreign import ccall osSetNotebookPageTitle :: WindowHandle -> PortString -> IO ()

foreign import ccall "osGetNotebookPagePos" getNotebookPagePos :: WindowHandle -> IO Int

foreign import ccall "osDestroyNotebookPage" destroyNotebookPage :: WindowHandle -> IO ()

getNotebookPageSize :: WindowHandle -> IO Size
getNotebookPageSize hwnd = withCSizeResult (osGetNotebookPageSize hwnd)
foreign import ccall osGetNotebookPageSize :: WindowHandle -> Ptr CInt -> IO ()

setNotebookPageBitmap :: WindowHandle -> Maybe Bitmap -> IO ()
setNotebookPageBitmap hwnd (Just bmp) = do
  map <- takeMVar windowBitmaps
  withCBitmap bmp (osSetNotebookPageBitmap hwnd)
  putMVar windowBitmaps (insert hwnd bmp map)
setNotebookPageBitmap hwnd Nothing    = do
  map <- takeMVar windowBitmaps
  osSetNotebookPageBitmap hwnd nullPtr
  putMVar windowBitmaps (delete hwnd map)
foreign import ccall osSetNotebookPageBitmap :: WindowHandle -> BitmapHandle -> IO ()

getNotebookPageBitmap :: WindowHandle -> IO (Maybe Bitmap)
getNotebookPageBitmap hwnd = do
  map <- readMVar windowBitmaps
  return (PtrMap.lookup hwnd map)

-----------------------------------------------------------------------------------------
-- Splitter
-----------------------------------------------------------------------------------------

-- | Create a new horizontal or vertical splitter control.
createHorzSplitter :: WindowHandle     -- ^ The handle of the parent
          -> IO (WindowHandle,WindowHandle,WindowHandle) -- ^ The first handle in the tripple are the Splitter handle and the next
                                                                                      -- two are the handles of first and second panes
createHorzSplitter hwnd = allocaArray 2 $ \cpanes -> do
    hspl <- osCreateSplitter hwnd False cpanes
    [pane1,pane2] <- peekArray 2 cpanes
    return (hspl,pane1,pane2)

-- | Create a new horizontal or vertical splitter control.
createVertSplitter :: WindowHandle     -- ^ The handle of the parent
          -> IO (WindowHandle,WindowHandle,WindowHandle) -- ^ The first handle in the tripple are the Splitter handle and the next
                                                                                      -- two are the handles of first and second panes
createVertSplitter hwnd = allocaArray 2 $ \cpanes -> do
    hspl <- osCreateSplitter hwnd True cpanes
    [pane1,pane2] <- peekArray 2 cpanes
    return (hspl,pane1,pane2)

foreign import ccall osCreateSplitter :: WindowHandle -> Bool -> Ptr WindowHandle -> IO WindowHandle

getSplitterRequestSize :: WindowHandle -> IO Size
getSplitterRequestSize hwnd = withCSizeResult (osGetSplitterReqSize hwnd)
foreign import ccall osGetSplitterReqSize :: WindowHandle -> Ptr CInt -> IO ()

getSplitterRange :: WindowHandle -> IO (Int, Int)
getSplitterRange hwnd = allocaArray 2 $ \ptr -> do
    pos <- osGetSplitterRange hwnd ptr
    [min,max] <- peekArray 2 ptr
    return (fromCInt min, fromCInt max)
foreign import ccall osGetSplitterRange :: WindowHandle -> Ptr CInt -> IO ()

foreign import ccall "osSetSplitterPosition" setSplitterPosition :: WindowHandle -> Int -> IO ()
foreign import ccall "osGetSplitterPosition" getSplitterPosition :: WindowHandle -> IO Int

-----------------------------------------------------------------------------------------
-- TreeView
-----------------------------------------------------------------------------------------

foreign import ccall "osCreateTreeView" createTreeView :: WindowHandle -> IO WindowHandle

type TreeViewColumnType = CInt

treeViewColumnTypeInt    = 1 :: TreeViewColumnType
treeViewColumnTypeString = 2 :: TreeViewColumnType
treeViewColumnTypeBool   = 3 :: TreeViewColumnType

addTreeViewColumn :: WindowHandle -> String -> TreeViewColumnType -> IO CInt
addTreeViewColumn hwnd title typ = withPortString title (\ctitle -> osAddTreeViewColumn hwnd ctitle typ)
foreign import ccall osAddTreeViewColumn :: WindowHandle -> PortString -> TreeViewColumnType -> IO CInt

foreign import ccall "osAppendTreeViewItem" appendTreeViewItem :: WindowHandle -> RowHandle -> IO RowHandle

getTreeViewRequestSize :: WindowHandle -> IO Size
getTreeViewRequestSize hwnd = withCSizeResult (osGetTreeViewReqSize hwnd)
foreign import ccall osGetTreeViewReqSize :: WindowHandle -> Ptr CInt -> IO ()

-----------------------------------------------------------------------------------------
-- WebView
-----------------------------------------------------------------------------------------

foreign import ccall "osCreateWebView" createWebView :: WindowHandle -> IO WindowHandle

webViewLoadURL :: WindowHandle -> String -> IO ()
webViewLoadURL hwnd url = withPortString url (osWebViewLoadURL hwnd)
foreign import ccall osWebViewLoadURL :: WindowHandle -> PortString -> IO ()

getWebViewRequestSize :: WindowHandle -> IO Size
getWebViewRequestSize hwnd = withCSizeResult (osGetWebViewReqSize hwnd)
foreign import ccall osGetWebViewReqSize :: WindowHandle -> Ptr CInt -> IO ()
