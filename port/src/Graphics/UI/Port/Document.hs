{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Document
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Documents
-}
-----------------------------------------------------------------------------------------

module Graphics.UI.Port.Document
        (
        -- * Document templates
          DocumentTemplate(..), registerDocTemplate
        -- * Documents
        , Document
        , readDoc, writeDoc
        , getDocModified, getDocFilePath
        , newDoc, openDoc, revertDoc, saveDoc
        , openDocWindow, printDoc

        -- * Internals
        , clearDocTemplates, unregisterDocument
        )where

import Data.IORef
import Graphics.UI.Port.Types
import Graphics.UI.Port.PtrMap
import Control.Concurrent.MVar
import Control.Monad
import System.IO.Unsafe( unsafePerformIO )

data Document a = Document
    { docReference   :: !(IORef (Bool,a))        -- reference to pair of the document value and the flag for modification
    , docFilePath    :: !(IORef (Either FilePath Int))        -- reference to the current file path for the document
    , docWindowsList :: !(IORef [WindowHandle])  -- reference to the list of windows that uses this document
    , docTemplate    :: DocumentTemplate a       -- the template
    }

data DocumentTemplate a = DocumentTemplate
    { dtMimeType            :: String
    , dtOrder               :: Int
    , dtDescription         :: String
    , dtExtensions          :: [String]
    , dtNewDocument         :: IO a
    , dtOpenDocument        :: FilePath -> IO a
    , dtSaveDocument        :: FilePath -> a -> IO ()
    , dtPrintDocument       :: a -> IO ()
    , dtOpenWindow          :: Document a -> IO WindowHandle
    , dtCompatibleTemplates :: [String]
    }

data Holder w = forall a . Holder (w a)

-----------------------------------------------------------------------------------------
--  Document templeates
-----------------------------------------------------------------------------------------

{-# NOINLINE documentTemplates #-}
documentTemplates :: MVar [Holder DocumentTemplate]
documentTemplates = unsafePerformIO (newMVar [])

-- | Register new document template
registerDocTemplate :: DocumentTemplate a -> IO ()
registerDocTemplate template = do
    templates <- takeMVar documentTemplates
    putMVar documentTemplates ((Holder template):templates)

clearDocTemplates :: IO ()
clearDocTemplates = do
    templates <- takeMVar documentTemplates
    putMVar documentTemplates []

-----------------------------------------------------------------------------------------
--  Documents
-----------------------------------------------------------------------------------------

{-# NOINLINE documents #-}
documents :: MVar (PtrMap WindowHandle (Holder Document))
documents = unsafePerformIO (newMVar empty)

registerDocument :: WindowHandle -> Document a -> IO ()
registerDocument handle doc = do
    docs <- takeMVar documents
    putMVar documents (insert handle (Holder doc) docs)

unregisterDocument :: WindowHandle -> IO ()
unregisterDocument handle = do
    docs <- takeMVar documents
    putMVar documents (delete handle docs)

-----------------------------------------------------------------------------------------
--  Operations on documents
-----------------------------------------------------------------------------------------

-- | Read the value of a Document
readDoc :: Document a -> IO a
readDoc (Document ref _ _ _) = do
    (modified, x) <- readIORef ref
    return x

-- | Write a new value into a Document. The function automatically turns on the modification flag in the document.
writeDoc :: Document a -> a -> IO ()
writeDoc (Document ref _ refWindows _) x = do
    (modified, _) <- readIORef ref
    writeIORef ref (True,x)
    when (not modified) (readIORef refWindows >>= mapM_ (updateWindowModifiedState True))

-- | Retrieve the value of the modification flag in the document
getDocModified :: Document a -> IO Bool
getDocModified (Document ref _ _ _) = do
    (modified, x) <- readIORef ref
    return modified

-- | Returns the path to the file associated with the document. If there is no such file the returned value is Nothing.
getDocFilePath :: Document a -> IO (Maybe FilePath)
getDocFilePath (Document _ refPath _ _) = do
    path <- readIORef refPath
    return (case path of {Left path -> Just path; Right _ -> Nothing})

-- | Create a new document from the specified template
newDoc :: DocumentTemplate a -> IO (Document a)
newDoc templ = do
    x <- dtNewDocument templ
    index <- withMVar documents (getNextDocumentIndex templ . elems)
    ref <- newIORef (False, x)
    refPath <- newIORef (Right index)
    refWindows <- newIORef []
    return (Document ref refPath refWindows templ)

-- | Open the specified file and create a document associated with it.
openDoc :: FilePath -> DocumentTemplate a -> IO (Document a)
openDoc path templ = do
    x <- dtOpenDocument templ path
    ref <- newIORef (False, x)
    refPath <- newIORef (Left path)
    refWindows <- newIORef []
    return (Document ref refPath refWindows templ)

-- | Revert the document
revertDoc :: Document a -> IO ()
revertDoc (Document ref refPath refWindows templ) = do
    path <- readIORef refPath
    (modified, x) <- readIORef ref
    case path of
        Left path | modified -> do
            x <- dtOpenDocument templ path
            writeIORef ref (False, x)
            readIORef refWindows >>= mapM_ (updateWindowModifiedState False)
        _ -> return ()

-- | Save the document to the file specified with its path. The function automatically turns off the modification flag.
saveDoc :: FilePath -> Document a -> IO ()
saveDoc path (Document ref refPath refWindows templ) = do
    (modified, x) <- readIORef ref
    when modified $ do
        dtSaveDocument templ path x
        writeIORef ref (False, x)
        writeIORef refPath (Left path)
        readIORef refWindows >>= mapM_ (updateWindowModifiedState False)

-- | Open new window to display the document.
openDocWindow :: Document a -> IO WindowHandle
openDocWindow d@(Document _ _ _ templ) = do
    window <- dtOpenWindow templ d
    registerDocument window d
    return window

-- | Print the document
printDoc :: Document a -> IO ()
printDoc (Document ref _ _ templ) = do
    (modified, x) <- readIORef ref
    dtPrintDocument templ x

updateWindowModifiedState :: Bool -> WindowHandle -> IO ()
updateWindowModifiedState flag hwnd = return ()  -- TODO

getNextDocumentIndex :: DocumentTemplate a -> [Holder Document] -> IO Int
getNextDocumentIndex dt [] = return 0
getNextDocumentIndex dt (Holder doc : docs) = do
    path <- readIORef (docFilePath doc)
    case path of
        Right index | dtMimeType (docTemplate doc) == dtMimeType dt -> do
            index' <- getNextDocumentIndex dt docs
            return (max (index+1) index')
        _ -> getNextDocumentIndex dt docs
