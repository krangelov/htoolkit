-----------------------------------------------------------------------------------------
{-| Module      :  Messages
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The message functions create, display, and operate a message box. The
    message box contains an application-defined message and any combination
    of predefined icons and push buttons.
-}
-----------------------------------------------------------------------------------------

module Graphics.UI.GIO.Messages
			( messageAlert
			, messageConfirm
			, messageWarning
			, messageQuestion
			, messageError
			, QuestionAnswer(..)
			, messageCancelQuestion
			, messageConfirmSave 
			) where

import Graphics.UI.Port (QuestionAnswer(..))
import qualified Graphics.UI.Port as Lib

-- | The messageAlert box provides an OK button and an image which indicates that
-- the given message is just for information.
messageAlert :: String -> IO ()
messageAlert = Lib.messageAlert

-- | The messageConfirm box, like the 'messageAlert' box provides an OK button, and in addition 
-- a Cancel button. An image indicates that the given message is just for information. 
-- The function returns True when the box is closed with the OK button; in all other cases it returns False.
messageConfirm :: String -> IO Bool
messageConfirm = Lib.messageConfirm

-- | The messageWarning box provides an OK button and an image which indicates that
-- the given message is a warning.
messageWarning :: String -> IO ()
messageWarning = Lib.messageWarning

-- | The messageQuestion box provides Yes and No buttons and an image which indicates that
-- the given message is a question. The function returns True for Yes button and False for No answer.
messageQuestion :: String -> IO Bool
messageQuestion = Lib.messageQuestion

-- | The messageError box provides OK and Cancel buttons and an image which indicates that
-- it is an error message. The function returns True when the box is closed
-- with the OK button; in all other cases it returns False.
messageError :: String -> IO Bool
messageError = Lib.messageError

-- | The messageCancelQuestion box like the 'messageQuestion' box provides an Yes and No buttons,and in addition 
-- a Cancel button. An image indicates that the given message is a question. 
messageCancelQuestion :: String -> IO QuestionAnswer
messageCancelQuestion = Lib.messageCancelQuestion

-- | The messageConfirmSave box is applicable when the application asks whether the document should be saved or not.
messageConfirmSave :: String -> IO QuestionAnswer
messageConfirmSave = Lib.messageConfirmSave
