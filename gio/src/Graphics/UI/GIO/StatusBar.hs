-----------------------------------------------------------------------------------------
{-| Module      :  StatusBar
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Defines an API for status bar creation.
    A status bar is a horizontal band at the bottom of an application window
    in which the application can display various kinds of status information.

-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.StatusBar
	( -- * StatusBar
	  StatusBar, sb
	, pushStatusBarContext, popStatusBarContext, withStatus

	  -- * Indicator
	, Indicator
	,  indicatorAt, indicator
	) where

import qualified Graphics.UI.Port as Lib
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events
import Control.Exception(bracket_)

data StatusBar = StatusBar

sb :: StatusBar
sb = StatusBar

instance Visible StatusBar where
  visible = newAttr (\sb -> Lib.getStatusBarVisible) (\sb -> Lib.setStatusBarVisible)

instance Titled StatusBar where
  title = newAttr (\sb -> Lib.getStatusBarTitle) (\sb -> Lib.setStatusBarTitle)
  
instance Countable StatusBar where
  count = readAttr "count" (\sb -> Lib.getStatusBarIndicatorsCount)


-- | Push a new status message onto the status bar stack and display it.
pushStatusBarContext :: StatusBar -> String -> IO ()
pushStatusBarContext sb title = Lib.pushStatusBarContext title

-- | Remove current status message, and display previous status
-- message, if any.  It is fine to call this with an empty stack.
popStatusBarContext :: StatusBar -> IO ()
popStatusBarContext sb = Lib.popStatusBarContext

-- | Push a new status message onto the status bar stack and display it.
withStatus :: StatusBar -> String -> IO a -> IO a
withStatus sb title action =
	bracket_
		(pushStatusBarContext sb title)
		(popStatusBarContext sb)
		action


--------------------------------------------------------------------
-- Status bar indicators
--------------------------------------------------------------------

newtype Indicator = Indicator IndicatorHandle
hindicator (Indicator h) = h

indicatorAt :: Maybe Int                               -- ^ The position where to place the indicator or Nothing if you want to append it.
       -> [Prop Indicator]                             -- ^ The setup of the indicator attributes
       -> StatusBar                                    -- ^ The statusbar (i.e. 'sb')
       -> IO Indicator                                 -- ^ The created indicator
indicatorAt pos props sb
  = do ind   <- do hindicator <- Lib.createIndicator pos
                   return (Indicator hindicator)
       set ind props
       return ind

indicator :: [Prop Indicator] -> StatusBar -> IO Indicator
indicator = indicatorAt Nothing

instance Titled Indicator where
  title = newStdAttr hindicator Lib.getIndicatorTitle Lib.setIndicatorTitle

instance Commanding Indicator where
  command = newStdEvent hindicator Lib.getIndicatorCommandHandler Lib.setIndicatorCommandHandler Lib.setIndicatorCommandDefHandler

instance Deadly Indicator where
  destroyWidget m = Lib.destroyIndicator (hindicator m)
  destroy         = newStdEvent hindicator Lib.getIndicatorDestroyHandler Lib.setIndicatorDestroyHandler Lib.setIndicatorDestroyDefHandler

instance Positioned Indicator where
  pos = readAttr "pos" (Lib.getIndicatorPos . hindicator)
