module Main where

import Graphics.UI.GIO
import WormShow
import WormState
import HighScore
import System.Random
import Data.IORef
import Control.Monad(when)


-- GUI constants.
hiScoresFile	= "wormhi"
nrOfHiScores	= 8


-- Start of the program.
main :: IO ()
main = do
    hiscores <- readHiScores hiScoresFile
    start "Worm" "1.0" SDI [] (startWorm hiscores)

startWorm :: HiScores -> IO ()
startWorm best = do
    ref <- newIORef (initState best)

    -- File menu
    mfile <- menu [title =: "File"] mainMenu
    mnew  <- menuitem [title =: "New",  accel =: KeyChar '\^N'] mfile
    mplay <- menuitem [title =: "Play", accel =: KeyChar '\^P'] mfile
    menuline mfile
    mexit <- menuitem [title =: "Exit",  on command =: halt] mfile

    -- Options menu
    mopts <- menu [title =: "Options"] mainMenu
    mSlow   <- menuRadioItem 
    		[ title =: "Slow"
    		, on command =: onSetSpeed ref easySpeed
    		] mopts
    mMedium <- menuRadioItem 
    		[ title =: "Medium"
		, on command =: onSetSpeed ref mediumSpeed
		] mopts
    mFast   <- menuRadioItem 
    		[ title =: "Fast"
		, on command =: onSetSpeed ref hardSpeed
		] mopts
    setMenuRadioGroup [mSlow, mMedium, mFast]
    menuline mopts
    menuitem [title =: "High Scores"
             ,accel =: KeyChar '\^S'
             ,on command =: onShowBest ref
             ] mopts

    -- Help menu
    mhelp <- menu [title =: "Help"] mainMenu
    menuitem [title =: "About Worm..."
             ,on command =: onAbout
             ] mhelp

    -- Main window
    w <- window [ bgcolor    =: wormBackGroundColour
                , bkDrawMode =: True
                , view       =: Size 488 303
                , on paint   =: onPaint ref
                , on dismiss =: halt
                , resizeable =: False
                ]
    showWindow w

    -- Timer
    tm <- timer [enabled =: False]
    set tm [on command =: onTimer ref mnew mopts mplay mexit tm w]

    set mplay [on command =: onPlay ref mnew mopts mplay mexit tm w]
    set mnew [on command =: onNew ref w]

onPaint ref can _ _ = do
    state <- readIORef ref
    drawGame state can

onNew ref w = do
    modifyIORef ref (initState . best)
    repaint w

onPlay ref mnew mopts mplay mexit tm w = do
    state <- readIORef ref
    set mplay [title =: "Stop"
              ,on command =: onStop ref mnew mopts mplay mexit tm w
              ]
    set mnew  [enabled =: False]
    set mopts [enabled =: False]
    set mexit [enabled =: False]
    set w  [on keyboard =: onKeyboard ref mnew mopts mplay mexit tm w]
    set tm [enabled =: True
           ,interval =: speed (gamelevel state)
           ]

onStop ref mnew mopts mplay mexit tm w = do
    set mplay [title =: "Play"
              ,on command =: onPlay ref mnew mopts mplay mexit tm w
              ]
    set mnew  [enabled =: True]
    set mopts [enabled =: True]
    set mexit [enabled =: True]
    set w  [off keyboard]
    set tm [enabled =: False]

onHalt ref mnew mopts mplay mexit tm w = do
    onStop ref mnew mopts mplay mexit tm w 
    onNew ref w

onAbout = do
    logo <- readBitmap "../images/logo.bmp" []
    runAboutDialog "Worm" "1.0"
                   "(C) Krasimir Angelov, 2003"
                   "The Worm is an example program\nfreely distributed with HToolkit"
                   [] [] [] logo Nothing

onSetSpeed ref speed =
    modifyIORef ref (\state -> state{gamelevel=
                           (gamelevel state){fix=speed,speed=speed}})

onShowBest ref = do
    state <- readIORef ref
    showHiScores "Worm High Scores:" (best state)

onTimer ref mnew mopts mplay mexit tm w = do
    state <- readIORef ref
    let (event,state1) = stepGame state
    writeIORef ref state1
    case event of
        IncreaseLevel -> switchLevel state1
        DecreaseLevel -> switchLevel state1
        Collide       -> nextLife state
        _             -> paintIn w UnBuffered (drawStep state state1)
        where
            switchLevel :: State -> IO ()
            switchLevel state@(State {gamelevel=gamelevel}) = do
                set w [off keyboard]
                set tm [interval =: 80
                       ,on command =: betweenLevels nrAnimationSteps (-1)
                       ]
                where
                    betweenLevels :: Int -> Int -> IO ()
                    betweenLevels animationStep step
                        | animationStep<=1 =
                            set tm [on command =: betweenLevels 2 1]
                        | animationStep<=nrAnimationSteps = do
                            paintIn w UnBuffered
                                (drawAnimation animationStep step)
                            set tm [on command =: betweenLevels (animationStep+step) step]
                        | otherwise = do
                            set tm [interval =: speed gamelevel
                                   ,on command =: onTimer ref mnew mopts mplay mexit tm w
                                   ]
                            set w [on keyboard =: onKeyboard ref mnew mopts mplay mexit tm w]
                            repaint w

            nextLife :: State -> IO ()
            nextLife state@(State {gamelevel=gamelevel
                                  ,foodsupply=foodsupply
                                  ,points=points
                                  ,best=best
                                  ,worm=worm
                                  ,lives=lives
                                  })
                | lives>0 =
                    let
                        deadWorm :: Worm -> IO ()
                        deadWorm (segment:rest) = do
                            paintIn w UnBuffered (eraseSegment segment)
                            set tm [on command =: deadWorm rest]
                        deadWorm [] = do
                            set tm [interval =: speed gamelevel
                                   ,on command =: onTimer ref mnew mopts mplay mexit tm w
                                   ]
                            set w  [on keyboard =: onKeyboard ref mnew mopts mplay mexit tm w]
                            repaint w
                    in do
                        set w  [off keyboard]
                        set tm [interval =: 100
                               ,on command =: deadWorm worm
                               ]
                | itsAHighScore nrOfHighScores points best = do
                    onHalt ref mnew mopts mplay mexit tm w
                    refName <- newIORef ""
                    dlg <- dialog [] Nothing
                    lbl1 <- label [title =: "Game Over with a new high score!"] dlg
                    lbl2 <- label [title =: "Your name:"] dlg
                    e <- entry [] dlg
                    let onOK refName dlg e = do
                        get e title >>= writeIORef refName
                        destroyWidget dlg
                    btnOK <- button [title =: "OK"
                                    ,on command =: onOK refName dlg e
                                    ] dlg
                    set dlg [layout =: padding 5 
                                         (lbl1 ^^^ padding 15 (lbl2 ^^^ hfill e) ^^^ hcenter btnOK)]
                    runDialog dlg
                    name <- readIORef refName
                    when (name /= "") $ do
                        let best' = addScore nrOfHighScores (HiScore name points) best
                        writeHiScores hiScoresFile best'
                        modifyIORef ref (\state -> state{best=best'})
                | otherwise = do
                        onHalt ref mnew mopts mplay mexit tm w
                        messageAlert "Game Over, no high score."


onKeyboard ref mnew mopts mplay mexit tm w (KeyDown key _) = do
    modifyIORef ref (\state@(State {dir=dir}) -> case key of
            KeyArrowUp    _ | dir == West  || dir == East  -> state{dir=North}
            KeyArrowDown  _ | dir == West  || dir == East  -> state{dir=South}
            KeyArrowLeft  _ | dir == North || dir == South -> state{dir=West}
            KeyArrowRight _ | dir == North || dir == South -> state{dir=East}
            _                                              -> state)
    onTimer ref mnew mopts mplay mexit tm w
onKeyboard ref mnew mopts mplay mexit tm w _ = return ()
