module HighScore
        ( HiScores, HiScore(..)
        , readHiScores
        , writeHiScores
        , itsAHighScore
        , addScore
        , showHiScores
        ) where

import	Graphics.UI.GIO

type HiScores = [HiScore]
data HiScore
   = HiScore
        { name  :: !String
        , score :: !Int
        }
        deriving (Show,Read)

-- Read in the high scores:
readHiScores :: FilePath -> IO HiScores
readHiScores fname = do
    content <- fmap lines (readFile fname)
    return (map read content)

-- Write the high scores:
writeHiScores :: FilePath -> HiScores -> IO ()
writeHiScores fname highs = do
    let content = map show highs
    writeFile fname (unlines content)

-- Determine whether, given the number of high scores, a given score is actually a new high score:
itsAHighScore :: Int -> Int -> HiScores -> Bool
itsAHighScore nrOfHiScores score' hiscores
    | score'== 0                   = False
    | length hiscores<nrOfHiScores = True
    | otherwise                    = any (\hiscore -> score' > score hiscore) hiscores


-- Add a HiScore to the current list of high scores:
addScore :: Int -> HiScore -> HiScores -> HiScores
addScore nrOfHighScores hi hiscores =
    take nrOfHighScores (addscore hi hiscores)
    where
        addscore :: HiScore -> HiScores -> HiScores
        addscore hi' hiscores@(hi:his)
            | score hi > score hi' = hi  : addscore hi' his
            | otherwise            = hi' : hiscores
        addscore hi [] = [hi]

-- Display high scores in a modal dialog to the user:
showHiScores :: String -> HiScores -> IO ()
showHiScores header highs = do
    w <- dialog [title =: "High Scores", resizeable =: False] Nothing
    hdr <- label [title =: header] w
    btnOK <- button [title =: "OK", on command =: dismissWidget w >> return ()] w
    cs <- sequence [label [title =: show hi++". "++take 20 name++" "++show score] w
                               | (hi,HiScore{name=name,score=score}) <- zip [1..] highs]
    set w [layout =: padding 10 (padding 15 hdr ^^^ column cs ^^^ padding 15 (hcenter btnOK))]
    runDialog w
