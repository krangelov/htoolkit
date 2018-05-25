module WormState
        ( -- data structures
          State(..)
        , Direction(..)
        , Level(..)
        , Food(..)
        , GameEvent(..)

          -- type aliases
        , Grow, Obstacle, Segment, Worm, Points, Lives

          -- constants
        , sizeX, sizeY

        , nrOfWorms, nrOfLevels, nrOfHighScores
        , pointsPerLevel
        , startLevel
        , easySpeed, mediumSpeed, hardSpeed
        , accelation

        , initState
        , stepGame
        ) where

import	Graphics.UI.GIO
import	HighScore
import  System.Random


-- The worm data types.

data State
   = State
        { gamelevel  :: Level
        , food       :: Food
        , foodsupply :: [Food]
        , grow       :: Grow
        , points     :: Points
        , dir        :: Direction
        , worm       :: Worm
        , best       :: HiScores
        , lives      :: Lives
        }
data Direction
   = North | West | East | South deriving Eq
data Level
   = Level
        { fix        :: Int
        , speed      :: Int
        , level      :: Int
        , obstacles  :: [Obstacle]
        }
data Food = Food Int Point
type Grow      = Int
type Obstacle  = Rect
type Segment   = Point
type Worm      = [Segment]
type Points    = Int
type Lives     = Int

data GameEvent
   = Scored | Collide | IncreaseLevel | DecreaseLevel | None deriving Show

sizeX           = 45 :: Int
sizeY           = 26 :: Int

nrOfWorms       = 4   :: Int
nrOfLevels      = 8   :: Int
nrOfHighScores  = 10  :: Int
pointsPerLevel  = 500 :: Int
startLevel      = 0   :: Int

easySpeed       = 300 :: Int
mediumSpeed     = 200 :: Int
hardSpeed       = 100 :: Int
accelation      = 100 :: Int


-- Initial State.
initState :: HiScores -> State
initState best =
    State
        { gamelevel  = initlevel
        , food       = food
        , foodsupply = foodsupply
        , grow       = 0
        , points     = 0
        , dir        = East
        , worm       = initworm
        , best       = best
        , lives      = nrOfWorms
        }
        where
            (food,foodsupply) = newFood initworm initlevel (randoms (mkStdGen 0))
            initlevel         = initLevel easySpeed
            initworm          = newWorm initlevel

stepGame :: State -> (GameEvent,State)
stepGame state@(State { gamelevel=curlevel
                      , food=food@(Food value pos)
                      , foodsupply=foodsupply
                      , grow=grow
                      , points=points
                      , dir=dir
                      , worm=worm
                      , best=best
                      , lives=lives
                      })
    | levelpoints > levelpoints1 = 
        let
            newlevel            = decreaseLevel curlevel
            (food1,foodsupply1) = newFood worm1 newlevel foodsupply
            initworm            = newWorm newlevel
        in (DecreaseLevel,state
               { food=food1
               , foodsupply=foodsupply1
               , grow=0
               , points=points1
               , worm=initworm
               , gamelevel=newlevel
               , dir=East
               , lives=lives-1
               })
    | levelpoints1 > levelpoints =
        let
            newlevel            = increaseLevel curlevel
            (food1,foodsupply1) = newFood worm1 newlevel foodsupply
            initworm            = newWorm newlevel
        in (IncreaseLevel,state
               { food=food1
               , foodsupply=foodsupply1
               , grow=0
               , points=points1
               , worm=initworm
               , gamelevel=newlevel
               , dir=East
               , lives=lives+1
               })
    | scored = 
        let
            (food1,foodsupply1) = newFood worm1 curlevel foodsupply
        in (Scored,state
               { food=food1
               , foodsupply=foodsupply1
               , grow=grow1
               , points=points1
               , worm=worm1
               })
    | collide= (Collide,state
               { grow=0
               , points=points1
               , worm=newWorm curlevel
               , dir=East
               , lives=lives-1
               })
    | otherwise = (None,state
               { grow=grow1
               , worm=worm1
               })
    where
        levelpoints  = points  `div` pointsPerLevel
        levelpoints1 = points1 `div` pointsPerLevel

        hd    = newHead dir (head worm)

        worm1 | grow == 0 = hd : init worm
              | otherwise = hd : worm

        grow1 | scored    = grow+(value*3) `div` 2
              | otherwise = max 0 (grow-1)

        points1 | scored    = points+value*(length worm1) `div` 2
                | collide   = max 0 (points-100)
                | otherwise = points

        collide =  (not (pointInRect hd (Rect 1 1 sizeX sizeY)))
                  || (any (pointInRect hd) (obstacles curlevel))
                  || (hd `elem` worm)

        scored = hd == pos

        newHead :: Direction -> Segment -> Segment
        newHead North (Point x y) = Point x (y-1)
        newHead South (Point x y) = Point x (y+1)
        newHead West  (Point x y) = Point (x-1) y
        newHead East  (Point x y) = Point (x+1) y

-- Make a new initial worm.
newWorm :: Level -> Worm
newWorm Level{level=level} =
    [Point x y | x<-[5,4..1]]
    where
        y = startHeights !! (level `mod` nrOfLevels)

startHeights :: [Int]
startHeights = [13,5,13,13,13,1,1,14]

-- Construct the next level.
initLevel :: Int -> Level
initLevel fix =
    Level {fix=fix,speed=fix,level=startLevel,obstacles=sampleObstacles!!startLevel}

decreaseLevel :: Level -> Level
decreaseLevel curlevel@(Level {speed=speed,level=level}) =
    let
        newLevel = level-1
        newSpeed = if level `mod` nrOfLevels==0 && level/=0 then speed+accelation else speed
    in
        curlevel{ fix       = newSpeed
                , speed     = newSpeed
                , level     = newLevel
                , obstacles = sampleObstacles !! (newLevel `mod` nrOfLevels)
                }

increaseLevel :: Level -> Level
increaseLevel curlevel@(Level {speed=speed,level=level}) =
    let
        newLevel = level+1
        newSpeed = if level `mod` nrOfLevels==0 && level/=0 then speed-accelation else speed
    in
        curlevel{ fix       = newSpeed
                , speed     = newSpeed
                , level     = newLevel
                , obstacles = sampleObstacles !! (newLevel `mod` nrOfLevels)
                }

sampleObstacles :: [[Obstacle]]
sampleObstacles =
    [ []
    , [Rect 12 11 34 16]
    , [Rect 12  1 34  3, Rect 12 24 34 26]
    , [Rect  7  7 38  9, Rect  7 17 38 19]
    , [Rect  1  1 18 10, Rect 28 17 45 26]
    , [Rect 14  3 15 24, Rect 30  3 31 24]
    , [Rect  3 13 43 14, Rect 22  3 24 24]
    , [Rect  3  3 20 12, Rect 26 15 43 24]
    ]

-- Generate a food supply.
instance Random Food where  
    random seed = (Food value pos, seed3)
        where
            (random1,seed1) = random seed
            (random2,seed2) = random seed1
            (random3,seed3) = random seed2
            foodx           = (incMod random2 (sizeX-2))+1
            foody           = (incMod random3 (sizeY-2))+1
            pos             = Point foodx foody
            value           = incMod random1 9

            incMod a b      = (a `mod` b)+1

    randomR _ seed = random seed

-- Think of some new random food.
newFood :: Worm -> Level -> [Food] -> (Food, [Food])
newFood worm level@(Level {obstacles=obstacles}) (food@(Food _ pos):foods)
    | pos `elem` worm || any (pointInRect pos) obstacles = newFood worm level foods
    | otherwise                                          = (food, foods)
