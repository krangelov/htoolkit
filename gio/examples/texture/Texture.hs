module Main where

import System.Random
import Graphics.UI.GIO
import Data.Array
import Data.List
import Data.IORef

main
  = start "texture" "1.0" SDI [] $ do
      w <- window [ domain =: addh (sz 200 0) imageSize
                  , view   =: addh (sz 200 0) imageSize
                  ]

      opts <- notebook [] w

      inputsRef <- newIORef []
      turbulence <- notebookPage [title =: "turbulence"] opts
      addScaleButton <- button [title =: "Add"] turbulence
      updateButton <- button [title =: "Update"] w
      let buttons = hstretch addScaleButton
      setTurbulenceLayout inputsRef turbulence buttons

      waves <- notebookPage [title =: "waves"] opts
      nrOfWavesInput <- entry [] waves
      updateWavesButton <- button [title =: "Update"] waves
      set waves [layout =: hstretch nrOfWavesInput]

      bmp <- createBitmap imageSize
      imgView <- compoundControl [on paint =: paintImage bmp] w

      mbox <- groupBox [title =: "Modifiers"] w
      noneRadio   <- radioBox [title =: "None"  ] mbox
      circleRadio <- radioBox [title =: "Circle"] mbox
      marbleRadio <- radioBox [title =: "Marble"] mbox
      setRadioBoxGroup [noneRadio,circleRadio,marbleRadio]
      set mbox [layout =: noneRadio ^^^ circleRadio ^^^ marbleRadio]

      set w [layout =: (opts ^^^ hstretch mbox ^^^ hstretch updateButton) <<< fill imgView]

      set addScaleButton [on command =: addScaleInput inputsRef turbulence buttons]
      set updateButton   [on command =: updateImage bmp opts inputsRef nrOfWavesInput circleRadio marbleRadio imgView]

      showWindow w
      return ()
  where
    addScaleInput inputsRef turbulence buttons = do
      inputs <- readIORef inputsRef
      input <- entry [] turbulence
      writeIORef inputsRef (inputs++[input])
      setTurbulenceLayout inputsRef turbulence buttons
   
    setTurbulenceLayout inputsRef w buttons = do
      inputs <- readIORef inputsRef
      set w [layout =: vertical inputs ^^^ buttons]

    updateImage bmp opts inputsRef nrOfWavesInput circleRadio marbleRadio imgView = do
      pos <- get opts selected
      if pos == 0
        then updateTurbulence bmp inputsRef circleRadio marbleRadio imgView
        else updateWaves bmp nrOfWavesInput circleRadio marbleRadio imgView

    updateTurbulence bmp inputsRef circleRadio marbleRadio imgView = do
      inputs <- readIORef inputsRef
      gen <- newStdGen
      let m0 = noise gen ((0,0),(sw imageSize,sh imageSize))
      ms <- mapM (fmap ((\n -> scale n n m0) . read) . flip get title) inputs
      let m1 = combine ms
      m2 <- applyMods circleRadio marbleRadio m1
      
      paintInBitmap bmp windowPen (paintTexture m2 m2 m2)
      repaint imgView

    updateWaves bmp nrOfWavesInput circleRadio marbleRadio imgView = do
      nrOfWaves <- fmap read (get nrOfWavesInput title)
      gen <- newStdGen
      let m0 = combine (take nrOfWaves [wave w1 w2 ((0,0),(sw imageSize,sh imageSize)) | (w1,w2) <- randoms gen])
      m1 <- applyMods circleRadio marbleRadio m0
      paintInBitmap bmp windowPen (paintTexture m1 m1 m1)
      repaint imgView
      
    applyMods circleRadio marbleRadio m = do
      b <- get circleRadio checked
      if b
        then return (circle m)
        else do b <- get marbleRadio checked
                if b
                  then return (marble m)
                  else return m

imageSize = sz 400 400

data Matrix = Matrix { matrixBounds :: ((Int,Int),(Int,Int))
                     , matrixValue  :: (Int,Int) -> Double
                     }

noise :: StdGen -> ((Int,Int),(Int,Int)) -> Matrix
noise gen bnds =
  let a = listArray bnds (randoms gen)
  in Matrix bnds (\ij -> a ! ij)

matrix2array m =
  listArray (matrixBounds m) (map (matrixValue m) (Data.Array.range (matrixBounds m)))

scale :: Double -> Double -> Matrix -> Matrix
scale sx sy m =
  Matrix ((a,b),(a+m',b+n')) compute
  where
    ((a,b),(c,d)) = matrixBounds m
    m' = truncate (fromIntegral (c-a)*sx)
    n' = truncate (fromIntegral (d-b)*sy)

    compute (i,j) = (c00*(1-fy) + c01*fy)*(1-fx) +
                    (c10*(1-fy) + c11*fy)*fx
      where
        (x0,fx) = properFraction ((fromIntegral i)/sx)
        (y0,fy) = properFraction ((fromIntegral j)/sy)
        x  = a + x0
        y  = b + y0
        c00= matrixValue m (x,y)
        c01= matrixValue m (x,min d (y+1))
        c10= matrixValue m (min a (x+1),y)
        c11= matrixValue m (min a (x+1),min d (y+1))

combine :: [Matrix] -> Matrix
combine ms =
  Matrix ((a,b),(c,d)) (\ij -> sum (map (flip matrixValue ij) ms) / fromIntegral (length ms))
  where
    (as,bs,cs,ds) = unzip4 [(a1,b1,c1,d1) | ((a1,b1),(c1,d1)) <- map matrixBounds ms]
    a = maximum as
    b = maximum bs
    c = minimum cs
    d = minimum ds

circle :: Matrix -> Matrix
circle m =
  Matrix ((a,b),(c,d)) (\(i,j) -> 0.5+0.5*sin (0.2 * sqrt (fromIntegral ((i-x0)^2 + (j-y0)^2)) + 10 * matrixValue m (i,j)))
  where
    ((a,b),(c,d)) = matrixBounds m
    x0 = (a+c) `div` 2
    y0 = (b+d) `div` 2

marble :: Matrix -> Matrix
marble m =
  Matrix (matrixBounds m) (\(i,j) -> 0.5+0.5*sin (0.2 * fromIntegral (i+j)+30 * matrixValue m (i,j)))

wave :: Double -> Double -> ((Int,Int),(Int,Int)) -> Matrix
wave w1 w2 ((a,b),(c,d)) =
  Matrix ((a,b),(c,d)) (\(i,j) -> 0.5+0.5*sin (0.2 * (w1*fromIntegral i+w2*fromIntegral j)))

paintTexture mr mg mb can
  = do mapM_ drawPixel (Data.Array.range ((0,0),(sw imageSize,sh imageSize)))
       where
         drawPixel (i,j) = do
           let r = truncate (matrixValue mr (i,j)*255)
               g = truncate (matrixValue mg (i,j)*255)
               b = truncate (matrixValue mb (i,j)*255)
           setCanvasPen can [pen =: windowPen{penColor=rgbColor r g b}]
           drawPoint (Point i j) can

paintImage image can updFrame updAreas
  = do drawBitmap (pt 0 0) image can

instance (Random a, Random b) => Random (a,b) where
  random g =
    let (a,g')  = random g
        (b,g'') = random g'
    in ((a,b),g'')
  randomR ((aMin,bMin),(aMax,bMax)) g =
    let (a,g')  = randomR (aMin,aMax) g
        (b,g'') = randomR (bMin,bMax) g'
    in ((a,b),g'')
