-----------------------------------------------------------------------------------------
{-| Module      :  Layout
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The layout module helps to position controls (i.e. buttons, listboxes, etc) on
    a container. Allthough it is possible to position them by hand using the 'Dimensions' 
    class, it is much easier to position them using layout combinators. Furthermore,
    the parent widget will automatically reposition them when for example the size of
    the parent widget changes. Also, a parent widget can determine its initial view size
    based on the given layout.

    Here is an example that puts three buttons next to each other:

    > do w <- window [title =: "hi controls"]
    >    a <- button [text =: "A"] w
    >    b <- button [text =: "B"] w
    >    c <- button [text =: "C"] w
    >    set w [layout =: a <<< b <<< c]

    If @a@ and @b@ should have been placed on top of @c@, we would write:

    > (a <<< b) ^^^ c

    Each control has some minimal size. We can increase this with the 'vfix' and 'hfix'
    combinators. We can give for example @c@ a width of 80:

    > (a <<< b) ^^^ hfix 80 c

    Padding around the controls can be added with the 'pad' combinators. For example:

    > (hpad 10 a <<< b) ^^^ vpad 10 (hfix 80 c)

    Sometimes, it is easier to add padding between each control in a layout. The
    'padding' combinator can be used to do this. Padding can also be added between
    controls with the extended operators ('^^^^') and ('<<<<'):

    > (a <<<< b) ^^^^ b

    Each control has both an /inherited/ and /occupied/ area. The inherited area is the
    area that it gets from the parent widget. The occupied area is the actual area that
    is occupied by the control. If the occupied area is smaller than the inherited area,
    the control is positioned in the upper-left corner. A control can also /stretch/
    to fill the available space. For example, we can make control @a@ stretch vertically
    and control @c@ horizontally:

    > (vfill a <<<< b) ^^^^ hfill c

    There is also /glue/: an empty control that stretches to fill the available space.
    Using glue, we can for example put the controls in the lower-right corner:

    > hglue <<< (vglue ^^^ ((a <<< b) ^^^ c))

    Or in the middle:

    > hglue <<< (vglue ^^^ ((a <<< b) ^^^ c) ^^^ vglue) <<< hglue

    This pattern is actually captured in the /center/ combinators:

    > hcenter c  = row [hglue,c,hglue]

    The most primitive combinator is the 'grid'. It positions a matrix of elements in
    an aligned grid. A grid is flexible -- if a grid contains objects that stretch 
    horizontally or vertically, the entire grid will also stretch. Since ('^^^'), ('<<<'),
    'row', 'column' and others have all been formulated in terms of 'grid', these will also
    stretch automatically if one of their arguments stretches.
    
    For example, the following code will /not/ position item @a@ centered on top of item @b@:

    > hcenter a ^^^ b

    Instead, it will stretch out to fill the entire inherited space and center @a@ horizontally
    in that space. The @b@ item has no stretch and will float to the left. To center the @a@
    control relative to @b@, one needs to prevent the parent grid from stretching. The 'rigid'
    combinator can be used to do that:

    > rigid (hcenter a ^^^ b)
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Layout
             ( 
             -- * Controls
               Control
             , pack
             , Layout
             -- * Containers
             , Container
             , layout
             , autosize
             , layoutSize
             , relayout
             , hwindow
             -- * Operators
             , (^^^^), (<<<<), (^^^), (<<<) 
             -- * Layout
             , tabular, horizontal, vertical
             , grid, row, column
             , above, beside
             -- * Stretch
             , hfill, vfill, fill
             , hstretch, vstretch, stretch
             , hrigid, vrigid, rigid
             , hfix, vfix
            -- * Padding
             , hpad, vpad, hvpad, pad
             , leftpad, rightpad, toppad, bottompad
             , padding, innerpadding
             -- * Glue
             , hglue, vglue, glue
             , hcenter, vcenter, center
             -- * Invisible controls
             , empty, hrod, vrod 
             -- * Translation
             , moveTo, moveBy, move
             -- * Internal
             -- ** Functions
             , stdPack
             , getLayoutSize
             , layoutInRect
             , updateControlsVisibility
             , extractControls
             ) where

import Data.List( transpose, intersperse, (\\) )
import qualified Graphics.UI.Port as Port
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events

infixl 7 <<<, <<<<
infixl 6 ^^^, ^^^^

-- | Place two controls on top of each other. (See also 'above' and 'col').
(^^^) :: (Control w, Control v) => w -> v -> Layout
(^^^) = above

-- | Place two controls next to each other. (See also 'beside' and 'row').
(<<<) :: (Control w, Control v) => w -> v -> Layout
(<<<) = beside

-- | Place two controls on top of each other with some padding in between (10 pts). (See also 'vertical').
(^^^^) :: (Control w, Control v) => w -> v -> Layout
(^^^^) x y = vertical [pack x,pack y]

-- | Place two controls next to each other with some padding in between (10 pts). (See also 'horizontal').
(<<<<) :: (Control w, Control v) => w -> v -> Layout
(<<<<) x y = horizontal [pack x,pack y]

-- | Place controls above each other with some padding in between (10 pts). (See also 'column').
-- A vertical layout stretches automatically if it contains glue.
vertical :: Control c => [c] -> Layout
vertical cs   = column (intersperse (vrod 10) (map pack cs))

-- | Place controls beside each other with some padding in between (10 pts). (See also 'row').
-- A horizontal layout stretches automatically if it contains glue.
horizontal :: Control c => [c] -> Layout
horizontal cs = row (intersperse (hrod 10) (map pack cs))


-- | Place controls in a grid with some padding in between (10 pts). (See also 'grid').
-- A table stretches automatically if it contains glue (use 'rigid' to prevent this).
tabular :: Control c => [[c]] -> Layout
tabular css   = grid (intersperse vrods (map (intersperse (hrod 10)) (map (map pack) css)))
              where
                vrods  | null css   = []
                       | otherwise  = replicate (length (head css)) (vrod 10)

{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | Controls can be layed out on containers.
class Control w where
  -- | Create a 'Layout' from a normal control.
  pack :: w -> Layout


-- | The container is a widget which can layout other widget
class Container w where
  -- | The layout of controls within the window. This 
  -- attribute is write-only. If a control is not assigned 
  -- to the layout, it will not show up 
  -- (unless explicitly positioned using its 'Dimensions').
  -- The window will automatically update the layout when 
  -- resized or when a control changes its appearance.
  layout     :: Control c => Attr w c
  
  -- | Controls whether the widget will automatically resize
  -- to display all controls.
  autosize :: Attr w Bool
       
  -- | The layoutSize of widget is the minimum size needed to
  -- layout the controls assigned to it. This 
  -- attribute is read-only.
  layoutSize :: Attr w Size
  
  -- | The 'relayout' event is called whenever the controls in the window need to
  -- be repositioned. For example, when the window is resized or when a control changes
  -- its appearance.
  relayout :: Event w (IO ())
  
  -- | Internal
  hwindow :: w -> WindowHandle

instance Control Layout where
  pack = id

-- | Internal: creates a standard 'pack' function from a parent handle,
-- control handle and a function that returns the preferred size.
stdPack :: (w -> WindowHandle) -> (w -> WindowHandle) -> (WindowHandle -> IO Size) -> w -> Layout
stdPack getParent getControl getPrefferedSize w
  = Control (getParent w) (getControl w) getPrefferedSize

{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | A layout control. This control is not visible but it determines
-- the layout of other controls.
data Layout     = Grid    [[Layout]]
                | Changer (Pack -> Pack) Layout
                | Control WindowHandle WindowHandle (WindowHandle -> IO Size) -- parent handle, control handle, get request size
                | Empty

{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | Primitive: An empty invisible control with a zero size.
empty :: Layout
empty = Empty

-- | Primitive: Create a grid of controls, gives precise control over alignment. (See also 'tabular').
-- A grid will stretch automatically if it contains glue. To prevent this, use the 'rigid' combinator.
grid :: Control c => [[c]] -> Layout
grid css  = Grid (map (map pack) css)

-- | Place a list of controls next to each other. (See also 'horizontal').
-- A row stretches automatically if it contains glue.
row :: Control c => [c] -> Layout
row cs    = Grid [map pack cs]

-- | Place a list of controls above each other. (See also 'vertical').
-- A column stretches automatically if it contains glue.
column :: Control c => [c] -> Layout
column cs    = Grid (map (\c -> [pack c]) cs)

-- | Lay out two controls on top of each other. (See also ('^^^')).
above :: (Control w, Control v) => w -> v -> Layout
above w v   = column [pack w,pack v]

-- | Lay out two controls beside each other. (See also ('<<<')).
beside :: (Control w, Control v) => w -> v -> Layout
beside w v  = row [pack w,pack v]


-- | Make a control rigid -- it won't stretch to fit the available space (default).
rigid :: Control w => w -> Layout
rigid w = Changer (\l -> l{ stretchX = False, stretchY = False, fillX = False, fillY = False }) (pack w)

-- | Make a control vertically rigid -- it won't stretch to fit the available space (default).
vrigid :: Control w => w -> Layout
vrigid w = Changer (\l -> l{ stretchY = False, fillY = False }) (pack w)

-- | Make a control horizontally rigid -- it won't stretch to fit the available space (default).
hrigid :: Control w => w -> Layout
hrigid w = Changer (\l -> l{ stretchX = False, fillX = False }) (pack w)


-- | The control will stretch both horizontally and vertically to fill the available space.
-- Any grid with at least one filling item will also stretch to fill 
-- the entire available space.
fill :: Control w => w -> Layout
fill  w = Changer (\l -> l{ stretchX = True, stretchY = True, fillX = True, fillY = True }) (pack w)

-- | The control will stretch horizontally to fit in the available space.
-- Any grid with at least one filling item will also stretch to fill 
-- the entire available space.
hfill :: Control w => w -> Layout
hfill w  = Changer (\l -> l{ stretchX = True, fillX = True }) (pack w)

-- | The control stretches vertically to fit in the available space.
-- Any grid with at least one filling item will also stretch to fill 
-- the entire available space.
vfill :: Control w => w -> Layout
vfill w  = Changer (\l -> l{ stretchY = True, fillY = True }) (pack w)


-- | The control will stretch both horizontally and vertically to fill the available space.
-- When the control is placed in grid then the available width is equal to maximal width
-- of controls in the same column and the available height is equal to maximal height 
-- of controls in the same row.
stretch :: Control w => w -> Layout
stretch  w = Changer (\l -> l{ stretchX = True, stretchY = True, fillX = False, fillY = False }) (pack w)

-- | The control will stretch horizontally to fit in the available space.
-- When the control is placed in grid then the available space is equal to maximal width
-- of controls in the same column.
hstretch :: Control w => w -> Layout
hstretch w  = Changer (\l -> l{ stretchX = True, fillX = False }) (pack w)

-- | The control stretches vertically to fit in the available space.
-- When the control is placed in grid then the available space is equal to maximal height 
-- of controls in the same row.
vstretch :: Control w => w -> Layout
vstretch w  = Changer (\l -> l{ stretchY = True, fillY = False }) (pack w)


-- | Set the preferred width of a control.
hfix :: Control c => Int -> c -> Layout
hfix w c  = Changer (\l -> l{ prefW = w }) (pack c)

-- | Set the preferred height of a control.
vfix :: Control c => Int -> c -> Layout
vfix h c  = Changer (\l -> l{ prefH = h }) (pack c)

-- | Render the control normally, and than move it to the specified position.
moveTo :: Control c => Point -> c -> Layout
moveTo p c = move (\_ sz -> p) (pack c)

-- | Render the control normally, and than move relative to its rendered position.
moveBy :: Control c => Size -> c -> Layout
moveBy s c = move (\p sz -> pointMove s p) (pack c)

-- | Render the control normally, and than change its position according to its
-- rendered position and size.
move :: Control c => (Point -> Size -> Point) -> c -> Layout
move f c = Changer (\l -> l{ translate = f }) (pack c)


-- | Add padding to all sides of a control.
pad :: Control c => Int -> c -> Layout
pad x c = hvpad x x c

-- | Add both horizontal and vertical padding.
hvpad :: Control c => Int -> Int -> c -> Layout
hvpad h v c = hpad h (vpad v c)

-- | Add horizontal padding on both sides.
hpad :: Control c => Int -> c -> Layout
hpad h c  = row [hrod h, pack c, hrod h]

-- | Add vertical padding on both the top and bottom.
vpad :: Control c => Int -> c -> Layout
vpad v c  = column [vrod v, pack c, vrod v]

leftpad,rightpad,toppad,bottompad :: Control c => Int -> c -> Layout
leftpad x c   = hrod x <<< c
rightpad x c  = c <<< hrod x
toppad x c    = vrod x ^^^ c
bottompad x c = c ^^^ vrod x

-- | An empty control that stretches both horizontally and vertically.
glue :: Layout
glue   = fill empty

-- | An empty control that stretches horizontally.
hglue :: Layout
hglue  = hfill empty

-- | An empty control that stretches vertically.
vglue :: Layout
vglue  = vfill empty

-- | Center both horizontally and vertically
center :: Control w => w -> Layout
center w = vcenter (hcenter w)

-- | Center a control horizontally.
hcenter :: Control w => w -> Layout
hcenter w = row [hglue,pack w,hglue]

-- | Center a control vertically.
vcenter :: Control w => w -> Layout
vcenter w = column [vglue,pack w,vglue]

-- | An invisible control of a certain width and height. 
box :: Int -> Int -> Layout
box w h = vfix h (hfix w empty)

-- | A layout control of a certain width with no height. Used to implement padding.
hrod :: Int -> Layout
hrod w = vstretch (hfix w empty)

-- | A layout control of a certain height with no width. Used to implement padding.
vrod :: Int -> Layout
vrod h  = hstretch (vfix h empty)

-- | Add a padding between each control in the layout, including a padding around
-- the entire control. Note that this also adds padding around glue.
padding :: Control c => Int -> c -> Layout
padding m c
  = pad m (innerpadding m c)

-- | Add a padding between each control in the layout, but not on the outside of
-- the parent control. Note that this also adds padding around glue.
innerpadding :: Control c => Int -> c -> Layout
innerpadding m c
  = inner (pack c)
  where
    inner lay   
      = case lay of
          Changer f l   -> Changer f (inner l)
          Control _ _ _ -> lay
          Empty         -> lay
          Grid css      -> Grid (intersperse vrods (map (intersperse (hrod m)) (map (map inner) css)))
                        where
                          vrods  | null css   = []
                                 | otherwise  = replicate (length (head css)) (vrod m)

{--------------------------------------------------------------------
  
--------------------------------------------------------------------}
-- | Return the minimal size needed to display this control.
getLayoutSize :: Control c => c -> IO Size
getLayoutSize c
  = do p <- layoutToPack (pack c)
       let (rs,needed) = layoutPack 0 0 0 0 p
       return needed

-- | Positions a controls in a certain rectangle
layoutInRect :: Control c => Rect -> c -> IO Size
layoutInRect r c
  | null parents            = return (sz 0 0)
  | any (/= parent) parents = ioError (userError "Layout.layout: laying out controls from different windows")
  | otherwise = do p <- layoutToPack layout
                   let (rs,needed) = layoutPack x y w h p
                   mapM_ moveResize rs
                   return needed
  where
    layout      = pack c
    (Point x y) = topLeft r
    (Size w h)  = rectSize r
       
    parents   = map fst (extractControls layout)
    parent    = head parents
    
    moveResize (rect,layout) =
		case layout of
			Control hparent hcontrol get -> Port.moveResizeControl hcontrol rect
			Empty                        -> return ()


updateControlsVisibility :: Layout -> Layout -> IO ()
updateControlsVisibility old_lay new_lay = do
	mapM_ (\c -> Port.setControlVisible c False) (old_controls \\ new_controls)
	mapM_ (\c -> Port.setControlVisible c True ) (new_controls \\ old_controls)
	where
		old_controls = map snd (extractControls old_lay)
		new_controls = map snd (extractControls new_lay)
		
-- | The 'extractControls' function extracts the list of all controls
-- which are included in the given 'Layout'. Each control is reperesented
-- with a tuple of two handles. The first handle is the handle of its parent
-- and the second is the handle of the control itself.
extractControls :: Layout -> [(WindowHandle,WindowHandle)]
extractControls lay
  = case lay of
      Grid lss        -> concat (map (concat . map extractControls) lss)
      Changer f l     -> extractControls l
      Empty           -> []
      Control hp hc _ -> [(hp,hc)]


{--------------------------------------------------------------------
  The actual layout algorithm works on the (non-IO) Pack structure.
--------------------------------------------------------------------}
data Pack  = Table{ fillX    :: Bool, fillY    :: Bool
                  , stretchX :: Bool, stretchY :: Bool
                  , prefW    :: Int,  prefH    :: Int
                  , translate :: Point -> Size -> Point
                  , rows :: [[Pack]] 
                  }
           | Item { fillX    :: Bool, fillY    :: Bool
                  , stretchX :: Bool, stretchY :: Bool
                  , prefW    :: Int,  prefH    :: Int
                  , translate :: Point -> Size -> Point
                  , control :: Layout
                  }

table :: [[Pack]] -> Pack
table rows  = Table (any (any fillX)    rows) (any (any fillY)    rows)
                    (any (all stretchX) cols) (any (all stretchY) rows)
                    (sum (widths rows)) (sum (heights rows))
                    (\pos sz -> pos)
                    rows
    where
        cols = columns rows

layoutToPack :: Layout -> IO Pack
layoutToPack layout
  = case layout of
     Grid lss         -> do lss' <- mapM (mapM layoutToPack) lss
                            return (table lss')
     Control hparent hcontrol getPrefSize
                      -> do (Size w h) <- getPrefSize hcontrol
                            return (Item False False False False w h (\pos sz -> pos) layout)
     Empty            -> return (Item False False False False 0 0 (\pos sz -> pos) layout)
     Changer f lay    -> do fmap f (layoutToPack lay)


{--------------------------------------------------------------------
  Daan's ultra simple power-layout algorithm :-)
  ** Modified and extended by Krasimir Angelov **
--------------------------------------------------------------------}
maximal :: [Int] -> Int
maximal = foldr max 0

columns :: [[Pack]] -> [[Pack]]
columns rows   = transpose rows

widths, heights :: [[Pack]] -> [Int]
widths rows  
  = map (maximal . map prefW) (columns rows)

heights rows 
  = map (maximal . map prefH) rows


layoutPack :: Int -> Int -> Int -> Int -> Pack -> ([(Rect,Layout)],Size)
layoutPack x y w h (Item fillx filly stretchx stretchy prefW prefH trans layout)
  = let wd   = if stretchx then max w prefW else prefW
        ht   = if stretchy then max h prefH else prefH
        sz   = Size wd ht
        pos  = trans (pt x y) sz
    in ([(rectAt pos sz,layout)],sz)

layoutPack x y w h (Table fillx filly stretchx stretchy prefW prefH trans rows)
  = let mws      = widths rows
        mhs      = heights rows
        
        dw       = max 0 (w - prefW)
        dh       = max 0 (h - prefH)
        wfill    = map (\col -> any fillX col || (not fillx && all stretchX col)) (columns rows)
        hfill    = map (\row -> any fillY row || (not filly && all stretchY row)) rows
        dws      = deltas dw wfill
        dhs      = deltas dh hfill

        ws  | fillx || stretchx = zipWith (+) dws mws
            | otherwise         = mws

        hs  | filly || stretchy = zipWith (+) dhs mhs
            | otherwise         = mhs
       
        needed = sz (sum ws) (sum hs)
		
        pos = trans (pt x y) needed
        
        xs  = scanl (+) (px pos) ws
        ys  = scanl (+) (py pos) hs

    in (concat [concat [fst (layoutPack x y w h item) | (item,w,x) <- zip3 row ws xs] | (row,h,y) <- zip3 rows hs ys],needed)
  where
    deltas total stretches
      | count == 0 = repeat 0
      | otherwise  = loop rest stretches
      where
        count = length (filter id stretches)
        (delta,rest) = divMod total count
      
        loop n []             = []
        loop n (stretch:xs)   | stretch && n > 0 = (delta+1):loop (n-1) xs
                              | stretch          = delta:loop 0 xs
                              | otherwise        = 0:loop n xs
