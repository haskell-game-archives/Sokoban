{-# LANGUAGE LambdaCase #-}
module Types.Square (
  getSquare,
  showSquare,
  drawSquareAt,
  getPoint,
  Square(..)
) where

import Graphics.Gloss

data Square
  = Player Point
  | Box Point 
  | Wall Point 
  | Switch Point 
  | Floor Point
  deriving (Show, Eq)

getSquare :: Char -> [Point -> Square]
getSquare = \case
  '#' -> [Wall]
  '@' -> [Player]
  '.' -> [Switch]
  '*' -> [Switch, Box]
  ' ' -> [Floor]
  '$' -> [Box]
  _   -> []

wallSprite, playerSprite, switchSprite :: IO Picture
wallSprite   = loadBMP "./Images/assets/Wall.bmp"
playerSprite = loadBMP "./Images/assets/Player.bmp"
switchSprite = loadBMP "./Images/assets/Switch.bmp"

{- for pictures: --wallSprite   >>= (return . drawPictureAt p) -}
showSquare :: Square -> IO Picture
showSquare = \case
  Wall   p -> return $ Color black  $ drawSquareAt p 16
  Player p -> return $ Color red    $ drawSquareAt p 16 
  Box    p -> return $ Color violet $ drawSquareAt p 16
  Switch p -> return $ Color blue   $ drawSquareAt p 16  
  Floor  p -> return $ Color white  $ drawSquareAt p 16

drawSquareAt :: Point -> Float -> Picture
drawSquareAt (x, y) scale = Translate (16*x) (16*y) $ rectangleSolid scale scale

drawPictureAt :: Point -> Picture -> Picture
drawPictureAt (x, y) = Translate (16*x) (16*y)

drawBMPAt :: Point -> String -> IO Picture
drawBMPAt (x, y) path = Translate (16*x) (16*y) <$> loadBMP path

getPoint :: Square -> Point
getPoint = \case
  Wall   p -> p
  Player p -> p
  Switch p -> p
  Box    p -> p
  Floor  p -> p