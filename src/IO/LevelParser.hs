module IO.LevelParser (
  parseLevel,
  parseSasquatch,
  toThousand
) where

import Graphics.Gloss (Point)
import Types.Misc
import Types.Square
import Text.ParserCombinators.Parsec
import Text.Parsec
import Types.Level
import Types.Game (toThousand)

parseLevel :: String -> Level Square
parseLevel level =
  case player initWorld of
    (Player (-1,-1)) -> error "No player on board"
    (Player _      ) -> initWorld
    _                -> error "Internal misbehavior -> Player is not what it seems."
  where
    cIndices = let l = length lns `div` 2
                in [fromIntegral (-l)..] :: [Float]
    rIndices = let l = maximum (map length lns) `div` 2
                in [fromIntegral (-l)..] :: [Float]
    lns = reverse $ lines level
    lns' = zip cIndices lns
    sqrs :: (Float, String) -> [Square]
    sqrs (a, xs) = concatMap fix $ zip (map getSquare xs) (zip rIndices (repeat a) :: [Point])
      where fix (xs, y) = map ($y) xs
    populateWorld [] w = w
    populateWorld (p:points) w@(Level plr bxs wls sws) = populateWorld points w'
      where w' = case p of
                    p@(Player _)   -> Level p bxs wls sws
                    b@(Box _ )     -> Level plr (b:bxs) wls sws
                    w@(Wall _)     -> Level plr bxs (w:wls) sws
                    s@(Switch _)   -> Level plr bxs wls (s:sws)
                    _              -> w
    emptyWorld = Level (Player (-1,-1)) [] [] []
    initWorld = populateWorld (lns' >>= sqrs) emptyWorld

parseSasquatch :: String -> Either ParseError [String]
parseSasquatch = parse (many level) "(unknown)"

level :: Parser String
level = do
  spaces
  char ';'
  space
  many $ oneOf ['0'..'9']
  optional space
  optional title
  space
  many $ noneOf ";"

title :: Parser ()
title = do
  char '\''
  many $ noneOf "\'"
  char '\''
  return ()