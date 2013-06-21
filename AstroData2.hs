module AstroData2
  ( Species(..)
  , parseSpecies
  , showSpecies ) where

import Text.Parsec

import Debug.Trace ( traceShow )


data Species = D | G | H | Hp | He | Hep | Hepp | Hm | H2 | H2p
             | R | S | H2xD | Cx | Cy | Cz | Mv
  deriving (Show, Read, Eq, Ord)

parseSpecies :: String -> Species
parseSpecies = read . (replace '-' 'm') . (replace '+' 'p')

showSpecies :: Species -> String
showSpecies = (replace 'p' '+') . (replace 'm' '-') . show

replace x y = map (\e -> if e == x then y else e)