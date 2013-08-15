module LookupTable (mcLookup) where

import CellTypes

mcLookup :: Cell8 Bool -> [(MyVertex, MyVertex)]
mcLookup c =
    case c of 
      (Cell8 False False False False False False False False) -> []
      (Cell8 True False False False False False False False) -> [(A,B),(D,A),(E,A)]
      (Cell8 False True False False False False False False) -> [(A,B),(B,C),(B,F)]
      (Cell8 True True False False False False False False) -> [(D,A),(B,F),(E,A),(B,C),(D,A),(B,F)]
      (Cell8 False False True False False False False False) -> [(B,C),(C,D),(G,C)]
      (Cell8 True False True False False False False False) -> [(B,C),(C,D),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 False True True False False False False False) -> [(C,D),(B,F),(G,C),(A,B),(C,D),(B,F)]
      (Cell8 True True True False False False False False) -> [(D,A),(B,F),(E,A),(C,D),(B,F),(G,C),(C,D),(D,A),(B,F)]
      (Cell8 False False False True False False False False) -> [(C,D),(D,A),(D,H)]
      (Cell8 True False False True False False False False) -> [(C,D),(E,A),(D,H),(A,B),(C,D),(E,A)]
      (Cell8 False True False True False False False False) -> [(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False True False False False False) -> [(C,D),(E,A),(D,H),(C,D),(B,F),(E,A),(B,C),(C,D),(B,F)]
      (Cell8 False False True True False False False False) -> [(B,C),(D,H),(G,C),(B,C),(D,A),(D,H)]
      (Cell8 True False True True False False False False) -> [(B,C),(D,H),(G,C),(B,C),(E,A),(D,H),(A,B),(B,C),(E,A)]
      (Cell8 False True True True False False False False) -> [(B,F),(D,H),(G,C),(D,A),(B,F),(D,H),(A,B),(D,A),(B,F)]
      (Cell8 True True True True False False False False) -> [(B,F),(D,H),(G,C),(B,F),(E,A),(D,H)]
      (Cell8 False False False False True False False False) -> [(E,F),(H,E),(E,A)]
      (Cell8 True False False False True False False False) -> [(D,A),(E,F),(H,E),(A,B),(D,A),(E,F)]
      (Cell8 False True False False True False False False) -> [(E,F),(H,E),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 True True False False True False False False) -> [(D,A),(E,F),(H,E),(B,C),(E,F),(B,F),(B,C),(D,A),(E,F)]
      (Cell8 False False True False True False False False) -> [(E,F),(H,E),(E,A),(B,C),(C,D),(G,C)]
      (Cell8 True False True False True False False False) -> [(D,A),(E,F),(H,E),(A,B),(D,A),(E,F),(B,C),(C,D),(G,C)]
      (Cell8 False True True False True False False False) -> [(C,D),(B,F),(G,C),(A,B),(C,D),(B,F),(E,F),(H,E),(E,A)]
      (Cell8 True True True False True False False False) -> [(D,A),(E,F),(H,E),(C,D),(B,F),(G,C),(C,D),(E,F),(B,F),(C,D),(D,A),(E,F)]
      (Cell8 False False False True True False False False) -> [(E,F),(H,E),(E,A),(C,D),(D,A),(D,H)]
      (Cell8 True False False True True False False False) -> [(C,D),(H,E),(D,H),(C,D),(E,F),(H,E),(A,B),(C,D),(E,F)]
      (Cell8 False True False True True False False False) -> [(E,F),(H,E),(E,A),(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False True True False False False) -> [(C,D),(H,E),(D,H),(C,D),(E,F),(H,E),(B,C),(E,F),(B,F),(B,C),(C,D),(E,F)]
      (Cell8 False False True True True False False False) -> [(B,C),(D,H),(G,C),(B,C),(D,A),(D,H),(E,F),(H,E),(E,A)]
      (Cell8 True False True True True False False False) -> [(B,C),(D,H),(G,C),(B,C),(H,E),(D,H),(B,C),(E,F),(H,E),(A,B),(B,C),(E,F)]
      (Cell8 False True True True True False False False) -> [(B,F),(D,H),(G,C),(H,E),(B,F),(D,H),(E,F),(H,E),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True False False False) -> [(B,F),(D,H),(G,C),(H,E),(B,F),(D,H),(E,F),(H,E),(B,F)]
      (Cell8 False False False False False True False False) -> [(E,F),(F,G),(B,F)]
      (Cell8 True False False False False True False False) -> [(E,F),(F,G),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False True False False) -> [(B,C),(E,F),(F,G),(A,B),(B,C),(E,F)]
      (Cell8 True True False False False True False False) -> [(D,A),(E,F),(E,A),(B,C),(E,F),(F,G),(B,C),(D,A),(E,F)]
      (Cell8 False False True False False True False False) -> [(E,F),(F,G),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 True False True False False True False False) -> [(E,F),(F,G),(B,F),(B,C),(C,D),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 False True True False False True False False) -> [(C,D),(F,G),(G,C),(C,D),(E,F),(F,G),(A,B),(C,D),(E,F)]
      (Cell8 True True True False False True False False) -> [(D,A),(E,F),(E,A),(C,D),(F,G),(G,C),(C,D),(E,F),(F,G),(C,D),(D,A),(E,F)]
      (Cell8 False False False True False True False False) -> [(E,F),(F,G),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 True False False True False True False False) -> [(C,D),(E,A),(D,H),(A,B),(C,D),(E,A),(E,F),(F,G),(B,F)]
      (Cell8 False True False True False True False False) -> [(B,C),(E,F),(F,G),(A,B),(B,C),(E,F),(C,D),(D,A),(D,H)]
      (Cell8 True True False True False True False False) -> [(C,D),(E,A),(D,H),(C,D),(E,F),(E,A),(B,C),(E,F),(F,G),(B,C),(C,D),(E,F)]
      (Cell8 False False True True False True False False) -> [(B,C),(D,H),(G,C),(B,C),(D,A),(D,H),(E,F),(F,G),(B,F)]
      (Cell8 True False True True False True False False) -> [(F,G),(D,H),(G,C),(F,G),(E,A),(D,H),(E,F),(F,G),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 False True True True False True False False) -> [(F,G),(D,H),(G,C),(D,A),(F,G),(D,H),(D,A),(E,F),(F,G),(A,B),(D,A),(E,F)]
      (Cell8 True True True True False True False False) -> [(F,G),(D,H),(G,C),(F,G),(E,A),(D,H),(E,F),(F,G),(E,A)]
      (Cell8 False False False False True True False False) -> [(H,E),(B,F),(E,A),(F,G),(H,E),(B,F)]
      (Cell8 True False False False True True False False) -> [(D,A),(F,G),(H,E),(A,B),(F,G),(B,F),(A,B),(D,A),(F,G)]
      (Cell8 False True False False True True False False) -> [(A,B),(H,E),(E,A),(A,B),(F,G),(H,E),(A,B),(B,C),(F,G)]
      (Cell8 True True False False True True False False) -> [(D,A),(F,G),(H,E),(B,C),(D,A),(F,G)]
      (Cell8 False False True False True True False False) -> [(H,E),(B,F),(E,A),(F,G),(H,E),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 True False True False True True False False) -> [(D,A),(F,G),(H,E),(C,D),(F,G),(G,C),(C,D),(D,A),(F,G),(A,B),(B,C),(B,F)]
      (Cell8 False True True False True True False False) -> [(C,D),(F,G),(G,C),(A,B),(H,E),(E,A),(A,B),(F,G),(H,E),(A,B),(C,D),(F,G)]
      (Cell8 True True True False True True False False) -> [(D,A),(F,G),(H,E),(C,D),(F,G),(G,C),(C,D),(D,A),(F,G)]
      (Cell8 False False False True True True False False) -> [(H,E),(B,F),(E,A),(F,G),(H,E),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 True False False True True True False False) -> [(C,D),(H,E),(D,H),(C,D),(F,G),(H,E),(A,B),(F,G),(B,F),(A,B),(C,D),(F,G)]
      (Cell8 False True False True True True False False) -> [(C,D),(H,E),(D,H),(C,D),(F,G),(H,E),(B,C),(C,D),(F,G),(A,B),(D,A),(E,A)]
      (Cell8 True True False True True True False False) -> [(C,D),(H,E),(D,H),(C,D),(F,G),(H,E),(B,C),(C,D),(F,G)]
      (Cell8 False False True True True True False False) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H),(D,A),(B,F),(E,A),(B,C),(D,A),(B,F)]
      (Cell8 True False True True True True False False) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True True True True False False) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True True False False) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H)]
      (Cell8 False False False False False False True False) -> [(F,G),(G,H),(G,C)]
      (Cell8 True False False False False False True False) -> [(F,G),(G,H),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False False True False) -> [(F,G),(G,H),(G,C),(A,B),(B,C),(B,F)]
      (Cell8 True True False False False False True False) -> [(D,A),(B,F),(E,A),(B,C),(D,A),(B,F),(F,G),(G,H),(G,C)]
      (Cell8 False False True False False False True False) -> [(C,D),(F,G),(G,H),(B,C),(C,D),(F,G)]
      (Cell8 True False True False False False True False) -> [(C,D),(F,G),(G,H),(B,C),(C,D),(F,G),(A,B),(D,A),(E,A)]
      (Cell8 False True True False False False True False) -> [(C,D),(F,G),(G,H),(A,B),(F,G),(B,F),(A,B),(C,D),(F,G)]
      (Cell8 True True True False False False True False) -> [(D,A),(B,F),(E,A),(D,A),(F,G),(B,F),(C,D),(F,G),(G,H),(C,D),(D,A),(F,G)]
      (Cell8 False False False True False False True False) -> [(F,G),(G,H),(G,C),(C,D),(D,A),(D,H)]
      (Cell8 True False False True False False True False) -> [(C,D),(E,A),(D,H),(A,B),(C,D),(E,A),(F,G),(G,H),(G,C)]
      (Cell8 False True False True False False True False) -> [(F,G),(G,H),(G,C),(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False True False False True False) -> [(G,H),(E,A),(D,H),(G,H),(B,F),(E,A),(F,G),(G,H),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 False False True True False False True False) -> [(D,A),(G,H),(D,H),(D,A),(F,G),(G,H),(B,C),(D,A),(F,G)]
      (Cell8 True False True True False False True False) -> [(G,H),(E,A),(D,H),(A,B),(G,H),(E,A),(A,B),(F,G),(G,H),(A,B),(B,C),(F,G)]
      (Cell8 False True True True False False True False) -> [(D,A),(G,H),(D,H),(D,A),(F,G),(G,H),(A,B),(F,G),(B,F),(A,B),(D,A),(F,G)]
      (Cell8 True True True True False False True False) -> [(G,H),(E,A),(D,H),(G,H),(B,F),(E,A),(F,G),(G,H),(B,F)]
      (Cell8 False False False False True False True False) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A)]
      (Cell8 True False False False True False True False) -> [(D,A),(E,F),(H,E),(A,B),(D,A),(E,F),(F,G),(G,H),(G,C)]
      (Cell8 False True False False True False True False) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 True True False False True False True False) -> [(D,A),(G,H),(H,E),(B,C),(G,H),(G,C),(B,C),(D,A),(G,H),(E,F),(F,G),(B,F)]
      (Cell8 False False True False True False True False) -> [(C,D),(F,G),(G,H),(B,C),(C,D),(F,G),(E,F),(H,E),(E,A)]
      (Cell8 True False True False True False True False) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H),(B,C),(E,F),(F,G),(A,B),(B,C),(E,F)]
      (Cell8 False True True False True False True False) -> [(A,B),(H,E),(E,A),(A,B),(G,H),(H,E),(A,B),(C,D),(G,H),(E,F),(F,G),(B,F)]
      (Cell8 True True True False True False True False) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H),(E,F),(F,G),(B,F)]
      (Cell8 False False False True True False True False) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A),(C,D),(D,A),(D,H)]
      (Cell8 True False False True True False True False) -> [(C,D),(H,E),(D,H),(C,D),(E,F),(H,E),(A,B),(C,D),(E,F),(F,G),(G,H),(G,C)]
      (Cell8 False True False True True False True False) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A),(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False True True False True False) -> [(G,H),(H,E),(D,H),(E,F),(F,G),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 False False True True True False True False) -> [(D,A),(G,H),(D,H),(D,A),(F,G),(G,H),(B,C),(D,A),(F,G),(E,F),(H,E),(E,A)]
      (Cell8 True False True True True False True False) -> [(B,C),(E,F),(F,G),(A,B),(B,C),(E,F),(G,H),(H,E),(D,H)]
      (Cell8 False True True True True False True False) -> [(G,H),(H,E),(D,H),(E,F),(F,G),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True False True False) -> [(G,H),(H,E),(D,H),(E,F),(F,G),(B,F)]
      (Cell8 False False False False False True True False) -> [(G,H),(B,F),(G,C),(E,F),(G,H),(B,F)]
      (Cell8 True False False False False True True False) -> [(G,H),(B,F),(G,C),(E,F),(G,H),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False True True False) -> [(B,C),(G,H),(G,C),(B,C),(E,F),(G,H),(A,B),(B,C),(E,F)]
      (Cell8 True True False False False True True False) -> [(D,A),(E,F),(E,A),(B,C),(G,H),(G,C),(B,C),(E,F),(G,H),(B,C),(D,A),(E,F)]
      (Cell8 False False True False False True True False) -> [(C,D),(E,F),(G,H),(B,C),(E,F),(B,F),(B,C),(C,D),(E,F)]
      (Cell8 True False True False False True True False) -> [(D,A),(E,F),(E,A),(C,D),(E,F),(G,H),(C,D),(D,A),(E,F),(A,B),(B,C),(B,F)]
      (Cell8 False True True False False True True False) -> [(C,D),(E,F),(G,H),(A,B),(C,D),(E,F)]
      (Cell8 True True True False False True True False) -> [(D,A),(E,F),(E,A),(C,D),(E,F),(G,H),(C,D),(D,A),(E,F)]
      (Cell8 False False False True False True True False) -> [(G,H),(B,F),(G,C),(E,F),(G,H),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 True False False True False True True False) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A),(C,D),(B,F),(G,C),(A,B),(C,D),(B,F)]
      (Cell8 False True False True False True True False) -> [(D,A),(G,H),(D,H),(D,A),(E,F),(G,H),(A,B),(D,A),(E,F),(B,C),(C,D),(G,C)]
      (Cell8 True True False True False True True False) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A),(B,C),(C,D),(G,C)]
      (Cell8 False False True True False True True False) -> [(D,A),(G,H),(D,H),(D,A),(E,F),(G,H),(B,C),(E,F),(B,F),(B,C),(D,A),(E,F)]
      (Cell8 True False True True False True True False) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 False True True True False True True False) -> [(D,A),(G,H),(D,H),(D,A),(E,F),(G,H),(A,B),(D,A),(E,F)]
      (Cell8 True True True True False True True False) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A)]
      (Cell8 False False False False True True True False) -> [(H,E),(B,F),(E,A),(G,H),(B,F),(G,C),(G,H),(H,E),(B,F)]
      (Cell8 True False False False True True True False) -> [(G,H),(B,F),(G,C),(D,A),(G,H),(H,E),(A,B),(G,H),(B,F),(A,B),(D,A),(G,H)]
      (Cell8 False True False False True True True False) -> [(B,C),(G,H),(G,C),(A,B),(H,E),(E,A),(A,B),(G,H),(H,E),(A,B),(B,C),(G,H)]
      (Cell8 True True False False True True True False) -> [(D,A),(G,H),(H,E),(B,C),(G,H),(G,C),(B,C),(D,A),(G,H)]
      (Cell8 False False True False True True True False) -> [(H,E),(B,F),(E,A),(B,C),(H,E),(B,F),(B,C),(G,H),(H,E),(B,C),(C,D),(G,H)]
      (Cell8 True False True False True True True False) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True False True True True False) -> [(A,B),(H,E),(E,A),(A,B),(G,H),(H,E),(A,B),(C,D),(G,H)]
      (Cell8 True True True False True True True False) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H)]
      (Cell8 False False False True True True True False) -> [(H,E),(B,F),(E,A),(G,H),(B,F),(G,C),(G,H),(H,E),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 True False False True True True True False) -> [(C,D),(B,F),(G,C),(A,B),(C,D),(B,F),(G,H),(H,E),(D,H)]
      (Cell8 False True False True True True True False) -> [(G,H),(H,E),(D,H),(B,C),(C,D),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 True True False True True True True False) -> [(G,H),(H,E),(D,H),(B,C),(C,D),(G,C)]
      (Cell8 False False True True True True True False) -> [(D,A),(B,F),(E,A),(B,C),(D,A),(B,F),(G,H),(H,E),(D,H)]
      (Cell8 True False True True True True True False) -> [(G,H),(H,E),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True True True True True False) -> [(G,H),(H,E),(D,H),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True True True False) -> [(G,H),(H,E),(D,H)]
      (Cell8 False False False False False False False True) -> [(G,H),(H,E),(D,H)]
      (Cell8 True False False False False False False True) -> [(G,H),(H,E),(D,H),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False False False True) -> [(G,H),(H,E),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False False False False False True) -> [(D,A),(B,F),(E,A),(B,C),(D,A),(B,F),(G,H),(H,E),(D,H)]
      (Cell8 False False True False False False False True) -> [(G,H),(H,E),(D,H),(B,C),(C,D),(G,C)]
      (Cell8 True False True False False False False True) -> [(G,H),(H,E),(D,H),(B,C),(C,D),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 False True True False False False False True) -> [(C,D),(B,F),(G,C),(A,B),(C,D),(B,F),(G,H),(H,E),(D,H)]
      (Cell8 True True True False False False False True) -> [(H,E),(B,F),(E,A),(G,H),(B,F),(G,C),(G,H),(H,E),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 False False False True False False False True) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H)]
      (Cell8 True False False True False False False True) -> [(A,B),(H,E),(E,A),(A,B),(G,H),(H,E),(A,B),(C,D),(G,H)]
      (Cell8 False True False True False False False True) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False True False False False True) -> [(H,E),(B,F),(E,A),(B,C),(H,E),(B,F),(B,C),(G,H),(H,E),(B,C),(C,D),(G,H)]
      (Cell8 False False True True False False False True) -> [(D,A),(G,H),(H,E),(B,C),(G,H),(G,C),(B,C),(D,A),(G,H)]
      (Cell8 True False True True False False False True) -> [(B,C),(G,H),(G,C),(A,B),(H,E),(E,A),(A,B),(G,H),(H,E),(A,B),(B,C),(G,H)]
      (Cell8 False True True True False False False True) -> [(G,H),(B,F),(G,C),(D,A),(G,H),(H,E),(A,B),(G,H),(B,F),(A,B),(D,A),(G,H)]
      (Cell8 True True True True False False False True) -> [(H,E),(B,F),(E,A),(G,H),(B,F),(G,C),(G,H),(H,E),(B,F)]
      (Cell8 False False False False True False False True) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A)]
      (Cell8 True False False False True False False True) -> [(D,A),(G,H),(D,H),(D,A),(E,F),(G,H),(A,B),(D,A),(E,F)]
      (Cell8 False True False False True False False True) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 True True False False True False False True) -> [(D,A),(G,H),(D,H),(D,A),(E,F),(G,H),(B,C),(E,F),(B,F),(B,C),(D,A),(E,F)]
      (Cell8 False False True False True False False True) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A),(B,C),(C,D),(G,C)]
      (Cell8 True False True False True False False True) -> [(D,A),(G,H),(D,H),(D,A),(E,F),(G,H),(A,B),(D,A),(E,F),(B,C),(C,D),(G,C)]
      (Cell8 False True True False True False False True) -> [(G,H),(E,A),(D,H),(E,F),(G,H),(E,A),(C,D),(B,F),(G,C),(A,B),(C,D),(B,F)]
      (Cell8 True True True False True False False True) -> [(G,H),(B,F),(G,C),(E,F),(G,H),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 False False False True True False False True) -> [(D,A),(E,F),(E,A),(C,D),(E,F),(G,H),(C,D),(D,A),(E,F)]
      (Cell8 True False False True True False False True) -> [(C,D),(E,F),(G,H),(A,B),(C,D),(E,F)]
      (Cell8 False True False True True False False True) -> [(D,A),(E,F),(E,A),(C,D),(E,F),(G,H),(C,D),(D,A),(E,F),(A,B),(B,C),(B,F)]
      (Cell8 True True False True True False False True) -> [(C,D),(E,F),(G,H),(B,C),(E,F),(B,F),(B,C),(C,D),(E,F)]
      (Cell8 False False True True True False False True) -> [(D,A),(E,F),(E,A),(B,C),(G,H),(G,C),(B,C),(E,F),(G,H),(B,C),(D,A),(E,F)]
      (Cell8 True False True True True False False True) -> [(B,C),(G,H),(G,C),(B,C),(E,F),(G,H),(A,B),(B,C),(E,F)]
      (Cell8 False True True True True False False True) -> [(G,H),(B,F),(G,C),(E,F),(G,H),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True False False True) -> [(G,H),(B,F),(G,C),(E,F),(G,H),(B,F)]
      (Cell8 False False False False False True False True) -> [(G,H),(H,E),(D,H),(E,F),(F,G),(B,F)]
      (Cell8 True False False False False True False True) -> [(G,H),(H,E),(D,H),(E,F),(F,G),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False True False True) -> [(B,C),(E,F),(F,G),(A,B),(B,C),(E,F),(G,H),(H,E),(D,H)]
      (Cell8 True True False False False True False True) -> [(D,A),(G,H),(D,H),(D,A),(F,G),(G,H),(B,C),(D,A),(F,G),(E,F),(H,E),(E,A)]
      (Cell8 False False True False False True False True) -> [(G,H),(H,E),(D,H),(E,F),(F,G),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 True False True False False True False True) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A),(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True False False True False True) -> [(C,D),(H,E),(D,H),(C,D),(E,F),(H,E),(A,B),(C,D),(E,F),(F,G),(G,H),(G,C)]
      (Cell8 True True True False False True False True) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A),(C,D),(D,A),(D,H)]
      (Cell8 False False False True False True False True) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H),(E,F),(F,G),(B,F)]
      (Cell8 True False False True False True False True) -> [(A,B),(H,E),(E,A),(A,B),(G,H),(H,E),(A,B),(C,D),(G,H),(E,F),(F,G),(B,F)]
      (Cell8 False True False True False True False True) -> [(D,A),(G,H),(H,E),(C,D),(D,A),(G,H),(B,C),(E,F),(F,G),(A,B),(B,C),(E,F)]
      (Cell8 True True False True False True False True) -> [(C,D),(F,G),(G,H),(B,C),(C,D),(F,G),(E,F),(H,E),(E,A)]
      (Cell8 False False True True False True False True) -> [(D,A),(G,H),(H,E),(B,C),(G,H),(G,C),(B,C),(D,A),(G,H),(E,F),(F,G),(B,F)]
      (Cell8 True False True True False True False True) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 False True True True False True False True) -> [(D,A),(E,F),(H,E),(A,B),(D,A),(E,F),(F,G),(G,H),(G,C)]
      (Cell8 True True True True False True False True) -> [(F,G),(G,H),(G,C),(E,F),(H,E),(E,A)]
      (Cell8 False False False False True True False True) -> [(G,H),(E,A),(D,H),(G,H),(B,F),(E,A),(F,G),(G,H),(B,F)]
      (Cell8 True False False False True True False True) -> [(D,A),(G,H),(D,H),(D,A),(F,G),(G,H),(A,B),(F,G),(B,F),(A,B),(D,A),(F,G)]
      (Cell8 False True False False True True False True) -> [(G,H),(E,A),(D,H),(A,B),(G,H),(E,A),(A,B),(F,G),(G,H),(A,B),(B,C),(F,G)]
      (Cell8 True True False False True True False True) -> [(D,A),(G,H),(D,H),(D,A),(F,G),(G,H),(B,C),(D,A),(F,G)]
      (Cell8 False False True False True True False True) -> [(G,H),(E,A),(D,H),(G,H),(B,F),(E,A),(F,G),(G,H),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 True False True False True True False True) -> [(F,G),(G,H),(G,C),(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True False True True False True) -> [(C,D),(E,A),(D,H),(A,B),(C,D),(E,A),(F,G),(G,H),(G,C)]
      (Cell8 True True True False True True False True) -> [(F,G),(G,H),(G,C),(C,D),(D,A),(D,H)]
      (Cell8 False False False True True True False True) -> [(D,A),(B,F),(E,A),(D,A),(F,G),(B,F),(C,D),(F,G),(G,H),(C,D),(D,A),(F,G)]
      (Cell8 True False False True True True False True) -> [(C,D),(F,G),(G,H),(A,B),(F,G),(B,F),(A,B),(C,D),(F,G)]
      (Cell8 False True False True True True False True) -> [(C,D),(F,G),(G,H),(B,C),(C,D),(F,G),(A,B),(D,A),(E,A)]
      (Cell8 True True False True True True False True) -> [(C,D),(F,G),(G,H),(B,C),(C,D),(F,G)]
      (Cell8 False False True True True True False True) -> [(D,A),(B,F),(E,A),(B,C),(D,A),(B,F),(F,G),(G,H),(G,C)]
      (Cell8 True False True True True True False True) -> [(F,G),(G,H),(G,C),(A,B),(B,C),(B,F)]
      (Cell8 False True True True True True False True) -> [(F,G),(G,H),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True True False True) -> [(F,G),(G,H),(G,C)]
      (Cell8 False False False False False False True True) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H)]
      (Cell8 True False False False False False True True) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False False True True) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 True True False False False False True True) -> [(F,G),(D,H),(G,C),(F,G),(H,E),(D,H),(D,A),(B,F),(E,A),(B,C),(D,A),(B,F)]
      (Cell8 False False True False False False True True) -> [(C,D),(H,E),(D,H),(C,D),(F,G),(H,E),(B,C),(C,D),(F,G)]
      (Cell8 True False True False False False True True) -> [(C,D),(H,E),(D,H),(C,D),(F,G),(H,E),(B,C),(C,D),(F,G),(A,B),(D,A),(E,A)]
      (Cell8 False True True False False False True True) -> [(C,D),(H,E),(D,H),(C,D),(F,G),(H,E),(A,B),(F,G),(B,F),(A,B),(C,D),(F,G)]
      (Cell8 True True True False False False True True) -> [(H,E),(B,F),(E,A),(F,G),(H,E),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 False False False True False False True True) -> [(D,A),(F,G),(H,E),(C,D),(F,G),(G,C),(C,D),(D,A),(F,G)]
      (Cell8 True False False True False False True True) -> [(C,D),(F,G),(G,C),(A,B),(H,E),(E,A),(A,B),(F,G),(H,E),(A,B),(C,D),(F,G)]
      (Cell8 False True False True False False True True) -> [(D,A),(F,G),(H,E),(C,D),(F,G),(G,C),(C,D),(D,A),(F,G),(A,B),(B,C),(B,F)]
      (Cell8 True True False True False False True True) -> [(H,E),(B,F),(E,A),(F,G),(H,E),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 False False True True False False True True) -> [(D,A),(F,G),(H,E),(B,C),(D,A),(F,G)]
      (Cell8 True False True True False False True True) -> [(A,B),(H,E),(E,A),(A,B),(F,G),(H,E),(A,B),(B,C),(F,G)]
      (Cell8 False True True True False False True True) -> [(D,A),(F,G),(H,E),(A,B),(F,G),(B,F),(A,B),(D,A),(F,G)]
      (Cell8 True True True True False False True True) -> [(H,E),(B,F),(E,A),(F,G),(H,E),(B,F)]
      (Cell8 False False False False True False True True) -> [(F,G),(D,H),(G,C),(F,G),(E,A),(D,H),(E,F),(F,G),(E,A)]
      (Cell8 True False False False True False True True) -> [(F,G),(D,H),(G,C),(D,A),(F,G),(D,H),(D,A),(E,F),(F,G),(A,B),(D,A),(E,F)]
      (Cell8 False True False False True False True True) -> [(F,G),(D,H),(G,C),(F,G),(E,A),(D,H),(E,F),(F,G),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 True True False False True False True True) -> [(B,C),(D,H),(G,C),(B,C),(D,A),(D,H),(E,F),(F,G),(B,F)]
      (Cell8 False False True False True False True True) -> [(C,D),(E,A),(D,H),(C,D),(E,F),(E,A),(B,C),(E,F),(F,G),(B,C),(C,D),(E,F)]
      (Cell8 True False True False True False True True) -> [(B,C),(E,F),(F,G),(A,B),(B,C),(E,F),(C,D),(D,A),(D,H)]
      (Cell8 False True True False True False True True) -> [(C,D),(E,A),(D,H),(A,B),(C,D),(E,A),(E,F),(F,G),(B,F)]
      (Cell8 True True True False True False True True) -> [(E,F),(F,G),(B,F),(C,D),(D,A),(D,H)]
      (Cell8 False False False True True False True True) -> [(D,A),(E,F),(E,A),(C,D),(F,G),(G,C),(C,D),(E,F),(F,G),(C,D),(D,A),(E,F)]
      (Cell8 True False False True True False True True) -> [(C,D),(F,G),(G,C),(C,D),(E,F),(F,G),(A,B),(C,D),(E,F)]
      (Cell8 False True False True True False True True) -> [(E,F),(F,G),(B,F),(B,C),(C,D),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 True True False True True False True True) -> [(E,F),(F,G),(B,F),(B,C),(C,D),(G,C)]
      (Cell8 False False True True True False True True) -> [(D,A),(E,F),(E,A),(B,C),(E,F),(F,G),(B,C),(D,A),(E,F)]
      (Cell8 True False True True True False True True) -> [(B,C),(E,F),(F,G),(A,B),(B,C),(E,F)]
      (Cell8 False True True True True False True True) -> [(E,F),(F,G),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 True True True True True False True True) -> [(E,F),(F,G),(B,F)]
      (Cell8 False False False False False True True True) -> [(B,F),(D,H),(G,C),(H,E),(B,F),(D,H),(E,F),(H,E),(B,F)]
      (Cell8 True False False False False True True True) -> [(B,F),(D,H),(G,C),(H,E),(B,F),(D,H),(E,F),(H,E),(B,F),(A,B),(D,A),(E,A)]
      (Cell8 False True False False False True True True) -> [(B,C),(D,H),(G,C),(B,C),(H,E),(D,H),(B,C),(E,F),(H,E),(A,B),(B,C),(E,F)]
      (Cell8 True True False False False True True True) -> [(B,C),(D,H),(G,C),(B,C),(D,A),(D,H),(E,F),(H,E),(E,A)]
      (Cell8 False False True False False True True True) -> [(C,D),(H,E),(D,H),(C,D),(E,F),(H,E),(B,C),(E,F),(B,F),(B,C),(C,D),(E,F)]
      (Cell8 True False True False False True True True) -> [(E,F),(H,E),(E,A),(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True False False True True True) -> [(C,D),(H,E),(D,H),(C,D),(E,F),(H,E),(A,B),(C,D),(E,F)]
      (Cell8 True True True False False True True True) -> [(E,F),(H,E),(E,A),(C,D),(D,A),(D,H)]
      (Cell8 False False False True False True True True) -> [(D,A),(E,F),(H,E),(C,D),(B,F),(G,C),(C,D),(E,F),(B,F),(C,D),(D,A),(E,F)]
      (Cell8 True False False True False True True True) -> [(C,D),(B,F),(G,C),(A,B),(C,D),(B,F),(E,F),(H,E),(E,A)]
      (Cell8 False True False True False True True True) -> [(D,A),(E,F),(H,E),(A,B),(D,A),(E,F),(B,C),(C,D),(G,C)]
      (Cell8 True True False True False True True True) -> [(E,F),(H,E),(E,A),(B,C),(C,D),(G,C)]
      (Cell8 False False True True False True True True) -> [(D,A),(E,F),(H,E),(B,C),(E,F),(B,F),(B,C),(D,A),(E,F)]
      (Cell8 True False True True False True True True) -> [(E,F),(H,E),(E,A),(A,B),(B,C),(B,F)]
      (Cell8 False True True True False True True True) -> [(D,A),(E,F),(H,E),(A,B),(D,A),(E,F)]
      (Cell8 True True True True False True True True) -> [(E,F),(H,E),(E,A)]
      (Cell8 False False False False True True True True) -> [(B,F),(D,H),(G,C),(B,F),(E,A),(D,H)]
      (Cell8 True False False False True True True True) -> [(B,F),(D,H),(G,C),(D,A),(B,F),(D,H),(A,B),(D,A),(B,F)]
      (Cell8 False True False False True True True True) -> [(B,C),(D,H),(G,C),(B,C),(E,A),(D,H),(A,B),(B,C),(E,A)]
      (Cell8 True True False False True True True True) -> [(B,C),(D,H),(G,C),(B,C),(D,A),(D,H)]
      (Cell8 False False True False True True True True) -> [(C,D),(E,A),(D,H),(C,D),(B,F),(E,A),(B,C),(C,D),(B,F)]
      (Cell8 True False True False True True True True) -> [(C,D),(D,A),(D,H),(A,B),(B,C),(B,F)]
      (Cell8 False True True False True True True True) -> [(C,D),(E,A),(D,H),(A,B),(C,D),(E,A)]
      (Cell8 True True True False True True True True) -> [(C,D),(D,A),(D,H)]
      (Cell8 False False False True True True True True) -> [(D,A),(B,F),(E,A),(C,D),(B,F),(G,C),(C,D),(D,A),(B,F)]
      (Cell8 True False False True True True True True) -> [(C,D),(B,F),(G,C),(A,B),(C,D),(B,F)]
      (Cell8 False True False True True True True True) -> [(B,C),(C,D),(G,C),(A,B),(D,A),(E,A)]
      (Cell8 True True False True True True True True) -> [(B,C),(C,D),(G,C)]
      (Cell8 False False True True True True True True) -> [(D,A),(B,F),(E,A),(B,C),(D,A),(B,F)]
      (Cell8 True False True True True True True True) -> [(A,B),(B,C),(B,F)]
      (Cell8 False True True True True True True True) -> [(A,B),(D,A),(E,A)]
      (Cell8 True True True True True True True True) -> []