{-# LANGUAGE EmptyDataDecls, ExistentialQuantification #-}

module Meta where

class Meta1 m where
  null :: m -> ()
  null = const ()

data Meta = forall m . Meta1 m => M m 

data Width = Width Int
data Height = Height Int

instance Meta1 Width
instance Meta1 Height

size :: [Meta]
size = [M (Width 10) , M (Height 20)]

