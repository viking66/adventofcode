{-# LANGUAGE ExistentialQuantification #-}

module Types where

data Showable = forall a . Show a => MakeShowable a

instance Show Showable where
  show (MakeShowable s) = show s

pack :: Show a => a -> Showable
pack = MakeShowable
