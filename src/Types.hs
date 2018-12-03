{-# LANGUAGE ExistentialQuantification #-}

module Types where

data Showable = forall a . Show a => MakeShowable a

pack :: Show a => a -> Showable
pack = MakeShowable

putShowable :: Showable -> IO ()
putShowable (MakeShowable s) = putStrLn $ show s
