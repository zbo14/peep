module Whole (Whole,isComplete) where

class Whole a where 
    isComplete :: a -> Bool
