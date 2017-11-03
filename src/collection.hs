{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Collection (
    Collection,
    Update(None,Insert,Replace),
    Collection.lookup,
    update,
    chunks,
    withChunks
) where 

import Control.Monad.State (State,state)
import Chunk (Chunk,index,size,defaultSize)
import Data.List (find)
import Util (enumerate,insert,replace)
import Whole (Whole)

data Update = None | Insert Int | Replace Int deriving (Show)

class (Chunk a,Whole a,Chunk b) => Collection a b where
    chunks :: a -> [b]
    withChunks :: [b] -> a -> a
    lookup :: Int -> a -> Maybe b 
    lookup idx = find ((==idx) . index) . chunks
    update :: b -> State a Update
    update ch = state $ \c -> 
        let si = index ch
            sz = size c
            dsz = defaultSize ch
            dv = sz `div` dsz
            ns = if sz `mod` dsz == 0 then dv else dv+1 in
        if si < 0 then (None,c)
            else let chs = chunks c in 
            case find (\(pos,ch') -> index ch' >= si) $ enumerate chs of 
                Just (pos,ch') -> if index ch' == si 
                    then (Replace pos,withChunks (replace pos ch chs) c)  
                    else (Insert pos,withChunks (insert pos ch chs) c)
                otherwise -> let pos = length chs in (Insert pos,withChunks (insert pos ch chs) c)