module Routing
(
    Bucket,
    Result(Routing.Contact,Contacts),
    Update(None,Move,Push,Replace),
    new,
    Routing.find,
    update
) where

import Const (bucketSize)
import Control.Monad.State
import Control.Applicative ((<|>))
import Bucket (Bucket,defaultEmpty,contacts,push,move,split,inRange)
import Contact (Contact)
import Data.List (find,reverse,sortBy)
import Id (Id,eq,sort)
import Info (Info)

new :: [Bucket]
new = [Bucket.defaultEmpty]

-- Find

data Result = Contact Contact | Contacts [Contact] deriving (Show,Eq)

find :: Id a => a -> [Bucket] -> Maybe Result
find i g = 
    groupBuckets i [] g >>= \(g1,b:g2) -> findContact i b <|> (return $ findClosest i (contacts b) g1 g2)

groupBuckets :: Id a => a -> [Bucket] -> [Bucket] -> Maybe ([Bucket],[Bucket])
groupBuckets _ _ [] = Nothing
groupBuckets i g1 g2@(b:_) | b `inRange` i = pure (g1,g2)
groupBuckets i g1 (b:g2) = groupBuckets i (b:g1) g2

findContact :: Id a => a -> Bucket -> Maybe Result
findContact i b = Data.List.find (eq i) (contacts b) >>= return . Routing.Contact

findClosest :: Id a => a -> [Contact] -> [Bucket] -> [Bucket] -> Result
findClosest i cs [] [] = Contacts $ take bucketSize $ sortBy (sort i) cs
findClosest i cs (b:g1) [] = 
    if length cs' >= bucketSize 
        then Contacts cs' 
        else findClosest i cs' g1 []
    where cs' = take bucketSize . sortBy (Id.sort i) $ cs ++ contacts b
findClosest i cs [] (b:g2) = 
    if length cs' >= bucketSize 
        then Contacts cs' 
        else findClosest i cs' [] g2
    where cs' = take bucketSize . sortBy (Id.sort i) $ cs ++ contacts b
findClosest i cs (b1:g1) (b2:g2) =
    if length cs' >= bucketSize 
        then Contacts cs' 
        else findClosest i cs' g1 g2
    where cs' = take bucketSize . sortBy (Id.sort i) $  cs ++ contacts b1 ++ contacts b2

-- Update 

data Update = None | Move | Push | Replace ([Bucket],[Bucket]) deriving (Show,Eq)

update :: (Id a,Info b) => a -> b -> State [Bucket] Update
update id info = state $ \g ->
    case groupBuckets info [] g of
        Just (g1,b:g2) -> update' "move" id info b g1 g2 g
        otherwise -> (None,g)

update' :: (Id a,Info b) => String -> a -> b -> Bucket -> [Bucket] -> [Bucket] -> [Bucket] -> (Update,[Bucket])
update' "move" id info b g1 g2 g = 
    if res then (Move, reverse g1 ++ (b':g2)) else update' "push" id info b g1 g2 g  
    where (res,b') = runState (move info) b
update' "push" id info b g1 g2 g  = 
    if res then (Push, reverse g1 ++ (b':g2)) else update' "split" id info b g1 g2 g
    where (res,b') = runState (push info) b
update' "split" id info b g1 g2 g | b `inRange` id =
    if b1 `inRange` info 
        then let (res,b1') = runState (push info) b1 in if res 
            then (Push, reverse g1 ++ (b1':b2:g2)) 
            else (None, g)
        else let (res,b2') = runState (push info) b2 in if res 
            then (Push, reverse g1 ++ (b1:b2':g2)) 
            else (None, g)
    where (b1,b2) = split b
update' _ _ _ b g1 g2 g = (Replace (g1, b:g2), g)