module Bucket
(
    Bucket,
    Bucket.new,
    defaultLowBytes,
    defaultRangeBytes,
    defaultLowId,
    empty,
    defaultEmpty,
    inRange,
    split,
    contacts,
    low,
    high,
    range,
    updated,
    push,
    move,
    Bucket.remove,
    withContacts
) where

import Const (bucketSize,idSize)
import Control.Monad.State
import Binary (btoi,itob,padTo,add,div2,prev)
import Contact (Contact,fromInfo)
import Data.ByteString.Lazy (ByteString,replicate)
import Id (Id,bytes,cmp,eq)
import IdBytes (new)
import Info (Info)
import Util (now)

defaultLowBytes = Data.ByteString.Lazy.replicate (fromIntegral idSize) 0
defaultRangeBytes = itob (2^160)
Just defaultLowId = IdBytes.new defaultLowBytes

data Bucket = Bucket {
    contacts :: [Contact],
    low :: ByteString,
    high :: ByteString,
    range :: ByteString,
    updated :: IO Int
}

instance Id Bucket where
    bytes = Bucket.low

instance Eq Bucket where 
    Bucket{contacts=cs1,low=l1,high=h1,range=r1} == Bucket{contacts=cs2,low=l2,high=h2,range=r2} =
        cs1 == cs2 && l1 == l2 && h1 == h2 && r1 == r2

instance Ord Bucket where 
    b1 `compare` b2 = Id.cmp b1 b2

instance Show Bucket where 
    show (Bucket{contacts=cs,low=l,high=h,range=r}) = 
        "BUCKET {CONTACTS=" ++ show cs ++ ", LOW=" ++ show l ++ ", HIGH=" ++ show h ++ ", RANGE=" ++ show r ++ "}" 

empty :: Id a => a -> ByteString -> Bucket
empty = Bucket.new []

new :: Id a => [Contact] -> a -> ByteString -> Bucket
new cs i r = 
    Bucket {
        contacts=cs,
        low=l,
        high=h,
        range=r,
        updated=now
    } where l = bytes i
            h = padTo idSize $ add l (prev r)

defaultEmpty :: Bucket
defaultEmpty = empty defaultLowId defaultRangeBytes

update :: Bucket -> Bucket
update (Bucket{contacts=cs,low=l,high=h,range=r}) =
    Bucket {
        contacts=cs,
        low=l,
        high=h,
        range=r,
        updated=now
    }

withContacts :: Bucket -> [Contact] -> Bucket 
withContacts (Bucket{low=l,high=h,range=r,updated=tm}) cs = 
    Bucket {
        contacts=cs,
        low=l,
        high=h,
        range=r,
        updated=tm
    }

inRange :: Id a => Bucket -> a -> Bool
inRange (Bucket{low=l,high=h}) = inRange' l h

inRange' :: Id a => ByteString -> ByteString -> a -> Bool 
inRange' l h i = 
    (n >= btoi l) && (n <= btoi h)
    where n = btoi $ Id.bytes i

split :: Bucket -> (Bucket,Bucket)
split (Bucket{contacts=cs,low=l,high=h,range=r}) = 
    (b1,b2)
    where r' = div2 r
          m = padTo idSize $ add l r'
          h' = prev m
          (cs1,cs2) = foldr (\c (cs1,cs2) -> if inRange' l h' c then (c:cs1,cs2) else (cs1,c:cs2)) ([],[]) cs
          b1 = Bucket{contacts=cs1,low=l,high=h',range=r',updated=now}
          b2 = Bucket{contacts=cs2,low=m,high=h,range=r',updated=now} 

groupContacts :: Id a => a -> [Contact] -> [Contact] -> Maybe ([Contact],[Contact])
groupContacts _ _ [] = Nothing 
groupContacts i g1 g2@(c:_) | i `eq` c = pure (g1,g2)
groupContacts i g1 (c:g2) = groupContacts i (c:g1) g2

push :: Info a => a -> State Bucket Bool
push i = state $ \b@Bucket{contacts=cs} -> 
    if length cs == bucketSize
        then (False,b) 
        else let b' = update $ withContacts b $ fromInfo i : cs in (True,b')

move :: Id a => a -> State Bucket Bool
move i = state $ \b@Bucket{contacts=cs} ->
    case groupContacts i [] cs of
        Just (g1,c:g2) -> let b' = update . withContacts b $ (c : reverse g1) ++ g2 in (True,b')
        otherwise -> (False,b)

remove :: Id a => a -> State Bucket Bool
remove i = state $ \b@Bucket{contacts=cs} ->
    case groupContacts i [] cs of
        Just (g1,c:g2) -> let b' = update . withContacts b $ reverse g1 ++ g2 in (True,b')
        otherwise -> (False,b)