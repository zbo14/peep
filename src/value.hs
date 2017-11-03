{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Value 
(
    Dict,
    List,
    Value(I,B,D,L),
    Wrapper,
    emptyDict,
    newDict,
    dictInsert,
    dictLookup,
    wrap,
    unwrap
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Map (Map,empty,fromList,insert,lookup)

type Dict = Map ByteString Value

type List = [Value]

data Value = I Int | B ByteString | D Dict | L List deriving (Show,Eq,Ord)

class Wrapper a where
    wrap :: a -> Value
    unwrap :: Value -> Maybe a
    unwrap _ = Nothing 

instance Wrapper Int where 
    wrap = I
    unwrap (I i) = return i

instance Wrapper ByteString where 
    wrap = B
    unwrap (B b) = return b

instance Wrapper Dict where 
    wrap = D
    unwrap (D d) = return d

instance Wrapper List where 
    wrap = L
    unwrap (L l)= return l

emptyDict :: Dict 
emptyDict = empty

newDict :: [(String,Value)] -> Dict 
newDict = fromList . map (\(f,s) -> (pack f,s))

dictInsert :: Wrapper a => String -> a -> Dict -> Dict
dictInsert k v = insert (pack k) (wrap v)

dictLookup :: String -> Dict -> Maybe Value
dictLookup = Data.Map.lookup . pack