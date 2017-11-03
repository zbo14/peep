module ContactFixt
(
    defaultMin, 
    defaultMax, 
    defaultNumc, 
    defaultContacts,
    generateContacts 
) where 


import Binary (itob,padTo)
import Const (idSize)
import Contact (Contact,new)
import IdBytes (new)

defaultMin = 0
defaultMax = 2 ^ 160
defaultNumc = 32

generateContacts :: Integer -> Integer -> Integer -> [Contact]
generateContacts min max numc =
    foldr (\n res ->
        let Just id = IdBytes.new . padTo idSize . itob $ min + n * intv
            p = 1024 + (fromIntegral n) in
        (Contact.new id p (127,0,0,1)) : res 
    ) [] [0..numc-1]
    where intv = max `div` numc

defaultContacts = generateContacts defaultMin defaultMax defaultNumc