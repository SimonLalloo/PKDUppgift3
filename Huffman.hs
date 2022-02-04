-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit
import Debug.Trace

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES:
 -}
characterCounts :: String -> Table Char Int
characterCounts = characterCounts' Table.empty

characterCounts' :: Table Char Int -> String -> Table Char Int
characterCounts' t [] = t
characterCounts' t (x:xs) =
    if exists t x
        then
            let Just v = Table.lookup t x
            in characterCounts' (Table.insert t x (v+1)) xs
        else characterCounts' (Table.insert t x 1) xs



-- modify and add comments as needed
data HuffmanTree = HuffmanTree HuffmanTree Int HuffmanTree
                | Leaf Char Int
                deriving Show



{- huffmanTree t
   PRE: t maps each key to a positive value
        t is not empty
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLES:
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t =
    let
        queue = Table.iterate t (\q (a,b) -> PriorityQueue.insert q ( Leaf a b, b)) PriorityQueue.empty
    in
        huffmanTree' queue

huffmanTree' :: PriorityQueue HuffmanTree -> HuffmanTree
huffmanTree' q =
    let
        ((a1,b1),xs) = PriorityQueue.least q
    in
        if not (PriorityQueue.is_empty xs)
            then
                let
                    ((a2,b2),ys) = PriorityQueue.least xs
                in
                    huffmanTree' (PriorityQueue.insert ys (HuffmanTree a1 (b1+b2) a2, b1+b2))
            else
                a1



{- codeTable h
   PRE: h = (HuffmanTree h1 _ h2)
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = codeTable' h []

codeTable' :: HuffmanTree -> BitCode -> Table Char BitCode
codeTable' (Leaf x _) code = Table.insert Table.empty x code
codeTable' (HuffmanTree h1 _ h2) code = update (codeTable' h1 (code ++ [False])) (codeTable' h2 (code ++ [True]))

update :: Table Char BitCode -> Table Char BitCode -> Table Char BitCode
update t1 = Table.iterate t1 (\t (a,b) -> Table.insert t a b)



{- encode h s
   PRE: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLES: 
 -}
encode :: HuffmanTree -> String -> BitCode
encode h [] = []
encode h (s:ss) = encodeAux (Table.lookup (codeTable h) s) ++ encode h ss
encodeAux (Just n) = n

tree = huffmanTree (characterCounts "hej jag heter simon.")
{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s = (huffmanTree (characterCounts s), encode (huffmanTree (characterCounts s))s )


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress = undefined
decompress h [] = []
decompress h bits = find h bits 

-- False - vänster dvs h1 _ h2 gå till h1
-- True - höger dvs h2

find :: HuffmanTree -> BitCode -> String
find h [] = []
find (Leaf x  _) b = x ++ find (h1 n h2) b
findCharacter (HuffmanTree h1 n h2) (b:bs)
              | b = find h2 bs 
              | otherwise = find h1 bs 


--let (h, bits) = compress s in decompress h bits

--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
