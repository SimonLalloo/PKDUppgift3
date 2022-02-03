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

{-  characterCounts s
    Counts the occurences of each character in a string.
    RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s.
    EXAMPLES:
        characterCounts "test" == T [('t',2),('e',1),('s',1)]
        characterCounts "" == T []
 -}
characterCounts :: String -> Table Char Int
characterCounts = characterCounts' Table.empty

{-  characterCounts' t s
    Counts the occurences of each character in a string.
    RETURNS: Each character and the number of its occurences in s, inserted into t.
    EXAMPLES: 
        characterCounts' Table.empty "test" == T [('t',2),('e',1),('s',1)]
        characterCounts' (Table.insert Table.empty 'a' 5) "test" == T [('a',5),('t',2),('e',1),('s',1)]
-}
characterCounts' :: Table Char Int -> String -> Table Char Int
-- VARIANT: length (x:xs)
characterCounts' t [] = t
characterCounts' t (x:xs) =
    if exists t x
        then
            let Just v = Table.lookup t x
            in characterCounts' (Table.insert t x (v+1)) xs
        else characterCounts' (Table.insert t x 1) xs



{-  HuffmanTree
    A Huffman tree

    INVARIANT:
-}
data HuffmanTree = HuffmanTree HuffmanTree Int HuffmanTree
                | Leaf Char Int
                deriving Show



{-  huffmanTree t
    Makes a huffman tree from a table of characters and their counts.
    PRE: t maps each key to a positive value && t is not empty
    RETURNS: a Huffman tree based on the character counts in t
    EXAMPLES: 
        huffmanTree (characterCounts "test") == HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t =
    let
        queue = Table.iterate t (\q (a,b) -> PriorityQueue.insert q ( Leaf a b, b)) PriorityQueue.empty
    in
        huffmanTree' queue

{-  huffmanTree' q
    Makes a huffman tree from a queue of characters and their counts.
    PRE: PriorityQueue.is_empty q == False
    RETURNS: a Huffman tree based on the character counts in q
    EXAMPLES: 
        huffmanTree' (BinoHeap [Node 0 1 (Leaf 's' 1) [],Node 1 1 (Leaf 'e' 1) [Node 0 2 (Leaf 't' 2) []]])
        == HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))
-}
huffmanTree' :: PriorityQueue HuffmanTree -> HuffmanTree
-- VARIANT: The length of q
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



{-  codeTable h
    Turns a huffman tree into a code table
    RETURNS: a table that maps each character in h to its Huffman code
    EXAMPLES:
        codeTable (Leaf 'a' 5) == T [('a',[])]
        codeTable (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1)))
        == T [('s',[True,True]),('e',[True,False]),('t',[False])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = codeTable' h []

{-  codeTable' h b
    Makes a code table
    RETURNS: a table that maps each character in h to its Huffman code
    EXAMPLES:
        codeTable' (Leaf 'a' 5) [] == T [('a',[])]
        codeTable' (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) []
        == T [('s',[True,True]),('e',[True,False]),('t',[False])]
        codeTable' (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) [True, False]
        == T [('s',[True,False,True,True]),('e',[True,False,True,False]),('t',[True,False,False])]
-}
codeTable' :: HuffmanTree -> BitCode -> Table Char BitCode
-- VARIANT: The amount of nodes in the tree
codeTable' (Leaf x _) code = Table.insert Table.empty x code
codeTable' (HuffmanTree h1 _ h2) code = combine (codeTable' h1 (code ++ [False])) (codeTable' h2 (code ++ [True]))

{-  combine t1 t2
    Combines two tables.
    RETURNS: All key-value pairs from t1 inserted into t2, with the value from t1 if the key exists
    in both.
    EXAMPLES:
        combine a b == T [('a',[True]),('d',[True,True]),('c',[True,True,False]),('b',[True,True])]
        where
            a = (Table.insert (Table.insert (Table.insert (Table.empty) 'a' [True]) 'b' [True,True]) 'c' [True,True,False]);
            b = (Table.insert (Table.insert (Table.insert (Table.empty) 'a' [True]) 'd' [True,True]) 'c' [False,True,False])
-}
combine :: Table Char BitCode -> Table Char BitCode -> Table Char BitCode
combine t1 = Table.iterate t1 (\t (a,b) -> Table.insert t a b)



{- encode h s
   PRE: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLES:
 -}
encode :: HuffmanTree -> String -> BitCode
encode = undefined


{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress = undefined


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress = undefined


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
