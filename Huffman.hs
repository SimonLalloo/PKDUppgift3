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
-- Simon Lalloo & Matilda Sundberg


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
    A binary tree consisting of subtrees or leaves
    INVARIANT: The int is equal to the sum of the ints of each nodes children. Void is the empty tree.
-}
data HuffmanTree = HuffmanTree HuffmanTree Int HuffmanTree
                | Leaf Char Int
                | Void
                deriving Show



{-  huffmanTree t
    Makes a huffman tree from a table of characters and their counts.
    PRE: t maps each key to a positive value
    RETURNS: a Huffman tree based on the character counts in t
    EXAMPLES: 
        huffmanTree (characterCounts "test") == HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))
        huffmanTree $ characterCounts "" == Void
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t =
    let
        queue = Table.iterate t (\q (a,b) -> PriorityQueue.insert q ( Leaf a b, b)) PriorityQueue.empty
    in
        huffmanTree' queue

{-  huffmanTree' q
    Makes a huffman tree from a queue of characters and their counts.
    RETURNS: a Huffman tree based on the character counts in q
    EXAMPLES: 
        huffmanTree' (BinoHeap [Node 0 1 (Leaf 's' 1) [],Node 1 1 (Leaf 'e' 1) [Node 0 2 (Leaf 't' 2) []]])
        == HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))
        huffmanTree' PriorityQueue.empty == Void
-}
huffmanTree' :: PriorityQueue HuffmanTree -> HuffmanTree
-- VARIANT: The length of q
huffmanTree' q
    | PriorityQueue.is_empty q = Void
    | otherwise =
        let
            ((a1,b1),xs) = PriorityQueue.least q
        in
            if PriorityQueue.is_empty xs
                then a1
                else
                    let
                        ((a2,b2),ys) = PriorityQueue.least xs
                    in
                        huffmanTree' (PriorityQueue.insert ys (HuffmanTree a1 (b1+b2) a2, b1+b2))



{-  codeTable h
    Turns a huffman tree into a code table
    PRE: h is not Void
    RETURNS: a table that maps each character in h to its Huffman code
    EXAMPLES:
        codeTable (Leaf 'a' 5) == T [('a',[])]
        codeTable (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1)))
        == T [('s',[True,True]),('e',[True,False]),('t',[False])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable (Leaf x _) = Table.insert Table.empty x [True]
codeTable h = codeTable' h []

{-  codeTable' h b
    Makes a code table
    PRE: h is not Void
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



{-  encode h s
    Encodes a string using a huffman tree
    PRE: All characters in s appear in h
    RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
    EXAMPLES: 
        encode (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) "test" == [False,True,False,True,True,False]
        encode (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) "set" == [True,True,True,False,False]
        encode Void "" == []
 -}
encode :: HuffmanTree -> String -> BitCode
-- VARIANT: length of s
encode h [] = []
encode h (s:ss) = (\(Just n) -> n) (Table.lookup (codeTable h) s) ++ encode h ss



{-  compress s
    Compresses a string to a bitcode and its huffman tree
    RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
    EXAMPLES:
        compress "test" == (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1)),[False,True,False,True,True,False])
        compress "ttt" == (Leaf 't' 3,[True,True,True])
        compress "" == (Void,[])
 -}
compress :: String -> (HuffmanTree, BitCode)
compress [] = (Void, [])
compress s =
    let h = huffmanTree $ characterCounts s
    in (h, encode h s) 



{-  decompress h bits
    Decompress a bitcode using its huffman tree
    PRE: bits is a concatenation of valid Huffman code words for h
    RETURNS: the decoding of bits under h
    EXAMPLES:
        decompress (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) [False,True,False,True,True,False] == "test"
        decompress (Leaf 't' 3) [True,True,True] == "ttt"
        decompress Void [] == ""
 -}
decompress :: HuffmanTree -> BitCode -> String
-- VARIANT: length bits
decompress h [] = ""
decompress h bits = 
    let (x,n) = decompress' h bits 0
    in x : decompress h (drop n bits)

{-  decompress' h c n
    Decompress the first character from a bitcode using its huffman tree
    PRE: h is not Void
    RETURNS: (The first character decoded from c using h, n as the amount of c that was used)
    EXAMPLES: 
        decompress' (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) [False,True,False,True,True,False] 0 == ('t',1)
        decompress' (HuffmanTree (Leaf 't' 2) 4 (HuffmanTree (Leaf 'e' 1) 2 (Leaf 's' 1))) [False,True,False,True,True,False] 5 == ('t',6)
-}
decompress' :: HuffmanTree -> BitCode -> Int -> (Char, Int)
-- VARIANT: the height of h
decompress' (Leaf x _) _ n = 
    if n /= 0
        then (x,n)
        else (x,1)
decompress' (HuffmanTree h1 _ h2) (x:xs) n
    | x = decompress' h2 xs (n+1)
    |otherwise = decompress' h1 xs (n+1)
    


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
