-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

valAtRoot :: Tree a -> Maybe a
valAtRoot t = case t of
                Empty -> Nothing
                (Node x (_) (_)) -> Just x 

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

treeSize :: Tree a -> Int
treeSize Empty               = 0
treeSize (Node a left right) = 1 + treeSize left + treeSize right

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

treeMax :: Tree Int -> Int
treeMax Empty         = 0
treeMax t             = maximum $ treeList t

treeList :: Tree Int -> [Int]
treeList Empty = []
treeList (Node x l r) = 
  let leftList  = x : treeList l
      rightList = treeList r
   in leftList ++ rightList

------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
allValues condition Empty  = True
allValues condition (Node x l r)
  | condition x  == False             = False
  | (allValues condition l)  == False = False
  | (allValues condition r)  == False = False
  | otherwise = True

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty         = Empty
mapTree f (Node x l r)  = (Node (f x) (mapTree f l) (mapTree f r))

------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty)
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty)
--                         (Node 4 Empty Empty))
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty
--                         (Node 2 Empty Empty))
--                 (Node 3 Empty
--                         (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

cull :: Eq a => a -> Tree a -> Tree a
cull val Empty = Empty
cull val (Node x l r)
  | x == val    = Empty
  | otherwise   = Node x (cull val l) (cull val r)



------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty)
--                     (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty)
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty
--                             (Node 0 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty
--                             (Node 1 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   True

isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty                = True
isOrdered (Node x Empty Empty) = True
isOrdered (Node x left right)
  | allValues (<x) left && allValues (>x) right = case isOrdered left of
                                                    True  -> isOrdered right
                                                    False -> False
  | otherwise = False

isOrdered' :: Ord a => Tree a -> Bool
isOrdered' Empty = True
isOrdered' (Node v l r) =
  allValues (<v) l && allValues (>v) r && isOrdered' l && isOrdered' r

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk _ Empty = Nothing
walk [] (Node x _ _)      = Just x
walk [StepL] (Node x l r) = getVal l
walk [StepR] (Node x l r) = getVal r
walk (y:ys) tree  = getVal $ getTree (y:ys) tree

getTree :: [Step] -> Tree a -> Tree a 
getTree _ Empty = Empty
getTree [StepL] (Node x l r) = l
getTree [StepR] (Node x l r) = r
getTree (y:ys) tree =
  let nextBranch = getTree [y] tree
   in getTree ys nextBranch

getVal :: Tree a -> Maybe a
getVal Empty        = Nothing
getVal (Node x _ _) = Just x

walk' :: [Step] -> Tree a -> Maybe a
walk' []            (Node v _ _) = Just v
walk' (StepL:steps) (Node _ l _) = walk steps l
walk' (StepR:steps) (Node _ _ r) = walk steps r
walk' _             _            = Nothing

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)

set :: [Step] -> a -> Tree a -> Tree a
set []    _  Empty        = Empty
set []   val (Node x l r) = (Node val l r)
set (s:steps) val tree
  | (length (s:steps)) > ((treeSize tree)-1) = tree 
  | otherwise =
    let findNode = getTree (s:steps) tree -- finds the node that needs changing
        dNode    = changeNode val findNode -- changes the first value of the node and binds it
     in putNode (s:steps) dNode tree      -- places the node

changeNode :: a -> Tree a -> Tree a
changeNode a (Node x l r)  = (Node a l r)

putNode :: [Step] -> Tree a -> Tree a -> Tree a -- traverses the tree until it finds the position
putNode _ Empty Empty  = Empty                  -- and inserts the node and its branches there
putNode [] _ node = node
putNode [StepL] newNode (Node x l r) = (Node x newNode r)
putNode [StepR] newNode (Node x l r) = (Node x l newNode)
putNode (StepL:ys) newNode (Node x l r) = (Node x (putNode ys newNode l) r)
putNode (StepR:ys) newNode (Node x l r) = (Node x l (putNode ys newNode r))

set' :: [Step] -> a -> Tree a -> Tree a
set' []            val (Node _ l r) = Node val l r
set' (StepL:steps) val (Node v l r) = Node v (set steps val l) r
set' (StepR:steps) val (Node v l r) = Node v l (set steps val r)
set' _             _   t            = t

------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty) (Node 1 Empty Empty))(Node 5 Empty Empty))
--                ==>  Just [StepL,StepR]

search :: Eq a => a -> Tree a -> Maybe [Step]
search _ Empty = Nothing
search x (Node y ltree rtree)
  | x == y = Just []
  | otherwise = case search x ltree of
                  Just steps -> Just (StepL:steps)
                  Nothing      -> case search x rtree of
                                    Just steps -> Just (StepR:steps)
                                    Nothing    -> Nothing


search' :: Eq a => a -> Tree a -> Maybe [Step]      
search' _ Empty = Nothing
search' x (Node y Empty rtree)
  | x == y = Just []
  | otherwise = case search' x rtree of
                  Just steps -> Just (StepR:steps)
                  Nothing    -> Nothing
search' x (Node y ltree rtree)
  | x == y = Just []
  | otherwise = case search' x ltree of
                  Just steps -> Just (StepL:steps)
                  Nothing      -> case search' x rtree of
                                    Just steps -> Just (StepR:steps)
                                    Nothing    -> Nothing




