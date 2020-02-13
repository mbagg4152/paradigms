-- Binary Search Tree example

data BSTnode = MTnode |
               Node BSTnode Int BSTnode
               deriving (Show)

emptyTree = MTnode


insert :: BSTnode -> Int -> BSTnode
insert MTnode x = Node MTnode x MTnode
insert (Node t1 v t2) x
  | x < v = Node (insert t1 x) v t2
  | otherwise = Node t1 v (insert t2 x)


