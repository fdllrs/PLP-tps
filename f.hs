data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving (Eq , Show)
type Procesador a b = a -> [b]
data RoseTree a = Rose a [RoseTree a] deriving (Eq,Show)
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving (Eq,Show)
sufijos :: Procesador [a] [a]
sufijos  = foldr(\x acc -> (x : head acc) : acc) [[]] 


foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT fRose (Rose n hijos) = fRose n (map rec hijos)
  where rec = foldRT fRose

foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT f b Nil = b
foldAT f b (Tern a iz cent der) = f a (rec iz) (rec cent) (rec der)
 where 
    rec = foldAT f b

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo x list) = f x (map onlyTrie list)
 where 
    onlyTrie (a,b) = (a, foldTrie f b) 

unoxuno :: Procesador [a] [a]  -- lambda
unoxuno = map (: [])  -- esto espera el a para pegarlo a una lista. 

-- ej 4)

testTree :: AT Int
testTree = Tern 16
                  (Tern 1
                      (Tern 9 Nil Nil Nil)
                      (Tern 7 Nil Nil Nil)
                      (Tern 2 Nil Nil Nil))
                  (Tern 14
                      (Tern 0 Nil Nil Nil)
                      (Tern 3 Nil Nil Nil)
                      (Tern 6 Nil Nil Nil))
                  (Tern 10
                      (Tern 8 Nil Nil Nil)
                      (Tern 5 Nil Nil Nil)
                      (Tern 4 Nil Nil Nil))


test2Tree :: RoseTree Int
test2Tree = Rose 1 
                  [ Rose 2 
                      [ Rose 4 [] 
                      , Rose 5 [] 
                      ]
                  , Rose 3 
                      [ Rose 6 [] 
                      , Rose 7 
                          [ Rose 8 []
                          , Rose 9 []
                          ]
                      ]
                  ]
main :: IO ()
main = print (preorderRose test2Tree)

preorder :: Procesador (AT a) a -- lambda
preorder  = foldAT(\x iz cen der -> x : concat [iz,cen,der]) []

postorder :: Procesador (AT a) a -- lambda
postorder = foldAT(\x iz cen der -> concat [iz,cen,der,[x]]) []

inorder :: Procesador (AT a) a 
inorder = foldAT(\x iz cen der -> concat [iz,cen ++ [x],der]) []

-- ej 5)

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRT(\x acc -> x : concat acc)   
                                      
ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRT(\x acc -> case acc of 
                                [] -> [[x]]
                                _ -> map (x : ) (concat acc))  

-- 6)
caminos 