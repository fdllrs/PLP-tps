module Proceso (Procesador, AT (Nil, Tern), RoseTree (Rose), Trie (TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc, (++!), (.!)) where

import Test.HUnit

-- Definiciones de tipos

type Procesador a b = a -> [b]

-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving (Eq)

-- E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
-- Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving (Eq)

-- E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []]
-- es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving (Eq)

-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.

-- Definiciones de Show

instance (Show a) => Show (RoseTree a) where
  show = showRoseTree 0
    where
      showRoseTree :: (Show a) => Int -> RoseTree a -> String
      showRoseTree indent (Rose value children) =
        replicate indent ' '
          ++ show value
          ++ "\n"
          ++ concatMap (showRoseTree (indent + 2)) children

instance (Show a) => Show (AT a) where
  show = showAT 0
    where
      showAT :: (Show a) => Int -> AT a -> String
      showAT _ Nil = replicate 2 ' ' ++ "Nil"
      showAT indent (Tern value left middle right) =
        replicate indent ' '
          ++ show value
          ++ "\n"
          ++ showSubtree (indent + 2) left
          ++ showSubtree (indent + 2) middle
          ++ showSubtree (indent + 2) right

      showSubtree :: (Show a) => Int -> AT a -> String
      showSubtree indent subtree =
        case subtree of
          Nil -> replicate indent ' ' ++ "Nil\n"
          _ -> showAT indent subtree

instance (Show a) => Show (Trie a) where
  show = showTrie ""
    where
      showTrie :: (Show a) => String -> Trie a -> String
      showTrie indent (TrieNodo maybeValue children) =
        let valueLine = case maybeValue of
              Nothing -> indent ++ "<vacío>\n"
              Just v -> indent ++ "Valor: " ++ show v ++ "\n"
            childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
         in valueLine ++ childrenLines

-- Ejercicio 1
procVacio :: Procesador a b
procVacio = const []

procId :: Procesador a a
procId a = [a]

procCola :: Procesador [a] a
procCola = tail

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ hijos) = hijos

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT a = case a of
  Nil -> []
  Tern raiz h1 h2 h3 -> [h1, h2, h3]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo raiz hijos) = case raiz of
  Nothing -> [Nothing]
  Just raiz -> [Just raiz]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo _ hijos) = hijos

-- Ejercicio 2
foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT f b Nil = b
foldAT f b (Tern a ri rc rd) = f a (rec ri) (rec rc) (rec rd)
  where
    rec = foldAT f b

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose fRose (Rose n hijos) = fRose n (map rec hijos)
  where
    rec = foldRose fRose

foldTrie :: (Maybe a -> [(Char, b)] -> b) -> Trie a -> b
foldTrie f (TrieNodo x list) = f x (map onlyTrie list)
  where
    onlyTrie (a, b) = (a, foldTrie f b)

-- Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = map (: []) -- esto espera el a para pegarlo a una lista.

sufijos :: Procesador [a] [a]
sufijos = foldr (\x acc -> (x : head acc) : acc) [[]]

-- Ejercicio 4
preorder :: Procesador (AT a) a
preorder = foldAT (\x ri rc rd -> x : concat [ri, rc, rd]) []

postorder :: Procesador (AT a) a
postorder = foldAT (\x ri rc rd -> concat [ri, rc, rd, [x]]) []

inorder :: Procesador (AT a) a
inorder = foldAT (\x ri rc rd -> concat [ri, rc ++ [x], rd]) []

-- Ejercicio 5
preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\x rec -> x : concat rec)

hojasRose :: Procesador (RoseTree a) a
hojasRose = undefined

ramasRose :: Procesador (RoseTree a) [a]
ramasRose =
  foldRose
    ( \x rec -> case rec of
        [] -> [[x]]
        _ -> map (x :) (concat rec)
    )

-- Ejercicio 6
caminos :: Procesador (Trie a) String
caminos = foldTrie (\_ rec -> "" : concatMap (\(c, rec2) -> map ([c] ++) rec2) rec)

-- Ejercicio 7
palabras :: Procesador (Trie a) String
palabras =
  foldTrie
    ( \m rec -> case m of
        Nothing -> concatMap (\(c, rec2) -> map ([c] ++) rec2) rec
        Just x -> "" : concatMap (\(c, rec2) -> map ([c] ++) rec2) rec
    )

-- Ejercicio 8
-- 8.a)
ifProc :: (a -> Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc cond proc1 proc2 struct = if cond struct then proc1 struct else proc2 struct

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) proc1 proc2 struct = proc1 struct ++ proc2 struct

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) proc1 proc2 struct = concatMap proc1 (proc2 struct)

-- Ejercicio 9
-- elem x foldAT (\x r1 rc rd -> r1 ++ rc ++ rd ++ [x]) [] (Tern a h1 h2 h3)
-- elem x a (\x r1 rc rd -> concat [r1, rc, rd, [x]]) a (foldAT f b h1) (foldAT f b h2) (foldAT f b h3)
-- elem x (concat [(foldAT f b h1), (foldAT f b h2), (foldAT f b h3), [a]])

-- lema : elem x (concat [a,b,c,d]) = elem x a || elem x b || elem x c || elem x d

-- elem x (foldAT f b h1) || elem x (foldAT f b h2) || elem x (foldAT f b h3) || elem x [a]
-- elem x (preorder h1) || elem x (preorder h2) || elem x (preorder h3) || elem x [a]
-- (... voy por el otro camino)
-- elem x (postorder h1) || elem x (postorder h2) || elem x (postorder h3) || elem x [a]
-- luego por HI son iguales las primeras 3 componentes de la disyunción, y la cuarta componente es trivialmente igual (elem x [a] = elem x [a])

{-Estructuras de Test-}

testTreeNil :: AT Int
testTreeNil = Nil

testTree1 :: AT Int
testTree1 =
  Tern
    16
    ( Tern
        1
        (Tern 9 Nil Nil Nil)
        (Tern 7 Nil Nil Nil)
        (Tern 2 Nil Nil Nil)
    )
    ( Tern
        14
        (Tern 0 Nil Nil Nil)
        (Tern 3 Nil Nil Nil)
        (Tern 6 Nil Nil Nil)
    )
    ( Tern
        10
        (Tern 8 Nil Nil Nil)
        (Tern 5 Nil Nil Nil)
        (Tern 4 Nil Nil Nil)
    )

testTree2 :: AT Int
testTree2 =
  Tern
    89
    ( Tern
        9
        ( Tern
            87
            ( Tern
                23
                ( Tern
                    101
                    (Tern 77 Nil Nil Nil)
                    Nil
                    Nil
                )
                Nil
                Nil
            )
            Nil
            Nil
        )
        Nil
        Nil
    )
    Nil
    Nil

testTree3 :: AT Int
testTree3 =
  Tern
    99
    ( Tern
        99
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
    )
    ( Tern
        99
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
    )
    ( Tern
        99
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
        ( Tern
            99
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
            (Tern 99 Nil Nil Nil)
        )
    )

testTree4 :: AT Int
testTree4 =
  Tern
    1
    ( Tern
        2
        ( Tern
            5
            (Tern 14 Nil Nil Nil)
            (Tern 15 Nil Nil Nil)
            (Tern 16 Nil Nil Nil)
        )
        ( Tern
            6
            (Tern 17 Nil Nil Nil)
            (Tern 18 Nil Nil Nil)
            (Tern 19 Nil Nil Nil)
        )
        ( Tern
            7
            (Tern 20 Nil Nil Nil)
            (Tern 21 Nil Nil Nil)
            (Tern 22 Nil Nil Nil)
        )
    )
    ( Tern
        3
        ( Tern
            8
            (Tern 23 Nil Nil Nil)
            (Tern 24 Nil Nil Nil)
            (Tern 25 Nil Nil Nil)
        )
        ( Tern
            9
            (Tern 26 Nil Nil Nil)
            (Tern 27 Nil Nil Nil)
            (Tern 28 Nil Nil Nil)
        )
        ( Tern
            10
            (Tern 29 Nil Nil Nil)
            (Tern 30 Nil Nil Nil)
            (Tern 31 Nil Nil Nil)
        )
    )
    ( Tern
        4
        ( Tern
            11
            (Tern 32 Nil Nil Nil)
            (Tern 33 Nil Nil Nil)
            (Tern 34 Nil Nil Nil)
        )
        ( Tern
            12
            (Tern 35 Nil Nil Nil)
            (Tern 36 Nil Nil Nil)
            (Tern 37 Nil Nil Nil)
        )
        ( Tern
            13
            (Tern 38 Nil Nil Nil)
            (Tern 39 Nil Nil Nil)
            (Tern 40 Nil Nil Nil)
        )
    )

testTree5 :: AT Int
testTree5 =
  Tern
    15
    ( Tern
        8
        Nil
        ( Tern
            22
            Nil
            Nil
            ( Tern
                34
                ( Tern
                    54
                    Nil
                    ( Tern
                        32
                        Nil
                        Nil
                        (Tern 88 Nil Nil Nil)
                    )
                    Nil
                )
                Nil
                Nil
            )
        )
        Nil
    )
    Nil
    Nil

testTree6 :: AT Char
testTree6 =
  Tern
    'a'
    ( Tern
        'a'
        ( Tern
            'v'
            (Tern 'p' Nil Nil Nil)
            (Tern 'e' Nil Nil Nil)
            (Tern 'r' Nil Nil Nil)
        )
        ( Tern
            'i'
            (Tern 'o' Nil Nil Nil)
            (Tern 'n' Nil Nil Nil)
            (Tern 'v' Nil Nil Nil)
        )
        ( Tern
            'v'
            (Tern 's' Nil Nil Nil)
            (Tern 'e' Nil Nil Nil)
            (Tern 'x' Nil Nil Nil)
        )
    )
    ( Tern
        'a'
        (Tern 'o' Nil Nil Nil)
        (Tern 's' Nil Nil Nil)
        (Tern 'o' Nil Nil Nil)
    )
    ( Tern
        'a'
        Nil
        Nil
        Nil
    )

testTrie1 :: Trie Bool
testTrie1 =
  TrieNodo
    Nothing
    [ ('a', TrieNodo (Just True) []),
      ( 'b',
        TrieNodo
          Nothing
          [ ( 'a',
              TrieNodo
                (Just True)
                [('d', TrieNodo Nothing [])]
            )
          ]
      ),
      ('c', TrieNodo (Just True) [])
    ]

testRose :: RoseTree Int
testRose =
  Rose
    1
    [ Rose
        2
        [ Rose 4 [],
          Rose 5 []
        ],
      Rose
        3
        [ Rose 6 [],
          Rose
            7
            [ Rose 8 [],
              Rose 9 []
            ]
        ]
    ]

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests =
  test
    [ -- Reemplazar los tests de prueba por tests propios
      "ejercicio1" ~: testsEj1,
      "ejercicio2" ~: testsEj2,
      "ejercicio3" ~: testsEj3,
      "ejercicio4" ~: testsEj4,
      "ejercicio5" ~: testsEj5,
      "ejercicio6" ~: testsEj6,
      "ejercicio7" ~: testsEj7,
      "ejercicio8a" ~: testsEj8a,
      "ejercicio8b" ~: testsEj8b,
      "ejercicio8c" ~: testsEj8c
    ]

testsEj1 =
  test
    [ -- Casos de test para el ejercicio 1
      0 -- Caso de test 1 - expresión a testear
        ~=? 0, -- Caso de test 1 - resultado esperado
      1 -- Caso de test 2 - expresión a testear
        ~=? 1 -- Caso de test 2 - resultado esperado
    ]

testsEj2 =
  test
    [ -- Casos de test para el ejercicio 2
      (0, 0) -- Caso de test 1 - expresión a testear
        ~=? (0, 0) -- Caso de test 1 - resultado esperado
    ]

testsEj3 =
  test
    [ -- Casos de test para el ejercicio 3
      'a' -- Caso de test 1 - expresión a testear
        ~=? 'a' -- Caso de test 1 - resultado esperado
    ]

testsEj4 =
  test
    [ -- Casos de test para el ejercicio 4
      "" -- Caso de test 1 - expresión a testear
        ~=? "" -- Caso de test 1 - resultado esperado
    ]

testsEj5 =
  test
    [ -- Casos de test para el ejercicio 5
      0 -- Caso de test 1 - expresión a testear
        ~=? 0 -- Caso de test 1 - resultado esperado
    ]

testsEj6 =
  test
    [ -- Casos de test para el ejercicio 6
      False -- Caso de test 1 - expresión a testear
        ~=? False -- Caso de test 1 - resultado esperado
    ]

testsEj7 =
  test
    [ -- Casos de test para el ejercicio 7
      True -- Caso de test 1 - expresión a testear
        ~=? True -- Caso de test 1 - resultado esperado
    ]

testsEj8a =
  test
    [ -- Casos de test para el ejercicio 7
      True -- Caso de test 1 - expresión a testear
        ~=? True -- Caso de test 1 - resultado esperado
    ]

testsEj8b =
  test
    [ -- Casos de test para el ejercicio 7
      True -- Caso de test 1 - expresión a testear
        ~=? True -- Caso de test 1 - resultado esperado
    ]

testsEj8c =
  test
    [ -- Casos de test para el ejercicio 7
      True -- Caso de test 1 - expresión a testear
        ~=? True -- Caso de test 1 - resultado esperado
    ]