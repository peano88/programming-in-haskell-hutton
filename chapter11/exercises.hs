import Basic

treeleaves :: Tree a -> Int
treeleaves (Node _ []) = 1
treeleaves (Node _ ts) = sum . map treeleaves $ ts

countNodes :: Tree a -> Int
countNodes (Node _ ts) = (1+) . sum . map countNodes $ ts

-- >>> do putStr $ show (treeleaves (Node [Node [], Node []]))

countGameCombinations :: Int
countGameCombinations = treeleaves (gametree empty O)

countGameNodes :: Int
countGameNodes = countNodes (gametree empty O)

maxDepth :: Tree a -> Int
maxDepth (Node _ []) = 0 -- avoid maximum exception on empty list
maxDepth (Node _ ts) = (1+) . maximum . map maxDepth $ ts

