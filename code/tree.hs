data Tree = Empty 
          | Branch Tree Tree Integer
          deriving (Show, Eq)
eg = Branch 
    (Branch 
          (Branch Empty Empty 10) 
          (Branch Empty Empty 47) 
          33
    ) 
    (Branch 
          Empty 
          (Branch Empty Empty 93) 
          81
    ) 
    60


tfold :: (Integer -> Integer -> Integer) -> Integer -> Tree -> Integer
tfold fn initialValue Empty = initialValue
tfold fn initialValue (Branch l r v) =
  fn v (fn foldLeftChild foldRightChild)
  where
      foldLeftChild  = doFold l
      foldRightChild = doFold r
      doFold = tfold fn initialValue

tsum :: Tree -> Integer
tsum = tfold (+) 0

tadd :: Tree -> Integer -> Tree
tadd Empty value = Branch Empty Empty value
tadd (Branch l r nodeValue) newValue 
  | newValue < nodeValue = Branch (tadd l newValue) r nodeValue
  | otherwise            = Branch l (tadd r newValue) nodeValue

tbuild:: [Integer] -> Tree
tbuild = foldl tadd Empty 
