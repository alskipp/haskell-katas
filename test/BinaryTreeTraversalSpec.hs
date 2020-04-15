module BinaryTreeTraversalSpec where

import Test.Hspec

data Tree a = Nil | Node (Tree a) a (Tree a)

-- 1.) Root node, 2.) traverse left subtree, 3.) traverse right subtree.
preOrder :: Tree a -> [a]
preOrder Nil = []
preOrder (Node l root r) = root : preOrder l ++ preOrder r

-- 1.) Traverse left subtree, 2.) root node, 3.) traverse right subtree.
inOrder :: Tree a -> [a]
inOrder Nil = []
inOrder (Node l root r) = inOrder l ++ root : inOrder r

-- 1.) Traverse left subtree, 2.) traverse right subtree, 3.) root node.
postOrder :: Tree a -> [a]
postOrder Nil = []
postOrder (Node l root r) = postOrder l ++ postOrder r ++ [root]

a , b, c, d :: Tree Int
a = Node b 5 c
b = Node Nil 10 Nil
c = Node Nil 2 Nil
d = Node (Node (Node Nil 1 Nil) 2 (Node Nil 3 (Node Nil 4 Nil))) 5 Nil

spec :: Spec
spec = do
  describe "Basic tests" $ do
    it "preOrder should work for trivial examples" $ do
      preOrder (Nil :: Tree Int) `shouldBe` []
      preOrder a `shouldBe` [5, 10, 2]
      preOrder b `shouldBe` [10]
      preOrder c `shouldBe` [2]
      preOrder d `shouldBe` [5, 2, 1, 3, 4]
    it "inOrder should work for trivial examples" $ do
      inOrder (Nil :: Tree Int) `shouldBe` []
      inOrder a `shouldBe` [10, 5, 2]
      inOrder b `shouldBe` [10]
      inOrder c `shouldBe` [2]
      inOrder d `shouldBe` [1, 2, 3, 4, 5]
    it "postOrder should work for trivial examples" $ do
      postOrder (Nil :: Tree Int) `shouldBe` []
      postOrder a `shouldBe` [10, 2, 5]
      postOrder b `shouldBe` [10]
      postOrder c `shouldBe` [2]
      postOrder d `shouldBe` [1, 4, 3, 2, 5]
