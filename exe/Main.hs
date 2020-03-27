{-# LANGUAGE OverloadedStrings #-}

module Main where

main :: IO ()
main = putStrLn $ "Hello, Haskell!" ++ (show $ f 1 2)

f :: Int -> Int -> Int
f = (+)
