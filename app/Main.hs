module Main where

import Tree

zt = (T (T E 1 E) 2 (T E 3 E),[]) :: ZTree Int

main :: IO ()
main = do
    let t = Just zt
    print $ t >>= goLeft >>= goLeft >>= madd 0 >>= mgoTop
    print $ t >>= goRight >>= mattach (fst zt) >>= mgoTop
