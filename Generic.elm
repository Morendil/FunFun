module Generic where

import List exposing (foldr,repeat,map,map2,append,concatMap)

-- GENERIC

cartesian fn list1 list2 =
    concatMap (\x -> map (fn x) list2) list1

between min max x =
  x >= min && x < max

intersects (min1, max1) (min2, max2) =
  between min1 max1 min2 ||
  between min2 max2 min1

iterate : (a -> a) -> Int -> a -> a
iterate f n =
  foldr (<<) identity (repeat n f)

mapAllBut : (List a -> a -> a) -> List a -> List a
mapAllBut f l =
  map2 f (dropEach l) l

dropEach : List a -> List (List a)
dropEach l =
  case l of
    [] -> []
    x :: [] -> [[]]
    x :: xs -> append [xs] (map (append [x]) (dropEach xs))