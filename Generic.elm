module Generic where

import List exposing (foldr,repeat,map,map2,append,concatMap)

-- GENERIC

and : (a -> Bool) -> (a -> Bool) -> a -> Bool
and f g x =
  f x && g x

takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if | (predicate x) -> x :: takeWhile predicate xs
                  | otherwise -> []

addPair (x1,y1) (x2,y2) =
    (x1+x2,y1+y2)

cartesian fn list1 list2 =
    concatMap (\x -> map (fn x) list2) list1

between min max x =
  x >= min && x < max

pin minimum maximum x =
  max minimum (min maximum x)

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