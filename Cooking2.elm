import Graphics.Element exposing (show)
import List exposing (sum, foldr, filter)

add_underscore string = string ++"_"

odd x = (x % 2) == 1 

list = filter odd [1..23]
summed = foldr (+) 0 list
main = show summed