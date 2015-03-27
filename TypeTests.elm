import Graphics.Element (..)
import Text (..)

rec1 = {x="foo",y=11}
rec2 = {x="bar",z=12}

extract : {a|x:String} -> String
extract record = (foo record).x

foo : {a|x:String} -> {a|x:String}
foo r = {r | x <- r.x++"!"}

main = plainText <| extract rec1 ++ extract rec2