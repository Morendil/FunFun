import Text (..)

rec1 = {x="foo",y=11}
rec2 = {x="bar",z=12}

extract : {a|x:String} -> {a|x:String}
extract r = {r | x <- r.x++"!"}

main = plainText <| (extract rec1).x ++ (extract rec2).x 