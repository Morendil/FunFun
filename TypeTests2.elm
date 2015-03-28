import Text (..)

type Record = This {x:String,y:Float} | That {x:String,z:Float}

rec1 = This {x="foo",y=11}
rec2 = That {x="bar",z=12}

extract : Record -> {a|x:String}
extract wrapped =
    case wrapped of
        This r -> {x=r.x}
        That r -> {x=r.x}

main = plainText <| (extract rec1).x ++ (extract rec2).x
    -- String.concat (List.map (.x) (List.map extract [rec1,rec2]))