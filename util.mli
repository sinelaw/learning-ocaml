val pairs : 'a list -> ('a * 'a) list
val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val without : 'a -> 'a list -> 'a list
val list_pairs : 'a list -> ('a * 'a list) list
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
