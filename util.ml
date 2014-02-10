
let rec pairs = function
  | [] | [_] -> []
  | x :: xs -> 
     let paired_with x' l = List.fold_left (fun acc y -> (x',y) :: acc) [] l in
     paired_with x xs @ pairs xs

let uncurry f (x, y) = f x y

let rec without x = function
  | [] -> []
  | [x] -> []
  | x :: xs -> without x xs
  | y :: xs -> y :: without x xs

let list_pairs l = List.map (fun x -> (x, without x l)) l

let flip f x y = f y x
