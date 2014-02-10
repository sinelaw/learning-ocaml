
(*open Graphics;;*)

open Mass
open Vector

let max_x = 800
let max_y = 600
let screen_size = { x = (float_of_int max_x); y = (float_of_int max_y) }
let screen_bound = Range.range Vector.zero screen_size 
let velocity_bound = Range.range Vector.zero (150.0 *~ Vector.unit)
let size_bound = { Range.min = 10.0; Range.max = 20.0 }
let simulation_dt = 0.02
let force_strength = 30.0
let center = 0.5 *~ screen_size

let do_objs f objs = ignore (List.map f objs)

let rec delay duration =
  try
    Thread.delay duration
  with Unix.Unix_error (Unix.EAGAIN, _, _) -> delay duration

let rec run objs =
  let step os = 
    Graphics.auto_synchronize false;
    Graphics.clear_graph ();
    do_objs draw os;
    Graphics.auto_synchronize true
    (* do_objs print_obj os; *)
  in
  let force o = (force_strength *. 1.0 /. o.size) *~ ((-1.0 *~ o.position) +~ center)
  in
  let objs' = List.map (fun o -> simulate (force o) simulation_dt o) objs
  in
  delay simulation_dt;
  step objs';
  run objs'

let rec repeat f x = function
  | 0 -> []
  | n -> (f x) :: (repeat f x (n-1))
;;

let objs = repeat (fun _ -> Mass.random screen_bound velocity_bound size_bound) () 1000000;;


Graphics.open_graph " 800x600 ";;
run objs;;
