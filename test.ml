open Printf
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

let rec delay duration =
  try
    Thread.delay duration
  with _ -> delay duration

let draw_world os = 
  Graphics.clear_graph ();
  List.map draw os |> ignore;
  Graphics.synchronize ()

(* Simulate a constant pull toward the center *)
let force o = (force_strength *. 1.0 /. o.size) *~ (center -~ o.position)
let simulate_force o = simulate (force o) simulation_dt o

let rec run objs =
  let objs' = List.map simulate_force objs in
  delay simulation_dt;
  draw_world objs';
  Graphics.wait_next_event [Poll]
  |> (fun status -> if status.Graphics.keypressed
                    then ()
                    else run objs')


let rec repeat f x = function
  | 0 -> []
  | n -> (f x) :: (repeat f x (n-1))

let num_objs = 10

let objs = repeat (fun _ -> Mass.random screen_bound velocity_bound size_bound) () num_objs

;;
Graphics.open_graph "";;
Graphics.resize_window max_x max_y;;
Graphics.auto_synchronize false;
printf "Simulating...\n" ;;
run objs;;
printf "Bye!\n";;

