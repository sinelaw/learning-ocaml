
(*open Graphics;;*)

open Printf


let max_x = 800;;
let max_y = 600;;
Graphics.open_graph " 800x600 ";;

module Range = struct
  type 'a t = 
    { min : 'a;
      max : 'a
    }
end   
  
module Vector = struct
(* sig *)
(*   type t *)
(*   val ( +~ ) : t -> t -> t *)
(*   val ( -~ ) : t -> t -> t *)
(*   val ( *~ ) : float -> t -> t *)
(*   val random : float -> float -> t *)
(* end = struct *)
  type t  = 
    { x : float;
      y : float
    }

  let ( +~ ) v w =
    { x = v.x +. w.x;
      y = v.y +. w.y
    }

  let ( *~ ) t v =
    { x = v.x *. t;
      y = v.y *. t
    }

  let ( -~ ) v w = 
    (-1.0 *~ w) +~ v

  let random (bound:t Range.t) = 
    let diff:t = (bound.max -~ bound.min) in
    bound.min +~ { x = Random.float diff.x;
                   y = Random.float diff.y;
                 }
end

module Obj = struct
  type t =
    { position : vector;
      velocity : vector;
      size : float;
      color : Graphics.color
    }

  let print o =
    printf "{ x = %f, y = %f }\n" o.position.x o.position.y

  let simulate accel dt o = 
    { o with velocity = o.velocity +~ (dt *~ accel);
      position = o.position +~ (dt *~ o.velocity) +~ (0.5 *. dt *. dt *~ accel)
    }

  let random position_bound velocity_bound size_bound = 
    { position = random_vector position_bound;
      velocity = random_vector velocity_bound;
      size = size_bound.min +. Random.float (size_bound.max -. size_bound.min)
      color = Random.int 0xffffff
    }

  let draw o = 
    Graphics.set_color o.color;
    Graphics.fill_circle (int_of_float o.position.x) (int_of_float o.position.y) (int_of_float o.size)
end

let do_objs f objs = ignore (List.map f objs)

let velocity_bound = 150.0


let simulation_dt = 0.02
let force_strength = 30.0

let center = { x = 0.5 *. (float_of_int max_x); y = 0.5 *. (float_of_int max_y) }

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

let objs = repeat random_obj () 1000000;;
run objs;;

(* read_line ();; *)




