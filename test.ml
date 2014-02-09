
(*open Graphics;;*)

open Printf


let max_x = 640;;
let max_y = 480;;
Graphics.open_graph " 640x480 ";;

type vector = 
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

type obj =
  { position : vector;
    velocity : vector;
    size : float;
    color : Graphics.color
  }

let print_obj o =
  printf "{ x = %f, y = %f }\n" o.position.x o.position.y


(* v_(t+dt) = v_t + a * dt *)
let simulate accel dt o = 
  { o with velocity = o.velocity +~ (dt *~ accel);
           position = o.position +~ (dt *~ o.velocity) +~ (0.5 *. dt *. dt *~ accel)
  }

let draw o = 
  Graphics.set_color o.color;
  Graphics.fill_circle (int_of_float o.position.x) (int_of_float o.position.y) (int_of_float o.size)

let random_vector x_bound y_bound = 
  { x = Random.float x_bound;
    y = Random.float y_bound;
  }

let new_obj _ = 
  { position = random_vector (float_of_int max_x) (float_of_int max_y);
    velocity = random_vector (float_of_int 10) (float_of_int 10);
    size = 10.0 +. Random.float 20.0;
    color = Random.int 0xffffff
  }

let do_objs f objs = ignore (List.map f objs)

let simulation_dt = 0.01

let rec run objs =
  let step os = 
    do_objs draw os;
    do_objs print_obj os;
  in (*  *)
  let objs' = List.map (simulate {x=0.0;y=0.0} simulation_dt) objs
  in
  Thread.delay simulation_dt;
  step objs';
  run objs'

let objs = [ new_obj () ; new_obj () ];;


List.map draw objs;;

run objs;;


(* read_line ();; *)




