open Printf
open Vector

type t =
    { position : Vector.t;
      velocity : Vector.t;
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
  { position = Vector.random position_bound;
    velocity = Vector.random velocity_bound;
    size = size_bound.Range.min +. Random.float (size_bound.Range.max -. size_bound.Range.min);
    color = Random.int 0xffffff
  }

let draw o = 
  Graphics.set_color o.color;
  Graphics.fill_circle (int_of_float o.position.x) (int_of_float o.position.y) (int_of_float o.size)
