open Printf
open Vector
open Util

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

let newton_G = 6.674e-11

let gravity_force m1 m2 = newton_G *. m1.size *. m2.size /. (Vector.magnitude (m1.position -~ m2.position))
               
(* F_i = sum j = 1 to n, where j != i, of F(m_i, m_j) *)

let simulate_gravity dt masses = 
  let other_masses = Util.list_pairs masses 
  let gravity_accel m1 m2 = (gravity_force m1 m2) /. m1.size *~ (m1.position -~ m2.position) in
  let simulate_gravity m1 m2 = simulate (gravity_accel m1 m2) dt m1
  pairs masses 
  |> List.map (fun m1 m2 -> 


let random position_bound velocity_bound size_bound = 
  { position = Vector.random position_bound;
    velocity = Vector.random velocity_bound;
    size = size_bound.Range.min +. Random.float (size_bound.Range.max -. size_bound.Range.min);
    color = Random.int 0xffffff
  }

let draw o = 
  Graphics.set_color o.color;
  Graphics.fill_circle (int_of_float o.position.x) (int_of_float o.position.y) (int_of_float o.size)
