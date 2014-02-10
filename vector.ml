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

let random bound =
  let diff = (bound.Range.max -~ bound.Range.min) in
  bound.min +~ { x = Random.float diff.x;
                 y = Random.float diff.y;
               }

let unit = { x = 1.0; y = 1.0 }

let zero = 0.0 *~ unit

