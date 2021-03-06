type t = {
  position : Vector.t;
  velocity : Vector.t;
  size : float;
  color : int;
}
val print : t -> unit
val simulate : Vector.t -> float -> t -> t
val random : Vector.t Range.t -> Vector.t Range.t -> float Range.t -> t
val draw : t -> unit
