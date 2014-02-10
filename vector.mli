type t = { x : float; y : float; }
val ( +~ ) : t -> t -> t
val ( *~ ) : float -> t -> t
val ( -~ ) : t -> t -> t
val random : t Range.t -> t
val unit : t
val zero : t
val magnitude : t -> float
val length : t -> float
