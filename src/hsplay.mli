module type OrderedType = sig
  type t

  val equals : t -> t -> bool
  val compare : t -> t -> int


  val pp : t CCFormat.printer
end

module type S = sig
  type elt
  (** The type of the splay tree elements. *)

  type t
  (** The type of splay trees. *)

  val equals : t -> t -> bool

  val create : unit -> t
  val insert : t -> elt -> unit
  val mem : t -> elt -> bool

  val pp : t CCFormat.printer
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
