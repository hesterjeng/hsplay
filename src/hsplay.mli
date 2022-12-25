module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  (** The type of the splay tree elements. *)

  type t
  (** The type of splay trees. *)

  val create : unit -> t
  val insert : t -> elt -> unit
  val mem : t -> elt -> bool
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
