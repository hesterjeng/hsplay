module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S = sig
    type elt
    (** The type of the splay tree elements. *)

    type t
    (** The type of splay trees. *)

    val create : unit -> t

    val insert : t -> elt -> unit
end

module Make (Ord : OrderedType) : S with type elt = Ord.t
