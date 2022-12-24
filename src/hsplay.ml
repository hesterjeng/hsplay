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

module Make (Ord : OrderedType) =
  struct
    type elt = Ord.t

    type cell =
        {
          mutable key : elt;
          left : t;
          right : t;
        }

    and t = cell option ref

    let create () : t = ref None

    let _splay _tree _elt = ()

    let rec insert (tree : t) (x : elt) : unit =
      match !tree with
      | None -> tree := Some {key = x; left = ref None; right = ref None}; ()
      | Some {key; left; right} ->
        let comparison = Ord.compare x key in
        if comparison = 0 then () else
        if comparison < 0 then
          (
            insert left x;
          ) else
        (
          insert right x;
        )
  end
