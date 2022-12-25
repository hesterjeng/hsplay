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

    (* val insert : t -> elt -> unit *)
end

module Make (Ord : OrderedType) =
  struct
    type elt = Ord.t

    (* let equal_elt x y = Ord.compare x y = 0 *)

    type cell =
        {
          mutable depth : int;
          mutable key : elt;
          mutable parent : cell option;
          mutable left : cell option;
          mutable right : cell option;
        }

    and t = cell option ref

    let create () : t = ref None

    let _splay _tree _elt = ()

    let is_root ({parent; _} : cell) =
      parent = None

    let is_left_child_of child parent =
      parent.left = Some child

    let is_right_child_of child parent =
      parent.right = Some child

    let set_right_child_of (child : cell option) (parent : cell) =
        match child with
        | None -> parent.right <- child
        | Some child_cell ->
          parent.right <- child;
          child_cell.parent <- Some parent

    let set_left_child_of (child : cell option) (parent : cell) =
        match child with
        | None -> parent.left <- child
        | Some child_cell ->
          parent.left <- child;
          child_cell.parent <- Some parent

    let zig (x : cell) =
      match x.parent with
      | None -> invalid_arg "cell is root, cannot zig"
      | Some p ->
          assert (is_root p);
          x.parent <- None;
          p.parent <- Some x;
          if (p.left = Some x) then
            (
            (* zig left? *)
            set_left_child_of x.right p;
            set_right_child_of (Some p) x
            )
          else if (p.right = Some x) then
            (
            (* zig right? *)
            set_right_child_of x.left p;
            set_left_child_of (Some p) x
            )
          else
            invalid_arg "x is not a child of its parent..."

    (* update the great grandparent to point to x *)
    let update_great_grandparent x grandparent =
            match grandparent.parent with
            | None -> x.parent <- None
            | Some great_grandparent ->
              if great_grandparent.left = Some grandparent then
                set_left_child_of (Some x) great_grandparent
              else if great_grandparent.right = Some grandparent then
                set_right_child_of (Some x) great_grandparent
              else invalid_arg "could not find grandparent as child of great_grandparent"

    exception Cannot_zig_zig

    let zig_zig (x : cell) =
      match x.parent with
      | None -> invalid_arg "cell is root, cannot zig_zig"
      | Some parent ->
        match parent.parent with
        | None -> invalid_arg "cannot zig_zig, parent is root"
        | Some grandparent ->
          if (is_left_child_of parent grandparent && is_left_child_of x parent) then
            (
            (* left zig zig *)
            set_left_child_of x.right parent;
            set_right_child_of (Some parent) x;
            set_left_child_of (parent.right) grandparent;
            set_right_child_of (Some grandparent) parent;
            update_great_grandparent x grandparent
            )
          else if (is_right_child_of parent grandparent && is_right_child_of x parent) then
            (
            (* right zig zig *)
            set_right_child_of (x.left) parent;
            set_left_child_of (Some parent)  x;
            set_right_child_of (parent.left)  grandparent;
            set_left_child_of (Some grandparent) parent;
            update_great_grandparent x grandparent
            )
          else
            raise Cannot_zig_zig

    exception Cannot_zig_zag

    let zig_zag (x : cell) =
      match x.parent with
      | None -> invalid_arg "cell is root, cannot zig_zag"
      | Some parent ->
        match parent.parent with
        | None -> invalid_arg "cannot zig_zag, parent is root"
        | Some grandparent ->
          if (is_left_child_of parent grandparent && is_right_child_of x parent) then
            (
            (* left zig zag *)
            set_right_child_of (x.left) parent;
            set_left_child_of (x.right)  grandparent;
            set_left_child_of (Some parent) x;
            set_right_child_of (Some grandparent) x;
            update_great_grandparent x grandparent
            )
          else if (is_right_child_of parent grandparent && is_left_child_of x parent) then
            (
            (* right zig zag *)
            set_right_child_of (x.left) parent;
            set_left_child_of (x.right) grandparent;
            set_left_child_of (Some parent) x;
            set_right_child_of (Some grandparent) x;
            update_great_grandparent x grandparent
            )
          else
            raise Cannot_zig_zag

    let _splay x =
      while (x.parent <> None) do
        (
          try zig_zag x with
          | Cannot_zig_zag ->
          try zig_zig x with
            | Cannot_zig_zig ->
              zig x
        ) done


    (* let rec insert (tree : t) (x : elt) : unit = *)
    (*   match !tree with *)
    (*   | None -> tree := Some {key = x; parent = None; left = None; right = None}; () *)
    (*   | Some {key; parent = _; left; right} -> *)
    (*     let comparison = Ord.compare x key in *)
    (*     if comparison = 0 then () else *)
    (*     if comparison < 0 then *)
    (*       ( *)
    (*         insert left x; *)
    (*       ) else *)
    (*     ( *)
    (*       insert right x; *)
    (*     ) *)
  end
