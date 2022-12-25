module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val create : unit -> t

  (* val insert : t -> elt -> unit *)
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t

  (* let equal_elt x y = Ord.compare x y = 0 *)

  type cell = {
    mutable key: elt;
    mutable parent: cell option;
    mutable left: cell option;
    mutable right: cell option;
  }

  and t = cell option ref

  let create () : t = ref None
  let _splay _tree _elt = ()
  let is_root ({ parent; _ } : cell) = parent = None
  let is_left_child_of child parent = parent.left = Some child
  let is_right_child_of child parent = parent.right = Some child

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

  let set_root ~(root : t) (x : cell) = root := Some x

  (* let zig ~(root : t) (x : cell) = *)
  (*   match x.parent with *)
  (*   | None -> invalid_arg "cell is root, cannot zig" *)
  (*   | Some p -> *)
  (*       assert (is_root p); *)
  (*       x.parent <- None; *)
  (*       p.parent <- Some x; *)
  (*       if (p.left = Some x) then *)
  (*         ( *)
  (*         (\* zig left? *\) *)
  (*         set_left_child_of x.right p; *)
  (*         set_right_child_of (Some p) x; *)
  (*         set_root ~root x *)
  (*         ) *)
  (*       else if (p.right = Some x) then *)
  (*         ( *)
  (*         (\* zig right? *\) *)
  (*         set_right_child_of x.left p; *)
  (*         set_left_child_of (Some p) x; *)
  (*         set_root ~root x *)
  (*         ) *)
  (*       else *)
  (*         invalid_arg "x is not a child of its parent..." *)

  let zig_left ~(root : t) ~(parent : cell) (x : cell) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_root ~root x

  let zig_right ~(root : t) ~(parent : cell) (x : cell) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_root ~root x

  let zig ~(root : t) ~(parent : cell) (x : cell) =
    if parent.left = Some x then
      zig_left ~root ~parent x
    else if parent.right = Some x then
      zig_right ~root ~parent x
    else
      invalid_arg "cannot zig, x is not a child of its parent"

  (* update the great grandparent to point to x *)
  let update_great_grandparent ~(root : t) x grandparent =
    match grandparent.parent with
    | None ->
      x.parent <- None;
      set_root ~root x
    | Some great_grandparent ->
      if great_grandparent.left = Some grandparent then
        set_left_child_of (Some x) great_grandparent
      else if great_grandparent.right = Some grandparent then
        set_right_child_of (Some x) great_grandparent
      else
        invalid_arg "could not find grandparent as child of great_grandparent"

  (* let zig_zig ~(root : t) (x : cell) = *)
  (*   match x.parent with *)
  (*   | None -> invalid_arg "cell is root, cannot zig_zig" *)
  (*   | Some parent -> *)
  (*     match parent.parent with *)
  (*     | None -> invalid_arg "cannot zig_zig, parent is root" *)
  (*     | Some grandparent -> *)
  (*       if (is_left_child_of parent grandparent && is_left_child_of x parent) then *)
  (*         ( *)
  (*         (\* left zig zig *\) *)
  (*         set_left_child_of x.right parent; *)
  (*         set_right_child_of (Some parent) x; *)
  (*         set_left_child_of (parent.right) grandparent; *)
  (*         set_right_child_of (Some grandparent) parent; *)
  (*         update_great_grandparent x grandparent *)
  (*         ) *)
  (*       else if (is_right_child_of parent grandparent && is_right_child_of x parent) then *)
  (*         ( *)
  (*         (\* right zig zig *\) *)
  (*         set_right_child_of (x.left) parent; *)
  (*         set_left_child_of (Some parent)  x; *)
  (*         set_right_child_of (parent.left)  grandparent; *)
  (*         set_left_child_of (Some grandparent) parent; *)
  (*         update_great_grandparent x grandparent *)
  (*         ) *)
  (*       else *)
  (*         invalid_arg "cannot_zig_zig" *)

  let left_zig_zig ~(root : t) ~(parent : cell) ~(grandparent : cell) (x : cell)
      : unit =
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_left_child_of parent.right grandparent;
    set_right_child_of (Some grandparent) parent;
    update_great_grandparent ~root x grandparent

  let right_zig_zig ~(root : t) ~(parent : cell) ~(grandparent : cell)
      (x : cell) : unit =
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_right_child_of parent.left grandparent;
    set_left_child_of (Some grandparent) parent;
    update_great_grandparent ~root x grandparent

  (* let zig_zag ~(root : t) (x : cell) = *)
  (*   match x.parent with *)
  (*   | None -> invalid_arg "cell is root, cannot zig_zag" *)
  (*   | Some parent -> *)
  (*     match parent.parent with *)
  (*     | None -> invalid_arg "cannot zig_zag, parent is root" *)
  (*     | Some grandparent -> *)
  (*       if (is_left_child_of parent grandparent && is_right_child_of x parent) then *)
  (*         ( *)
  (*         (\* left zig zag *\) *)
  (*         set_right_child_of (x.left) parent; *)
  (*         set_left_child_of (x.right)  grandparent; *)
  (*         set_left_child_of (Some parent) x; *)
  (*         set_right_child_of (Some grandparent) x; *)
  (*         update_great_grandparent x grandparent *)
  (*         ) *)
  (*       else if (is_right_child_of parent grandparent && is_left_child_of x parent) then *)
  (*         ( *)
  (*         (\* right zig zag *\) *)
  (*         set_right_child_of (x.left) parent; *)
  (*         set_left_child_of (x.right) grandparent; *)
  (*         set_left_child_of (Some parent) x; *)
  (*         set_right_child_of (Some grandparent) x; *)
  (*         update_great_grandparent x grandparent *)
  (*         ) *)
  (*       else *)
  (*         invalid_arg "cannot zig zag" *)

  let left_zig_zag ~(root : t) ~(parent : cell) ~(grandparent : cell) (x : cell)
      : unit =
    set_right_child_of x.left parent;
    set_left_child_of x.right grandparent;
    set_left_child_of (Some parent) x;
    set_right_child_of (Some grandparent) x;
    update_great_grandparent ~root x grandparent

  let right_zig_zag ~(root : t) ~(parent : cell) ~(grandparent : cell)
      (x : cell) : unit =
    set_right_child_of x.left parent;
    set_left_child_of x.right grandparent;
    set_left_child_of (Some parent) x;
    set_right_child_of (Some grandparent) x;
    update_great_grandparent ~root x grandparent

  type splay_result = Done | Continue

  let splay_once ~(root : t) (x : cell) : splay_result =
    match x.parent with
    | None -> Done
    | Some parent ->
      (match parent.parent with
      | None ->
        zig ~root ~parent x;
        Continue
      | Some grandparent ->
        if is_left_child_of parent grandparent && is_left_child_of x parent then (
          left_zig_zig ~root ~parent ~grandparent x;
          Continue
        ) else if
            is_right_child_of parent grandparent && is_right_child_of x parent
          then (
          right_zig_zig ~root ~parent ~grandparent x;
          Continue
        ) else if
            is_left_child_of parent grandparent && is_right_child_of x parent
          then (
          left_zig_zag ~root ~parent ~grandparent x;
          Continue
        ) else if
            is_right_child_of parent grandparent && is_left_child_of x parent
          then (
          right_zig_zag ~root ~parent ~grandparent x;
          Continue
        ) else
          invalid_arg "cannot splay, malformed tree")

  let splay ~(root : t) (x : cell) =
    while splay_once ~root x <> Done do
      ()
    done

  (* let rec insert_ (tree : t) (x : elt) : unit = *)
  (*   match !tree with *)
  (*   | None -> tree := Some {key = x; parent = None; left = None; right = None}; () *)
  (*   | Some {key; parent = _; left; right} -> *)
  (*     let comparison = Ord.compare x key in *)
  (*     if comparison = 0 then () else *)
  (*     if comparison < 0 then *)
  (*       ( *)
  (*         insert_ left x; *)
  (*       ) else *)
  (*     ( *)
  (*       insert_ right x; *)
  (*     ) *)
end
