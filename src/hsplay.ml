module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val create : unit -> t
  val insert : t -> elt -> unit
  val mem : t -> elt -> bool
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

  let set_root ~(root : t) (x : cell) =
    assert (x.parent = None);
    root := Some x

  let zig_left ~(root : t) ~(parent : cell) (x : cell) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_root ~root x;
    assert (is_root x)

  let zig_right ~(root : t) ~(parent : cell) (x : cell) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_root ~root x;
    assert (is_root x)

  let zig ~(root : t) ~(parent : cell) (x : cell) =
    CCFormat.printf "@[zig@]@.";
    if parent.left = Some x then (
      CCFormat.printf "@[zig left@]@.";
      zig_left ~root ~parent x
    ) else if parent.right = Some x then (
      CCFormat.printf "@[zig right@]@.";
      zig_right ~root ~parent x
    ) else
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
    CCFormat.printf "@[splay once@]@.";
    match x.parent with
    | None ->
      CCFormat.printf "@[done@]@.";
      Done
    | Some parent ->
      (match parent.parent with
      | None ->
        CCFormat.printf "@[zig in splay once@]@.";
        zig ~root ~parent x;
        CCFormat.printf "@[completed zig@]@.";
        Done
      | Some grandparent ->
        if is_left_child_of parent grandparent && is_left_child_of x parent then (
          CCFormat.printf "@[left zig zig@]@.";
          left_zig_zig ~root ~parent ~grandparent x;
          Continue
        ) else if
            is_right_child_of parent grandparent && is_right_child_of x parent
          then (
          CCFormat.printf "@[right zig zig@]@.";
          right_zig_zig ~root ~parent ~grandparent x;
          Continue
        ) else if
            is_left_child_of parent grandparent && is_right_child_of x parent
          then (
          CCFormat.printf "@[left zig zag@]@.";
          left_zig_zag ~root ~parent ~grandparent x;
          Continue
        ) else if
            is_right_child_of parent grandparent && is_left_child_of x parent
          then (
          CCFormat.printf "@[right zig zag@]@.";
          right_zig_zag ~root ~parent ~grandparent x;
          Continue
        ) else
          invalid_arg "cannot splay, malformed tree")

  let splay ~(root : t) (x : cell) =
    while splay_once ~root x <> Done do
      CCFormat.printf "@[splaying@]@.";
      CCFormat.flush CCFormat.stdout ();
      ()
    done

  let rec add_ ~(root : t) (current : cell) (x : elt) : unit =
    CCFormat.printf "@[add@]@.";
    let { key; parent; left; right } = current in
    let comparison = Ord.compare x key in
    if comparison = 0 then
      splay ~root current
    else if comparison < 0 then (
      match left with
      | None ->
        CCFormat.printf "@[lt none@]@.";
        let new_left_child =
          { key = x; parent = Some current; left = None; right = None }
        in
        set_left_child_of (Some new_left_child) current;
        splay ~root new_left_child
      | Some lchild -> add_ ~root lchild x
    ) else if comparison > 0 then (
      match right with
      | None ->
        CCFormat.printf "@[rt none@]@.";
        let new_right_child =
          { key = x; parent = Some current; left = None; right = None }
        in
        set_left_child_of (Some new_right_child) current;
        splay ~root new_right_child
      | Some rchild -> add_ ~root rchild x
    )

  let insert (root : t) (x : elt) : unit =
    CCFormat.printf "@[insert@]@.";
    match !root with
    | None ->
      root := Some { key = x; parent = None; left = None; right = None };
      ()
    | Some current -> add_ ~root current x

  let rec mem_ ~(root : t) (current : cell) (x : elt) : bool =
    CCFormat.printf "@[mem@]@.";
    let { key; left; right; _ } = current in
    let comparison = Ord.compare x key in
    if comparison = 0 then (
      splay ~root current;
      true
    ) else if comparison < 0 then (
      match left with
      | None -> false
      | Some lchild -> mem_ ~root lchild x
    ) else (
      match right with
      | None -> false
      | Some lchild -> mem_ ~root lchild x
    )

  let mem (root : t) (x : elt) =
    match !root with
    | None -> false
    | Some current -> mem_ ~root current x
end
