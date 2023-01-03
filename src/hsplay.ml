open! Disable_polymorphism

module type OrderedType = sig
  type t

  val compare : t -> t -> int
  val pp : t CCFormat.printer
end

module type S = sig
  type elt
  type t

  val equals : t -> t -> bool
  val create : unit -> t
  val insert : t -> elt -> unit
  val mem : t -> elt -> bool
  val pp : t CCFormat.printer
  val only_in_tree_once : elt -> t -> bool
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t

  module Cell = struct
    type t = {
      mutable key: elt;
      mutable parent: t option;
      mutable left: t option;
      mutable right: t option;
    }

    (* Special comparison that uses polymorphic equality, but *)
    (* does not inspect the parent, because this would cause *)
    (* recursion errors during equality comparison. *)
    let rec equals x y =
      CCInt.equal (Ord.compare x.key y.key) 0
      && CCOption.equal equals x.left y.left
      && CCOption.equal equals x.right y.right

    let equals_opt = CCOption.equal equals

    let rec pp_cell fmt (x : t) =
      let parent_key =
        match x.parent with
        | Some p -> Some p.key
        | None -> None
      in
      CCFormat.fprintf fmt "@[cell content... %a... parent is %a@]@." Ord.pp
        x.key (CCOption.pp Ord.pp) parent_key;
      CCFormat.fprintf fmt "@[left: %a@]@." (CCOption.pp pp_cell) x.left;
      CCFormat.fprintf fmt "@[right: %a@]@." (CCOption.pp pp_cell) x.right
  end

  (* This is opened early so that equals is overloaded below, *)
  (* but we can still see the fields of the Cell.t record *)
  open Cell

  type t = Cell.t option ref

  let equals x y = equals_opt !x !y
  let create () : t = ref None
  let is_root ({ parent; _ } : Cell.t) = Cell.equals_opt parent None
  let is_left_child_of child parent = Cell.equals_opt parent.left (Some child)
  let is_right_child_of child parent = Cell.equals_opt parent.right (Some child)

  let pp fmt x =
    match !x with
    | Some cell -> CCFormat.fprintf fmt "@[%a@]@." pp_cell cell
    | None -> CCFormat.fprintf fmt "@[empty@]@."

  let set_right_child_of (child : Cell.t option) (parent : Cell.t) =
    match child with
    | None -> parent.right <- child
    | Some child_cell ->
      parent.right <- child;
      child_cell.parent <- Some parent

  let set_left_child_of (child : Cell.t option) (parent : Cell.t) =
    match child with
    | None -> parent.left <- child
    | Some child_cell ->
      parent.left <- child;
      child_cell.parent <- Some parent

  let set_root ~(root : t) (x : Cell.t) =
    assert (Cell.equals_opt x.parent None);
    root := Some x

  let zig_left ~(root : t) ~(parent : Cell.t) (x : Cell.t) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_root ~root x;
    (* CCFormat.printf "@[zig left done@]@."; *)
    assert (is_root x)

  let zig_right ~(root : t) ~(parent : Cell.t) (x : Cell.t) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_root ~root x;
    (* CCFormat.printf "@[zig right done@]@."; *)
    assert (is_root x)

  let zig ~(root : t) ~(parent : Cell.t) (x : Cell.t) =
    if Cell.equals_opt parent.left (Some x) then (
      (* CCFormat.printf "@[zig left@]@."; *)
      zig_left ~root ~parent x
    ) else if Cell.equals_opt parent.right (Some x) then (
      (* CCFormat.printf "@[zig right@]@."; *)
      zig_right ~root ~parent x
    ) else
      invalid_arg "cannot zig, x is not a child of its parent"

  (* update the great grandparent to point to x *)
  let update_great_grandparent ~(root : t) x grandparent =
    match grandparent.parent with
    | None ->
      (* CCFormat.printf "@[none in update great grandparent@]@."; *)
      x.parent <- None;
      set_root ~root x
    | Some great_grandparent ->
      (* CCFormat.printf "@[some in update great grandparent@]@."; *)
      if Cell.equals_opt great_grandparent.left (Some grandparent) then (
        (* CCFormat.printf "@[grandparent is left of great@]@."; *)
        set_left_child_of (Some x) great_grandparent
      ) else if Cell.equals_opt great_grandparent.right (Some grandparent) then (
        (* CCFormat.printf "@[grandparent is right of great@]@."; *)
        set_right_child_of (Some x) great_grandparent
      ) else (
        (* CCFormat.printf "@[could not find grandparent as child of great@]@."; *)
        invalid_arg "could not find grandparent as child of great_grandparent"
      )

  let left_zig_zig ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    update_great_grandparent ~root x grandparent;
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_left_child_of parent.right grandparent;
    set_right_child_of (Some grandparent) parent

  let right_zig_zig ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    update_great_grandparent ~root x grandparent;
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_right_child_of parent.left grandparent;
    set_left_child_of (Some grandparent) parent

  let left_zig_zag ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    update_great_grandparent ~root x grandparent;
    set_right_child_of x.left parent;
    set_left_child_of x.right grandparent;
    set_left_child_of (Some parent) x;
    set_right_child_of (Some grandparent) x

  let right_zig_zag ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    update_great_grandparent ~root x grandparent;
    set_left_child_of x.right parent;
    set_right_child_of x.left grandparent;
    set_right_child_of (Some parent) x;
    set_left_child_of (Some grandparent) x

  module Splay_result = struct
    type t = Done | Continue [@@deriving show, eq]
  end

  let splay_once ~(root : t) (x : Cell.t) : Splay_result.t =
    (* CCFormat.printf "@[splay once@]@."; *)
    match x.parent with
    | None ->
      (* CCFormat.printf "@[done@]@."; *)
      Splay_result.Done
    | Some parent ->
      (match parent.parent with
      | None ->
        (* CCFormat.printf "@[zig in splay once@]@."; *)
        zig ~root ~parent x;
        Splay_result.Done
      | Some grandparent ->
        (* CCFormat.printf "@[some grandparent in splay once@]@."; *)
        (* CCFormat.printf "@[current tree: %a@]@." pp root; *)
        (* CCFormat.printf "@[ok@]@."; *)
        if is_left_child_of parent grandparent && is_left_child_of x parent then (
          (* CCFormat.printf "@[left zig zig@]@."; *)
          left_zig_zig ~root ~parent ~grandparent x;
          Splay_result.Continue
        ) else if
            is_right_child_of parent grandparent && is_right_child_of x parent
          then (
          (* CCFormat.printf "@[right zig zig@]@."; *)
          right_zig_zig ~root ~parent ~grandparent x;
          Splay_result.Continue
        ) else if
            is_left_child_of parent grandparent && is_right_child_of x parent
          then (
          (* CCFormat.printf "@[left zig zag@]@."; *)
          left_zig_zag ~root ~parent ~grandparent x;
          Splay_result.Continue
        ) else if
            is_right_child_of parent grandparent && is_left_child_of x parent
          then (
          (* CCFormat.printf "@[right zig zag@]@."; *)
          right_zig_zag ~root ~parent ~grandparent x;
          Splay_result.Continue
        ) else
          invalid_arg "cannot splay, malformed tree")

  let rec splay ~(root : t) (x : Cell.t) =
    match splay_once ~root x with
    | Splay_result.Done -> ()
    | Splay_result.Continue ->
      (* CCFormat.printf "@[splaying...@]@."; *)
      splay ~root x

  let rec add_ ~(root : t) (current : Cell.t) (x : elt) : unit =
    (* CCFormat.printf "@[add %a@]@." Ord.pp x; *)
    let { key; parent; left; right } = current in
    let comparison = Ord.compare x key in
    if CCInt.equal comparison 0 then (
      (* CCFormat.printf "@[found inserted key@]@."; *)
      splay ~root current
    ) else if comparison < 0 then (
      match left with
      | None ->
        (* CCFormat.printf "@[lt none@]@."; *)
        let new_left_child =
          { key = x; parent = Some current; left = None; right = None }
        in
        set_left_child_of (Some new_left_child) current;
        splay ~root new_left_child
      | Some lchild -> add_ ~root lchild x
    ) else if comparison > 0 then (
      match right with
      | None ->
        (* CCFormat.printf "@[rt none@]@."; *)
        let new_right_child =
          { key = x; parent = Some current; left = None; right = None }
        in
        set_right_child_of (Some new_right_child) current;
        splay ~root new_right_child
      | Some rchild -> add_ ~root rchild x
    )

  let insert (root : t) (x : elt) : unit =
    (* CCFormat.printf "@[insert@]@."; *)
    match !root with
    | None ->
      (* CCFormat.printf "@[insert into empty@]@."; *)
      root := Some { key = x; parent = None; left = None; right = None };
      ()
    | Some current -> add_ ~root current x

  let rec mem_ ~(root : t) (current : Cell.t) (x : elt) : bool =
    (* CCFormat.printf "@[mem@]@."; *)
    let { key; left; right; _ } = current in
    let comparison = Ord.compare x key in
    if CCInt.equal comparison 0 then (
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

  module Invariants = struct
    let rec only_in_tree_once__ count elt cell : bool =
      let recurse cell =
        match cell.left, cell.right with
        | Some left, Some right ->
          only_in_tree_once__ count elt left
          && only_in_tree_once__ count elt right
        | Some left, None -> only_in_tree_once__ count elt left
        | None, Some right -> only_in_tree_once__ count elt right
        | None, None -> true
      in
      if CCInt.equal (Ord.compare elt cell.key) 0 then
        if CCInt.equal !count 1 then
          false
        else (
          count := 1;
          recurse cell
        )
      else
        recurse cell

    let only_in_tree_once_ elt cell = only_in_tree_once__ (ref 0) elt cell
  end

  let only_in_tree_once elt tree =
    match !tree with
    | Some root -> Invariants.only_in_tree_once_ elt root
    | None -> true
end
