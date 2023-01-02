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
end

module Make (Ord : OrderedType) = struct
  type elt = Ord.t

  (* let equal_elt x y = Ord.compare x y = 0 *)

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
      Ord.compare x.key y.key = 0
      && CCOption.equal equals x.left y.left
      && CCOption.equal equals x.right y.right
  end

  (* This is opened early so that equals is overloaded below, *)
  (* but we can still see the fields of the Cell.t record *)
  open Cell

  type t = Cell.t option ref

  let equals x y = !x = !y
  let create () : t = ref None
  let is_root ({ parent; _ } : Cell.t) = parent = None
  let is_left_child_of child parent = parent.left = Some child
  let is_right_child_of child parent = parent.right = Some child

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
    assert (x.parent = None);
    root := Some x

  let zig_left ~(root : t) ~(parent : Cell.t) (x : Cell.t) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_root ~root x;
    CCFormat.printf "@[zig left done@]@.";
    CCFormat.flush CCFormat.stdout ();
    assert (x.parent <> Some x);
    assert (is_root x)

  let zig_right ~(root : t) ~(parent : Cell.t) (x : Cell.t) =
    assert (is_root parent);
    x.parent <- None;
    parent.parent <- Some x;
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_root ~root x;
    CCFormat.printf "@[zig right done@]@.";
    CCFormat.flush CCFormat.stdout ();
    assert (is_root x)

  let zig ~(root : t) ~(parent : Cell.t) (x : Cell.t) =
    if parent.left = Some x then (
      CCFormat.printf "@[zig left@]@.";
      CCFormat.flush CCFormat.stdout ();
      zig_left ~root ~parent x
    ) else if parent.right = Some x then (
      CCFormat.printf "@[zig right@]@.";
      CCFormat.flush CCFormat.stdout ();
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

  let left_zig_zig ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    set_left_child_of x.right parent;
    set_right_child_of (Some parent) x;
    set_left_child_of parent.right grandparent;
    set_right_child_of (Some grandparent) parent;
    update_great_grandparent ~root x grandparent

  let right_zig_zig ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    set_right_child_of x.left parent;
    set_left_child_of (Some parent) x;
    set_right_child_of parent.left grandparent;
    set_left_child_of (Some grandparent) parent;
    update_great_grandparent ~root x grandparent

  let left_zig_zag ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    set_right_child_of x.left parent;
    set_left_child_of x.right grandparent;
    set_left_child_of (Some parent) x;
    set_right_child_of (Some grandparent) x;
    update_great_grandparent ~root x grandparent

  let right_zig_zag ~(root : t) ~(parent : Cell.t) ~(grandparent : Cell.t)
      (x : Cell.t) : unit =
    set_right_child_of x.left parent;
    set_left_child_of x.right grandparent;
    set_left_child_of (Some parent) x;
    set_right_child_of (Some grandparent) x;
    update_great_grandparent ~root x grandparent

  type splay_result = Done | Continue [@@deriving show]

  let splay_once ~(root : t) (x : Cell.t) : splay_result =
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

  let splay ~(root : t) (x : Cell.t) =
    (* while splay_once ~root x <> Done do *)
    (*   CCFormat.printf "@[splaying@]@."; *)
    (*   CCFormat.flush CCFormat.stdout (); *)
    (*   () *)
    (* done *)
    let res = splay_once ~root x in
    CCFormat.printf "@[splay result: %a@]@." pp_splay_result res;
    ()

  let rec add_ ~(root : t) (current : Cell.t) (x : elt) : unit =
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
      CCFormat.printf "@[insert into empty@]@.";
      root := Some { key = x; parent = None; left = None; right = None };
      ()
    | Some current -> add_ ~root current x

  let rec mem_ ~(root : t) (current : Cell.t) (x : elt) : bool =
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

  let rec pp_cell fmt (x : Cell.t) =
    CCFormat.fprintf fmt "@[cell content... %a@]@." Ord.pp x.key;
    CCFormat.fprintf fmt "@[left: %a@]@." (CCOption.pp pp_cell) x.left;
    CCFormat.fprintf fmt "@[right: %a@]@." (CCOption.pp pp_cell) x.right

  let pp fmt x =
    match !x with
    | Some cell -> CCFormat.fprintf fmt "@[%a@]@." pp_cell cell
    | None -> CCFormat.fprintf fmt "@[no splay tree@]@."
end
