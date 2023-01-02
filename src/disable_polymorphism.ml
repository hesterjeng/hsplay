type do_not_use_polymorphism = Dummy

let ( = ) (_x : do_not_use_polymorphism) (_y : do_not_use_polymorphism) =
  assert false

let ( <> ) (_x : do_not_use_polymorphism) (_y : do_not_use_polymorphism) =
  assert false

let compare (_x : do_not_use_polymorphism) (_y : do_not_use_polymorphism) =
  assert false

let equal_string (x : string) (y : string) = CCString.equal x y
let equal_list = CCList.equal

let equal_bool (x : bool) (y : bool) =
  match x, y with
  | true, true -> true
  | false, false -> true
  | _ -> false
