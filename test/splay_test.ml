let init_test = [ ("init", `Quick, fun _ -> ()) ]

let splay_once_test =
  [
    ( "splay_once_test",
      `Quick,
      fun _ ->
        let module S = Hsplay.Make (CCInt) in
        let s = S.create () in
        S.insert s 3;
        S.insert s 4;
        () );
  ]

let splay_twice_test =
  [
    ( "splay_twice_test",
      `Quick,
      fun _ ->
        let module S = Hsplay.Make (CCInt) in
        let s = S.create () in
        S.insert s 3;
        S.insert s 4;
        S.insert s 1;
        () );
  ]

let splay_thrice_test =
  [
    ( "splay_thrice_test",
      `Quick,
      fun _ ->
        let module S = Hsplay.Make (CCInt) in
        let s = S.create () in
        S.insert s 3;
        S.insert s 4;
        S.insert s 1;
        S.insert s 0;
        () );
  ]

let splay_ident_test =
  [
    ( "splay_ident_test",
      `Quick,
      fun _ ->
        let module S = Hsplay.Make (CCInt) in
        let s = S.create () in
        S.insert s 3;
        S.insert s 4;
        S.insert s 1;
        S.insert s 0;
        S.insert s 0;
        () );
  ]
