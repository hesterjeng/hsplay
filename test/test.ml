let () =
  Alcotest.run "Hsplay"
    [
      "init", Splay_test.init_test;
      "splay_once_test", Splay_test.splay_once_test;
      "splay_thrice_test", Splay_test.splay_thrice_test;
      "splay_ident_test", Splay_test.splay_ident_test;
      "splay_twice_test", Splay_test.splay_twice_test;
    ]
