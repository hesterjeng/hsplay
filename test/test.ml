let () =
  Alcotest.run "Hsplay"
    [
      ("basic_splay", Splay_test.basic_splay);
    ]
