class Main {
    y : Int ;
    x : Int <- 2 ;
    i : Int <- 0;

    main () : Int {
    while i < 5 loop {
      out_string(">");
      y <- in_int(); 
      out_string(">");
      x <- in_int();
      out_int(if x < y then y else x fi);
      i <- i + 1
    } pool
    };
}
