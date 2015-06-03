class Main {
    y : Int ;
    x : Int <- 2 ;
    print_fn(a: Int) : Int {
       out_int(a * 12)
   };
    main () : Int {
      {y <- 9;
      x <- print_fn( 42*x+(y-4));
      out_int(x)
      }
    };
}
