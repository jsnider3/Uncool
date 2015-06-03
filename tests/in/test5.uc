class Main {
    y : Int ;
    x : Int ;
    factorial(a: Int) : Int {
       if a < 2 then 1 else factorial(a-1) + factorial(a-2)
       fi
   };
    main () : Int {
      {y <- in_int();
      x <-factorial(y);
      out_int(x)
      }
    };
}
