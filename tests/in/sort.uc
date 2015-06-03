class Main {
    x : Int [];
    size : Int ;
    get_input(size : Int) : Int {
       let i: Int in {
         i <- 0;
         while i < size loop {
            out_string("> ");
            x[i] <- in_int();
            i <- i + 1
         } pool
    } tel
    };

    print_input(size : Int) : Int {
      let i : Int in {
         i <- 0;
         while i < size loop {
            out_int(x[i]);
            i <- i + 1
         } pool
      }
      tel
    };

    smallest(x : Int [], start: Int, stop: Int) : Int {
      let i : Int, small : Int in {
        i <- start; small <- start;
        while i < stop loop {
           small <- if x[i] < x[small] then i else small fi;
           i <- i + 1
        } pool; 
        small
    } tel
    };

    sort(x : Int[],size : Int) : Int {
      let i : Int, t : Int, e: Int in {
       i <- 0;
       while i < (size - 1) loop {
          e <- smallest(x,i,size);
          t <- x[e];
          x[e] <- x[i];
          x[i] <- t;
          i <- i + 1
       } pool 
    } tel
    };

    main () : Int { 
      {
         out_string("Number of elements ");
         size <- in_int();
         x <- new Int[size];
         get_input(size);
         sort(x,size);
	 out_string("\nresult: ");
         print_input(size)
      }
    };
}
