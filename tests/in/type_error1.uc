class Main {
    y : Bool ;
    x : Int <- true ;   -- bad initialization
    z : String <- "testing";
    main () : Int {
      {y <- 9;		-- bad assignment
      x <- y * 2;	-- bad operands * 
      y <- x > 2;
      z <- x > 2;	-- bad assignment
      out_int ( 42*x+(y-4)) -- bad operands -
      }
    };
}
