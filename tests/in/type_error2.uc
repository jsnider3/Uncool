

class Main {
    x : Int <- 7;
    f (x: Int): Int {
	out_int(x);
        let x: Int, y: Int in {
	   x <- 2; y <- 1 < 2;  -- assignment mismatch 
           out_string (x + y)  -- parameter type
        }
        tel;
        out_int(x)
    };

    main () : Int {
      {
	out_int(x);
        let x: Int, y: Bool in {
	   x <- f(1,2);    -- num of parameters 
	   y <- f(3);	   -- assignment mismatch
           out_int ( y)		-- parameter type 
        }
        tel;
        out_int(x)
      }
    };
}
