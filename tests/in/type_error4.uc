class Stack {
   st : Int [];
   size : Int;
   top : Int;
   init (s : Int) : Stack {
      st <- new Int[s];
      size <- s;
      top <- 0; self
   };
   push (item : Int) : Int {
      if top < size then {
         st[top] <- item; top <- top + 1
      } else ~1 fi
   };
   pop() : Int {
     if (0 < top) then {
        top <- top - 1; st[top]
     } else ~1 fi
   };
   top() : Int {
     if (0 < top) then {
        push(1);
        st[top-1]
     } else ~1 fi
   };
}


class Main {
    st: Stack;
    ss : Stack1;  -- unknown type
    x : Int;
    main () : Int {
      {st <- new Stack2( 10);  -- unknown type
       st.push();    -- wrong num parameters 
	     push(3);	-- unknown method 
	     st.pushy(5);	-- unknonw method
       x <- st.pop(1); -- wrong num parameters
       x <- st.pop();
       out_int(x) 
      }
    };
}
