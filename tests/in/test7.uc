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
        st[top-1]
     } else ~1 fi
   };
}


class Main {
    st: Stack;
    x : Int;
    main () : Int {
      {st = new Stack( 10);
       st.push(2); st.push(3); st.push(5);
       x <- st.pop(); x <- st.pop();
       out_int(x) 
      }
    };
}
