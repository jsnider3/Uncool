feature constructors needed:
two args for ID : typename and ID : INT_T
three args for the ones that assign
four args for functions with a nullable formal list option
fields should be {name,args,type,assignent}

On second thought the ones that assign right there can be typechecked immediately and then we can just use the two arg version.

In_int, in_string, out_int, out_string should be defined as members of Object, so we don't have to special case them.

Expr
Constants: TRUE_T, FALSE_T, INT_CONST, STR_CONST
One-arg operators: ISVOID, '~', NOT_T
Two-arg operators: NE, GT, GE, LT, LE, EQ, +, -, *, /
Method calls: 4 forms
Assignment/ID Lookup:
wHILE
If
expr_list
Let
