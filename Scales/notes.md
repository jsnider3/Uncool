## Compiler notes

The JVM is a hardcore stack machine. All variables are on an imaginary
stack and all instructions get their args from that stack unless they take
params explicitly.

* `ldc` - is the jasmin instruction for constants.
* `newarray int` - is the instruction for making arrays.
* `getfield` and `putfield` - are for reading/writing class fields.
* `new` - does what `new` does.
* `goto` and `if*` - are branches and loops.
* `invokevirtual` and `invokenonvirtual` - are for methods.
