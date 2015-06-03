TEST='scala scales.Main ../tests/in/sort.uc'

all: scales scales/Main.class scales/Uncool.class scales/exprs/Var.class
	@echo "Testing..."
	@eval ${TEST}

scales/Main.class: scales compiler.scala
	scalac compiler.scala

scales/Uncool.class: scales parser.scala
	scalac parser.scala

scales/exprs/Var.class: scales expressions.scala
	scalac expressions.scala

scales/Cls.class: scales Cls.scala
	scalac Cls.scala

scales/Log.class: scales log.scala
	scalac log.scala

scales:
	scalac *scala

clean:
	rm -rf *.class scales
