TEST='scala scales.Main ../tests/in/sort.uc'

all: scales/Main.class scales/Uncool.class scales/exprs/Var.class
	@echo "Testing..."
	@eval ${TEST}

scales/Main.class: compiler.scala scales/Uncool.class scales/exprs/Var.class scales/Log.class
	scalac compiler.scala

scales/Uncool.class: parser.scala scales/exprs/Var.class scales/Cls.class scales/Log.class
	scalac parser.scala

scales/exprs/Var.class: expressions.scala scales/Log.class
	scalac expressions.scala

scales/Cls.class: Cls.scala scales/Log.class
	scalac Cls.scala

scales/Log.class: log.scala
	scalac log.scala

clean:
	rm -rf *.class scales
