TEST='scala Main ../tests/in/sort.uc'

all: Main.class Uncool.class exprs/Var.class
	@echo "Testing..."
	@eval ${TEST}

Main.class: compiler.scala Uncool.class exprs/Var.class Cls.class
	@echo "Compiling..."
	@scalac compiler.scala

Uncool.class: parser.scala exprs/Var.class Cls.class
	@echo "Compiling..."
	@scalac parser.scala

exprs/Var.class: expressions.scala
	@echo "Compiling..."
	@scalac expressions.scala

Cls.class: Cls.scala
	@echo "Compiling..."
	@scalac Cls.scala

clean:
	rm -rf *.class exprs
