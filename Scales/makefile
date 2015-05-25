TEST='scala Compiler ../tests/in/sort.uc'

all: compiler
	@echo "Testing..."
	@eval ${TEST}

compiler: compiler.scala
	@echo "Compiling..."
	@scalac compiler.scala

clean:
	rm -f *.class
