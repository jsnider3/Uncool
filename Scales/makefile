TEST='scala Uncool ../tests/in/sort.uc'

all: Uncool
	@echo "Testing..."
	@eval ${TEST}

Uncool: parser.scala
	@echo "Compiling..."
	@scalac parser.scala

clean:
	rm -f *.class
