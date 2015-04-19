Parser.class: Parser.java scanner.java
	javac -classpath "" *.java

Parser.java: uncool.y
	byacc -J uncool.y

scanner.java: uncool.jflex
	jflex uncool.jflex

clean:
	-@rm *.class *.java *~ 2>/dev/null ||true
