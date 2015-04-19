Parser.class: Parser.java scanner.java
	javac -classpath "" *.java

Parser.java: uncool.y
	/home/white/bin/byacc -J uncool.y

scanner.java: uncool.jflex
	java -jar /home/white/JFlex.jar uncool.jflex

clean:
	-@rm *.class *.java *~ 2>/dev/null ||true
