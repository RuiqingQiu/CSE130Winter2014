SCALACHECK = /home/linux/ieng6/cs130w/public/scalacheck_2.10-1.11.3.jar
FSC = fsc -Djava.io.tmpdir=.scala-devel -cp .:$(SCALACHECK)
SCALA = scala -cp .:$(SCALACHECK)


all: bst decorators test

bst: 
	$(FSC) Bst.scala

decorators:
	$(FSC) Decorators.scala

json:
	$(FSC) Json.scala

test: bst decorators json 
	$(FSC) Test.scala
	$(SCALA) Test

clean:
	rm -f *.class *.tmp
	#$(FSC) -reset

distclean: clean
	$(FSC) -shutdown

turnin: 
	zip pa6_solution.zip Bst.scala Decorators.scala Json.scala
	validate_pa6 pa6_solution.zip 
	turnin -c cs130w -p pa6 pa6_solution.zip 

zip: 
	zip pa6.zip *.scala *.out Makefile
