all: compile

compile:
	dune build

run: compile
	-dune exec ./src/example.exe

clean:
	dune clean
	rm -f *.dot *.png
