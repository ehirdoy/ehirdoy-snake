NAME := llama-snake

# Default rule
default:
	jbuilder build

run_ocaml:
	_build/default/ocaml/bin/main.exe

run_reasonml:
	_build/default/reasonml/bin/main.exe

clean:
	jbuilder clean

.PHONY: default clean run
