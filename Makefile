NAME := llama-snake

# Default rule
default:
	jbuilder build

run:
	_build/default/bin/main.exe

clean:
	jbuilder clean

.PHONY: default clean run
