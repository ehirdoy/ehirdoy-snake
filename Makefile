NAME := llama-snake

# Default rule
default:
	jbuilder build bin/main.exe

install:
	opam-installer -i --prefix $(PREFIX) $(NAME).install

uninstall:
	opam-installer -u --prefix $(PREFIX) $(NAME).install

reinstall: uninstall reinstall

clean:
	rm -rf _build

.PHONY: default install uninstall reinstall clean
