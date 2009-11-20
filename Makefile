CFLAGS = -warn-error,A

all:
	ocamlbuild -no-hygiene -cflags $(CFLAGS) wot.otarget