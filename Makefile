CFLAGS = -warn-error,A,-g

all:
	ocamlbuild -no-hygiene -cflags $(CFLAGS) wot.otarget