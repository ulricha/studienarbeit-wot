CFLAGS = -warn-error,A

all:
	ocamlbuild -cflags $(CFLAGS) wot.otarget