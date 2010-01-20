CFLAGS = -warn-error,A

all:
	ocamlbuild -no-hygiene -cflags $(CFLAGS) wot.otarget

shodan:
	ocamlbuild -no-hygiene -cflags $(CFLAGS) wot-nondb-nonmpi.otarget