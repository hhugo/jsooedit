

all: editor.ml
	ocamlbuild editor.byte -use-ocamlfind \
		-pkgs zed,lwt \
		-pkgs js_of_ocaml,js_of_ocaml.syntax \
		-pkgs deriving.syntax.std,js_of_ocaml.deriving.syntax,js_of_ocaml.deriving \
		-syntax camlp4o
	js_of_ocaml +weak.js editor.byte
