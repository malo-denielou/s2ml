###############################################################################
# Session - Implementation                                                    #
#                                                                             #
# Makefile                                                                    #
###############################################################################
# $Id: Makefile 9310 2009-01-27 15:07:25Z denielou $

default: s2ml.exe s2ml-bc.exe

all: clean default

NC1= common.cmx mlsyntax.cmx syntax.cmx graph.cmx properties.cmx dotgen.cmx 
NC2= parser.cmx lexer.cmx geninterface.cmx generation.cmx main.cmx 
BC= common.cmo mlsyntax.cmo syntax.cmo graph.cmo properties.cmo dotgen.cmo parser.cmo lexer.cmo geninterface.cmo generation.cmo main.cmo  

REQNC= parser.ml lexer.ml $(NC1) parser.cmi $(NC2)
REQBC= parser.ml lexer.ml $(BC)


lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml parser.mli: parser.mly
	ocamlyacc -v parser.mly

parser.cmi: common.cmo syntax.cmo parser.mli
	ocamlc parser.mli

#############
# Byte-code #
#############
common.cmo: common.ml
	ocamlc -c common.ml

mlsyntax.cmo: mlsyntax.ml
	ocamlc -c mlsyntax.ml

syntax.cmo: syntax.ml common.cmo
	ocamlc -c syntax.ml

graph.cmo: graph.ml common.cmo syntax.cmo
	ocamlc -c graph.ml

properties.cmo: properties.ml graph.cmo common.cmo syntax.cmo
	ocamlc -c properties.ml

dotgen.cmo: dotgen.ml graph.cmo common.cmo syntax.cmo
	ocamlc -c dotgen.ml

parser.cmo: parser.ml parser.mli common.cmo syntax.cmo
	ocamlc -c parser.ml

lexer.cmo: lexer.ml common.cmo syntax.cmo
	ocamlc -c lexer.ml

geninterface.cmo: geninterface.ml graph.cmo common.cmo syntax.cmo
	ocamlc -c geninterface.ml

generation.cmo: generation.ml geninterface.cmo graph.cmo common.cmo syntax.cmo
	ocamlc -c generation.ml

main.cmo: main.ml generation.cmo geninterface.cmo properties.cmo graph.cmo common.cmo syntax.cmo
	ocamlc -c main.ml

###############
# Native-code #
###############
common.cmx: common.ml
	ocamlopt -c common.ml

mlsyntax.cmx: mlsyntax.ml
	ocamlopt -c mlsyntax.ml

syntax.cmx: syntax.ml common.cmx
	ocamlopt -c syntax.ml

graph.cmx: graph.ml common.cmx syntax.cmx
	ocamlopt -c graph.ml

properties.cmx: properties.ml graph.cmx common.cmx syntax.cmx
	ocamlopt -c properties.ml

dotgen.cmx: dotgen.ml graph.cmx common.cmx syntax.cmx
	ocamlopt -c dotgen.ml

parser.cmx: parser.ml parser.mli common.cmx syntax.cmx
	ocamlopt -c parser.ml

lexer.cmx: lexer.ml common.cmx syntax.cmx
	ocamlopt -c lexer.ml

geninterface.cmx: geninterface.ml graph.cmx common.cmx syntax.cmx
	ocamlopt -c geninterface.ml

generation.cmx: generation.ml geninterface.cmx graph.cmx common.cmx syntax.cmx
	ocamlopt -c generation.ml

main.cmx: main.ml generation.cmx geninterface.cmx properties.cmo graph.cmx common.cmx syntax.cmx
	ocamlopt -c main.ml



#%.cmi: %.mli
#	ocamlc $<

#%.cmo: %.ml
#	ocamlc -c $<

#%.cmx: %.ml
#	ocamlopt -c $<

s2ml.exe: $(REQNC)
	ocamlopt -o s2ml.exe $(NC1) $(NC2)

s2ml-bc.exe: $(REQBC)
	ocamlc -o s2ml-bc.exe $(BC)

clean:
	rm -f common.cmi common.cmo common.cmx common.o 
	rm -f mlsyntax.cmi mlsyntax.cmo mlsyntax.cmx mlsyntax.o 
	rm -f syntax.cmi syntax.cmo syntax.cmx syntax.o 
	rm -f graph.cmi graph.cmo graph.cmx graph.o 
	rm -f properties.cmi properties.cmo properties.cmx properties.o 
	rm -f dotgen.cmi dotgen.cmo dotgen.cmx dotgen.o 
	rm -f parser.cmi parser.cmo parser.cmx parser.o 
	rm -f lexer.cmi lexer.cmo lexer.cmx lexer.o 
	rm -f geninterface.cmi geninterface.cmo geninterface.cmx geninterface.o
	rm -f genseven.cmi genseven.cmo genseven.cmx genseven.o
	rm -f generation.cmi generation.cmo generation.cmx generation.o 
	rm -f main.cmi main.cmo main.cmx main.o
	rm -f parser.mli parser.ml parser.output
	rm -f lexer.ml
	rm -f s2ml.exe s2ml-bc.exe

gui: common.cmo
	ocamlc -I +lablgtk2 -o gui.exe common.cmo lablgtk.cma gtkInit.cmo gui.ml