#############################
# S2ML Compiler             #
# Makefile for the examples #
#############################


###################################
# Paths, libraries and source files
###################################

PREFIX = ../..
LIB = $(PREFIX)/src/lib
LIBSSL = $(LIB)/ocaml-ssl/
LIBFS = $(LIB)/fslib/
LIBSYMB = $(LIB)/symbolic/

FLAGS = -I $(LIBSSL)
BCFLAGS = -ccopt -L$(LIBSSL) unix.cma $(LIBSSL)openssl_impl.o -cclib -lcrypto 
NCFLAGS = -ccopt -L$(LIBSSL) unix.cmxa $(LIBSSL)openssl_impl.o -cclib -lcrypto 
BCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cma threads.cma
NCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cmxa threads.cmxa

BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)crypto.cmo $(LIBSSL)prins.cmo
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx $(LIBSSL)pi.cmx 
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)pi.ml $(LIBSYMB)data.cmo $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)pi.ml $(LIBSYMB)data.cmx $(LIBSYMB)crypto.cmx $(LIBSYMB)prins.cmx

DOT=dot
FSC=fsc.exe
ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/ocaml-ssl/global.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml
ARGLIBSYMB= --no-warnings --define fs --define symbolic $(LIBSYMB)global.ml $(LIBFS)pi.ml $(LIBSYMB)data.ml $(LIBSYMB)prins.ml $(LIBSYMB)crypto.ml
COMPCONC= $(FSC) $(ARGLIBCONC)
COMPSYMB= $(FSC) $(ARGLIBSYMB)

#################
# Generic targets
#################

default: symbolic
all: s2ml ocaml-ssl symb nc-exe bc-exe symb-exe
symbolic: s2ml symbolic symb-exe
concrete: s2ml ocaml-ssl nc-exe bc-exe

symb:
	$(MAKE) -C ../../src/lib/symbolic

ocaml-ssl:
	$(MAKE) -C ../../src/lib/ocaml-ssl

s2ml:
	$(MAKE) -C ../../src/s2ml

%.cmi : %.mli
	ocamlc -c $(FLAGS) $<

%.cmo: %.ml
	ocamlc -c $(FLAGS) $<

%.cmx: %.ml
	ocamlopt -c $(FLAGS) $<

##################
# Specific targets
##################

symb-exe: example3a-bc.exe example3a.exe

bc-exe: example3c-bc.exe example3s-bc.exe example3o-bc.exe 

nc-exe: example3c.exe example3s.exe example3o.exe 

# Session implementation
Shopping.ml Shopping_protocol.ml: Shopping.session s2ml
	../../src/s2ml/s2ml.exe < Shopping.session
	$(DOT) -Tps Shopping.dot -o Shopping.ps
	$(DOT) -Tps Shopping-ext.dot -o Shopping-ext.ps
	$(DOT) -Tps Shopping-full.dot -o Shopping-full.ps
	$(DOT) -Tpdf Shopping.dot -o Shopping.pdf
	$(DOT) -Tpdf Shopping-ext.dot -o Shopping-ext.pdf
	$(DOT) -Tpdf Shopping-full.dot -o Shopping-full.pdf

Shopping.mli: Shopping.ml

#############
# Executables
#############

nc: clean-session Shopping.ml Shopping-nc
bc: clean-session Shopping.ml Shopping-bc

Shopping-bc:
	ocamlc -c $(FLAGS) Shopping_protocol.mli
	ocamlc -c $(FLAGS) Shopping.mli
	ocamlc -c $(BCFLAGS) Shopping_protocol.ml
	ocamlc -c $(BCFLAGS) Shopping.ml

Shopping-nc:
	ocamlopt -c $(FLAGS) Shopping_protocol.mli
	ocamlopt -c $(FLAGS) Shopping.mli
	ocamlopt -c $(FLAGS) Shopping_protocol.ml
	ocamlopt -c $(FLAGS) Shopping.ml

example3c-bc.exe: bc 
	ocamlc -c $(FLAGS)s $(BCFLAGS) example3c.ml
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o example3c-bc.exe \
	  Shopping_protocol.cmo Shopping.cmo example3c.cmo

example3c.exe: nc
	ocamlopt -c $(FLAGS) $(NCFLAGS) example3c.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o example3c.exe \
	   Shopping_protocol.cmx Shopping.cmx example3c.cmx

example3s-bc.exe: bc
	ocamlc -c $(FLAGS) $(BCFLAGS) example3s.ml
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o example3s-bc.exe \
	   Shopping_protocol.cmo Shopping.cmo example3s.cmo

example3s.exe: nc
	ocamlopt -c $(FLAGS) $(NCFLAGS) example3s.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o example3s.exe \
	   Shopping_protocol.cmx Shopping.cmx example3s.cmx

example3o-bc.exe: bc 
	ocamlc -c $(FLAGS) $(BCFLAGS) example3o.ml
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o example3o-bc.exe \
	   Shopping_protocol.cmo Shopping.cmo example3o.cmo

example3o.exe: nc 
	ocamlopt -c $(NCFLAGS) example3o.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o example3o.exe \
	  Shopping_protocol.cmx Shopping.cmx example3o.cmx

example3a-bc.exe: Shopping.ml clean-session
	ocamlc -c $(BCSYMBFLAGS) Shopping_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Shopping.mli
	ocamlc -c $(BCSYMBFLAGS) Shopping_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Shopping.ml
	ocamlc -c $(BCSYMBFLAGS) example3a.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o example3a-bc.exe \
	  Shopping_protocol.cmo Shopping.cmo example3a.cmo

example3a.exe: Shopping.ml clean-session
	ocamlopt -c $(NCSYMBFLAGS) Shopping_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Shopping.mli
	ocamlopt -c $(NCSYMBFLAGS) Shopping_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Shopping.ml
	ocamlopt -c $(NCSYMBFLAGS) example3a.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o example3a.exe \
	  Shopping_protocol.cmx Shopping.cmx example3a.cmx

################
# F# Compilation
################

fs: s2ml example3c.exefs example3o.exefs example3s.exefs example3a.exe
	cd vstudio; make

example3c.exefs:	Shopping.mli Shopping.ml example3c.ml
	$(COMPCONC) Shopping.mli Shopping.ml example3c.ml -o example3c.exe

example3s.exefs:	Shopping.mli Shopping.ml example3s.ml
	$(COMPCONC) Shopping.mli Shopping.ml example3s.ml -o example3s.exe

example3o.exefs:	Shopping.mli Shopping.ml example3o.ml
	$(COMPCONC) Shopping.mli Shopping.ml example3o.ml -o example3o.exe

##########
# Cleaning
##########

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean

clean-session:	
	rm -f Shopping.cmi Shopping.cmo Shopping.cmx Shopping.o

clean:	
	rm -f example3c-bc.exe example3c.exe example3c.o example3c.cmi example3c.cmo example3c.cmx 
	rm -f example3s-bc.exe example3s.exe example3s.o example3s.cmi example3s.cmo example3s.cmx 
	rm -f example3o-bc.exe example3o.exe example3o.o example3o.cmi example3o.cmo example3o.cmx 
	rm -f example3a-bc.exe example3a.exe example3a.o example3a.cmi example3a.cmo example3a.cmx
	rm -f Shopping.mli Shopping.ml Shopping.cmi Shopping.cmo Shopping.cmx Shopping.o
	rm -f Shopping_protocol.mli Shopping_protocol.ml Shopping_protocol.cmi Shopping_protocol.cmo Shopping_protocol.cmx Shopping_protocol.o
	rm -f Shopping.dot Shopping.ps Shopping.pdf Shopping-ext.dot Shopping-ext.ps Shopping-ext.pdf Shopping-full.dot Shopping-full.ps Shopping-full.pdf
	rm -f vstudio/Shopping.dll
