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


BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)prins.cmo $(LIBSSL)crypto.cmo
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)prins.cmx $(LIBSSL)crypto.cmx
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)pi.ml $(LIBSYMB)data.cmo $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)pi.ml $(LIBSYMB)data.cmx $(LIBSYMB)crypto.cmx $(LIBSYMB)prins.cmx

FSC=fsc.exe
ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/ocaml-ssl/global.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml
COMPCONC= $(FSC) $(ARGLIBCONC)

#################
# Generic targets
#################

symbolic: s2ml clean-lib symb symb-exe
all: s2ml clean-lib ocaml-ssl symb nc-exe bc-exe symb-exe
concrete: s2ml clean-lib ocaml-ssl nc-exe bc-exe

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

symb-exe: wsj-symb-bc.exe wsj-symb.exe

bc-exe: client-bc.exe ws-bc.exe

nc-exe: client.exe ws.exe

# Session implementation
Wsj.ml: Wsj.session
	../../src/s2ml/s2ml.exe < Wsj.session
	dot -Tps Wsj.dot -o Wsj.ps
	dot -Tps Wsj-full.dot -o Wsj-full.ps
	dot -Tps Wsj-ext.dot -o Wsj-ext.ps
Wsj.mli: Wsj.ml

#############
# Executables
#############

nc: clean-session Wsj.cmi Wsj.cmx client.cmx ws.cmx
bc: clean-session Wsj.cmi Wsj.cmo client.cmo ws.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o client-bc.exe \
	  Wsj.cmo client.cmo

client.exe: nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Wsj.cmx client.cmx

ws-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o ws-bc.exe \
	  Wsj.cmo ws.cmo

ws.exe: nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o ws.exe \
	  Wsj.cmx ws.cmx

wsj-symb-bc.exe: clean-session Wsj.ml
	ocamlc -c $(BCSYMBFLAGS) Wsj.mli
	ocamlc -c $(BCSYMBFLAGS) Wsj.ml
	ocamlc -c $(BCSYMBFLAGS) wsj-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o wsj-symb-bc.exe \
	  Wsj.cmo wsj-symb.cmo

wsj-symb.exe: clean-session Wsj.ml
	ocamlopt -c $(NCSYMBFLAGS) Wsj.mli
	ocamlopt -c $(NCSYMBFLAGS) Wsj.ml
	ocamlopt -c $(NCSYMBFLAGS) wsj-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o wsj-symb.exe \
	  Wsj.cmx wsj-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	ws.exefs

client.exefs:	Wsj.mli Wsj.ml client.ml
	$(COMPCONC) Wsj.mli Wsj.ml client.ml -o client.exe

ws.exefs:	Wsj.mli Wsj.ml ws.ml
	$(COMPCONC) Wsj.mli Wsj.ml ws.ml -o ws.exe

##########
# Cleaning
##########

clean-session: 
	rm -f Wsj.ps Wsj.dot Wsj.mli Wsj.ml Wsj.cmi Wsj.cmo Wsj.cmx Wsj.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f wsj-bc.exe wsj.exe wsj.o wsj.cmi wsj.cmo wsj.cmx 
	rm -f wsj-symb-bc.exe wsj-symb.exe wsj-symb.o wsj-symb.cmi wsj-symb.cmo wsj-symb.cmx
	rm -f Wsj.mli Wsj.ml Wsj.cmi Wsj.cmo Wsj.cmx Wsj.o
	rm -f Wsj.ps Wsj.dot Wsj-ext.ps Wsj-ext.dot Wsj-full.ps Wsj-full.dot 

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean


