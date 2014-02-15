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
perv = ../../../../../lang-sec/msrc/f7/lib/pervasives.fs7 ../../../../../lang-sec/msrc/f7/lib/tuples.fs7
seven = ../../../../../lang-sec/msrc/f7/bin/f7.exe

FLAGS = -I $(LIBSSL)
BCFLAGS = -ccopt -L$(LIBSSL) unix.cma $(LIBSSL)openssl_impl.o -cclib -lcrypto 
NCFLAGS = -ccopt -L$(LIBSSL) unix.cmxa $(LIBSSL)openssl_impl.o -cclib -lcrypto 
BCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cma threads.cma
NCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cmxa threads.cmxa


BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)pi.cmo  $(LIBSSL)crypto.cmo $(LIBSSL)prins.cmo
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)pi.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx
BCSYMBFILES = $(LIBSYMB)global.cmo  $(LIBSYMB)data.cmo $(LIBSYMB)pi.cmo $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx  $(LIBSYMB)data.cmx $(LIBSYMB)pi.cmx $(LIBSYMB)crypto.cmx $(LIBSYMB)prins.cmx

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli 

FSC=fsc.exe
ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/global.ml ../../src/lib/fslib/base64.ml  ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Wsne_protocol.ml
COMPCONC= $(FSC) $(ARGLIBCONC)

#################
# Generic targets
#################
all: symbolic

symbolic: s2ml clean-lib symb symb-exe clean-cache
exe: s2ml clean-lib ocaml-ssl symb nc-exe bc-exe symb-exe clean-cache
concrete: s2ml clean-lib ocaml-ssl nc-exe bc-exe clean-cache

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

symb-exe: wsne-symb-bc.exe wsne-symb.exe

bc-exe: client-bc.exe ws-bc.exe

nc-exe: client.exe ws.exe

# Session implementation
Wsne.ml: Wsne.session
	../../src/s2ml/s2ml.exe < Wsne.session
	dot -Tps Wsne.dot -o Wsne.ps
	dot -Tps Wsne-full.dot -o Wsne-full.ps
	dot -Tps Wsne-ext.dot -o Wsne-ext.ps
	dot -Tpdf Wsne.dot -o Wsne.pdf
	dot -Tpdf Wsne-full.dot -o Wsne-full.pdf
	dot -Tpdf Wsne-ext.dot -o Wsne-ext.pdf

Wsne.tc7: $(seven) $(SYMLIBS) Wsne_protocol.ml7 Wsne_protocol.ml Wsne.ml7 Wsne.ml
	$(seven) -genlst -scripts Wsne -pervasives $(perv) $(SYMLIBS) Wsne_protocol.ml7 Wsne_protocol.ml Wsne.ml7 Wsne.ml > Wsne.tc7; egrep "Failed|error|Exception|Warning" Wsne.tc7; echo ""

Wsne.tc8: $(seven) $(SYMLIBS) Wsne.ml7 Wsne.ml
	$(seven) -debug -pervasives $(perv) $(SYMLIBS) Wsne.ml7 Wsne.ml > Wsne.tc8; egrep "Failed|error|Exception|Warning" Wsne.tc8; echo ""


Wsne.fsi: $(seven) $(SYMLIBS) Wsne.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts wsn $(SYMLIBS) Wsne.ml7 

#############
# Executables
#############

nc: clean-session Wsne.cmi Wsne.cmx client.cmx ws.cmx
bc: clean-session Wsne.cmi Wsne.cmo client.cmo ws.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o client-bc.exe \
	  Wsne.cmo client.cmo


client-o.exe: Wsne.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsne.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsne.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-o.exe \
	  Wsne.cmx client.cmx

client.exe: Wsne.ml client.ml
	$(COMPCONC) Wsne.mli Wsne.ml client.ml -o client.exe	

ws-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o ws-bc.exe \
	  Wsne.cmo ws.cmo


ws.exe: Wsne.ml ws.ml
	$(COMPCONC) Wsne.mli Wsne.ml ws.ml -o ws.exe	

ws-o.exe:  Wsne.ml ws.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsne.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsne.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) ws.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o ws-o.exe \
	  Wsne.cmx ws.cmx



wsne-symb-bc.exe: clean-session Wsne.ml
	ocamlc -c $(BCSYMBFLAGS) Wsne_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Wsne_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Wsne.mli
	ocamlc -c $(BCSYMBFLAGS) Wsne.ml
	ocamlc -c $(BCSYMBFLAGS) wsne-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o wsne-symb-bc.exe \
	  Wsne_protocol.cmo Wsne.cmo wsne-symb.cmo

wsne-symb.exe: clean-session Wsne.ml
	ocamlopt -c $(NCSYMBFLAGS) Wsne_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Wsne_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Wsne.mli
	ocamlopt -c $(NCSYMBFLAGS) Wsne.ml
	ocamlopt -c $(NCSYMBFLAGS) wsne-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o wsne-symb.exe \
	  Wsne_protocol.cmx Wsne.cmx wsne-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	ws.exefs

client.exefs:	Wsne.mli Wsne.ml client.ml
	$(COMPCONC) Wsne.mli Wsne.ml client.ml -o client.exe

ws.exefs:	Wsne.mli Wsne.ml ws.ml
	$(COMPCONC) Wsne.mli Wsne.ml ws.ml -o ws.exe

##########
# Cleaning
##########

clean-session: 
	rm -f Wsne.mli Wsne.ml Wsne.cmi Wsne.cmo Wsne.cmx Wsne.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f wsne-bc.exe wsne.exe wsne.o wsne.cmi wsne.cmo wsne.cmx 
	rm -f wsne-symb-bc.exe wsne-symb.exe wsne-symb.o wsne-symb.cmi wsne-symb.cmo wsne-symb.cmx
	rm -f Wsne.mli Wsne.ml Wsne.cmi Wsne.cmo Wsne.cmx Wsne.o
	rm -f Wsne_protocol.mli Wsne_protocol.ml Wsne_protocol.cmi Wsne_protocol.cmo Wsne_protocol.cmx Wsne_protocol.o
	rm -f Wsne.ps Wsne.dot Wsne-ext.ps Wsne-ext.dot Wsne-full.ps Wsne-full.dot 
	rm -f Wsne.pdf Wsne-ext.pdf Wsne-full.pdf 

clean-symb-lib:
	$(MAKE) -C ../../src/lib/symbolic clean

clean-lib: clean-symb-lib
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean

clean-cache:
	rm -f *.cache
