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
ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/global.ml ../../src/lib/fslib/base64.ml  ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Wsn_protocol.ml
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

symb-exe: wsn-symb-bc.exe wsn-symb.exe

bc-exe: client-bc.exe ws-bc.exe

nc-exe: client.exe ws.exe

# Session implementation
Wsn.ml: Wsn.session
	../../src/s2ml/s2ml.exe < Wsn.session
	dot -Tps Wsn.dot -o Wsn.ps
	dot -Tps Wsn-full.dot -o Wsn-full.ps
	dot -Tps Wsn-ext.dot -o Wsn-ext.ps
	dot -Tpdf Wsn.dot -o Wsn.pdf
	dot -Tpdf Wsn-full.dot -o Wsn-full.pdf
	dot -Tpdf Wsn-ext.dot -o Wsn-ext.pdf

Wsn.tc7: $(seven) $(SYMLIBS) Wsn_protocol.ml7 Wsn_protocol.ml Wsn.ml7 Wsn.ml
	$(seven) -genlst -scripts Wsn -pervasives $(perv) $(SYMLIBS) Wsn_protocol.ml7 Wsn_protocol.ml Wsn.ml7 Wsn.ml > Wsn.tc7; egrep "Failed|error|Exception|Warning" Wsn.tc7; echo ""

Wsn.tc8: $(seven) $(SYMLIBS) Wsn.ml7 Wsn.ml
	$(seven) -debug -pervasives $(perv) $(SYMLIBS) Wsn.ml7 Wsn.ml > Wsn.tc8; egrep "Failed|error|Exception|Warning" Wsn.tc8; echo ""


Wsn.fsi: $(seven) $(SYMLIBS) Wsn.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts wsn $(SYMLIBS) Wsn.ml7 

#############
# Executables
#############

nc: clean-session Wsn.cmi Wsn.cmx client.cmx ws.cmx
bc: clean-session Wsn.cmi Wsn.cmo client.cmo ws.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o client-bc.exe \
	  Wsn.cmo client.cmo


client-o.exe: Wsn.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsn.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsn.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-o.exe \
	  Wsn.cmx client.cmx

client.exe: Wsn.ml client.ml
	$(COMPCONC) Wsn.mli Wsn.ml client.ml -o client.exe	

ws-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o ws-bc.exe \
	  Wsn.cmo ws.cmo


ws.exe: Wsn.ml ws.ml
	$(COMPCONC) Wsn.mli Wsn.ml ws.ml -o ws.exe	

ws-o.exe:  Wsn.ml ws.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsn.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Wsn.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) ws.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o ws-o.exe \
	  Wsn.cmx ws.cmx



wsn-symb-bc.exe: clean-session Wsn.ml
	ocamlc -c $(BCSYMBFLAGS) Wsn_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Wsn_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Wsn.mli
	ocamlc -c $(BCSYMBFLAGS) Wsn.ml
	ocamlc -c $(BCSYMBFLAGS) wsn-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o wsn-symb-bc.exe \
	  Wsn_protocol.cmo Wsn.cmo wsn-symb.cmo

wsn-symb.exe: clean-session Wsn.ml
	ocamlopt -c $(NCSYMBFLAGS) Wsn_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Wsn_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Wsn.mli
	ocamlopt -c $(NCSYMBFLAGS) Wsn.ml
	ocamlopt -c $(NCSYMBFLAGS) wsn-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o wsn-symb.exe \
	  Wsn_protocol.cmx Wsn.cmx wsn-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	ws.exefs

client.exefs:	Wsn.mli Wsn.ml client.ml
	$(COMPCONC) Wsn.mli Wsn.ml client.ml -o client.exe

ws.exefs:	Wsn.mli Wsn.ml ws.ml
	$(COMPCONC) Wsn.mli Wsn.ml ws.ml -o ws.exe

##########
# Cleaning
##########

clean-session: 
	rm -f Wsn.mli Wsn.ml Wsn.cmi Wsn.cmo Wsn.cmx Wsn.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f wsn-bc.exe wsn.exe wsn.o wsn.cmi wsn.cmo wsn.cmx 
	rm -f wsn-symb-bc.exe wsn-symb.exe wsn-symb.o wsn-symb.cmi wsn-symb.cmo wsn-symb.cmx
	rm -f Wsn.mli Wsn.ml Wsn.cmi Wsn.cmo Wsn.cmx Wsn.o
	rm -f Wsn_protocol.mli Wsn_protocol.ml Wsn_protocol.cmi Wsn_protocol.cmo Wsn_protocol.cmx Wsn_protocol.o
	rm -f Wsn.ps Wsn.dot Wsn-ext.ps Wsn-ext.dot Wsn-full.ps Wsn-full.dot 
	rm -f Wsn.pdf Wsn-ext.pdf Wsn-full.pdf

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
