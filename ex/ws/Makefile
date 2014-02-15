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
sevenexe = ../../../../../lang-sec/msrc/f7/bin/f7.exe
seven = $(sevenexe) -pervasives $(perv) -genlst

fs2f = ../../../../../lang-sec/msrc/fs2pv/bin/fs2f.exe -debug

FLAGS = -I $(LIBSSL)
BCFLAGS = -ccopt -L$(LIBSSL) unix.cma $(LIBSSL)openssl_impl.o -cclib -lcrypto 
NCFLAGS = -ccopt -L$(LIBSSL) unix.cmxa $(LIBSSL)openssl_impl.o -cclib -lcrypto 
BCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cma threads.cma
NCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cmxa threads.cmxa


BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)prins.cmo $(LIBSSL)crypto.cmo
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx  $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx $(LIBSSL)pi.cmx 
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)pi.cmo $(LIBSYMB)data.cmo $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)pi.cmx $(LIBSYMB)data.cmx $(LIBSYMB)crypto.cmx $(LIBSYMB)prins.cmx

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Ws_protocol.ml7

FSC=fsc.exe
ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/pi.ml ../../src/lib/fslib/global.ml  ../../src/lib/fslib/base64.ml  ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Ws_protocol.ml
COMPCONC= $(FSC) $(ARGLIBCONC)

#################
# Generic targets
#################

symbolic: s2ml clean-lib symb symb-exe clean-cache
all: s2ml clean-lib ocaml-ssl symb nc-exe bc-exe symb-exe clean-cache
concrete: s2ml clean-lib ocaml-ssl nc-exe bc-exe clean-cache
openssl: s2ml clean-lib ocaml-ssl client-o.exe ws-o.exe

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

symb-exe: ws-symb-bc.exe ws-symb.exe

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Ws.ml: Ws.session
	../../src/s2ml/s2ml.exe < Ws.session
	dot -Tps Ws.dot -o Ws.ps
	dot -Tps Ws-full.dot -o Ws-full.ps
	dot -Tps Ws-ext.dot -o Ws-ext.ps
	dot -Tpdf Ws.dot -o Ws.pdf
	dot -Tpdf Ws-full.dot -o Ws-full.pdf
	dot -Tpdf Ws-ext.dot -o Ws-ext.pdf
Ws.mli: Ws.ml

Ws.ml7: Ws.ml

#############
# Executables
#############

nc: clean-session Ws_protocol.cmi Ws_protocol.cmx Ws.cmi Ws.cmx client.cmx server.cmx
bc: clean-session Ws_protocol.cmi Ws_protocol.cmo Ws.cmi Ws.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o client-bc.exe \
	  Ws.cmo client.cmo

client-o.exe: Ws.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-o.exe \
	  Ws_protocol.cmx Ws.cmx client.cmx


client.exe: Ws.ml client.ml
	$(COMPCONC) Ws.mli Ws.ml client.ml -o client.exe	

ws.exe: Ws.ml ws.ml
	$(COMPCONC) Ws.mli Ws.ml uws.ml -o ws.exe	

ws-o.exe:  Ws.ml uws.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Ws.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) uws.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o ws-o.exe \
	  Ws_protocol.cmx Ws.cmx uws.cmx

server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Ws.cmo server.cmo

server.exe: nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Ws.cmx server.cmx

ws-symb-bc.exe: clean-session Ws.ml
	ocamlc -c $(BCSYMBFLAGS) Ws_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Ws_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Ws.mli
	ocamlc -c $(BCSYMBFLAGS) Ws.ml
	ocamlc -c $(BCSYMBFLAGS) ws-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o ws-symb-bc.exe \
	  Ws_protocol.cmo Ws.cmo ws-symb.cmo

ws-symb.exe: clean-session Ws.ml
	ocamlopt -c $(NCSYMBFLAGS) Ws_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Ws_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Ws.mli
	ocamlopt -c $(NCSYMBFLAGS) Ws.ml
	ocamlopt -c $(NCSYMBFLAGS) ws-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o ws-symb.exe \
	  Ws_protocol.cmx Ws.cmx ws-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Ws.mli Ws.ml client.ml
	$(COMPCONC) Ws.mli Ws.ml client.ml -o client.exe

server.exefs:	Ws.mli Ws.ml server.ml
	$(COMPCONC) Ws.mli Ws.ml server.ml -o server.exe

Ws.tc7: Ws.ml7 Ws.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Ws_protocol.ml Ws.ml7 Ws.ml -scripts Ws > Ws.tc7 ; egrep "Failed|error|Exception|Warning" Ws.tc7; echo ""

Ws.f: $(SYMLIBS) Ws.mli Ws.ml
	$(fs2f) $(SYMLIBS) Ws.mli Ws.ml 

Ws.fsi: $(SYMLIBS) Ws.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Ws.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Ws.mli Ws.ml Ws.cmi Ws.cmo Ws.cmx Ws.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f ws-symb-bc.exe ws-symb.exe ws-symb.o ws-symb.cmi ws-symb.cmo ws-symb.cmx
	rm -f Ws.mli Ws.ml Ws.cmi Ws.cmo Ws.cmx Ws.o
	rm -f Ws_protocol.mli Ws_protocol.ml Ws_protocol.cmi Ws_protocol.cmo Ws_protocol.cmx Ws_protocol.o
	rm -f Ws.ps Ws.dot Ws-ext.ps Ws-ext.dot Ws-full.ps Ws-full.dot 
	rm -f Ws.pdf Ws-ext.pdf Ws-full.pdf 

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean

clean-cache:
	rm -f *.cache