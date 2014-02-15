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


BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)crypto.cmo $(LIBSSL)prins.cmo 
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx $(LIBSSL)pi.cmx 
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)pi.cmo $(LIBSYMB)data.cmo  $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)pi.cmx $(LIBSYMB)data.cmx $(LIBSYMB)crypto.cmx  $(LIBSYMB)prins.cmx

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Rpc_protocol.ml7


FSC=fsc.exe

ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Rpc_protocol.ml

COMPCONC= $(FSC) $(ARGLIBCONC)

#################
# Generic targets
#################

symbolic: s2ml clean-lib symb symb-exe clean-cache
all: s2ml clean-lib ocaml-ssl symb nc-exe bc-exe symb-exe clean-cache
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

symb-exe: rpc-symb-bc.exe rpc-symb.exe 

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Rpc.ml: Rpc.session
	../../src/s2ml/s2ml.exe < Rpc.session
	dot -Tps Rpc.dot -o Rpc.ps
	dot -Tps Rpc-full.dot -o Rpc-full.ps
	dot -Tps Rpc-ext.dot -o Rpc-ext.ps
	dot -Tpdf Rpc.dot -o Rpc.pdf
	dot -Tpdf Rpc-full.dot -o Rpc-full.pdf
	dot -Tpdf Rpc-ext.dot -o Rpc-ext.pdf
Rpc.mli: Rpc.ml

Rpc.ml7: Rpc.ml

#############
# Executables
#############

nc: clean-session Rpc.cmi Rpc.cmx client.cmx server.cmx
nc2:  Rpc.cmi Rpc.cmx client.cmx server.cmx
bc: clean-session Rpc.cmi Rpc.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-bc.exe \
	  Rpc.cmo client.cmo

client-o.exe: Rpc.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Rpc_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Rpc_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Rpc.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Rpc.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Rpc_protocol.cmx Rpc.cmx client.cmx

client.exe: Rpc.ml client.ml
	$(COMPCONC) Rpc.mli Rpc.ml client.ml -o client.exe	

server.exe: Rpc.ml server.ml
	$(COMPCONC) Rpc.mli Rpc.ml server.ml -o server.exe	

c.exe:  nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Rpc.cmx client.cmx

server-o.exe:  Rpc.ml server.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Rpc.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Rpc.ml
	ocamlc -c $(NCFLAGS) Rpc_protocol.mli
	ocamlc -c $(NCFLAGS) Rpc_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) server.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Rpc.cmx server.cmx

s.exe: nc2
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Rpc.cmx server.cmx

server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Rpc.cmo server.cmo

rpc-symb-bc.exe: clean-session Rpc.ml
	ocamlc -c $(BCSYMBFLAGS) Rpc_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Rpc_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Rpc.mli
	ocamlc -c $(BCSYMBFLAGS) Rpc.ml
	ocamlc -c $(BCSYMBFLAGS) rpc-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o rpc-symb-bc.exe \
	  Rpc_protocol.cmo Rpc.cmo rpc-symb.cmo

rpc-symb.exe: clean-session Rpc.ml
	ocamlopt -c $(NCSYMBFLAGS) Rpc_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Rpc_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Rpc.mli
	ocamlopt -c $(NCSYMBFLAGS) Rpc.ml
	ocamlopt -c $(NCSYMBFLAGS) rpc-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o rpc-symb.exe \
	  Rpc_protocol.cmx Rpc.cmx rpc-symb.cmx


partial.exe: Rpc.ml
	ocamlopt -c $(NCSYMBFLAGS) Rpc_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Rpc_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Rpc.mli
	ocamlopt -c $(NCSYMBFLAGS) Rpc.ml
	ocamlopt -c $(NCSYMBFLAGS) rpc-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o rpc-symb.exe \
	  Rpc_protocol.cmx Rpc.cmx rpc-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Rpc.mli Rpc.ml client.ml
	$(COMPCONC) Rpc.mli Rpc.ml client.ml -o client.exe

server.exefs:	Rpc.mli Rpc.ml server.ml
	$(COMPCONC) Rpc.mli Rpc.ml server.ml -o server.exe

Rpc.tc7: Rpc.ml7 Rpc.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Rpc_protocol.ml Rpc.ml7 Rpc.ml -scripts Rpc > Rpc.tc7 ; egrep "Failed|error|Exception|Warning" Rpc.tc7; echo ""

Rpc.f: $(SYMLIBS) Rpc.mli Rpc.ml
	$(fs2f) $(SYMLIBS) Rpc.mli Rpc.ml 

Rpc.fsi: $(SYMLIBS) Rpc.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Rpc.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Rpc.ps Rpc.dot Rpc.mli Rpc.ml Rpc.cmi Rpc.cmo Rpc.cmx Rpc.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f rpc-symb-bc.exe rpc-symb.exe rpc-symb.o rpc-symb.cmi rpc-symb.cmo rpc-symb.cmx
	rm -f Rpc.mli Rpc.ml Rpc.cmi Rpc.cmo Rpc.cmx Rpc.o
	rm -f Rpc.ps Rpc.dot Rpc-ext.ps Rpc-ext.dot Rpc-full.ps Rpc-full.dot 
	rm -f Rpc.pdf Rpc-ext.pdf Rpc-full.pdf 
	rm -f Rpc_protocol.mli Rpc_protocol.ml Rpc_protocol.cmi Rpc_protocol.cmo Rpc_protocol.cmx Rpc_protocol.o

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean



clean-cache:
	rm -f *.cache

