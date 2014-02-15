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

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Forward_protocol.ml7


FSC=fsc.exe

ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Forward_protocol.ml

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

symb-exe: forward-symb-bc.exe forward-symb.exe 

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Forward.ml: Forward.session
	../../src/s2ml/s2ml.exe < Forward.session
	dot -Tps Forward.dot -o Forward.ps
	dot -Tps Forward-full.dot -o Forward-full.ps
	dot -Tps Forward-ext.dot -o Forward-ext.ps
	dot -Tpdf Forward.dot -o Forward.pdf
	dot -Tpdf Forward-full.dot -o Forward-full.pdf
	dot -Tpdf Forward-ext.dot -o Forward-ext.pdf
Forward.mli: Forward.ml

Forward.ml7: Forward.ml

#############
# Executables
#############

nc: clean-session Forward.cmi Forward.cmx client.cmx server.cmx
nc2:  Forward.cmi Forward.cmx client.cmx server.cmx
bc: clean-session Forward.cmi Forward.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-bc.exe \
	  Forward.cmo client.cmo

client-o.exe: Forward.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Forward_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Forward_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Forward.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Forward.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Forward_protocol.cmx Forward.cmx client.cmx

client.exe: Forward.ml client.ml
	$(COMPCONC) Forward.mli Forward.ml client.ml -o client.exe	

server.exe: Forward.ml server.ml
	$(COMPCONC) Forward.mli Forward.ml server.ml -o server.exe	

c.exe:  nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Forward.cmx client.cmx

server-o.exe:  Forward.ml server.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Forward.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Forward.ml
	ocamlc -c $(NCFLAGS) Forward_protocol.mli
	ocamlc -c $(NCFLAGS) Forward_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) server.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Forward.cmx server.cmx

s.exe: nc2
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Forward.cmx server.cmx

server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Forward.cmo server.cmo

forward-symb-bc.exe: clean-session Forward.ml
	ocamlc -c $(BCSYMBFLAGS) Forward_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Forward_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Forward.mli
	ocamlc -c $(BCSYMBFLAGS) Forward.ml
	ocamlc -c $(BCSYMBFLAGS) forward-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o forward-symb-bc.exe \
	  Forward_protocol.cmo Forward.cmo forward-symb.cmo

forward-symb.exe: clean-session Forward.ml
	ocamlopt -c $(NCSYMBFLAGS) Forward_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Forward_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Forward.mli
	ocamlopt -c $(NCSYMBFLAGS) Forward.ml
	ocamlopt -c $(NCSYMBFLAGS) forward-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o forward-symb.exe \
	  Forward_protocol.cmx Forward.cmx forward-symb.cmx


partial.exe: Forward.ml
	ocamlopt -c $(NCSYMBFLAGS) Forward_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Forward_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Forward.mli
	ocamlopt -c $(NCSYMBFLAGS) Forward.ml
	ocamlopt -c $(NCSYMBFLAGS) forward-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o forward-symb.exe \
	  Forward_protocol.cmx Forward.cmx forward-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Forward.mli Forward.ml client.ml
	$(COMPCONC) Forward.mli Forward.ml client.ml -o client.exe

server.exefs:	Forward.mli Forward.ml server.ml
	$(COMPCONC) Forward.mli Forward.ml server.ml -o server.exe

Forward.tc7: Forward.ml7 Forward.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Forward_protocol.ml Forward.ml7 Forward.ml -scripts Forward > Forward.tc7 ; egrep "Failed|error|Exception|Warning" Forward.tc7; echo ""

Forward.f: $(SYMLIBS) Forward.mli Forward.ml
	$(fs2f) $(SYMLIBS) Forward.mli Forward.ml 

Forward.fsi: $(SYMLIBS) Forward.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Forward.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Forward.ps Forward.dot Forward.mli Forward.ml Forward.cmi Forward.cmo Forward.cmx Forward.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f forward-symb-bc.exe forward-symb.exe forward-symb.o forward-symb.cmi forward-symb.cmo forward-symb.cmx
	rm -f Forward.mli Forward.ml Forward.cmi Forward.cmo Forward.cmx Forward.o
	rm -f Forward.ps Forward.dot Forward-ext.ps Forward-ext.dot Forward-full.ps Forward-full.dot 
	rm -f Forward.pdf Forward-ext.pdf Forward-full.pdf 
	rm -f Forward_protocol.mli Forward_protocol.ml Forward_protocol.cmi Forward_protocol.cmo Forward_protocol.cmx Forward_protocol.o

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean



clean-cache:
	rm -f *.cache

