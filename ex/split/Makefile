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
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx 
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

symb-exe: split-symb-bc.exe split-symb.exe 

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Split.ml: Split.session
	../../src/s2ml/s2ml.exe < Split.session
	dot -Tps Split.dot -o Split.ps
	dot -Tps Split-full.dot -o Split-full.ps
	dot -Tps Split-ext.dot -o Split-ext.ps
Split.mli: Split.ml

#############
# Executables
#############

nc: clean-session Split.cmi Split.cmx client.cmx server.cmx
bc: clean-session Split.cmi Split.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o client-bc.exe \
	  Split.cmo client.cmo


client.exe: Split.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Split.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Split.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Split.cmx client.cmx

server.exe:  Split.ml server.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Split.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Split.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) server.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Split.cmx server.cmx

receiver.exe:  Split.ml receiver.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Split.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Split.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) receiver.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o receiver.exe \
	  Split.cmx receiver.cmx


server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Split.cmo server.cmo


split-symb-bc.exe: clean-session Split.ml
	ocamlc -c $(BCSYMBFLAGS) Split.mli
	ocamlc -c $(BCSYMBFLAGS) Split.ml
	ocamlc -c $(BCSYMBFLAGS) split-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o split-symb-bc.exe \
	  Split.cmo split-symb.cmo

split-symb.exe: clean-session Split.ml
	ocamlopt -c $(NCSYMBFLAGS) Split.mli
	ocamlopt -c $(NCSYMBFLAGS) Split.ml
	ocamlopt -c $(NCSYMBFLAGS) split-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o split-symb.exe \
	  Split.cmx split-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Split.mli Split.ml client.ml
	$(COMPCONC) Split.mli Split.ml client.ml -o client.exe

server.exefs:	Split.mli Split.ml server.ml
	$(COMPCONC) Split.mli Split.ml server.ml -o server.exe

##########
# Cleaning
##########

clean-session: 
	rm -f Split.ps Split.dot Split.mli Split.ml Split.cmi Split.cmo Split.cmx Split.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f split-symb-bc.exe split-symb.exe split-symb.o split-symb.cmi split-symb.cmo fwd-symb.cmx
	rm -f Split.mli Split.ml Split.cmi Split.cmo Split.cmx Split.o
	rm -f Split.ps Split.dot Split-ext.ps Split-ext.dot Split-full.ps Splity-full.dot 

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean


