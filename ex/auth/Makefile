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

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Auth_protocol.ml7


FSC=fsc.exe

ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Auth_protocol.ml

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

symb-exe: auth-symb-bc.exe auth-symb.exe 

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Auth.ml: Auth.session
	../../src/s2ml/s2ml.exe < Auth.session
	dot -Tps Auth.dot -o Auth.ps
	dot -Tps Auth-full.dot -o Auth-full.ps
	dot -Tps Auth-ext.dot -o Auth-ext.ps
	dot -Tpdf Auth.dot -o Auth.pdf
	dot -Tpdf Auth-full.dot -o Auth-full.pdf
	dot -Tpdf Auth-ext.dot -o Auth-ext.pdf
Auth.mli: Auth.ml

Auth.ml7: Auth.ml

#############
# Executables
#############

nc: clean-session Auth.cmi Auth.cmx client.cmx server.cmx
nc2:  Auth.cmi Auth.cmx client.cmx server.cmx
bc: clean-session Auth.cmi Auth.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-bc.exe \
	  Auth.cmo client.cmo

client-o.exe: Auth.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Auth_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Auth_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Auth.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Auth.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Auth_protocol.cmx Auth.cmx client.cmx

client.exe: Auth.ml client.ml
	$(COMPCONC) Auth.mli Auth.ml client.ml -o client.exe	

server.exe: Auth.ml server.ml
	$(COMPCONC) Auth.mli Auth.ml server.ml -o server.exe	

c.exe:  nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Auth.cmx client.cmx

server-o.exe:  Auth.ml server.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Auth.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Auth.ml
	ocamlc -c $(NCFLAGS) Auth_protocol.mli
	ocamlc -c $(NCFLAGS) Auth_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) server.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Auth.cmx server.cmx

s.exe: nc2
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Auth.cmx server.cmx

server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Auth.cmo server.cmo

auth-symb-bc.exe: clean-session Auth.ml
	ocamlc -c $(BCSYMBFLAGS) Auth_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Auth_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Auth.mli
	ocamlc -c $(BCSYMBFLAGS) Auth.ml
	ocamlc -c $(BCSYMBFLAGS) auth-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o auth-symb-bc.exe \
	  Auth_protocol.cmo Auth.cmo auth-symb.cmo

auth-symb.exe: clean-session Auth.ml
	ocamlopt -c $(NCSYMBFLAGS) Auth_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Auth_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Auth.mli
	ocamlopt -c $(NCSYMBFLAGS) Auth.ml
	ocamlopt -c $(NCSYMBFLAGS) auth-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o auth-symb.exe \
	  Auth_protocol.cmx Auth.cmx auth-symb.cmx


partial.exe: Auth.ml
	ocamlopt -c $(NCSYMBFLAGS) Auth_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Auth_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Auth.mli
	ocamlopt -c $(NCSYMBFLAGS) Auth.ml
	ocamlopt -c $(NCSYMBFLAGS) auth-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o auth-symb.exe \
	  Auth_protocol.cmx Auth.cmx auth-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Auth.mli Auth.ml client.ml
	$(COMPCONC) Auth.mli Auth.ml client.ml -o client.exe

server.exefs:	Auth.mli Auth.ml server.ml
	$(COMPCONC) Auth.mli Auth.ml server.ml -o server.exe

Auth.tc7: Auth.ml7 Auth.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Auth_protocol.ml Auth.ml7 Auth.ml -scripts Auth > Auth.tc7 ; egrep "Failed|error|Exception|Warning" Auth.tc7; echo ""

Auth.f: $(SYMLIBS) Auth.mli Auth.ml
	$(fs2f) $(SYMLIBS) Auth.mli Auth.ml 

Auth.fsi: $(SYMLIBS) Auth.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Auth.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Auth.ps Auth.dot Auth.mli Auth.ml Auth.cmi Auth.cmo Auth.cmx Auth.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f auth-symb-bc.exe auth-symb.exe auth-symb.o auth-symb.cmi auth-symb.cmo auth-symb.cmx
	rm -f Auth.mli Auth.ml Auth.cmi Auth.cmo Auth.cmx Auth.o
	rm -f Auth.ps Auth.dot Auth-ext.ps Auth-ext.dot Auth-full.ps Auth-full.dot 
	rm -f Auth.pdf Auth-ext.pdf Auth-full.pdf 
	rm -f Auth_protocol.mli Auth_protocol.ml Auth_protocol.cmi Auth_protocol.cmo Auth_protocol.cmx Auth_protocol.o

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean



clean-cache:
	rm -f *.cache

