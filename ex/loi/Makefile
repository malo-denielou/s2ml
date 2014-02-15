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

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Loi_protocol.ml7


FSC=fsc.exe

ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Loi_protocol.ml

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

symb-exe: loi-symb-bc.exe loi-symb.exe 

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Loi.ml: Loi.session
	../../src/s2ml/s2ml.exe < Loi.session
	dot -Tps Loi.dot -o Loi.ps
	dot -Tps Loi-full.dot -o Loi-full.ps
	dot -Tps Loi-ext.dot -o Loi-ext.ps
	dot -Tpdf Loi.dot -o Loi.pdf
	dot -Tpdf Loi-full.dot -o Loi-full.pdf
	dot -Tpdf Loi-ext.dot -o Loi-ext.pdf
Loi.mli: Loi.ml

Loi.ml7: Loi.ml

#############
# Executables
#############

nc: clean-session Loi.cmi Loi.cmx client.cmx server.cmx
nc2:  Loi.cmi Loi.cmx client.cmx server.cmx
bc: clean-session Loi.cmi Loi.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client-bc.exe \
	  Loi.cmo client.cmo

client-o.exe: Loi.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Loi_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Loi_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Loi.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Loi.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Loi_protocol.cmx Loi.cmx client.cmx

client.exe: Loi.ml client.ml
	$(COMPCONC) Loi.mli Loi.ml client.ml -o client.exe	

server.exe: Loi.ml server.ml
	$(COMPCONC) Loi.mli Loi.ml server.ml -o server.exe	

c.exe:  nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Loi.cmx client.cmx

server-o.exe:  Loi.ml server.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Loi.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Loi.ml
	ocamlc -c $(NCFLAGS) Loi_protocol.mli
	ocamlc -c $(NCFLAGS) Loi_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) server.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Loi.cmx server.cmx

s.exe: nc2
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Loi.cmx server.cmx

server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Loi.cmo server.cmo

loi-symb-bc.exe: clean-session Loi.ml
	ocamlc -c $(BCSYMBFLAGS) Loi_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Loi_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Loi.mli
	ocamlc -c $(BCSYMBFLAGS) Loi.ml
	ocamlc -c $(BCSYMBFLAGS) loi-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o loi-symb-bc.exe \
	  Loi_protocol.cmo Loi.cmo loi-symb.cmo

loi-symb.exe: clean-session Loi.ml
	ocamlopt -c $(NCSYMBFLAGS) Loi_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Loi_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Loi.mli
	ocamlopt -c $(NCSYMBFLAGS) Loi.ml
	ocamlopt -c $(NCSYMBFLAGS) loi-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o loi-symb.exe \
	  Loi_protocol.cmx Loi.cmx loi-symb.cmx


partial.exe: Loi.ml
	ocamlopt -c $(NCSYMBFLAGS) Loi_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Loi_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Loi.mli
	ocamlopt -c $(NCSYMBFLAGS) Loi.ml
	ocamlopt -c $(NCSYMBFLAGS) loi-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o loi-symb.exe \
	  Loi_protocol.cmx Loi.cmx loi-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Loi.mli Loi.ml client.ml
	$(COMPCONC) Loi.mli Loi.ml client.ml -o client.exe

server.exefs:	Loi.mli Loi.ml server.ml
	$(COMPCONC) Loi.mli Loi.ml server.ml -o server.exe

Loi.tc7: Loi.ml7 Loi.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Loi_protocol.ml Loi.ml7 Loi.ml -scripts Loi > Loi.tc7 ; egrep "Failed|error|Exception|Warning" Loi.tc7; echo ""

Loi.f: $(SYMLIBS) Loi.mli Loi.ml
	$(fs2f) $(SYMLIBS) Loi.mli Loi.ml 

Loi.fsi: $(SYMLIBS) Loi.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Loi.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Loi.ps Loi.dot Loi.mli Loi.ml Loi.cmi Loi.cmo Loi.cmx Loi.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f loi-symb-bc.exe loi-symb.exe loi-symb.o loi-symb.cmi loi-symb.cmo loi-symb.cmx
	rm -f Loi.mli Loi.ml Loi.cmi Loi.cmo Loi.cmx Loi.o
	rm -f Loi.ps Loi.dot Loi-ext.ps Loi-ext.dot Loi-full.ps Loi-full.dot 
	rm -f Loi.pdf Loi-ext.pdf Loi-full.pdf 
	rm -f Loi_protocol.mli Loi_protocol.ml Loi_protocol.cmi Loi_protocol.cmo Loi_protocol.cmx Loi_protocol.o

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean



clean-cache:
	rm -f *.cache

