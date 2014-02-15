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


BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)crypto.cmo $(LIBSSL)prins.cmo $(LIBSSL)pi.cmo 
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx $(LIBSSL)pi.cmx 
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)pi.cmo $(LIBSYMB)data.cmo  $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)pi.cmx $(LIBSYMB)data.cmx $(LIBSYMB)crypto.cmx  $(LIBSYMB)prins.cmx

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Single_protocol.ml7


FSC=fsc.exe

ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Single_protocol.ml

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

symb-exe: single-symb-bc.exe single-symb.exe 

bc-exe: single-c-bc.exe single-w-bc.exe

nc-exe: single-c-o.exe single-w-o.exe

# Session implementation
Single.ml: Single.session
	../../src/s2ml/s2ml.exe < Single.session
	dot -Tps Single.dot -o Single.ps
	dot -Tps Single-full.dot -o Single-full.ps
	dot -Tps Single-ext.dot -o Single-ext.ps
	dot -Tpdf Single.dot -o Single.pdf
	dot -Tpdf Single-full.dot -o Single-full.pdf
	dot -Tpdf Single-ext.dot -o Single-ext.pdf
Single.mli: Single.ml

Single.ml7: Single.ml

#############
# Executables
#############

nc: Single_protocol.cmi Single_protocol.cmx Single.cmi Single.cmx single-c.cmx single-w.cmx
nc2:  Single.cmi Single.cmx single-c.cmx single-w.cmx
bc:  Single_protocol.cmi Single_protocol.cmo Single.cmi Single.cmo single-c.cmo single-w.cmo

single-c-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o single-c-bc.exe \
	  Single_protocol.cmo Single.cmo single-c.cmo

single-c-o.exe: Single.ml single-c.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) single-c.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o single-c.exe \
	  Single_protocol.cmx Single.cmx single-c.cmx

single-c.exe: Single.ml single-c.ml
	$(COMPCONC) Single.mli Single.ml single-c.ml -o single-c.exe	

single-w.exe: Single.ml single-w.ml
	$(COMPCONC) Single.mli Single.ml single-w.ml -o single-w.exe	

c.exe:  nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o single-c.exe \
	  Single.cmx single-c.cmx

single-w-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o single-w-bc.exe \
	  Single_protocol.cmo Single.cmo single-w.cmo

single-w-o.exe:  Single.ml single-w.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Single.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) single-w.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o single-w.exe \
	  Single_protocol.cmx Single.cmx single-w.cmx

s.exe: nc2
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o single-w.exe \
	  Single.cmx single-w.cmx

single-symb-bc.exe: clean-session Single.ml
	ocamlc -c $(BCSYMBFLAGS) Single_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Single_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Single.mli
	ocamlc -c $(BCSYMBFLAGS) Single.ml
	ocamlc -c $(BCSYMBFLAGS) single-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o single-symb-bc.exe \
	  Single_protocol.cmo Single.cmo single-symb.cmo

single-symb.exe: clean-session Single.ml
	ocamlopt -c $(NCSYMBFLAGS) Single_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Single_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Single.mli
	ocamlopt -c $(NCSYMBFLAGS) Single.ml
	ocamlopt -c $(NCSYMBFLAGS) single-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o single-symb.exe \
	  Single_protocol.cmx Single.cmx single-symb.cmx


partial.exe: Single.ml
	ocamlopt -c $(NCSYMBFLAGS) Single_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Single_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Single.mli
	ocamlopt -c $(NCSYMBFLAGS) Single.ml
	ocamlopt -c $(NCSYMBFLAGS) single-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o single-symb.exe \
	  Single_protocol.cmx Single.cmx single-symb.cmx

################
# F# Compilation
################

fs:	s2ml	single-c.exefs	single-w.exefs

single-c.exefs:	Single.mli Single.ml single-c.ml
	$(COMPCONC) Single.mli Single.ml single-c.ml -o single-c.exe

single-w.exefs:	Single.mli Single.ml single-w.ml
	$(COMPCONC) Single.mli Single.ml single-w.ml -o single-w.exe

Single.tc7: Single.ml7 Single.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Single_protocol.ml Single.ml7 Single.ml -scripts Single > Single.tc7 ; egrep "Failed|error|Exception|Warning" Single.tc7; echo ""

Single.f: $(SYMLIBS) Single.mli Single.ml
	$(fs2f) $(SYMLIBS) Single.mli Single.ml 

Single.fsi: $(SYMLIBS) Single.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Single.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Single.dot Single.mli Single.ml Single.cmi Single.cmo Single.cmx Single.o 

clean:	
	rm -f single-c-bc.exe single-c.exe single-c.o single-c.cmi single-c.cmo single-c.cmx 
	rm -f single-w-bc.exe single-w.exe single-w.o single-w.cmi single-w.cmo single-w.cmx 
	rm -f single-symb-bc.exe single-symb.exe single-symb.o single-symb.cmi single-symb.cmo single-symb.cmx
	rm -f Single.mli Single.ml Single.cmi Single.cmo Single.cmx Single.o
	rm -f Single.ps Single.dot Single-ext.ps Single-ext.dot Single-full.ps Single-full.dot
	rm -f Single.pdf Single-ext.pdf Single-full.pdf
	rm -f Single_protocol.mli Single_protocol.ml Single_protocol.cmi Single_protocol.cmo Single_protocol.cmx Single_protocol.o

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean



clean-cache:
	rm -f *.cache

