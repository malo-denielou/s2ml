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
BCFLAGS = -thread  -ccopt -L$(LIBSSL) unix.cma threads.cma $(LIBSSL)openssl_impl.o -cclib -lcrypto 
NCFLAGS = -thread  -ccopt -L$(LIBSSL) unix.cmxa threads.cmxa $(LIBSSL)openssl_impl.o -cclib -lcrypto 
BCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cma threads.cma
NCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cmxa threads.cmxa


BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)crypto.cmo $(LIBSSL)prins.cmo $(LIBSSL)pi.cmo 
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx $(LIBSSL)pi.cmx 
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)pi.cmo $(LIBSYMB)data.cmo  $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)pi.cmx $(LIBSYMB)data.cmx $(LIBSYMB)crypto.cmx  $(LIBSYMB)prins.cmx

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli Conf_protocol.ml7


FSC=fsc.exe

ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml Conf_protocol.ml

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

symb-exe: conf-symb-bc.exe conf-symb.exe 

bc-exe: conf-c-bc.exe conf-a-bc.exe conf-p-bc.exe

nc-exe: conf-c-o.exe conf-a-o.exe conf-p-o.exe

# Session implementation
Conf.ml: Conf.session
	../../src/s2ml/s2ml.exe < Conf.session
	dot -Tps Conf.dot -o Conf.ps
	dot -Tps Conf-full.dot -o Conf-full.ps
	dot -Tps Conf-ext.dot -o Conf-ext.ps
	dot -Tpdf Conf.dot -o Conf.pdf
	dot -Tpdf Conf-full.dot -o Conf-full.pdf
	dot -Tpdf Conf-ext.dot -o Conf-ext.pdf
Conf.mli: Conf.ml

Conf.ml7: Conf.ml

#############
# Executables
#############

nc: Conf_protocol.cmi Conf_protocol.cmx Conf.cmi Conf.cmx conf-c.cmx conf-a.cmx conf-p.cmx
nc2:  Conf.cmi Conf.cmx conf-c.cmx conf-a.cmx conf-p.cmx
bc:  Conf_protocol.cmi Conf_protocol.cmo Conf.cmi Conf.cmo conf-c.cmo conf-a.cmo conf-p.cmo

conf-c-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o conf-c-bc.exe \
	  Conf_protocol.cmo Conf.cmo conf-c.cmo

conf-c-o.exe: Conf.ml conf-c.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) conf-c.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o conf-c.exe \
	  Conf_protocol.cmx Conf.cmx conf-c.cmx

conf-c.exe: Conf.ml conf-c.ml
	$(COMPCONC) Conf.mli Conf.ml conf-c.ml -o conf-c.exe	

conf-a.exe: Conf.ml conf-a.ml
	$(COMPCONC) Conf.mli Conf.ml conf-a.ml -o conf-a.exe	

c.exe:  nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o conf-c.exe \
	  Conf.cmx conf-c.cmx

conf-a-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o conf-a-bc.exe \
	  Conf_protocol.cmo Conf.cmo conf-a.cmo

conf-a-o.exe:  Conf.ml conf-a.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) conf-a.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o conf-a.exe \
	  Conf_protocol.cmx Conf.cmx conf-a.cmx

conf-p-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o conf-p-bc.exe \
	  Conf_protocol.cmo Conf.cmo conf-p.cmo

conf-p-o.exe:  Conf.ml conf-p.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Conf.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) conf-p.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o conf-p.exe \
	  Conf_protocol.cmx Conf.cmx conf-p.cmx

s.exe: nc2
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o conf-a.exe \
	  Conf.cmx conf-a.cmx

conf-symb-bc.exe: clean-session Conf.ml
	ocamlc -c $(BCSYMBFLAGS) Conf_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Conf_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Conf.mli
	ocamlc -c $(BCSYMBFLAGS) Conf.ml
	ocamlc -c $(BCSYMBFLAGS) conf-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o conf-symb-bc.exe \
	  Conf_protocol.cmo Conf.cmo conf-symb.cmo

conf-symb.exe: clean-session Conf.ml
	ocamlopt -c $(NCSYMBFLAGS) Conf_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Conf_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Conf.mli
	ocamlopt -c $(NCSYMBFLAGS) Conf.ml
	ocamlopt -c $(NCSYMBFLAGS) conf-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o conf-symb.exe \
	  Conf_protocol.cmx Conf.cmx conf-symb.cmx


partial.exe: Conf.ml
	ocamlopt -c $(NCSYMBFLAGS) Conf_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Conf_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Conf.mli
	ocamlopt -c $(NCSYMBFLAGS) Conf.ml
	ocamlopt -c $(NCSYMBFLAGS) conf-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o conf-symb.exe \
	  Conf_protocol.cmx Conf.cmx conf-symb.cmx

################
# F# Compilation
################

fs:	s2ml	conf-c.exefs	conf-a.exefs

conf-c.exefs:	Conf.mli Conf.ml conf-c.ml
	$(COMPCONC) Conf.mli Conf.ml conf-c.ml -o conf-c.exe

conf-a.exefs:	Conf.mli Conf.ml conf-a.ml
	$(COMPCONC) Conf.mli Conf.ml conf-a.ml -o conf-a.exe

Conf.tc7: Conf.ml7 Conf.ml $(sevenexe) $(SYMLIBS) 
	$(seven) $(SYMLIBS) Conf_protocol.ml Conf.ml7 Conf.ml -scripts Conf > Conf.tc7 ; egrep "Failed|error|Exception|Warning" Conf.tc7; echo ""

Conf.f: $(SYMLIBS) Conf.mli Conf.ml
	$(fs2f) $(SYMLIBS) Conf.mli Conf.ml 

Conf.fsi: $(SYMLIBS) Conf.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Conf.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Conf.dot Conf.mli Conf.ml Conf.cmi Conf.cmo Conf.cmx Conf.o 

clean:	
	rm -f conf-c-bc.exe conf-c.exe conf-c.o conf-c.cmi conf-c.cmo conf-c.cmx 
	rm -f conf-a-bc.exe conf-a.exe conf-a.o conf-a.cmi conf-a.cmo conf-a.cmx 
	rm -f conf-p-bc.exe conf-p.exe conf-p.o conf-p.cmi conf-p.cmo conf-p.cmx 
	rm -f conf-symb-bc.exe conf-symb.exe conf-symb.o conf-symb.cmi conf-symb.cmo conf-symb.cmx
	rm -f Conf.mli Conf.ml Conf.cmi Conf.cmo Conf.cmx Conf.o
	rm -f Conf.ps Conf.dot Conf-ext.ps Conf-ext.dot Conf-full.ps Conf-full.dot
	rm -f Conf.pdf Conf-ext.pdf Conf-full.pdf
	rm -f Conf_protocol.mli Conf_protocol.ml Conf_protocol.cmi Conf_protocol.cmo Conf_protocol.cmx Conf_protocol.o

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean



clean-cache:
	rm -f *.cache

