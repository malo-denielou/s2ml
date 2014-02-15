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
fs2f = ../../../../../lang-sec/msrc/fs2pv/bin/fs2f.exe -debug

FLAGS = -I $(LIBSSL)
BCFLAGS = -ccopt -L$(LIBSSL) unix.cma $(LIBSSL)openssl_impl.o -cclib -lcrypto 
NCFLAGS = -ccopt -L$(LIBSSL) unix.cmxa $(LIBSSL)openssl_impl.o -cclib -lcrypto 
BCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cma threads.cma
NCSYMBFLAGS = -thread -I $(LIBSYMB) unix.cmxa threads.cmxa

SYMLIBS = $(LIBSYMB)global.ml7 $(LIBSYMB)data.ml7 $(LIBSYMB)pi.mli $(LIBSYMB)crypto.ml7 $(LIBSYMB)prins.ml7 $(LIBSYMB)base64.mli 

BCFILES = $(LIBSSL)global.cmo $(LIBSSL)base64.cmo $(LIBSSL)data.cmo $(LIBSSL)crypto.cmo $(LIBSSL)prins.cmo 
NCFILES = $(LIBSSL)global.cmx $(LIBSSL)base64.cmx $(LIBSSL)data.cmx $(LIBSSL)crypto.cmx $(LIBSSL)prins.cmx 
BCSYMBFILES = $(LIBSYMB)global.cmo $(LIBSYMB)data.cmo $(LIBSYMB)pi.ml $(LIBSYMB)crypto.cmo $(LIBSYMB)prins.cmo
NCSYMBFILES = $(LIBSYMB)global.cmx $(LIBSYMB)data.cmx $(LIBSYMB)pi.ml $(LIBSYMB)crypto.cmx $(LIBSYMB)prins.cmx

FSC=fsc.exe
ARGLIBCONC= --no-warnings --define fs --define concrete -r System.Security.dll ../../src/lib/fslib/data.ml  ../../src/lib/fslib/global.ml  ../../src/lib/fslib/pi.ml ../../src/lib/fslib/base64.ml  ../../src/lib/fslib/crypto.ml ../../src/lib/fslib/prins.ml
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

symb-exe: proxy-symb-bc.exe proxy-symb.exe 

bc-exe: client-bc.exe server-bc.exe

nc-exe: client.exe server.exe

# Session implementation
Proxy.ml Proxy_protocol.ml: Proxy.session
	../../src/s2ml/s2ml.exe < Proxy.session
	dot -Tps Proxy.dot -o Proxy.ps
	dot -Tps Proxy-full.dot -o Proxy-full.ps
	dot -Tps Proxy-ext.dot -o Proxy-ext.ps
Proxy.mli: Proxy.ml

Proxy.ml7: Proxy.ml

#############
# Executables
#############

nc: clean-session Proxy.cmi Proxy.cmx client.cmx server.cmx
bc: clean-session Proxy.cmi Proxy.cmo client.cmo server.cmo

client-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o client-bc.exe \
	  Proxy.cmo client.cmo


client.exe: Proxy_protocol.ml Proxy.ml client.ml
	$(COMPCONC) Proxy_protocol.ml Proxy.mli Proxy.ml  client.ml -o client.exe	

client-o.exe: Proxy_protocol.mli Proxy_protocol.ml Proxy.ml client.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) client.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o client.exe \
	  Proxy_protocol.cmx Proxy.cmx client.cmx

ws.exe: Proxy_protocol.ml Proxy.ml ws.ml
	$(COMPCONC)  Proxy_protocol.ml Proxy.mli Proxy.ml ws.ml -o ws.exe

ws-o.exe:  Proxy_protocol.ml Proxy.ml ws.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) ws.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o ws.exe \
	  Proxy_protocol.cmx Proxy.cmx ws.cmx

proxy.exe: Proxy_protocol.ml Proxy.ml proxy.ml
	$(COMPCONC) Proxy_protocol.ml Proxy.mli  Proxy.ml uproxy.ml -o proxy.exe

proxy-o.exe:  Proxy_protocol.ml Proxy.ml uproxy.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy_protocol.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy.mli
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy_protocol.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) Proxy.ml
	ocamlopt -c $(NCFLAGS) $(FLAGS) uproxy.ml
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o proxy.exe \
	  Proxy_protocol.cmx Proxy.cmx uproxy.cmx

server-bc.exe: bc
	ocamlc -custom  $(FLAGS) $(BCFLAGS) $(BCFILES) \
	  -o server-bc.exe \
	  Proxy.cmo server.cmo

server.exe: nc
	ocamlopt $(FLAGS) $(NCFLAGS) $(NCFILES) \
	  -o server.exe \
	  Proxy.cmx server.cmx

proxy-symb-bc.exe: clean-session Proxy_protocol.ml Proxy.ml
	ocamlc -c $(BCSYMBFLAGS) Proxy_protocol.mli
	ocamlc -c $(BCSYMBFLAGS) Proxy.mli
	ocamlc -c $(BCSYMBFLAGS) Proxy_protocol.ml
	ocamlc -c $(BCSYMBFLAGS) Proxy.ml
	ocamlc -c $(BCSYMBFLAGS) proxy-symb.ml
	ocamlc -custom $(BCSYMBFLAGS) $(BCSYMBFILES) \
	  -o proxy-symb-bc.exe \
	  Proxy_protocol.cmo Proxy.cmo proxy-symb.cmo

proxy-symb.exe: clean-session Proxy_protocol.ml Proxy.ml
	ocamlopt -c $(NCSYMBFLAGS) Proxy_protocol.mli
	ocamlopt -c $(NCSYMBFLAGS) Proxy.mli
	ocamlopt -c $(NCSYMBFLAGS) Proxy_protocol.ml
	ocamlopt -c $(NCSYMBFLAGS) Proxy.ml
	ocamlopt -c $(NCSYMBFLAGS) proxy-symb.ml
	ocamlopt $(NCSYMBFLAGS) $(NCSYMBFILES) \
	  -o proxy-symb.exe \
	  Proxy_protocol.cmx Proxy.cmx proxy-symb.cmx

################
# F# Compilation
################

fs:	s2ml	client.exefs	server.exefs

client.exefs:	Proxy.mli Proxy_protocol.ml Proxy.ml client.ml
	$(COMPCONC) Proxy.mli Proxy_protocol.ml Proxy.ml client.ml -o client.exe

server.exefs:	Proxy.mli Proxy_protocol.ml Proxy.ml server.ml
	$(COMPCONC) Proxy.mli Proxy_protocol.ml Proxy.ml server.ml -o server.exe

Proxy.tc7: $(SYMLIBS) Proxy_protocol.ml7 Proxy_protocol.ml Proxy.ml7 Proxy.ml
	$(seven) -pervasives $(perv) $(SYMLIBS) Proxy_protocol.ml7 Proxy_protocol.ml Proxy.ml7 Proxy.ml > Proxy.tc7 ; egrep "Failed|error|Exception|Warning" Proxy.tc7; echo ""

Proxy-debug.tc7: $(SYMLIBS) Proxy_protocol.ml7 Proxy_protocol.ml Proxy.ml7 Proxy.ml
	$(seven) -nokindcheck -debug -scripts Proxy -pervasives $(perv) $(SYMLIBS) Proxy_protocol.ml7 Proxy_protocol.ml Proxy.ml7 Proxy.ml > Proxy.tc7 ; egrep "Failed|error|Exception|Warning" Proxy.tc7; echo ""

Proxy.f: $(SYMLIBS) Proxy.mli Proxy.ml
	$(fs2f) $(SYMLIBS) Proxy.mli Proxy.ml 

Proxy.fsi: $(SYMLIBS) Proxy.ml7 
	$(seven) -pervasives $(perv) -debug -genfsi -ask -nokindcheck -scripts ws $(SYMLIBS) Proxy.ml7 

##########
# Cleaning
##########

clean-session: 
	rm -f Proxy.ps Proxy.dot Proxy.mli Proxy.ml Proxy.cmi Proxy.cmo Proxy.cmx Proxy.o 

clean:	
	rm -f client-bc.exe client.exe client.o client.cmi client.cmo client.cmx 
	rm -f server-bc.exe server.exe server.o server.cmi server.cmo server.cmx 
	rm -f proxy-symb-bc.exe proxy-symb.exe proxy-symb.o proxy-symb.cmi proxy-symb.cmo proxy-symb.cmx
	rm -f Proxy.ps Proxy.dot Proxy.mli Proxy.ml Proxy.cmi Proxy.cmo Proxy.cmx Proxy.o
	rm -f Proxy_protocol.mli Proxy_protocol.ml Proxy_protocol.cmi Proxy_protocol.cmo Proxy_protocol.cmx Proxy_protocol.o
	rm -f Proxy_protocol.ml7 Proxy.ml7
	rm -f Proxy.dot Proxy-ext.dot Proxy-full.dot 

clean-lib:
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean

clean-all: clean
	$(MAKE) -C ../../src/lib/symbolic clean
	$(MAKE) -C ../../src/lib/ocaml-ssl clean
	$(MAKE) -C ../../src/s2ml clean

clean-cache:
	rm -f *.cache


