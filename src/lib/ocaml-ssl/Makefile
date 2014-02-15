
FLAGS = -thread 

all: interfaces bc nc

interfaces: global.cmi base64.cmi data.cmi prins.cmi crypto.cmi pi.cmi

nc: global.cmx base64.cmx data.cmx prins.cmx crypto.cmx pi.cmx openssl_impl.o
bc: global.cmo base64.cmo data.cmo prins.cmo crypto.cmo pi.cmo openssl_impl.o


%.cmi: %.mli
	ocamlc $(FLAGS) $<

%.cmo: %.ml
	ocamlc -c $(FLAGS) $<

%.cmx: %.ml
	ocamlopt -c $(FLAGS) $<

openssl_impl.o: openssl_impl.c
	ocamlopt -c $(FLAGS) -cc "cc" -ccopt "-o openssl_impl.o " openssl_impl.c


clean:
	rm -f pi.o pi.cmi pi.cmo pi.cmx 
	rm -f global.o global.cmi global.cmo global.cmx 
	rm -f base64.o base64.cmi base64.cmo base64.cmx 
	rm -f crypto.o crypto.cmi crypto.cmo crypto.cmx 
	rm -f data.o data.cmi data.cmo data.cmx 
	rm -f openssl_impl.o openssl_impl.cmi openssl_impl.cmo openssl_impl.cmx 
	rm -f prins.o prins.cmi prins.cmo prins.cmx
