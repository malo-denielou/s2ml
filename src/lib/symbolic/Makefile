
FLAGS = -thread 

all: interfaces bc nc

interfaces: global.cmi base64.cmi data.cmi pi.cmi prins.cmi crypto.cmi 

nc: global.cmx base64.cmx data.cmx pi.cmx prins.cmx crypto.cmx
bc: global.cmo base64.cmo data.cmo pi.cmo prins.cmo crypto.cmo


%.cmi: %.mli
	ocamlc $(FLAGS) $<

%.cmo: %.ml
	ocamlc -c $(FLAGS) $<

%.cmx: %.ml
	ocamlopt -c $(FLAGS) $<

clean:
	rm -f global.o global.cmi global.cmo global.cmx 
	rm -f base64.o base64.cmi base64.cmo base64.cmx 
	rm -f pi.o pi.cmi pi.cmo pi.cmx 
	rm -f crypto.o crypto.cmi crypto.cmo crypto.cmx 
	rm -f data.o data.cmi data.cmo data.cmx 
	rm -f prins.o prins.cmi prins.cmo prins.cmx