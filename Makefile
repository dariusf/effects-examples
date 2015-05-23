all: concurrent generator state ref

concurrent: sched.mli sched.ml concurrent.ml
	ocamlc -o concurrent sched.mli sched.ml concurrent.ml

generator: generator.ml
	ocamlc -o generator generator.ml

state: state.ml
	ocamlc -o state state.ml

ref: ref.ml
	ocamlc -o ref ref.ml

clean:
	rm -f *.cmi *.cmo *.o concurrent generator *~ a.out state ref
