clean:
	rm -f example-without-binomial_b.f
	rm -f example-without-binomial_b.msg
	rm -f example-without-binomial
	rm -f example-with-binomial_b.f
	rm -f example-with-binomial_b.msg
	rm -f example-with-binomial
	rm -f example-without-bisection.c
	rm -f example-without-bisection
	rm -f example-with-bisection.c
	rm -f example-with-bisection
	rm -f a-space.data
	rm -f a-time.data
	rm -f b-space.data
	rm -f b-time.data
	rm -f c-space.data
	rm -f c-time.data
	rm -f d-space.data
	rm -f d-time.data
	rm -f e-space.data
	rm -f e-time.data
	rm -f f-space.data
	rm -f f-time.data
	rm -f g-space.data
	rm -f g-time.data
	rm -f h-space.data
	rm -f h-time.data

real-clean: clean
	rm -f run-example.text
	rm -f example-space.pdf
	rm -f example-time.pdf

example-without-binomial: example-without-binomial.f
	tapenade3.6 -o example-without-binomial\
		-b -root f example-without-binomial.f
	gfortran -o example-without-binomial -O3\
		example-without-binomial.f example-without-binomial_b.f\
		~/pkg/tapenade3.6/ADFirstAidKit/adStack.c\
		~/pkg/tapenade3.6/ADFirstAidKit/adBuffer.f

example-with-binomial: example-with-binomial.f
	tapenade3.6 -o example-with-binomial\
		-b -root f example-with-binomial.f
	gfortran -o example-with-binomial -O3\
		example-with-binomial.f example-with-binomial_b.f\
		~/pkg/tapenade3.6/ADFirstAidKit/adStack.c\
		~/pkg/tapenade3.6/ADFirstAidKit/adBuffer.f\
		~/pkg/tapenade3.6/ADFirstAidKit/treeverse.f

example-without-bisection: example-without-bisection.vlad
	vlad-compiler 10000 example-without-bisection

example-with-bisection: example-with-bisection.vlad
	vlad-compiler 10000 example-with-bisection

run-example.text: example-without-binomial\
                  example-with-binomial\
                  example-without-bisection\
                  example-with-bisection\
                  run-example
	unbuff >run-example.text run-example

plots: run-example.text
	qsci <run-example.sc
	gnuplot example.gnuplot
