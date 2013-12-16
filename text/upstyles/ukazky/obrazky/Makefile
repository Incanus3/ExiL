all: ukazky.ps

ukazky.ps: ukazky.dvi
	dvips -t a4 -o ukazky.ps ukazky.dvi
	dvipdfm -p a4 -o ukazky.pdf ukazky.dvi
	
ukazky.dvi: pokus.1 automaty.1 ukazky.tex
	csplain ukazky.tex
	
automaty.1: automaty.mp
	mpost automaty.mp

pokus.1: pokus.mp
	mpost pokus.mp

clean:
	rm -f *.lo* *.aux *.ind *.idx *.ilg *.toc *.dvi *.mpx

cleanall: clean
	rm -f *.ps *.pdf *.[1-9]*
