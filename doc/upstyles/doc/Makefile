FORMAT	= '2:0L@.8(22.4cm,0cm)+1L@.8(22.4cm,13.25cm)'

all: mp
	cslatex upstyles.tex
	cslatex upstyles.tex
	makeindex upstyles.idx
	cslatex upstyles.tex
	./outlines upstyles.dvi
	dvipdfmx -p a4 -o upstyles.pdf upstyles.dvi
	dvips -t a4 -o upstyles.ps upstyles.dvi 2>/dev/null
	pstops $(FORMAT) upstyles.ps >upstyles-a5.ps

mp: upstyles.1

upstyles.1: upstyles.mp
	mpost upstyles.mp

delbak:
	rm -f *.aux *.log *~ *.bak *.toc *.mpx *.ind *.ilg *.idx *.dvi upstyles.[1-9]*

clean:
	rm -f *.aux *.log *~ *.bak *.dvi *.ps *.pdf *.toc *.mpx *.ind *.ilg *.idx upstyles.[1-9]*
