build:
	pdfcslatex -shell-escape diplomka.tex
	bibtex diplomka.aux
	pdfcslatex -shell-escape diplomka.tex

psbuild:
	cslatex -shell-escape diplomka.tex
	bibtex diplomka.aux
	cslatex -shell-escape diplomka.tex
	dvipdfm diplomka.dvi

clean:
	rm -f *fasl
	rm -f *~
	rm -f *.lo[fgtl]
	rm -f *.aux
	rm -f *.out
	rm -f *.toc
	rm -f *.bbl
	rm -f *.blg
