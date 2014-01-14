build:
	pdfcslatex -shell-escape diplomka.tex
	bibtex diplomka.aux
	pdfcslatex -shell-escape diplomka.tex

clean:
	rm -f *fasl
	rm -f *~
	rm -f *.lo[fgtl]
	rm -f *.aux
	rm -f *.out
	rm -f *.toc
	rm -f *.bbl
	rm -f *.blg
