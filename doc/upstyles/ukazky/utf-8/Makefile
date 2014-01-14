all: sample-upbook.dvi sample-updiplom.dvi sample-upreport.dvi sample-uproject.dvi sample-upsimple.dvi export

sample-upbook.dvi: sample-upbook.tex
	cslatex sample-upbook.tex
	cslatex sample-upbook.tex
	makeindex sample-upbook.idx
	cslatex sample-upbook.tex
	./outlines sample-upbook.dvi
	dvips -t a4 -o sample-upbook.ps sample-upbook.dvi 2>/dev/null
	dvipdfmx -p a4 -o sample-upbook.pdf sample-upbook.dvi

sample-updiplom.dvi: sample-updiplom.tex
	cslatex sample-updiplom.tex
	cslatex sample-updiplom.tex
	./outlines sample-updiplom.dvi
	dvips -t a4 -o sample-updiplom.ps sample-updiplom.dvi 2>/dev/null
	dvipdfmx -p a4 -o sample-updiplom.pdf sample-updiplom.dvi

sample-upreport.dvi: sample-upreport.tex
	cslatex sample-upreport.tex
	cslatex sample-upreport.tex
	makeindex sample-upreport.idx
	cslatex sample-upreport.tex
	./outlines sample-upreport.dvi
	dvips -t a4 -o sample-upreport.ps sample-upreport.dvi 2>/dev/null
	dvipdfmx -p a4 -o sample-upreport.pdf sample-upreport.dvi

sample-uproject.dvi: sample-uproject.tex
	cslatex sample-uproject.tex
	cslatex sample-uproject.tex
	./outlines sample-uproject.dvi
	dvips -t a4 -o sample-uproject.ps sample-uproject.dvi 2>/dev/null
	dvipdfmx -p a4 -o sample-uproject.pdf sample-uproject.dvi

sample-upsimple.dvi: sample-upsimple.tex
	cslatex sample-upsimple.tex
	cslatex sample-upsimple.tex
	./outlines sample-upsimple.dvi
	dvips -t a4 -o sample-upsimple.ps sample-upsimple.dvi 2>/dev/null
	dvipdfmx -p a4 -o sample-upsimple.pdf sample-upsimple.dvi

export:
	for f in sample*tex; do \
		cat $$f | grep -v 'inputenc' | recode u8..iso8859-2 > ../iso8859-2/$$f; \
		cat $$f | grep -v 'inputenc' | recode u8..cp1250 > ../cp1250/$$f; \
	done
	cp -f sample*pdf sample*ps sample*dvi ../iso8859-2
	cp -f sample*pdf sample*ps sample*dvi ../cp1250

clean:
	rm -f *.lo* *.aux *.ind *.idx *.ilg *.toc
