
all: subdirs alb_xml2sexp

subdirs:
	cd xmltok && $(MAKE) all
	cd xmlparse && $(MAKE) all

clean:
	cd xmltok && $(MAKE) clean
	cd xmlparse && $(MAKE) clean
	-$(RM) *.o *.a *~ alb_xml2sexp

alb_xml2sexp: xml2sexp.c hashtab.c recycle.c
	gcc -I. -Wall -W -Winline -Wno-unused -g -c hashtab.c
	gcc -I. -Wall -W -Winline -Wno-unused -g -c recycle.c
	gcc -I. -Wall -W -Winline -Wno-unused -g -c xml2sexp.c
	gcc -Wall -W -Winline -Wno-unused -g -o alb_xml2sexp xml2sexp.o hashtab.o recycle.o xmlparse/libexpat_parse.a xmltok/libexpat_tok.a
