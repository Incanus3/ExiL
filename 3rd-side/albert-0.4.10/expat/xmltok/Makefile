all: libexpat_tok.a

clean:
	-$(RM) *.o *.a *~

libexpat_tok.a: xmltok.c xmlrole.c
	gcc -Wall -W -Winline -Wno-unused -g -c xmltok.c
	gcc -Wall -W -Winline -Wno-unused -g -c xmlrole.c
	ar r libexpat_tok.a xmltok.o xmlrole.o
