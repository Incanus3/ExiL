all: libexpat_parse.a

clean:
	-$(RM) *.o *.a *~

libexpat_parse.a: xmlparse.c hashtable.c
	gcc -I../xmltok -Wall -W -Winline -Wno-unused -g -c xmlparse.c
	gcc -Wall -W -Winline -Wno-unused -g -c hashtable.c
	ar r libexpat_parse.a xmlparse.o hashtable.o

