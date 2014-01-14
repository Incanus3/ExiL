CC	= cc
FLAGS	= \
	--ansi --pedantic -Wall -O9 -mcpu=pentiumpro -march=pentiumpro \
	-ffast-math -fomit-frame-pointer -finline-functions -fstrength-reduce \
	-fexpensive-optimizations -funroll-loops -fmove-all-movables
	
outlines: outlines.o
	$(CC) -o outlines outlines.o
	strip outlines
	
outlines.o: outlines.c outlines.h
	$(CC) $(FLAGS) -c outlines.c

outlines.h: ascii.txt il2.txt recode.pl
	./recode.pl il2.txt ascii.txt >outlines.h
	
clean:
	rm -f *.bak *~ outlines outlines.h
