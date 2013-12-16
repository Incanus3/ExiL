
/*  Magic Outlines
 *  Copyright (C) 2001  Vilem Vychodil, <vilem.vychodil@upol.cz>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "outlines.h"

#define SIZE  (64 * 1024)
#define BASE  11

char nav [] = "pdf:outline";
char *buff = NULL;
char sbuffer [SIZE];
FILE *f;
int result, j = 0, i = 0, size = 0;

int
main (int argc, char **argv)
{
    /* check input arguments */
    if (argc != 2) {
	fprintf (stderr, "synopsis: %s [dvi file]\n", argv [0]);
	return 1;
    }

    /* read input file */
    f = fopen (argv [1], "rb");
    while (! feof (f)) {
	result = fread (&sbuffer, 1, SIZE, f);
	buff = (char *) (! buff) ?
	    malloc (result) :
	    realloc (buff, size + result);
	memmove (buff + size, &sbuffer, result);
	size += result;
    }
    fclose (f);

    /* change outlines */
    while (i < size) {

	/* find outline */
	while (i < size) {
	    if (j >= 11) break;
	    j = (buff [i ++] == nav [j]) ? j + 1 : 0;
	}

	/* check command mode */
	if ((buff [i - BASE - 2] == (signed char) 0xEF) || 
	    (buff [i - BASE - 3] == (signed char) 0xF0)) {

	    /* recode values */
	    for (; ((i < size) && (buff [i] != '(')); i ++);
	    for (i ++; 
		 ((i < size) && (buff [i] != ')')); 
		 buff [i] = (signed char) recode [(unsigned char) buff [i]],
		     i ++);
	}

	/* find next occurence */
	j = 0;
    }

    /* write result */
    f = fopen (argv [1], "wb");
    result = fwrite (buff, 1, size, f);
    fclose (f);
    free ((char *) buff);

    if (result != size) {
	fprintf (stderr, "fatal: %s cannot write output?\n", argv [0]);
	return 2;
     }

    /* finish */
    return 0;
}
