/*
 * xml2esis.c
 * Copyright (c) 1999 - Stig E Sandø
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <sys/stat.h>
#include <expat/xmlparse.h>


static char res_buffer[2048];

static void
contentComing(void *userData, const XML_Char *buffer, int len) {
    
    int j;
    int i = 0;
    unsigned char first_byte, second_byte;
    unsigned char val;
    int useful_str = 0;
    
    if (len) res_buffer[i++]= '-';
    
    for (j=0; j < len; j++) {
	val = (unsigned char)buffer[j];
	if (val < 128) {
	    res_buffer[i++] = (char)val;
	}
	else {
	    first_byte = buffer[j] - 0xC0; /* 000xxxxx*/
	    first_byte = first_byte  >> 2;  /* 00000xxx*/
	    second_byte = buffer[j] << 6;   /* xx000000*/
	    second_byte = second_byte | (buffer[j+1] - 0x80);
	    res_buffer[i++] = (char)second_byte; /* going for iso-8859-1 */
	    j++;
	}
    }
    res_buffer[i] = '\0';

    for (j=1; j < i; j++) {
	if (res_buffer[j] == '\n' ||
	    res_buffer[j] == '\t' ||
	    res_buffer[j] == '\r') res_buffer[j] = ' ';
	
	if (res_buffer[j] != ' ') {
/*	    printf ("Triggered %s >> on (%c,%d)\n", res_buffer, res_buffer[j], res_buffer[j];*/
	    useful_str = 1;
	}
    }

    if (useful_str) {
	puts(res_buffer);
    }

}

static void
startElement(void *userData, const XML_Char *name, const XML_Char **atts) {

    while (*atts) {
	printf("A%s CDATA %s\n", atts[0], atts[1]);
	atts++;
	atts++;
    }
    
    printf("(%s\n", name);
}

static void
endElement(void *userData, const XML_Char *name) {
    printf(")%s\n", name);
}

static void
processingInstr(void *userData, const XML_Char *target,	const XML_Char *data) {
    printf("?%s %s\n", target, data);
}

static int
isFile(const char *filename) {
   
  struct stat stbuf;

  if (stat(filename, &stbuf)) return 0;
  return ((stbuf.st_mode & S_IFMT) == S_IFREG) ? 1 : 0;
}


char parsearray[BUFSIZ];

int
main(int argc, char *argv[]) {

    const char *filename = argv[1];
    
    FILE *infile;
    XML_Parser parser;
    
    int done;
    int depth = 0;

    if (!filename || !isFile(filename)) {
	return 2;
    }
    else {
	infile = fopen(filename, "r");
    }
    
    parser = XML_ParserCreate("ISO-8859-1");
    XML_SetUserData(parser, &depth); 
    XML_SetElementHandler(parser, startElement, endElement);
    XML_SetCharacterDataHandler(parser, contentComing);
    XML_SetProcessingInstructionHandler(parser, processingInstr);

    do {
	size_t len = fread(parsearray, 1, sizeof(parsearray), infile);
	done = len < sizeof(parsearray);
	if (!XML_Parse(parser, parsearray, len, done)) {
	    fprintf(stderr, "%s at line %d\n", XML_ErrorString(XML_GetErrorCode(parser)), XML_GetCurrentLineNumber(parser));
	    return 1;
	}
    } while (!done);
    
    XML_ParserFree(parser);
    puts("C");
    
    return 0;
}
