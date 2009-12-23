/*
 * xml2sexp.c
 * Copyright (c) 2000 - Stig E Sandø
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

/* THIS FILE IS AN UGLY HACK */

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include "xmlparse/xmlparse.h"
/*#include <glib.h>*/
#include <stdlib.h>
#include "hashtab.h"

static char res_buffer[65536];

/* count how far in content we have come.. */
static int cont_ctr = 0;
static int useful_str = 0;

static void
newContentComing(void *userData, const XML_Char *buffer, int len) {

    int j;
    
    for (j=0; j < len; j++) {
	/* fix later */
	if (0) { }
	/*
	else if (buffer[j] == '\n') {
	    res_buffer[cont_ctr++] = '\\';
	    res_buffer[cont_ctr++] = '0';
	    res_buffer[cont_ctr++] = '1';
	    res_buffer[cont_ctr++] = '2';
	}
	else if (buffer[j] == '\t') {
	    res_buffer[cont_ctr++] = '\\';
	    res_buffer[cont_ctr++] = '0';
	    res_buffer[cont_ctr++] = '1';
	    res_buffer[cont_ctr++] = '1';
	}
	else if (buffer[j] == '\r') {
	    res_buffer[cont_ctr++] = '\\';
	    res_buffer[cont_ctr++] = '0';
	    res_buffer[cont_ctr++] = '1';
	    res_buffer[cont_ctr++] = '5';
	}
	*/
	else if (buffer[j] == '\\') {
	    res_buffer[cont_ctr++] = '\\';
	    res_buffer[cont_ctr++] = '\\';
	}
	else if (buffer[j] == '"') {
	    res_buffer[cont_ctr++] = '\\';
	    res_buffer[cont_ctr++] = '"';
	}
	
	else {
	    res_buffer[cont_ctr++] = buffer[j];
	}
	
	if (buffer[j] != ' ' &&
	    buffer[j] != '\n' &&
	    buffer[j] != '\t' &&
	    buffer[j] != '\r') {
	    useful_str = 1;
	}
    }
}
    
static void
contentComing(void *userData, const XML_Char *buffer, int len) {
    
    int j;
    int i = 0;
    unsigned char first_byte, second_byte;
    unsigned char val;
    int useful_str = 0;
    
    if (len) res_buffer[i++]= '-';

    /* utf-8 to iso8859-1 conversion */
/*    
    for (j=0; j < len; j++) {
	val = (unsigned char)buffer[j];
	if (val < 128) {
	    res_buffer[i++] = (char)val;
	}
	else {
	    first_byte = buffer[j] - 0xC0; (* 000xxxxx*)
	    first_byte = first_byte  >> 2;  (* 00000xxx*)
	    second_byte = buffer[j] << 6;   (* xx000000*)
	    second_byte = second_byte | (buffer[j+1] - 0x80);
	    res_buffer[i++] = (char)second_byte; (* going for iso-8859-1 *)
	    j++;
	}
    }
    */

    for (j=0; j < len; j++) {
	res_buffer[i++] = buffer[j];
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
possibleOutputOfContent() {

    if (cont_ctr > 0 && useful_str) {
	res_buffer[cont_ctr] = 0;
	printf("\"%s\"",res_buffer);
	cont_ctr = 0;
	useful_str = 0;
	
    }
    else if (cont_ctr > 0) {
	cont_ctr = 0;
	useful_str = 0;
    }

}



static const XML_Char *
getRealName(const XML_Char *name) {

    static htab *locHash = NULL;
    static int counter = 1;
    int found;
    int len = strlen(name);
    
    if (!locHash) {
	locHash = hcreate(10);
    }
    
    found = hfind(locHash, name, len);
    if (found==TRUE) {
	return hstuff(locHash);
    }
    else {
	/* we must add it */
	char *retval = malloc(50); /* leak */
	sprintf(retval, "#%d#", counter);
	hadd(locHash, name, len, retval);
	retval = malloc(50); /* leak */
	sprintf(retval, "#%d=|%s|", counter, name);
	counter++;
	return retval;
    }
    
}

static htab *locHash_2 = NULL;
static int counter_2 = 1;

static void
initTable2() {
        
    if (!locHash_2) {
	locHash_2 = hcreate(10);
    }
}

static void
keyPrinter(char* key, char *val, void *foo) {
    printf("    (cl:setf (cl:svref array %s) '|%s|)\n", val, key);
}

static void
printTable2() {

    htab *t = locHash_2;
	
    printf("(cl:setq apispec-xml::*loc-sym-table*\n  (cl:let ((array (cl:make-array '(%d))))\n",
	   counter_2);

    if (hfirst(t)) {
	do {                   /* go to first element */
	    keyPrinter(hkey(t),hstuff(t),NULL); /* dump it to stdout */
	} while (hnext(t));                    /* go to next element */
    }

    printf("\n    array))\n");
    
}

static const XML_Char *
getRealName2(const XML_Char *name) {

    int found;
    int len = strlen(name);
    
    found = hfind(locHash_2, name, len);
    if (found) {
	return hstuff(locHash_2);
    }
    else {
	/* we must add it */
	char *retval = malloc(50); /* leak */
	sprintf(retval, "%d", counter_2);
	hadd(locHash_2, strdup(name), len, retval);
	counter_2++;
	return retval;
    }
    
}

static char *
prettifyAttrVal(const XML_Char *buffer) {

    int len = strlen(buffer);

    int ctr = 0;
    int j = 0;
    
    for (j = 0; j < len; j++) {

	if (0) { }
	
	else if (buffer[j] == '\\') {
	    res_buffer[ctr++] = '\\';
	    res_buffer[ctr++] = '\\';
	}
	else if (buffer[j] == '"') {
	    res_buffer[ctr++] = '\\';
	    res_buffer[ctr++] = '"';
	}
	
	else {
	    res_buffer[ctr++] = buffer[j];
	}	

    }

    res_buffer[ctr] = 0;

    return res_buffer;
}

static void
startElement(void *userData, const XML_Char *name, const XML_Char **atts) {

    possibleOutputOfContent();
    
    printf("(%s ", getRealName2(name));
   
    
    if (!(*atts)) {
	printf("nil ");
    }
    else {
	printf("(");
	while (*atts) {
	    printf("(%s . \"%s\")", getRealName2(atts[0]), prettifyAttrVal(atts[1]));
	    atts++;
	    atts++;
	}
	printf(") ");
    }
    

    
}

static void
endElement(void *userData, const XML_Char *name) {
    possibleOutputOfContent();    
    printf(")");
}

static void
processingInstr(void *userData, const XML_Char *target,	const XML_Char *data) {
    /*
    possibleOutputOfContent();
    printf("?%s %s\n", target, data);
    */
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

    initTable2();
    
    parser = XML_ParserCreate("ISO-8859-1");
//    parser = XML_ParserCreate("UTF-8");
    XML_SetUserData(parser, &depth); 
    XML_SetElementHandler(parser, startElement, endElement);
    XML_SetCharacterDataHandler(parser, newContentComing);
    XML_SetProcessingInstructionHandler(parser, processingInstr);

    printf("(cl:in-package :xml-names)\n");

    printf("(cl:setq apispec-xml::*just-read-objs* '(");
    do {
	size_t len = fread(parsearray, 1, sizeof(parsearray), infile);
	done = len < sizeof(parsearray);
	if (!XML_Parse(parser, parsearray, len, done)) {
	    fprintf(stderr, "%s at line %d\n", XML_ErrorString(XML_GetErrorCode(parser)), XML_GetCurrentLineNumber(parser));
	    return 1;
	}
    } while (!done);

    printf(" ))\n");

    printTable2();
    
    XML_ParserFree(parser);
    
    return 0;
}
