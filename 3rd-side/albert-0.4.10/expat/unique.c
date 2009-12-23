/*
** By Bob Jenkins, February 22, 1997
** This is an example of how to use the hash table.
**
** Given an input (stdin) with lines in any order
**   produce an output (stdout) with duplicate lines removed.
** Lines may not be longer than 4096 characters.
*/

#ifndef STANDARD
#include "standard.h"
#endif
#ifndef HASHTAB
#include "hashtab.h"
#endif
#define LINELEN 4096

int main()
{
  ub1   buf[LINELEN];
  ub1 *key;
  ub4  keyl;
  htab *t;

  t = hcreate(8);                      /* create hash table */

  /* read in all the lines */
  while (gets((char *)buf))            /* get line from stdin */
  {
    keyl = strlen((char *)buf);
    if (hadd(t, buf, keyl, (void *)0)) /* if not a duplicate */
    {
      key = (ub1 *)malloc(keyl);       /* dumb use of malloc */
      memcpy(key, buf, keyl);          /* copy buf into key */
      hkey(t)=key;                     /* replace buf with key */
    }
  }

  if (hfirst(t)) do                    /* go to first element */
  {
    printf("%.*s\n",hkeyl(t),hkey(t)); /* dump it to stdout */
  }
  while (hnext(t));                    /* go to next element */

  while (hcount(t))                    /* while the table is not empty */
  {
    free(hkey(t));                     /* free memory for the line */
    hdel(t);                           /* delete from hash table */
  }

  hdestroy(t);                         /* destroy hash table */

  return SUCCESS;
}
