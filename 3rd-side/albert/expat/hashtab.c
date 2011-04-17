/*
--------------------------------------------------------------------
By Bob Jenkins.  hashtab.c

This implements a hash table.
* Keys are unique.  Adding an item fails if the key is already there.
* Keys and items are pointed at, not copied.  If you change the value
  of the key after it is inserted then hfind will not be able to find it.
* The hash table maintains a position that can be set and queried.
* The table length doubles dynamically and never shrinks.  The insert
  that causes table doubling may take a long time.
* The table length splits when the table length equals the number of items
  Comparisons usually take 7 instructions.
  Computing a hash value takes 35+6n instructions for an n-byte key.

  hcreate  - create a hash table
  hdestroy - destroy a hash table
   hcount  - The number of items in the hash table
   hkey    - key at the current position
   hkeyl   - key length at the current position
   hstuff  - stuff at the current position
  hfind    - find an item in the table
   hadd    - insert an item into the table
   hdel    - delete an item from the table
  hstat    - print statistics about the table
   hfirst  - position at the first item in the table
   hnext   - move the position to the next item in the table
--------------------------------------------------------------------
*/

#ifndef STANDARD
#include "standard.h"
#endif
#ifndef LOOKUPA
#include "lookupa.h"
#endif
#ifndef HASHTAB
#include "hashtab.h"
#endif
#ifndef RECYCLE
#include "recycle.h"
#endif
#include <stdlib.h>
#include <string.h>

/*
--------------------------------------------------------------------
mix -- mix 3 32-bit values reversibly.
For every delta with one or two bit set, and the deltas of all three
  high bits or all three low bits, whether the original value of a,b,c
  is almost all zero or is uniformly distributed,
* If mix() is run forward or backward, at least 32 bits in a,b,c
  have at least 1/4 probability of changing.
* If mix() is run forward, every bit of c will change between 1/3 and
  2/3 of the time.  (Well, 22/100 and 78/100 for some 2-bit deltas.)
mix() takes 36 machine instructions, but only 18 cycles on a superscalar
  machine.  (Pentiums and Sparcs do not appear to be superscalar machines,
  despite claims to the contrary.)  No faster mixer seems to work,
  that's the result of my brute-force search.  There were about 2^^68
  hashes to choose from.  I only tested about a billion of those.
--------------------------------------------------------------------
*/
#define mix(a,b,c) \
{ \
  a -= b; a -= c; a ^= (c>>13); \
  b -= c; b -= a; b ^= (a<<8); \
  c -= a; c -= b; c ^= (b>>13); \
  a -= b; a -= c; a ^= (c>>12);  \
  b -= c; b -= a; b ^= (a<<16); \
  c -= a; c -= b; c ^= (b>>5); \
  a -= b; a -= c; a ^= (c>>3);  \
  b -= c; b -= a; b ^= (a<<10); \
  c -= a; c -= b; c ^= (b>>15); \
}

/*
--------------------------------------------------------------------
lookup() -- hash a variable-length key into a 32-bit value
  k     : the key (the unaligned variable-length array of bytes)
  len   : the length of the key, counting by bytes
  level : can be any 4-byte value
Returns a 32-bit value.  Every bit of the key affects every bit of
the return value.  Every 1-bit and 2-bit delta achieves avalanche.
About 6len+35 instructions.

The best hash table sizes are powers of 2.  There is no need to do
mod a prime (mod is sooo slow!).  If you need less than 32 bits,
use a bitmask.  For example, if you need only 10 bits, do
  h = (h & hashmask(10));
In which case, the hash table should have hashsize(10) elements.

If you are hashing n strings (ub1 **)k, do it like this:
  for (i=0, h=0; i<n; ++i) h = lookup( k[i], len[i], h);

By Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial.

See http://burtleburtle.net/bob/hash/evahash.html
Use for hash table lookup, or anything where one collision in 2^32 is
acceptable.  Do NOT use for cryptographic purposes.
--------------------------------------------------------------------
*/

ub4 h_lookup( k, length, level)
register ub1 *k;        /* the key */
register ub4  length;   /* the length of the key */
register ub4  level;    /* the previous hash, or an arbitrary value */
{
   register ub4 a,b,c,len;

   /* Set up the internal state */
   len = length;
   a = b = 0x9e3779b9;  /* the golden ratio; an arbitrary value */
   c = level;           /* the previous hash value */

   /*---------------------------------------- handle most of the key */
   while (len >= 12)
   {
      a += (k[0] +((ub4)k[1]<<8) +((ub4)k[2]<<16) +((ub4)k[3]<<24));
      b += (k[4] +((ub4)k[5]<<8) +((ub4)k[6]<<16) +((ub4)k[7]<<24));
      c += (k[8] +((ub4)k[9]<<8) +((ub4)k[10]<<16)+((ub4)k[11]<<24));
      mix(a,b,c);
      k += 12; len -= 12;
   }

   /*------------------------------------- handle the last 11 bytes */
   c += length;
   switch(len)              /* all the case statements fall through */
   {
   case 11: c+=((ub4)k[10]<<24);
   case 10: c+=((ub4)k[9]<<16);
   case 9 : c+=((ub4)k[8]<<8);
      /* the first byte of c is reserved for the length */
   case 8 : b+=((ub4)k[7]<<24);
   case 7 : b+=((ub4)k[6]<<16);
   case 6 : b+=((ub4)k[5]<<8);
   case 5 : b+=k[4];
   case 4 : a+=((ub4)k[3]<<24);
   case 3 : a+=((ub4)k[2]<<16);
   case 2 : a+=((ub4)k[1]<<8);
   case 1 : a+=k[0];
     /* case 0: nothing left to add */
   }
   mix(a,b,c);
   /*-------------------------------------------- report the result */
   return c;
}


/*
--------------------------------------------------------------------
mixc -- mixc 8 4-bit values as quickly and thoroughly as possible.
Repeating mix() three times achieves avalanche.
Repeating mix() four times eliminates all funnels and all
  characteristics stronger than 2^{-11}.
--------------------------------------------------------------------
*/
#define mixc(a,b,c,d,e,f,g,h) \
{ \
   a^=b<<11; d+=a; b+=c; \
   b^=c>>2;  e+=b; c+=d; \
   c^=d<<8;  f+=c; d+=e; \
   d^=e>>16; g+=d; e+=f; \
   e^=f<<10; h+=e; f+=g; \
   f^=g>>4;  a+=f; g+=h; \
   g^=h<<8;  b+=g; h+=a; \
   h^=a>>9;  c+=h; a+=b; \
}

/*
--------------------------------------------------------------------
checksum() -- hash a variable-length key into a 256-bit value
  k     : the key (the unaligned variable-length array of bytes)
  len   : the length of the key, counting by bytes
  state : an array of CHECKSTATE 4-byte values (256 bits)
The state is the checksum.  Every bit of the key affects every bit of
the state.  There are no funnels.  About 112+6.875len instructions.

If you are hashing n strings (ub1 **)k, do it like this:
  for (i=0; i<8; ++i) state[i] = 0x9e3779b9;
  for (i=0, h=0; i<n; ++i) checksum( k[i], len[i], state);

(c) Bob Jenkins, 1996.  bob_jenkins@burtleburtle.net.  You may use this
code any way you wish, private, educational, or commercial, as long
as this whole comment accompanies it.

See http://burtleburtle.net/bob/hash/evahash.html
Use to detect changes between revisions of documents, assuming nobody
is trying to cause collisions.  Do NOT use for cryptography.
--------------------------------------------------------------------
*/
void  checksum( k, len, state)
register ub1 *k;
register ub4  len;
register ub4 *state;
{
   register ub4 a,b,c,d,e,f,g,h,length;

   /* Use the length and level; add in the golden ratio. */
   length = len;
   a=state[0]; b=state[1]; c=state[2]; d=state[3];
   e=state[4]; f=state[5]; g=state[6]; h=state[7];

   /*---------------------------------------- handle most of the key */
   while (len >= 32)
   {
      a += (k[0] +(k[1]<<8) +(k[2]<<16) +(k[3]<<24));
      b += (k[4] +(k[5]<<8) +(k[6]<<16) +(k[7]<<24));
      c += (k[8] +(k[9]<<8) +(k[10]<<16)+(k[11]<<24));
      d += (k[12]+(k[13]<<8)+(k[14]<<16)+(k[15]<<24));
      e += (k[16]+(k[17]<<8)+(k[18]<<16)+(k[19]<<24));
      f += (k[20]+(k[21]<<8)+(k[22]<<16)+(k[23]<<24));
      g += (k[24]+(k[25]<<8)+(k[26]<<16)+(k[27]<<24));
      h += (k[28]+(k[29]<<8)+(k[30]<<16)+(k[31]<<24));
      mixc(a,b,c,d,e,f,g,h);
      mixc(a,b,c,d,e,f,g,h);
      mixc(a,b,c,d,e,f,g,h);
      mixc(a,b,c,d,e,f,g,h);
      k += 32; len -= 32;
   }

   /*------------------------------------- handle the last 31 bytes */
   h += length;
   switch(len)
   {
   case 31: h+=(k[30]<<24);
   case 30: h+=(k[29]<<16);
   case 29: h+=(k[28]<<8);
   case 28: g+=(k[27]<<24);
   case 27: g+=(k[26]<<16);
   case 26: g+=(k[25]<<8);
   case 25: g+=k[24];
   case 24: f+=(k[23]<<24);
   case 23: f+=(k[22]<<16);
   case 22: f+=(k[21]<<8);
   case 21: f+=k[20];
   case 20: e+=(k[19]<<24);
   case 19: e+=(k[18]<<16);
   case 18: e+=(k[17]<<8);
   case 17: e+=k[16];
   case 16: d+=(k[15]<<24);
   case 15: d+=(k[14]<<16);
   case 14: d+=(k[13]<<8);
   case 13: d+=k[12];
   case 12: c+=(k[11]<<24);
   case 11: c+=(k[10]<<16);
   case 10: c+=(k[9]<<8);
   case 9 : c+=k[8];
   case 8 : b+=(k[7]<<24);
   case 7 : b+=(k[6]<<16);
   case 6 : b+=(k[5]<<8);
   case 5 : b+=k[4];
   case 4 : a+=(k[3]<<24);
   case 3 : a+=(k[2]<<16);
   case 2 : a+=(k[1]<<8);
   case 1 : a+=k[0];
   }
   mixc(a,b,c,d,e,f,g,h);
   mixc(a,b,c,d,e,f,g,h);
   mixc(a,b,c,d,e,f,g,h);
   mixc(a,b,c,d,e,f,g,h);

   /*-------------------------------------------- report the result */
   state[0]=a; state[1]=b; state[2]=c; state[3]=d;
   state[4]=e; state[5]=f; state[6]=g; state[7]=h;
}


/* sanity check -- make sure ipos, apos, and count make sense */
static void  hsanity(t)
htab *t;
{
  ub4    i, end, counter;
  hitem *h;

  /* test that apos makes sense */
  end = (ub4)1<<(t->logsize);
  if (end < t->apos)
    printf("error:  end %ld  apos %ld\n", end, t->apos);

  /* test that ipos is in bucket apos */
  if (t->ipos)
  {
    for (h=t->table[t->apos];  h && h != t->ipos;  h = h->next)
      ;
    if (h != t->ipos)
      printf("error:ipos not in apos, apos is %ld\n", t->apos);
  }

  /* test that t->count is the number of elements in the table */
  counter=0;
  for (counter=0, i=0;  i<end;  ++i)
    for (h=t->table[i];  h;  h=h->next)
      ++counter;
  if (counter != t->count)
    printf("error: counter %ld  t->count %ld\n", counter, t->count);
}


/*
 * hgrow - Double the size of a hash table.
 * Allocate a new, 2x bigger array,
 * move everything from the old array to the new array,
 * then free the old array.
 */
static void hgrow( t)
htab  *t;    /* table */
{
  register ub4     newsize = (ub4)1<<(++t->logsize);
  register ub4     newmask = newsize-1;
  register ub4     i;
  register hitem **oldtab = t->table;
  register hitem **newtab = (hitem **)malloc(newsize*sizeof(hitem *));

  /* make sure newtab is cleared */
  for (i=0; i<newsize; ++i) newtab[i] = (hitem *)0;
  t->table = newtab;
  t->mask = newmask;

  /* Walk through old table putting entries in new table */
  for (i=newsize>>1; i--;)
  {
    register hitem *this, *that, **newplace;
    for (this = oldtab[i]; this;)
    {
      that = this;
      this = this->next;
      newplace = &newtab[(that->hval & newmask)];
      that->next = *newplace;
      *newplace = that;
    }
  }

  /* position the hash table on some existing item */
  hfirst(t);

  /* free the old array */
  free((char *)oldtab);

}

/* hcreate - create a hash table initially of size power(2,logsize) */
htab *hcreate(logsize)
word  logsize;    /* log base 2 of the size of the hash table */
{
  ub4 i,len;
  htab *t = (htab *)malloc(sizeof(htab));

  len = ((ub4)1<<logsize);
  t->table = (hitem **)malloc(sizeof(hitem *)*(ub4)len);
  for (i=0; i<len; ++i) t->table[i] = (hitem *)0;
  t->logsize = logsize;
  t->mask = len-1;
  t->count = 0;
  t->apos = (ub4)0;
  t->ipos = (hitem *)0;
  t->space = remkroot(sizeof(hitem));
  t->bcount = 0;
  return t;
}

/* hdestroy - destroy the hash table and free all its memory */
void hdestroy( t)
htab  *t;    /* the table */
{
  hitem *h;
  refree(t->space);
  free((char *)t->table);
  free((char *)t);
}

/* hcount() is a macro, see hashtab.h */
/* hkey() is a macro, see hashtab.h */
/* hkeyl() is a macro, see hashtab.h */
/* hstuff() is a macro, see hashtab.h */

/* hfind - find an item with a given key in a hash table */
word   hfind( t, key, keyl )
htab  *t;     /* table */
ub1   *key;   /* key to find */
ub4    keyl;  /* key length */
{
  hitem *h;
  ub4    x = h_lookup(key,keyl,0);
  ub4    y;
  for (h = t->table[y=(x&t->mask)]; h; h = h->next)
  {
    if ((x == h->hval) && 
        (keyl == h->keyl) && 
        !memcmp(key, h->key, keyl))
    {
      t->apos = y;
      t->ipos = h;
      return TRUE;
    }
  }
  return FALSE;
}

/*
 * hadd - add an item to a hash table.
 * return FALSE if the key is already there, otherwise TRUE.
 */
word hadd( t, key, keyl, stuff)
htab  *t;      /* table */
ub1   *key;    /* key to add to hash table */
ub4    keyl;   /* key length */
void  *stuff;  /* stuff to associate with this key */
{
  register hitem  *h,**hp;
  register ub4     y, x = h_lookup(key,keyl,0);

  /* make sure the key is not already there */
  for (h = t->table[(y=(x&t->mask))]; h; h = h->next)
  {
    if ((x == h->hval) && 
        (keyl == h->keyl) && 
        !memcmp(key, h->key, keyl))
    {
      t->apos = y;
      t->ipos = h;
      return FALSE;
    }
  }

  /* find space for a new item */
  h = (hitem *)renew(t->space);

  /* make the hash table bigger if it is getting full */
  if (++t->count > (ub4)1<<(t->logsize))
  {
    hgrow(t);
    y = (x&t->mask);
  }

  /* add the new key to the table */
  h->key   = key;
  h->keyl  = keyl;
  h->stuff = stuff;
  h->hval  = x;
  hp = &t->table[y];
  h->next = *hp;
  *hp = h;
  t->ipos = h;
  t->apos = y;

#ifdef HSANITY
  hsanity(t);
#endif  /* HSANITY */

  return TRUE;
}

/* hdel - delete the item at the current position */
word  hdel(t)
htab *t;      /* the hash table */
{
  hitem  *h;    /* item being deleted */
  hitem **ip;   /* a counter */

  /* check for item not existing */
  if (!(h = t->ipos)) return FALSE;

  /* remove item from its list */
  for (ip = &t->table[t->apos]; *ip != h; ip = &(*ip)->next)
    ;
  *ip = (*ip)->next;
  --(t->count);

  /* adjust position to something that exists */
  if (!(t->ipos = h->next)) hnbucket(t);

  /* recycle the deleted hitem node */
  redel(t->space, h);

#ifdef HSANITY
  hsanity(t);
#endif  /* HSANITY */

  return TRUE;
}

/* hfirst - position on the first element in the table */
word hfirst(t)
htab  *t;    /* the hash table */
{
  t->apos = t->mask;
  (void)hnbucket(t);
  return (t->ipos != (hitem *)0);
}

/* hnext() is a macro, see hashtab.h */

/*
 * hnbucket - Move position to the first item in the next bucket.
 * Return TRUE if we did not wrap around to the beginning of the table
 */
word hnbucket(t)
htab *t;
{
  ub4  oldapos = t->apos;
  ub4  end = (ub4)1<<(t->logsize);
  ub4  i;

  /* see if the element can be found without wrapping around */
  for (i=oldapos+1; i<end; ++i)
  {
    if (t->table[i&t->mask])
    {
      t->apos = i;
      t->ipos = t->table[i];
      return TRUE;
    }
  }

  /* must have to wrap around to find the last element */
  for (i=0; i<=oldapos; ++i)
  {
    if (t->table[i])
    {
      t->apos = i;
      t->ipos = t->table[i];
      return FALSE;
    }
  }

  return FALSE;
}

void hstat(t)
htab  *t;
{
  ub4     i,j;
  double  total = 0.0;
  hitem  *h;
  hitem  *walk, *walk2, *stat = (hitem *)0;

  /* in stat, keyl will store length of list, hval the number of buckets */
  for (i=0; i<=t->mask; ++i)
  {
    for (h=t->table[i], j=0; h; ++j, h=h->next)
      ;
    for (walk=stat; walk && (walk->keyl != j); walk=walk->next)
      ;
    if (walk)
    {
      ++(walk->hval);
    }
    else
    {
      walk = (hitem *)renew(t->space);
      walk->keyl = j;
      walk->hval = 1;
      if (!stat || stat->keyl > j) {walk->next=stat; stat=walk;}
      else
      {
        for (walk2=stat;
             walk2->next && (walk2->next->keyl<j);
             walk2=walk2->next)
          ;
        walk->next = walk2->next;
        walk2->next = walk;
      }
    }
  }

  /* figure out average list length for existing elements */
  for (walk=stat; walk; walk=walk->next)
  {
    total+=(double)walk->hval*(double)walk->keyl*(double)walk->keyl;
  }
  if (t->count) total /= (double)t->count;
  else          total  = (double)0;

  /* print statistics */
  printf("\n");
  for (walk=stat; walk; walk=walk->next)
  {
    printf("items %ld:  %ld buckets\n", walk->keyl, walk->hval);
  }
  printf("\nbuckets: %ld  items: %ld  existing: %g\n\n",
         ((ub4)1<<t->logsize), t->count, total);

  /* clean up */
  while (stat)
  {
    walk = stat->next;
    redel(t->space, stat);
    stat = walk;
  }
}


