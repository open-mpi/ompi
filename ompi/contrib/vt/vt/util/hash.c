/*
 * this is taken from 
 * 'http://burtleburtle.net/bob/hash/evahash.html'
 */

#include "hash.h"

/* The mixing step */
#define mix(a,b,c) \
{ \
  a=a-b;  a=a-c;  a=a^(c>>13); \
  b=b-c;  b=b-a;  b=b^(a<<8);  \
  c=c-a;  c=c-b;  c=c^(b>>13); \
  a=a-b;  a=a-c;  a=a^(c>>12); \
  b=b-c;  b=b-a;  b=b^(a<<16); \
  c=c-a;  c=c-b;  c=c^(b>>5);  \
  a=a-b;  a=a-c;  a=a^(c>>3);  \
  b=b-c;  b=b-a;  b=b^(a<<10); \
  c=c-a;  c=c-b;  c=c^(b>>15); \
}

/* hash function */
unsigned int vt_hash(register unsigned char* k,
                     unsigned int length, unsigned int initval)
{
  register unsigned int a,b,c;  /* the internal state */
  unsigned int          len;    /* how many key bytes still need mixing */

  /* Set up the internal state */
  len = length;
  a = b = 0x9e3779b9;  /* the golden ratio; an arbitrary value */
  c = initval;         /* variable initialization of internal state */

  /*---------------------------------------- handle most of the key */
  while (len >= 12)
  {
    a=a+(k[0]+((unsigned int)k[1]<<8)+((unsigned int)k[2]<<16)
	 +((unsigned int)k[3]<<24));
    b=b+(k[4]+((unsigned int)k[5]<<8)+((unsigned int)k[6]<<16)
	 +((unsigned int)k[7]<<24));
    c=c+(k[8]+((unsigned int)k[9]<<8)+((unsigned int)k[10]<<16)
	 +((unsigned int)k[11]<<24));
    mix(a,b,c);
    k = k+12; len = len-12;
  }

  /*------------------------------------- handle the last 11 bytes */
  c = c+length;
  switch(len)              /* all the case statements fall through */
  {
    case 11: c=c+((unsigned int)k[10]<<24);
    case 10: c=c+((unsigned int)k[9]<<16);
    case 9 : c=c+((unsigned int)k[8]<<8);
    /* the first byte of c is reserved for the length */
    case 8 : b=b+((unsigned int)k[7]<<24);
    case 7 : b=b+((unsigned int)k[6]<<16);
    case 6 : b=b+((unsigned int)k[5]<<8);
    case 5 : b=b+k[4];
    case 4 : a=a+((unsigned int)k[3]<<24);
    case 3 : a=a+((unsigned int)k[2]<<16);
    case 2 : a=a+((unsigned int)k[1]<<8);
    case 1 : a=a+k[0];
    /* case 0: nothing left to add */
  }
  mix(a,b,c);
  /*-------------------------------------------- report the result */
  return c;
}
