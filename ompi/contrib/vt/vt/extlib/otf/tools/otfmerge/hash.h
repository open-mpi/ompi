/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2010.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

#ifndef HASH_H
#define HASH_H

#include "handler.h"

/* *** macros ****************************************/

/* 0x100 = 4096 */
#define HASH_SIZE	0x1000

#define HASH_GET_KEY(key)	\
{							\
  key += ~(key << 15);		\
  key ^=  (key >> 10);		\
  key +=  (key << 3);		\
  key ^=  (key >> 6);		\
  key += ~(key << 11);		\
  key ^=  (key >> 16);		\
  key &= HASH_SIZE - 1;     \
}

/* initialize the hash */
hashtabT* hash_new( void );

/* free all mem of the hash */
void hash_delete( hashtabT *hash );

/* add an entry to the hash */
void hash_add( hashtabT *hash, uint32_t entry );

/* search an entry in the hash */
streaminfoT* hash_search( hashtabT *hash, uint32_t entry);



#endif /* HASH_H */
