/*
 * this is taken from 
 * 'http://burtleburtle.net/bob/hash/evahash.html'
 */

#ifndef _HASH_H
#define _HASH_H

#ifdef __cplusplus
# define EXTERN extern "C" 
#else
# define EXTERN extern 
#endif

EXTERN unsigned int vt_hash(register unsigned char* k, unsigned int length, unsigned int initval);

#endif /* HASH_H */

