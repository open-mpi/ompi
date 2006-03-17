/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED
#define DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED

extern void* mmx_memcpy( void* dst, const void* src, size_t n );
extern void* mmx2_memcpy( void* dst, const void* src, size_t n );
extern void* sse_memcpy( void* dst, const void* src, size_t n );

/* for small memory blocks (<256 bytes) this version is faster */
#define small_memcpy(to,from,n)                                   \
{                                                                 \
    register unsigned long int dummy;                             \
    void *_dst = (to);                                            \
    const void *_src = (from);                                    \
    __asm__ __volatile__( "rep; movsb"                            \
                          :"=&D"(_dst), "=&S"(_src), "=&c"(dummy) \
                          :"0" (_dst), "1" (_src),"2" (n)         \
                          : "memory");                            \
}

#define MEMCPY( DST, SRC, BLENGTH ) \
    memcpy( (DST), (SRC), (BLENGTH) )
#if 0
#define MEMCPY( DST, SRC, BLENGTH ) \
do { \
    if( 128 > (BLENGTH) ) { \
        small_memcpy( (DST), (SRC), (BLENGTH) ); \
    } else if( (64*1024-100) > (BLENGTH) ) { \
        mmx_memcpy( (DST), (SRC), (BLENGTH) ); \
    } else {\
        mmx2_memcpy( (DST), (SRC), (BLENGTH) ); \
    } \
} while (0)
#endif

#endif  /* DATATYPE_MEMCPY_H_HAS_BEEN_INCLUDED */
