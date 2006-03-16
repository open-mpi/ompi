/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef DATATYPE_CHECKSUM_H_HAS_BEEN_INCLUDED
#define DATATYPE_CHECKSUM_H_HAS_BEEN_INCLUDED

#define OMPI_CSUM_ZERO 0

#define OMPI_REQUIRE_DATA_VALIDATION 0

#if OMPI_REQUIRE_DATA_VALIDATION
#include "opal/util/crc.h"

#define MEMCPY_CSUM( DST, SRC, BLENGTH, CONVERTOR ) \
do { \
    /*opal_output( 0, "memcpy dest = %p src = %p length = %d\n", (void*)(DST), (void*)(SRC), (int)(BLENGTH) ); */\
    (CONVERTOR)->checksum += opal_bcopy_uicsum_partial( (SRC), (DST), (BLENGTH), (BLENGTH), &(CONVERTOR)->csum_ui1, &(CONVERTOR)->csum_ui2 ); \
} while (0)

#define COMPUTE_CSUM( SRC, BLENGTH, CONVERTOR ) \
do { \
    /*opal_output( 0, "memcpy dest = %p src = %p length = %d\n", (void*)(DST), (void*)(SRC), (int)(BLENGTH) ); */\
    (CONVERTOR)->checksum += opal_uicsum_partial( (SRC), (BLENGTH), &(CONVERTOR)->csum_ui1, &(CONVERTOR)->csum_ui2 ); \
} while (0)

#define OMPI_CSUM( SRC, BLENGTH ) \
    opal_uicsum( (SRC), (BLENGTH) )

#else

#include "ompi/datatype/datatype_memcpy.h"

#define MEMCPY_CSUM( DST, SRC, BLENGTH, CONVERTOR ) \
    MEMCPY( (DST), (SRC), (BLENGTH) )

#define COMPUTE_CSUM( SRC, BLENGTH, CONVERTOR )

#define OMPI_CSUM( SRC, BLENGTH ) OMPI_CSUM_ZERO

#endif

/* ADLER_NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */
#define ADLER_NMAX 5551
#define MOD_ADLER 65521

#if OMPI_REQUIRE_DATA_VALIDATION

#define DO1(buf,i)  {_a += buf[i]; _b += _a;}
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);

#define COMPUTE_SPECIFIC_CHECKSUM( DATA, LENGTH, ADLER32) \
do { \
    uint8_t *_data = (DATA);   /* Pointer to the data to be summed */ \
    size_t _len = (LENGTH);    /* Length in bytes */ \
    uint32_t _a = (ADLER32) & 0xffff, \
             _b = ((ADLER32) >> 16) & 0xffff; \
\
    while( _len > 0 ) { \
        unsigned _tlen = _len > ADLER_NMAX ? ADLER_NMAX : _len; \
        _len -= _tlen; \
        while( _tlen >= 16 ) { \
            DO16(_data); \
            _data += 16; \
            _tlen -= 16; \
        } \
        if( 0 != _tlen ) do { \
            _a += *_data++; _b += _a; \
        } while( --_tlen > 0 ); \
        _a = _a % MOD_ADLER; \
        _b = _b % MOD_ADLER; \
    } \
    (ADLER32) = _b << 16 | _a; \
} while(0)
#else
#define COMPUTE_SPECIFIC_CHECKSUM( DATA, LENGTH, ADLER32 )
#endif  /* OMPI_REQUIRE_DATA_VALIDATION */

#endif  /* DATATYPE_CHECKSUM_H_HAS_BEEN_INCLUDED */
