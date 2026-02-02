/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018      FUJITSU LIMITED.  All rights reserved.
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * Copyright (c) 2024      Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#ifdef HAVE_IEEE754_H
#    include <ieee754.h>
#endif

#include <stddef.h>
#include <stdint.h>

#include "opal/util/arch.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_checksum.h"
#include "opal/datatype/opal_datatype_constructors.h"
#include "opal/types.h"

/*
 * Long-Term TODO:
 * We need a better way for the upper layer to convert
 * multiple, consecutive struct-types, e.g. float_int.
 * In the current design, the copy_float_heterogeneous and copy_float_heterogeneous
 * functions would be called 2*count times in total.
 * This is a big performance hit for a structure types.
 *
 * A better way would be to have a conversion registration functionality.
 */

static inline void opal_dt_swap_bytes(void *to_p, const void *from_p, const size_t size,
                                      size_t count)
{
    size_t i;
    size_t back_i = size - 1;
    uint8_t *to = (uint8_t *) to_p;
    uint8_t *from = (uint8_t *) from_p;

    /* Do the first element */
    for (i = 0; i < size; i++, back_i--) {
        to[back_i] = from[i];
    }
    /* Do all the others if any */
    while (count > 1) {
        to += size;
        from += size;
        count--;
        for (i = 0, back_i = size - 1; i < size; i++, back_i--) {
            to[back_i] = from[i];
        }
    }
}

static inline void opal_dt_swap_bytes_inplace(void *buf_p, const size_t size,
                                      size_t count)
{
    size_t i;
    size_t back_i = size - 1;
    uint8_t *buf = (uint8_t *) buf_p;
    uint8_t copy[32];

    assert(size <= 32);

    /* Do the first element */
    for (i = 0; i < size; i++) {
        copy[i] = buf[i];
    }
    for (i = 0; i < size; i++, back_i--) {
        buf[back_i] = copy[i];
    }
    /* Do all the others if any */
    while (count > 1) {
        buf += size;
        count--;
        for (i = 0; i < size; i++) {
            copy[i] = buf[i];
        }
        for (i = 0, back_i = size - 1; i < size; i++, back_i--) {
            buf[back_i] = copy[i];
        }
    }
}

#ifdef HAVE_IEEE754_H
struct bit128 {
    unsigned int mantissa3 : 32;
    unsigned int mantissa2 : 32;
    unsigned int mantissa1 : 32;
    unsigned int mantissa0 : 16;
    unsigned int exponent : 15;
    unsigned int negative : 1;
};

struct bit80 {
    unsigned int pad : 32;
    unsigned int empty : 16;
    unsigned int negative : 1;
    unsigned int exponent : 15;
    unsigned int mantissa0 : 32;
    unsigned int mantissa1 : 32;
};

static inline void opal_dt_swap_long_double(void *to_p, const void *from_p, const size_t size,
                                            size_t count, uint32_t remoteArch)
{

#    ifdef HAVE_IEEE754_H

    if ((opal_local_arch & OPAL_ARCH_LDISINTEL) && !(remoteArch & OPAL_ARCH_LDISINTEL)) {
#        ifdef __x86_64
        long double *to = (long double *) to_p;

        for (size_t i = 0; i < count; i++, to++) {
            union ieee854_long_double ld;
            struct bit128 *b = (struct bit128 *) to;
            ld.ieee.empty = 0;
            ld.ieee.mantissa0 = 0x80000000 | (((unsigned int) b->mantissa0 << 15) & 0x7FFF8000)
                                | ((b->mantissa1 >> 17) & 0x00007FFF);
            ld.ieee.mantissa1 = ((b->mantissa1 << 15) & 0xFFFF8000)
                                | ((b->mantissa2 << 17) & 0x000007FFF);
            ld.ieee.exponent = b->exponent;
            ld.ieee.negative = b->negative;
            MEMCPY(to, &ld, sizeof(long double));
        }
#        endif
    } else if (!(opal_local_arch & OPAL_ARCH_LDISINTEL) && (remoteArch & OPAL_ARCH_LDISINTEL)) {
#        ifdef __sparcv9
        long double *to = (long double *) to_p;

        for (size_t i = 0; i < count; i++, to++) {
            union ieee854_long_double ld;
            struct bit80 *b = (struct bit80 *) to;
            ld.ieee.mantissa3 = 0;
            ld.ieee.mantissa2 = 0;
            ld.ieee.mantissa0 = (b->mantissa0 << 1) | (b->mantissa1 & 0x80000000);
            ld.ieee.mantissa1 = (b->mantissa1 << 1) & 0xFFFFFFFE;
            ld.ieee.exponent = b->exponent;
            ld.ieee.negative = b->negative;
            MEMCPY(to, &ld, sizeof(long double));
        }
#        endif
    }
#    else
    assert(0);
#    endif
}
#else
#    define opal_dt_swap_long_double(to_p, from_p, size, count, remoteArch)
#endif

union fp_float64
{
  double value;
  struct {
#if defined(WORDS_BIGENDIAN)
    unsigned sign  :  1;
    unsigned exp   : 11;
    unsigned frac1 : 20;
    unsigned frac0 : 32;
#else
    unsigned frac0 : 32;
    unsigned frac1 : 20;
    unsigned exp   : 11;
    unsigned sign  :  1;
#endif
  } bits;
  char bytes[sizeof(double)];
};

union fp_float80
{
  long double value;
  struct {
#if defined(WORDS_BIGENDIAN)
    unsigned sign  :  1;
    unsigned exp   : 15;
    unsigned pad   : 16;
    unsigned frac1 : 32;
    unsigned frac0 : 32;
#else
    unsigned frac0 : 32;
    unsigned frac1 : 32;
    unsigned exp   : 15;
    unsigned sign  :  1;
    unsigned pad   : 16;
#endif
  } bits;
  char bytes[sizeof(long double)];
};

union fp_float128
{
  /*_Float128 value;*/
  struct {
#if defined(WORDS_BIGENDIAN)
    unsigned sign  :  1;
    unsigned exp   : 15;
    unsigned frac3 : 16;
    unsigned frac2 : 32;
    unsigned frac1 : 32;
    unsigned frac0 : 32;
#else
    unsigned frac0 : 32;
    unsigned frac1 : 32;
    unsigned frac2 : 32;
    unsigned frac3 : 16;
    unsigned exp   : 15;
    unsigned sign  :  1;
#endif
  } bits;
  char bytes[16];
};

// f64_to_f128 (copies a float64(local_endian) to a float128(local_endian))
static inline
void
f64_to_f128(unsigned char *f128_buf_to, const unsigned char *f64_buf_from, ssize_t count, ptrdiff_t from_extent)
{
    unsigned s,e,f[4],f0,f1;
    union fp_float64 ud;
    union fp_float128 uq;
    int f64_is_aligned;

    f64_is_aligned = 1;
    if ((uintptr_t)f64_buf_from & 0x7) {
        f64_is_aligned = 0;
    }
    if ((uintptr_t)from_extent & 0x7) {
        f64_is_aligned = 0;
    }

    do {
        /* input */
        if (f64_is_aligned) {
            ud.value = *(double*)f64_buf_from;
        } else {
            memcpy(&ud.value, f64_buf_from, sizeof(ud));
        }

        /* unpack */
        s = ud.bits.sign;
        e = ud.bits.exp;
        f0 = ud.bits.frac0;
        f1 = ud.bits.frac1;

        /* bias */
        if (e) e += 16383 - 1023;

        /* extend */
        f[3] = (f1 >> 4);
        f[2] = (f1 << 28) | (f0 >> 4);
        f[1] = (f0 << 28);
        f[0] = 0;

        /* pack */
        uq.bits.sign  = s;
        uq.bits.exp   = e;
        uq.bits.frac0 = f[0];
        uq.bits.frac1 = f[1];
        uq.bits.frac2 = f[2];
        uq.bits.frac3 = f[3];

        /* output */
        memcpy(f128_buf_to,uq.bytes,sizeof(uq));

        f64_buf_from += from_extent;
        f128_buf_to += sizeof(uq);
        count--;
   } while (count > 0);
}

// f80_to_f128 (copies an intel80(local_endian) to a float128(local_endian))
static inline
void
f80_to_f128(unsigned char *f128_buf_to, const unsigned char *f80_buf_from, ssize_t count, ptrdiff_t from_extent)
{
    unsigned s,e,f[4],f0,f1;
    union fp_float80 ul;
    union fp_float128 uq;
    int f80_is_aligned;

    f80_is_aligned = 1;
    if ((uintptr_t)f80_buf_from & 0xF) {
        f80_is_aligned = 0;
    }
    if ((uintptr_t)from_extent & 0xF) {
        f80_is_aligned = 0;
    }

    do {
        /* input */
        if (f80_is_aligned) {
            ul.value = *(long double*)f80_buf_from;
        } else {
            memcpy(&ul.value, f80_buf_from, sizeof(ul));
        }

        /* unpack */
        s = ul.bits.sign;
        e = ul.bits.exp;
        f0 = ul.bits.frac0;
        f1 = ul.bits.frac1;

        /* implicit bit */
        f1 &= ~(1 << 31);

        /* extend */
        f[3] = (f1 >> 15);
        f[2] = (f1 << 17) | (f0 >> 15);
        f[1] = (f0 << 17);
        f[0] = 0;

        /* pack */
        uq.bits.sign  = s;
        uq.bits.exp   = e;
        uq.bits.frac0 = f[0];
        uq.bits.frac1 = f[1];
        uq.bits.frac2 = f[2];
        uq.bits.frac3 = f[3];

        /* output */
        memcpy(f128_buf_to,uq.bytes,sizeof(uq));

        f80_buf_from += from_extent;
        f128_buf_to += sizeof(uq);
        count--;
   } while (count > 0);
}

// f128_to_f64 (copies a float128(local_endian) to a float64(local_endian))
static inline
void
f128_to_f64(unsigned char *f64_buf_to, const unsigned char *f128_buf_from, ssize_t count, ptrdiff_t to_extent)
{
    unsigned s,e,f[4],f0,f1;
    union fp_float64 ud;
    union fp_float128 uq;
    int f64_is_aligned;

    f64_is_aligned = 1;
    if ((uintptr_t)f64_buf_to & 0x7) {
        f64_is_aligned = 0;
    }
    if ((uintptr_t)to_extent & 0x7) {
        f64_is_aligned = 0;
    }

    do {
        /* input */
        memcpy(uq.bytes,f128_buf_from,sizeof(uq));

        /* unpack */
        s = uq.bits.sign;
        e = uq.bits.exp;
        f[0] = uq.bits.frac0;
        f[1] = uq.bits.frac1;
        f[2] = uq.bits.frac2;
        f[3] = uq.bits.frac3;

        /* bias */
        if (e) e -= 16383 - 1023;

        /* truncate */
        f1 = (f[3] << 4) | (f[2] >> 28);
        f0 = (f[2] << 4) | (f[1] >> 28);

        /* pack */
        ud.bits.sign  = s;
        ud.bits.exp   = e;
        ud.bits.frac0 = f0;
        ud.bits.frac1 = f1;

        /* output */
        if (f64_is_aligned) {
            *(double*)f64_buf_to = ud.value;
        } else {
            memcpy(f64_buf_to, &ud.value, sizeof(ud));
        }

        f64_buf_to += to_extent;
        f128_buf_from += sizeof(uq);
        count--;
    } while (count > 0);
}

// f128_to_f80 (copies a float128(local_endian) to an intel80(local_endian))
static inline
void
f128_to_f80(unsigned char *f80_buf_to, const unsigned char *f128_buf_from, ssize_t count, ptrdiff_t to_extent)
{
    unsigned s,e,f[4],f0,f1;
    union fp_float80 ul;
    union fp_float128 uq;
    int f80_is_aligned;

    f80_is_aligned = 1;
    if ((uintptr_t)f80_buf_to & 0xF) {
        f80_is_aligned = 0;
    }
    if ((uintptr_t)to_extent & 0xF) {
        f80_is_aligned = 0;
    }

    do {
        /* input */
        memcpy(uq.bytes,f128_buf_from,sizeof(uq));

        /* unpack */
        s = uq.bits.sign;
        e = uq.bits.exp;
        f[0] = uq.bits.frac0;
        f[1] = uq.bits.frac1;
        f[2] = uq.bits.frac2;
        f[3] = uq.bits.frac3;

        /* truncate */
        f1 = (f[3] << 15) | (f[2] >> 17);
        f0 = (f[2] << 15) | (f[1] >> 17);

        /* implicit bit */
        if (e)
            f1 |= (1 << 31);
        else
            f1 &= ~(1 << 31);

        /* pack */
        ul.bits.sign  = s;
        ul.bits.exp   = e;
        ul.bits.frac0 = f0;
        ul.bits.frac1 = f1;

        /* output */
        /* this started as *f80_buf_to = ul.value;
           but I'm reluctant to assume alignment */
        if (f80_is_aligned) {
            *(long double*)f80_buf_to = ul.value;
        } else {
            memcpy(f80_buf_to, &ul.value, sizeof(ul));
        }

        f80_buf_to += to_extent;
        f128_buf_from += sizeof(uq);
        count--;
    } while (count > 0);
}

#define LDBL_IS_F64(arch) \
  (                                                                        \
    (((arch) & OPAL_ARCH_LDMANTDIGISxx) == OPAL_ARCH_LDMANTDIGIS53)        \
    &&                                                                     \
    (((arch) & OPAL_ARCH_LDEXPSIZEISxx) == OPAL_ARCH_LDEXPSIZEIS11)        \
  )
#define LDBL_IS_F80(arch) \
  (                                                                        \
    (((arch) & OPAL_ARCH_LDMANTDIGISxx) == OPAL_ARCH_LDMANTDIGIS64)        \
    &&                                                                     \
    (((arch) & OPAL_ARCH_LDEXPSIZEISxx) == OPAL_ARCH_LDEXPSIZEIS15)        \
  )
#define LDBL_IS_F128(arch) \
  (                                                                        \
    (((arch) & OPAL_ARCH_LDMANTDIGISxx) == OPAL_ARCH_LDMANTDIGIS113)       \
    &&                                                                     \
    (((arch) & OPAL_ARCH_LDEXPSIZEISxx) == OPAL_ARCH_LDEXPSIZEIS15)        \
  )
#define LDBL_INFO_MASK (OPAL_ARCH_LDMANTDIGISxx | OPAL_ARCH_LDEXPSIZEISxx)

// ldbl_to_f128 (copies a long double(from_arch format) to a float128(local_endian))
static inline
void
ldbl_to_f128(unsigned char *f128_buf_to, const unsigned char *ldbl_buf_from, ssize_t count, int from_arch, ptrdiff_t from_extent)
{
#if defined(HAVE__FLOAT128) || defined(HAVE___FLOAT128)
    int ldbl_is_aligned;

    ldbl_is_aligned = 1;
    int alignment_mask = _Alignof(long double) - 1;
    if ((uintptr_t)ldbl_buf_from & alignment_mask) {
        ldbl_is_aligned = 0;
    }
    if ((uintptr_t)from_extent & alignment_mask) {
        ldbl_is_aligned = 0;
    }

    int f128_is_aligned;
    f128_is_aligned = 1;
    if ((uintptr_t)f128_buf_to & 0xF) {
        f128_is_aligned = 0;
    }

#if defined(HAVE__FLOAT128)
    _Float128 f128;
#elif defined(HAVE___FLOAT128)
    __float128 f128;
#endif

    do {
        if (ldbl_is_aligned && f128_is_aligned) {
#if defined(HAVE__FLOAT128)
            *(_Float128*)f128_buf_to = *(long double*)ldbl_buf_from;
#elif defined(HAVE___FLOAT128)
            *(__float128*)f128_buf_to = *(long double*)ldbl_buf_from;
#endif
        } else {
            long double ldbl;
            memcpy(&ldbl, ldbl_buf_from, sizeof(ldbl));
            f128 = ldbl;
            memcpy(f128_buf_to, &f128, sizeof(f128));
        }

        ldbl_buf_from += from_extent;
        f128_buf_to += sizeof(f128);
        count--;
    } while (count > 0);
#else  /* defined(HAVE__FLOAT128) || defined(HAVE___FLOAT128) */
    if (LDBL_IS_F64(from_arch)) {
        f64_to_f128(f128_buf_to, ldbl_buf_from, count, from_extent);
    } else if (LDBL_IS_F80(from_arch)) {
        f80_to_f128(f128_buf_to, ldbl_buf_from, count, from_extent);
    } else {
/*
 *  This could be an error condition, eg we're trying to process a
 *  long double from a format that isn't f128 (or doesn't appear to be)
 *  into f128.  But I think the reason not to error out is confidence
 *  in the detection.  I wouldn't want to produce a false failure.
 */
        do {
            memcpy(f128_buf_to, ldbl_buf_from, from_extent);

            ldbl_buf_from += from_extent;
            f128_buf_to += 16;
            count--;
        } while (count > 0);
    }
#endif  /* defined(HAVE__FLOAT128) || defined(HAVE___FLOAT128) */
}

// f128_to_ldbl (copies a float128(local_endian) to a long double(to_arch format))
static inline
void
f128_to_ldbl(unsigned char *ldbl_buf_to, const unsigned char *f128_buf_from, ssize_t count, int to_arch, ptrdiff_t to_extent)
{
#if defined(HAVE__FLOAT128) || defined(HAVE___FLOAT128) 
    int ldbl_is_aligned;

    ldbl_is_aligned = 1;
    int alignment_mask = _Alignof(long double) - 1;
    if ((uintptr_t)ldbl_buf_to & alignment_mask) {
        ldbl_is_aligned = 0;
    }
    if ((uintptr_t)to_extent & alignment_mask) {
        ldbl_is_aligned = 0;
    }

    int f128_is_aligned;
    f128_is_aligned = 1;
    if ((uintptr_t)f128_buf_from & 0xF) {
        f128_is_aligned = 0;
    }

#if defined(HAVE__FLOAT128)
    _Float128 f128;
#elif defined(HAVE___FLOAT128) 
    __float128 f128;
#endif

    do {
        if (ldbl_is_aligned && f128_is_aligned) {
#if defined(HAVE__FLOAT128)
            *(long double*)ldbl_buf_to = *(_Float128*)f128_buf_from;
#elif defined(HAVE___FLOAT128) 
            *(long double*)ldbl_buf_to = *(__float128*)f128_buf_from;
#endif
        } else {
            long double ldbl;
            memcpy(&f128, f128_buf_from, sizeof(f128));
            ldbl = f128;
            memcpy(ldbl_buf_to, &ldbl, sizeof(ldbl));
        }

        ldbl_buf_to += to_extent;
        f128_buf_from += sizeof(f128);
        count--;
    } while (count > 0);
#else  /* defined(HAVE__FLOAT128) || defined(HAVE___FLOAT128) */
    if (LDBL_IS_F64(to_arch)) {
        f128_to_f64(ldbl_buf_to, f128_buf_from, count, to_extent);
    } else if (LDBL_IS_F80(to_arch)) {
        f128_to_f80(ldbl_buf_to, f128_buf_from, count, to_extent);
    } else {
/*
 *  This could be an error condition, eg we're trying to process an
 *  f128 into a long double of a format that isn't f128 (or doesn't
 *  appear to be).  But I think the reason not to error out is confidence
 *  in the detection.  I wouldn't want to produce a false failure.
 */
        do {
            memcpy(ldbl_buf_to, f128_buf_from, to_extent);

            ldbl_buf_to += to_extent;
            f128_buf_from += 16;
            count--;
        } while (count > 0);
    }
#endif  /* defined(HAVE__FLOAT128) || defined(HAVE___FLOAT128) */
}

/**
 * BEWARE: Do not use the following macro with composed types such as
 * complex. As the swap is done using the entire type sizeof, the
 * wrong endianness translation will be done.  Instead, use the
 * COPY_2SAMETYPE_HETEROGENEOUS.
 */
#define COPY_TYPE_HETEROGENEOUS(TYPENAME, TYPE) COPY_TYPE_HETEROGENEOUS_INTERNAL(TYPENAME, TYPE, 0)

/*
 *  Summarizing the logic of the pFunc copy functions
 *  with regard to long doubles:
 *
 *  For terminology I'll use
 *  f64 : float64 which some architectures use as their long double
 *  f80 : x86 double extended format that uses 80 bytes, commonly used for long double
 *  f128 : ieee quad precision, sometimes available as _Float128 or the non-standard __float128
 *
 *    if !LONG_DOUBLE or both architecture have the same long double format:
 *      byte swap based on local/remote endianness differing
 *    else:
 *      if from_arch is not local endianness: byte swap to local endianness
 *      if from_arch isn't f128 : ldbl_to_f128
 *        if we have _Float128      : convert to _Float128
 *        else if from_arch LDBL is f80 : f80_to_f128
 *        else if from_arch LDBL is f64 : f64_to_f128
 *      if to_arch isn't f128 : f128_to_ldbl
 *        if we have _Float128      : convert from _Float128 to
 *        if to_arch LDBL is f80   : f128_to_f80
 *        if to_arch LDBL is f64   : f128_to_f64
 *      if to_arch is not local endianness : byte swap
 *
 *  And for all the above conversions the logic for handling size difference
 *  between the from/to type is the same:
 *    if (to_extent == from_extent == sizeof(TYPE))
 *      opal_dt_swap_bytes(to, from, sizeof(TYPE), count);
 *    else
 *      loop i=0..count-1
 *        opal_dt_swap_bytes(to, from, sizeof(TYPE), 1);
 *        to += to_extent;
 *        from += from_extent;
 *  so that's handled by a do while as an outer loop.
 */

#define COPY_TYPE_HETEROGENEOUS_INTERNAL(TYPENAME, TYPE, LONG_DOUBLE)                              \
    static int32_t copy_##TYPENAME##_heterogeneous(opal_convertor_t *pConvertor, size_t count,     \
                                                   const char *from, size_t from_len,              \
                                                   ptrdiff_t from_extent, char *to,                \
                                                   size_t to_length, ptrdiff_t to_extent,          \
                                                   ptrdiff_t *advance)                            \
    {                                                                                              \
        size_t countperblock, nblocksleft;                                                         \
        int from_arch, to_arch ;                                        \
        if (pConvertor->flags & CONVERTOR_SEND_CONVERSION) { /* pack */ \
            from_arch = opal_local_arch;                                \
            to_arch = pConvertor->remoteArch;                           \
        } else { /* unpack */                                           \
            from_arch = pConvertor->remoteArch;                         \
            to_arch = opal_local_arch;                                  \
        }                                                               \
        datatype_check(#TYPE, sizeof(TYPE), sizeof(TYPE), &count, from, from_len, from_extent, to, \
                       to_length, to_extent);                                                      \
        if ((to_extent == from_extent) && (to_extent == sizeof(TYPE))) {                           \
            countperblock = count;                                                                 \
            nblocksleft = 1;                                                                       \
        } else {                                                                                   \
            countperblock = 1;                                                                     \
            nblocksleft = count;                                                                   \
        }                                                                                          \
        do {                                                                                       \
            if (!(LONG_DOUBLE) || ((from_arch & LDBL_INFO_MASK) == (to_arch & LDBL_INFO_MASK))) {  \
                if ((from_arch & OPAL_ARCH_ISBIGENDIAN)                                            \
                    != (to_arch & OPAL_ARCH_ISBIGENDIAN))                                          \
                {                                                                                  \
                    opal_dt_swap_bytes(to, from, sizeof(TYPE), countperblock);                     \
                } else {                                                                           \
                    MEMCPY(to, from, countperblock * sizeof(TYPE));                                \
                }                                                                                  \
            } else {                                                                               \
                const char *tmp_from = from;                                                       \
                if ((from_arch & OPAL_ARCH_ISBIGENDIAN)                                            \
                    != (opal_local_arch & OPAL_ARCH_ISBIGENDIAN))                                  \
                {                                                                                  \
                    opal_dt_swap_bytes(to, tmp_from, sizeof(TYPE), countperblock);                 \
                    tmp_from = to;                                                                 \
                }                                                                                  \
                if (!LDBL_IS_F128(from_arch)) {                                                    \
                    ldbl_to_f128((unsigned char*)to, (const unsigned char*)tmp_from,               \
                        countperblock, from_arch, from_extent);                                    \
                    tmp_from = to;                                                                 \
                }                                                                                  \
                if (!LDBL_IS_F128(to_arch)) {                                                      \
                    f128_to_ldbl((unsigned char*)to, (const unsigned char*)tmp_from,               \
                        countperblock, to_arch, to_extent);                                        \
                    tmp_from = to;                                                                 \
                }                                                                                  \
                if ((to_arch & OPAL_ARCH_ISBIGENDIAN)                                              \
                    != (opal_local_arch & OPAL_ARCH_ISBIGENDIAN))                                  \
                {                                                                                  \
                    if (tmp_from == from) {                                                        \
                        opal_dt_swap_bytes(to, from, sizeof(TYPE), countperblock);                 \
                    } else {                                                                       \
                        opal_dt_swap_bytes_inplace(to, sizeof(TYPE), countperblock);               \
                    }                                                                              \
                }                                                                                  \
            }                                                                                      \
                                                                                                   \
            to += to_extent;                                                                       \
            from += from_extent;                                                                   \
            nblocksleft--;                                                                         \
        } while (nblocksleft > 0);                                                                 \
                                                                                                   \
        *advance = count * from_extent;                                                            \
        return count;                                                                              \
    }

#define COPY_2SAMETYPE_HETEROGENEOUS(TYPENAME, TYPE) \
    COPY_2SAMETYPE_HETEROGENEOUS_INTERNAL(TYPENAME, TYPE, 0)

#define COPY_2SAMETYPE_HETEROGENEOUS_INTERNAL(TYPENAME, TYPE, LONG_DOUBLE)                         \
    static int32_t copy_##TYPENAME##_heterogeneous(opal_convertor_t *pConvertor, size_t count,     \
                                                   const char *from, size_t from_len,              \
                                                   ptrdiff_t from_extent, char *to,                \
                                                   size_t to_length, ptrdiff_t to_extent,          \
                                                   ptrdiff_t *advance)                             \
    {                                                                                              \
        size_t countperblock, nblocksleft;                                                         \
        int from_arch, to_arch ;                                        \
        if (pConvertor->flags & CONVERTOR_SEND_CONVERSION) { /* pack */ \
            from_arch = opal_local_arch;                                \
            to_arch = pConvertor->remoteArch;                           \
        } else { /* unpack */                                           \
            from_arch = pConvertor->remoteArch;                         \
            to_arch = opal_local_arch;                                  \
        }                                                               \
        datatype_check(#TYPE, sizeof(TYPE), sizeof(TYPE), &count, from, from_len, from_extent, to, \
                       to_length, to_extent);                                                      \
        if ((to_extent == from_extent) && (to_extent == 2 * sizeof(TYPE))) {                       \
            countperblock = count * 2;                                                             \
            nblocksleft = 1;                                                                       \
        } else {                                                                                   \
            countperblock = 2;                                                                     \
            nblocksleft = count;                                                                   \
        }                                                                                          \
        do {                                                                                       \
                                                                                                   \
            if (!(LONG_DOUBLE) || ((from_arch & LDBL_INFO_MASK) == (to_arch & LDBL_INFO_MASK))) {  \
                if ((from_arch & OPAL_ARCH_ISBIGENDIAN)                                            \
                    != (to_arch & OPAL_ARCH_ISBIGENDIAN))                                          \
                {                                                                                  \
                    opal_dt_swap_bytes(to, from, sizeof(TYPE), countperblock);                     \
                } else {                                                                           \
                    MEMCPY(to, from, countperblock * sizeof(TYPE));                                \
                }                                                                                  \
            } else {                                                                               \
                const char *tmp_from = from;                                                       \
                if ((from_arch & OPAL_ARCH_ISBIGENDIAN)                                            \
                    != (opal_local_arch & OPAL_ARCH_ISBIGENDIAN))                                  \
                {                                                                                  \
                    opal_dt_swap_bytes(to, tmp_from, sizeof(TYPE), countperblock);                 \
                    tmp_from = to;                                                                 \
                }                                                                                  \
                if (!LDBL_IS_F128(from_arch)) {                                                    \
                    ldbl_to_f128((unsigned char*)to, (const unsigned char*)tmp_from,               \
                        countperblock, from_arch, from_extent/2);                                  \
                    tmp_from = to;                                                                 \
                }                                                                                  \
                if (!LDBL_IS_F128(to_arch)) {                                                      \
                    f128_to_ldbl((unsigned char*)to, (const unsigned char*)tmp_from,               \
                        countperblock, to_arch, to_extent/2);                                      \
                    tmp_from = to;                                                                 \
                }                                                                                  \
                if ((to_arch & OPAL_ARCH_ISBIGENDIAN)                                              \
                    != (opal_local_arch & OPAL_ARCH_ISBIGENDIAN))                                  \
                {                                                                                  \
                    if (tmp_from == from) {                                                        \
                        opal_dt_swap_bytes(to, from, sizeof(TYPE), countperblock);                 \
                    } else {                                                                       \
                        opal_dt_swap_bytes_inplace(to, sizeof(TYPE), countperblock);               \
                    }                                                                              \
                }                                                                                  \
            }                                                                                      \
                                                                                                   \
            to += to_extent;                                                                       \
            from += from_extent;                                                                   \
            nblocksleft--;                                                                         \
        } while (nblocksleft > 0);                                                                 \
                                                                                                   \
        *advance = count * from_extent;                                                            \
        return count;                                                                              \
    }

#define COPY_2TYPE_HETEROGENEOUS(TYPENAME, TYPE1, TYPE2)                                        \
    static int32_t copy_##TYPENAME##_heterogeneous(opal_convertor_t *pConvertor, size_t count,  \
                                                   const char *from, size_t from_len,           \
                                                   ptrdiff_t from_extent, char *to,             \
                                                   size_t to_length, ptrdiff_t to_extent,       \
                                                   ptrdiff_t *advance)                          \
    {                                                                                           \
        size_t i;                                                                               \
        int from_arch, to_arch ;                                        \
        if (pConvertor->flags & CONVERTOR_SEND_CONVERSION) { /* pack */ \
            from_arch = opal_local_arch;                                \
            to_arch = pConvertor->remoteArch;                           \
        } else { /* unpack */                                           \
            from_arch = pConvertor->remoteArch;                         \
            to_arch = opal_local_arch;                                  \
        }                                                               \
        datatype_check(#TYPENAME, sizeof(TYPE1) + sizeof(TYPE2), sizeof(TYPE1) + sizeof(TYPE2), \
                       &count, from, from_len, from_extent, to, to_length, to_extent);          \
                                                                                                \
        if ((pConvertor->remoteArch & OPAL_ARCH_ISBIGENDIAN)                                    \
            != (opal_local_arch & OPAL_ARCH_ISBIGENDIAN)) {                                     \
            /* source and destination are different endianness */                               \
            for (i = 0; i < count; i++) {                                                       \
                TYPE1 *to_1, *from_1;                                                           \
                TYPE2 *to_2, *from_2;                                                           \
                to_1 = (TYPE1 *) to;                                                            \
                from_1 = (TYPE1 *) from;                                                        \
                opal_dt_swap_bytes(to_1, from_1, sizeof(TYPE1), 1);                             \
                to_2 = (TYPE2 *) (to_1 + 1);                                                    \
                from_2 = (TYPE2 *) (from_1 + 1);                                                \
                opal_dt_swap_bytes(to_2, from_2, sizeof(TYPE2), 1);                             \
                to += to_extent;                                                                \
                from += from_extent;                                                            \
            }                                                                                   \
        } else if ((ptrdiff_t)(sizeof(TYPE1) + sizeof(TYPE2)) == to_extent                      \
                   && (ptrdiff_t)(sizeof(TYPE1) + sizeof(TYPE2)) == from_extent) {              \
            /* source and destination are contiguous */                                          \
            MEMCPY(to, from, count *(sizeof(TYPE1) + sizeof(TYPE2)));                           \
        } else {                                                                                \
            /* source or destination are non-contiguous */                                       \
            for (i = 0; i < count; i++) {                                                       \
                MEMCPY(to, from, sizeof(TYPE1) + sizeof(TYPE2));                                \
                to += to_extent;                                                                \
                from += from_extent;                                                            \
            }                                                                                   \
        }                                                                                       \
        *advance = count * from_extent;                                                         \
        return count;                                                                           \
    }

static inline void datatype_check(char *type, size_t local_size, size_t remote_size, size_t *count,
                                  const char *from, size_t from_len, ptrdiff_t from_extent,
                                  char *to, size_t to_len, ptrdiff_t to_extent)
{
    /* make sure the remote buffer is large enough to hold the data */
    if ((remote_size * *count) > from_len) {
        *count = from_len / remote_size;
        if ((*count * remote_size) != from_len) {
            DUMP("oops should I keep this data somewhere (excedent %d bytes)?\n",
                 from_len - (*count * remote_size));
        }
        DUMP("correct: copy %s count %d from buffer %p with length %d to %p space %d\n", "char",
             *count, from, from_len, to, to_len);
    } else {
        DUMP("         copy %s count %d from buffer %p with length %d to %p space %d\n", "char",
             *count, from, from_len, to, to_len);
    }
}

#define CXX_BOOL_COPY_LOOP(TYPE)                         \
    for (size_t i = 0; i < count; i++) {                 \
        bool *to_real = (bool *) to;                     \
        *to_real = *((TYPE *) from) == 0 ? false : true; \
        to += to_extent;                                 \
        from += from_extent;                             \
    }
static int32_t copy_cxx_bool_heterogeneous(opal_convertor_t *pConvertor, size_t count,
                                           const char *from, size_t from_len, ptrdiff_t from_extent,
                                           char *to, size_t to_length, ptrdiff_t to_extent,
                                           ptrdiff_t *advance)
{
    /* fix up the from extent */
    if ((pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) != (opal_local_arch & OPAL_ARCH_BOOLISxx)) {
        switch (pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) {
        case OPAL_ARCH_BOOLIS8:
            from_extent = 1;
            break;
        case OPAL_ARCH_BOOLIS16:
            from_extent = 2;
            break;
        case OPAL_ARCH_BOOLIS32:
            from_extent = 4;
            break;
        }
    }

    datatype_check("bool", sizeof(bool), sizeof(bool), &count, from, from_len, from_extent, to,
                   to_length, to_extent);

    if ((to_extent != sizeof(bool) || from_extent != sizeof(bool))
        || ((pConvertor->remoteArch & OPAL_ARCH_BOOLISxx)
            != (opal_local_arch & OPAL_ARCH_BOOLISxx))) {
        switch (pConvertor->remoteArch & OPAL_ARCH_BOOLISxx) {
        case OPAL_ARCH_BOOLIS8:
            CXX_BOOL_COPY_LOOP(int8_t);
            break;
        case OPAL_ARCH_BOOLIS16:
            CXX_BOOL_COPY_LOOP(int16_t);
            break;
        case OPAL_ARCH_BOOLIS32:
            CXX_BOOL_COPY_LOOP(int32_t);
            break;
        }
    } else {
        MEMCPY(to, from, count * sizeof(bool));
    }

    *advance = count * from_extent;
    return count;
}

COPY_TYPE_HETEROGENEOUS(int1, int8_t)
COPY_TYPE_HETEROGENEOUS(int2, int16_t)
COPY_TYPE_HETEROGENEOUS(int4, int32_t)
#ifdef HAVE_INT64_T
COPY_TYPE_HETEROGENEOUS(int8, int64_t)
#else
#    define copy_int8_heterogeneous NULL
#endif

#ifdef HAVE_INT128_T
COPY_TYPE_HETEROGENEOUS(int16, int128_t)
#else
#    define copy_int16_heterogeneous NULL
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 2
COPY_TYPE_HETEROGENEOUS(float2, short float)
#elif SIZEOF_FLOAT == 2
COPY_TYPE_HETEROGENEOUS(float2, float)
#elif SIZEOF_DOUBLE == 2
COPY_TYPE_HETEROGENEOUS(float2, double)
#elif SIZEOF_LONG_DOUBLE == 2
COPY_TYPE_HETEROGENEOUS(float2, long double)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 2
COPY_TYPE_HETEROGENEOUS(float2, opal_short_float_t)
#else
/* #error No basic type for copy function for opal_datatype_float2 found */
#    define copy_float2_heterogeneous NULL
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 4
COPY_TYPE_HETEROGENEOUS(float4, short float)
#elif SIZEOF_FLOAT == 4
COPY_TYPE_HETEROGENEOUS(float4, float)
#elif SIZEOF_DOUBLE == 4
COPY_TYPE_HETEROGENEOUS(float4, double)
#elif SIZEOF_LONG_DOUBLE == 4
COPY_TYPE_HETEROGENEOUS(float4, long double)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 4
COPY_TYPE_HETEROGENEOUS(float4, opal_short_float_t)
#else
/* #error No basic type for copy function for opal_datatype_float4 found */
#    define copy_float4_heterogeneous NULL
#endif

#if defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 8
COPY_TYPE_HETEROGENEOUS(float8, short float)
#elif SIZEOF_FLOAT == 8
COPY_TYPE_HETEROGENEOUS(float8, float)
#elif SIZEOF_DOUBLE == 8
COPY_TYPE_HETEROGENEOUS(float8, double)
#elif SIZEOF_LONG_DOUBLE == 8
COPY_TYPE_HETEROGENEOUS(float8, long double)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 8
COPY_TYPE_HETEROGENEOUS(float8, opal_short_float_t)
#else
/* #error No basic type for copy function for opal_datatype_float8 found */
#    define copy_float8_heterogeneous NULL
#endif

#if SIZEOF_LONG_DOUBLE == OPAL_SIZEOF_FLOAT12
COPY_TYPE_HETEROGENEOUS(float12, long double)
#elif SIZEOF_DOUBLE == OPAL_SIZEOF_FLOAT12
COPY_TYPE_HETEROGENEOUS(float12, double)
#elif SIZEOF_FLOAT == OPAL_SIZEOF_FLOAT12
COPY_TYPE_HETEROGENEOUS(float12, float)
#elif defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == OPAL_SIZEOF_FLOAT12
COPY_TYPE_HETEROGENEOUS(float12, short float)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == OPAL_SIZEOF_FLOAT12
COPY_TYPE_HETEROGENEOUS(float12, opal_short_float_t)
#else
/* #error No basic type for copy function for opal_datatype_float12 found */
#    define copy_float12_heterogeneous NULL
#endif

#if defined(HAVE__FLOAT128) && SIZEOF__FLOAT128 == 16
COPY_TYPE_HETEROGENEOUS(float16, _Float128)
#elif defined(HAVE___FLOAT128) && SIZEOF___FLOAT128 == 16
COPY_TYPE_HETEROGENEOUS(float16, __float128)
#elif defined(HAVE_SHORT_FLOAT) && SIZEOF_SHORT_FLOAT == 16
COPY_TYPE_HETEROGENEOUS(float16, short float)
#elif SIZEOF_FLOAT == 16
COPY_TYPE_HETEROGENEOUS(float16, float)
#elif SIZEOF_DOUBLE == 16
COPY_TYPE_HETEROGENEOUS(float16, double)
#elif SIZEOF_LONG_DOUBLE == 16
COPY_TYPE_HETEROGENEOUS_INTERNAL(float16, long double, 1)
#elif defined(HAVE_OPAL_SHORT_FLOAT_T) && SIZEOF_OPAL_SHORT_FLOAT_T == 16
COPY_TYPE_HETEROGENEOUS(float16, opal_short_float_t)
#else
/* #error No basic type for copy function for opal_datatype_float16 found */
#    define copy_float16_heterogeneous NULL
#endif

#if defined(HAVE_SHORT_FLOAT__COMPLEX)
COPY_2SAMETYPE_HETEROGENEOUS(short_float_complex, short float _Complex)
#elif defined(HAVE_OPAL_SHORT_FLOAT_COMPLEX_T)
COPY_2SAMETYPE_HETEROGENEOUS(short_float_complex, opal_short_float_complex_t)
#else
/* #error No basic type for copy function for opal_datatype_short_float_complex found */
#    define copy_short_float_complex_heterogeneous NULL
#endif

COPY_2SAMETYPE_HETEROGENEOUS(float_complex, float)

COPY_2SAMETYPE_HETEROGENEOUS(double_complex, double)

COPY_2SAMETYPE_HETEROGENEOUS_INTERNAL(long_double_complex, long double, 1)

#if defined(HAVE__FLOAT128) && defined(HAVE__FLOAT128__COMPLEX)
COPY_2SAMETYPE_HETEROGENEOUS_INTERNAL(float128_complex, _Float128, 1)
#elif defined(HAVE___FLOAT128) && defined(HAVE___FLOAT128__COMPLEX)
COPY_2SAMETYPE_HETEROGENEOUS_INTERNAL(float128_complex, __float128, 1)
#else
/* #error No _Float128 _Complex support available */
#    define copy_float128_complex_heterogeneous NULL
#endif

COPY_TYPE_HETEROGENEOUS(wchar, wchar_t)

#if SIZEOF_LONG == 8
static int32_t
copy_long_heterogeneous(opal_convertor_t *pConvertor, size_t count,
                        const char* from, size_t from_len, ptrdiff_t from_extent,
                        char* to, size_t to_length, ptrdiff_t to_extent,
                        ptrdiff_t *advance)
{
    size_t i;

    datatype_check("long", sizeof(long), pConvertor->master->remote_sizes[OPAL_DATATYPE_LONG], &count, from, from_len, from_extent, to,
                   to_length, to_extent);
    if (!((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_LONGIS64)) {  /* same sizeof(long) */
        if ((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_ISBIGENDIAN) {  /* different endianness */
            for (i = 0; i < count; i++) {
                opal_dt_swap_bytes(to, from, sizeof(long), 1);
                to += to_extent;
                from += from_extent;
            }
        } else {
            for (i = 0; i < count; i++) {
                *(long*)to = *(long*)from;
                to += to_extent;
                from += from_extent;
            }
        }
    } else {
        /* the two sides have different lengths for sizeof(long) */
        if( CONVERTOR_SEND & pConvertor->flags ) { /* we're doing a pack */
            assert(CONVERTOR_SEND_CONVERSION & pConvertor->flags);
            if ((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_ISBIGENDIAN) {
                /* different sizeof, we need to convert */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        int64_t val = *(int64_t*)from;
                        int32_t i32 = (int32_t)val;
                        opal_dt_swap_bytes(to, &i32, sizeof(int32_t), 1);
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        int32_t val = *(int32_t*)from;
                        int64_t i64 = (int64_t)val;
                        opal_dt_swap_bytes(to, &i64, sizeof(int64_t), 1);
                        to += to_extent;
                        from += from_extent;
                    }
                }
            } else {  /* both have the same endianness */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        long val = *(long*)from;
                        *(int32_t*)to = (int32_t)val;
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        long val = *(long*)from;
                        *(int64_t*)to = (int64_t)val;
                        to += to_extent;
                        from += from_extent;
                    }
                }
            }
        } else {  /* unpack */
            if ((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_ISBIGENDIAN) {
                /* different endianness */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        int32_t val;
                        opal_dt_swap_bytes(&val, from, sizeof(int32_t), 1);
                        *(long*)to = (long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        int64_t val;
                        opal_dt_swap_bytes(&val, from, sizeof(int64_t), 1);
                        *(long*)to = (long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                }
            } else {  /* both have the same endianness */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        int32_t val = *(int32_t*)from;
                        *(long*)to = (long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        int64_t val = *(int64_t*)from;
                        *(long*)to = (long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                }
            }
        }
    }
    *advance = count * from_extent;
    return count;
}

static int32_t
copy_unsigned_long_heterogeneous(opal_convertor_t *pConvertor, size_t count,
                                 const char* from, size_t from_len, ptrdiff_t from_extent,
                                 char* to, size_t to_length, ptrdiff_t to_extent,
                                 ptrdiff_t *advance)
{
    size_t i;

    datatype_check("unsigned long", sizeof(unsigned long), pConvertor->master->remote_sizes[OPAL_DATATYPE_UNSIGNED_LONG],
                   &count, from, from_len, from_extent, to, to_length, to_extent);
    if (!((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_LONGIS64)) {  /* same sizeof(long) */
        if ((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_ISBIGENDIAN) {  /* different endianness */
            for (i = 0; i < count; i++) {
                opal_dt_swap_bytes(to, from, sizeof(unsigned long), 1);
                to += to_extent;
                from += from_extent;
            }
        } else {
            for (i = 0; i < count; i++) {
                *(unsigned long*)to = *(unsigned long*)from;
                to += to_extent;
                from += from_extent;
            }
        }
    } else {
        /* the two sides have different lengths for sizeof(long) */
        if( CONVERTOR_SEND & pConvertor->flags ) { /* we're doing a pack */
            assert(CONVERTOR_SEND_CONVERSION & pConvertor->flags);
            if ((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_ISBIGENDIAN) {
                /* different sizeof, we need to convert */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        uint64_t val = *(uint64_t*)from;
                        uint32_t i32 = (uint32_t)val;
                        opal_dt_swap_bytes(to, &i32, sizeof(uint32_t), 1);
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        uint32_t val = *(uint32_t*)from;
                        uint64_t i64 = (uint64_t)val;
                        opal_dt_swap_bytes(to, &i64, sizeof(uint64_t), 1);
                        to += to_extent;
                        from += from_extent;
                    }
                }
            } else {  /* both have the same endianness */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        unsigned long val = *(unsigned long*)from;
                        *(uint32_t*)to = (uint32_t)val;
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        unsigned long val = *(unsigned long*)from;
                        *(uint64_t*)to = (uint64_t)val;
                        to += to_extent;
                        from += from_extent;
                    }
                }
            }
        } else {  /* unpack */
            if ((pConvertor->remoteArch ^ opal_local_arch) & OPAL_ARCH_ISBIGENDIAN) {
                /* different endianness */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        uint32_t val;
                        opal_dt_swap_bytes(&val, from, sizeof(uint32_t), 1);
                        *(unsigned long*)to = (unsigned long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        uint64_t val;
                        opal_dt_swap_bytes(&val, from, sizeof(uint64_t), 1);
                        *(unsigned long*)to = (unsigned long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                }
            } else {  /* both have the same endianness */
                if (opal_local_arch & OPAL_ARCH_LONGIS64) {
                    for (i = 0; i < count; i++) { /* from 8 to 4 bytes */
                        uint32_t val = *(uint32_t*)from;
                        *(unsigned long*)to = (unsigned long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                } else {
                    for (i = 0; i < count; i++) { /* from 4 to 8 bytes */
                        uint64_t val = *(uint64_t*)from;
                        *(unsigned long*)to = (unsigned long)val;
                        to += to_extent;
                        from += from_extent;
                    }
                }
            }
        }
    }
    *advance = count * from_extent;
    return count;
}
#endif  /* SIZEOF_LONG == 8 */

/* table of predefined copy functions - one for each MPI type */
conversion_fct_t opal_datatype_heterogeneous_copy_functions[OPAL_DATATYPE_MAX_PREDEFINED] = {
    [OPAL_DATATYPE_LOOP]                = NULL,
    [OPAL_DATATYPE_END_LOOP]            = NULL,
    [OPAL_DATATYPE_LB]                  = NULL,
    [OPAL_DATATYPE_UB]                  = NULL,
    [OPAL_DATATYPE_INT1]                = (conversion_fct_t) copy_int1_heterogeneous,
    [OPAL_DATATYPE_INT2]                = (conversion_fct_t) copy_int2_heterogeneous,
    [OPAL_DATATYPE_INT4]                = (conversion_fct_t) copy_int4_heterogeneous,
    [OPAL_DATATYPE_INT8]                = (conversion_fct_t) copy_int8_heterogeneous,
    [OPAL_DATATYPE_INT16]               = (conversion_fct_t) copy_int16_heterogeneous,
    [OPAL_DATATYPE_UINT1]               = (conversion_fct_t) copy_int1_heterogeneous,
    [OPAL_DATATYPE_UINT2]               = (conversion_fct_t) copy_int2_heterogeneous,
    [OPAL_DATATYPE_UINT4]               = (conversion_fct_t) copy_int4_heterogeneous,
    [OPAL_DATATYPE_UINT8]               = (conversion_fct_t) copy_int8_heterogeneous,
    [OPAL_DATATYPE_UINT16]              = (conversion_fct_t) copy_int16_heterogeneous,
    [OPAL_DATATYPE_FLOAT2]              = (conversion_fct_t) copy_float2_heterogeneous,
    [OPAL_DATATYPE_FLOAT4]              = (conversion_fct_t) copy_float4_heterogeneous,
    [OPAL_DATATYPE_FLOAT8]              = (conversion_fct_t) copy_float8_heterogeneous,
    [OPAL_DATATYPE_FLOAT12]             = (conversion_fct_t) copy_float12_heterogeneous,
    [OPAL_DATATYPE_FLOAT16]             = (conversion_fct_t) copy_float16_heterogeneous,
    [OPAL_DATATYPE_SHORT_FLOAT_COMPLEX] = (conversion_fct_t) copy_short_float_complex_heterogeneous,
    [OPAL_DATATYPE_FLOAT_COMPLEX]       = (conversion_fct_t) copy_float_complex_heterogeneous,
    [OPAL_DATATYPE_DOUBLE_COMPLEX]      = (conversion_fct_t) copy_double_complex_heterogeneous,
    [OPAL_DATATYPE_LONG_DOUBLE_COMPLEX] = (conversion_fct_t) copy_long_double_complex_heterogeneous,
    [OPAL_DATATYPE_BOOL]                = (conversion_fct_t) copy_cxx_bool_heterogeneous,
    [OPAL_DATATYPE_WCHAR]               = (conversion_fct_t) copy_wchar_heterogeneous,
#if SIZEOF_LONG == 4
    [OPAL_DATATYPE_LONG]                = (conversion_fct_t) copy_int4_heterogeneous,
    [OPAL_DATATYPE_UNSIGNED_LONG]       = (conversion_fct_t) copy_int4_heterogeneous,
#else
    [OPAL_DATATYPE_LONG]                = (conversion_fct_t) copy_long_heterogeneous,
    [OPAL_DATATYPE_UNSIGNED_LONG]       = (conversion_fct_t) copy_unsigned_long_heterogeneous,
#endif
    [OPAL_DATATYPE_FLOAT128_COMPLEX]    = (conversion_fct_t) copy_float128_complex_heterogeneous,
    [OPAL_DATATYPE_UNAVAILABLE]         = NULL,
};
