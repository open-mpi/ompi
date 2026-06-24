/*
 * Copyright (c) 2020-2021 IBM Corporation. All rights reserved.
 * Copyright (c) 2002      University of Chicago
 * Copyright (c) 2001      Argonne National Laboratory
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * This file is based on MPICH code which contained the following
 * notice in their top-level COPYRIGHT file:
 *
 *                    COPYRIGHT
 *
 * The following is a notice of limited availability of the code, and disclaimer
 * which must be included in the prologue of the code and in all source listings
 * of the code.
 *
 * Copyright Notice
 *  + 2002 University of Chicago
 *
 * Permission is hereby granted to use, reproduce, prepare derivative works, and
 * to redistribute to others.  This software was authored by:
 *
 * Mathematics and Computer Science Division
 * Argonne National Laboratory, Argonne IL 60439
 *
 * (and)
 *
 * Department of Computer Science
 * University of Illinois at Urbana-Champaign
 *
 *                   GOVERNMENT LICENSE
 *
 * Portions of this material resulted from work developed under a U.S.
 * Government Contract and are subject to the following license: the Government
 * is granted for itself and others acting on its behalf a paid-up, nonexclusive,
 * irrevocable worldwide license in this computer software to reproduce, prepare
 * derivative works, and perform publicly and display publicly.
 *
 *                   DISCLAIMER
 *
 * This computer code material was prepared, in part, as an account of work
 * sponsored by an agency of the United States Government.  Neither the United
 * States, nor the University of Chicago, nor any of their employees, makes any
 * warranty express or implied, or assumes any legal liability or responsibility
 * for the accuracy, completeness, or usefulness of any information, apparatus,
 * product, or process disclosed, or represents that its use would not infringe
 * privately owned rights.
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_PACK_UNPACK_PREDEFINED_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_PACK_UNPACK_PREDEFINED_H_HAS_BEEN_INCLUDED

#include "opal_config.h"
#include <stdint.h>
#include "opal/datatype/opal_datatype_constructors.h"
/*  Improve predefined pack/unpack performance using mpich methods.
 *
 *   For reference implementation, see:
 *   https://github.com/pmodels/mpich/blob/9ab5fd06af2a648bf24214f0d9cff0ee77ee3e7d/src/mpi/datatype/veccpy.h
 *
 *   The overhead of memcpy() was causing slowdown in the
 *   performance of predefined pack/unpack routines. So implement a
 *   manual copy for blocklengths of <= 8. It may also be useful to
 *   do a manual copy for larger blocklengths, but more data will have
 *   to be gathered to see if an implementation would be
 *   better over the current implementation.
 */

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_ONE(stride, blocklen) \
    {                                                                \
        for (; i; i--) {                                             \
            *_dest = *_src;                                          \
            _src += stride;                                          \
            _dest += blocklen;                                       \
        }                                                            \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_TWO(stride, blocklen) \
    {                                                                \
        for (; i > 1; i -= 2) {                                      \
            _dest[0] = _src[0];                                      \
            _dest[1] = _src[1];                                      \
            _src += stride;                                          \
            _dest += blocklen;                                       \
        }                                                            \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_THREE(stride, blocklen) \
    {                                                                  \
        for (; i > 2; i -= 3) {                                        \
            _dest[0] = _src[0];                                        \
            _dest[1] = _src[1];                                        \
            _dest[2] = _src[2];                                        \
            _src += stride;                                            \
            _dest += blocklen;                                         \
        }                                                              \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FOUR(stride, blocklen) \
    {                                                                 \
        for (; i > 3; i -= 4) {                                       \
            _dest[0] = _src[0];                                       \
            _dest[1] = _src[1];                                       \
            _dest[2] = _src[2];                                       \
            _dest[3] = _src[3];                                       \
            _src += stride;                                           \
            _dest += blocklen;                                        \
        }                                                             \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FIVE(stride, blocklen) \
    {                                                                 \
        for (; i > 4; i -= 5) {                                       \
            _dest[0] = _src[0];                                       \
            _dest[1] = _src[1];                                       \
            _dest[2] = _src[2];                                       \
            _dest[3] = _src[3];                                       \
            _dest[4] = _src[4];                                       \
            _src += stride;                                           \
            _dest += blocklen;                                        \
        }                                                             \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SIX(stride, blocklen) \
    {                                                                \
        for (; i > 5; i -= 6) {                                      \
            _dest[0] = _src[0];                                      \
            _dest[1] = _src[1];                                      \
            _dest[2] = _src[2];                                      \
            _dest[3] = _src[3];                                      \
            _dest[4] = _src[4];                                      \
            _dest[5] = _src[5];                                      \
            _src += stride;                                          \
            _dest += blocklen;                                       \
        }                                                            \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SEVEN(stride, blocklen) \
    {                                                                  \
        for (; i > 6; i -= 7) {                                        \
            _dest[0] = _src[0];                                        \
            _dest[1] = _src[1];                                        \
            _dest[2] = _src[2];                                        \
            _dest[3] = _src[3];                                        \
            _dest[4] = _src[4];                                        \
            _dest[5] = _src[5];                                        \
            _dest[6] = _src[6];                                        \
            _src += stride;                                            \
            _dest += blocklen;                                         \
        }                                                              \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_EIGHT(stride, blocklen) \
    {                                                                  \
        for (; i > 7; i -= 8) {                                        \
            _dest[0] = _src[0];                                        \
            _dest[1] = _src[1];                                        \
            _dest[2] = _src[2];                                        \
            _dest[3] = _src[3];                                        \
            _dest[4] = _src[4];                                        \
            _dest[5] = _src[5];                                        \
            _dest[6] = _src[6];                                        \
            _dest[7] = _src[7];                                        \
            _src += stride;                                            \
            _dest += blocklen;                                         \
        }                                                              \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_RESIDUAL_DATA() \
    {                                                 \
        if (i != 0) {                                 \
            for (; i > 0; i--) {                      \
                *_dest++ = *_src++;                   \
            }                                         \
        }                                             \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT(src_base, dest_base, count, blocklen, type, align)  \
    {                                                                                             \
        register unsigned long i = count;                                                         \
        if (((uintptr_t) src_base)  % (align) ||                                                  \
            ((uintptr_t) dest_base) % (align) ||                                                  \
            (elem->extent % (align) && cando_count > blocklen)) {                                 \
            return OPAL_ERROR;                                                                    \
        }                                                                                         \
        type *_src = (type *) src_base;                                                           \
        type *_dest = (type *) dest_base;                                                         \
        size_t stride = elem->extent / sizeof(type);                                              \
        if (blocklen == 1) {                                                                      \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_ONE(stride, blocklen);                         \
        } else if (blocklen == 2) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_TWO(stride, blocklen);                         \
        } else if (blocklen == 3) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_THREE(stride, blocklen);                       \
        } else if (blocklen == 4) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FOUR(stride, blocklen);                        \
        } else if (blocklen == 5) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FIVE(stride, blocklen);                        \
        } else if (blocklen == 6) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SIX(stride, blocklen);                         \
        } else if (blocklen == 7) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SEVEN(stride, blocklen);                       \
        } else if (blocklen == 8) {                                                               \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_EIGHT(stride, blocklen);                       \
        }                                                                                         \
        OPAL_DATATYPE_PACK_PREDEFINED_RESIDUAL_DATA()                                             \
        src_base = (unsigned char *) _src;                                                        \
        dest_base = (unsigned char *) _dest;                                                      \
    }

#define OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT(src_base, dest_base, count, blocklen,         \
                                                type, align)                                  \
    {                                                                                         \
        if (((uintptr_t) src_base)  % (align)  ||                                             \
            ((uintptr_t) dest_base) % (align) ||                                              \
            (elem->extent % (align) && cando_count > blocklen)) {                             \
            return OPAL_ERROR;                                                                \
        }                                                                                     \
        type *_src = (type *) src_base;                                                       \
        type *_dest = (type *) dest_base;                                                     \
        /* elem's extent but in terms of count rather than bytes */                           \
        size_t stride = elem->extent / sizeof(type);                                          \
        register unsigned long i = count;                                                     \
        /* (reversing the meanings of blocklen and stride and using the "PACK" macro) */      \
        if (blocklen == 1) {                                                                  \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_ONE(blocklen, stride);                     \
        } else if (blocklen == 2) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_TWO(blocklen, stride);                     \
        } else if (blocklen == 3) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_THREE(blocklen, stride);                   \
        } else if (blocklen == 4) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FOUR(blocklen, stride);                    \
        } else if (blocklen == 5) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FIVE(blocklen, stride);                    \
        } else if (blocklen == 6) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SIX(blocklen, stride);                     \
        } else if (blocklen == 7) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SEVEN(blocklen, stride);                   \
        } else if (blocklen == 8) {                                                           \
            OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_EIGHT(blocklen, stride);                   \
        }                                                                                     \
        OPAL_DATATYPE_PACK_PREDEFINED_RESIDUAL_DATA()                                         \
        src_base = (unsigned char *) _src;                                                    \
        dest_base = (unsigned char *) _dest;                                                  \
    }

static inline int opal_datatype_unpack_predefined_element(unsigned char **rtn_src,
                                                          unsigned char **rtn_dest,
                                                          size_t cando_count,
                                                          const ddt_elem_desc_t *elem)
{
    size_t blocklen;
    int id;

    id = elem->common.type;
    blocklen = elem->blocklen;

    unsigned char *src = *rtn_src;
    unsigned char *dest = *rtn_dest;

/*
 *  Here as an example of how we want to call our macro, if the incoming id
 *  were OPAL_DATATYPE_INT4, we want
 *    OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT(src, dest, cando_count, stride, blocklen, int32_t);
 *  which requires us to know that int32_t is what an OPAL_DATATYPE_INT4 is.  That's
 *  fine for INT4 but for others like FLOAT12 it's more involved.
 *
 *  There's already a lot of code in opal_datatype_internal.h to maintain those
 *  mappings, so we recycle it by calling OPAL_DATATYPE_HANDLE_INT4()
 *  etc which are the same macros that decide that INT4 is int32_t and
 *  that some of the base opal types aren't available.
 *
 *  Otherwise we'd have to copy and maintain essentially the same blob of
 *  macros that already exist in opal_datatype_internal.h.
 */
#define OPAL_DATATYPE_MYUNPACK(NAME)                                     \
    do {                                                                 \
        OPAL_DATATYPE_HANDLE_##NAME(OPAL_DATATYPE_MYUNPACK_AVAILABLE,    \
                                    OPAL_DATATYPE_MYUNPACK_NOTAVAIL, 0); \
    } while (0)

#define OPAL_DATATYPE_MYUNPACK_AVAILABLE(TYPE, ALIGN, NAME, unused)                              \
    do {                                                                                         \
        OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT(src, dest, cando_count, blocklen, TYPE, ALIGN);  \
        success = true;                                                                          \
    } while (0)

#define OPAL_DATATYPE_MYUNPACK_NOTAVAIL(NAME, unused) \
    do {                                              \
        success = false;                              \
    } while (0)

    bool success = false;
    switch (id) {
    case OPAL_DATATYPE_INT1:
        // The below macro should expand to
        // OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT(src, dest, cando_count, stride, blocklen,
        // int8_t); by using OPAL_DATATYPE_HANDLE_* where it finds that INT1 means int8_t etc
        OPAL_DATATYPE_MYUNPACK(INT1);
        break;
    case OPAL_DATATYPE_INT2:
        OPAL_DATATYPE_MYUNPACK(INT2);
        break;
    case OPAL_DATATYPE_INT4:
        OPAL_DATATYPE_MYUNPACK(INT4);
        break;
    case OPAL_DATATYPE_INT8:
        OPAL_DATATYPE_MYUNPACK(INT8);
        break;
    case OPAL_DATATYPE_INT16:
        OPAL_DATATYPE_MYUNPACK(INT16);
        break;
    case OPAL_DATATYPE_UINT1:
        OPAL_DATATYPE_MYUNPACK(UINT1);
        break;
    case OPAL_DATATYPE_UINT2:
        OPAL_DATATYPE_MYUNPACK(UINT2);
        break;
    case OPAL_DATATYPE_UINT4:
        OPAL_DATATYPE_MYUNPACK(UINT4);
        break;
    case OPAL_DATATYPE_UINT8:
        OPAL_DATATYPE_MYUNPACK(UINT8);
        break;
    case OPAL_DATATYPE_UINT16:
        OPAL_DATATYPE_MYUNPACK(UINT16);
        break;
    case OPAL_DATATYPE_FLOAT2:
        OPAL_DATATYPE_MYUNPACK(FLOAT2);
        break;
    case OPAL_DATATYPE_FLOAT4:
        OPAL_DATATYPE_MYUNPACK(FLOAT4);
        break;
    case OPAL_DATATYPE_FLOAT8:
        OPAL_DATATYPE_MYUNPACK(FLOAT8);
        break;
    case OPAL_DATATYPE_FLOAT12:
        OPAL_DATATYPE_MYUNPACK(FLOAT12);
        break;
    case OPAL_DATATYPE_FLOAT16:
        OPAL_DATATYPE_MYUNPACK(FLOAT16);
        break;
    case OPAL_DATATYPE_SHORT_FLOAT_COMPLEX:
        OPAL_DATATYPE_MYUNPACK(SHORT_FLOAT_COMPLEX);
        break;
    case OPAL_DATATYPE_FLOAT_COMPLEX:
        OPAL_DATATYPE_MYUNPACK(FLOAT_COMPLEX);
        break;
    case OPAL_DATATYPE_DOUBLE_COMPLEX:
        OPAL_DATATYPE_MYUNPACK(DOUBLE_COMPLEX);
        break;
    case OPAL_DATATYPE_LONG_DOUBLE_COMPLEX:
        OPAL_DATATYPE_MYUNPACK(LONG_DOUBLE_COMPLEX);
        break;
    case OPAL_DATATYPE_FLOAT128_COMPLEX:
        OPAL_DATATYPE_MYUNPACK(FLOAT128_COMPLEX);
        break;
    case OPAL_DATATYPE_BOOL:
        OPAL_DATATYPE_MYUNPACK(BOOL);
        break;
    case OPAL_DATATYPE_WCHAR:
        OPAL_DATATYPE_MYUNPACK(WCHAR);
        break;
    default:
        return OPAL_ERROR;
    }
    if (!success) {
        return OPAL_ERROR;
    }

    *rtn_src = src;
    *rtn_dest = dest;
    return OPAL_SUCCESS;
}

static inline int opal_datatype_pack_predefined_element(unsigned char **rtn_src,
                                                        unsigned char **rtn_dest,
                                                        size_t cando_count,
                                                        const ddt_elem_desc_t *elem)
{
    size_t blocklen;
    int id;

    id = elem->common.type;
    blocklen = elem->blocklen;

    unsigned char *src = *rtn_src;
    unsigned char *dest = *rtn_dest;

#define OPAL_DATATYPE_MYPACK(NAME)                                                                 \
    do {                                                                                           \
        OPAL_DATATYPE_HANDLE_##NAME(OPAL_DATATYPE_MYPACK_AVAILABLE, OPAL_DATATYPE_MYPACK_NOTAVAIL, \
                                    0);                                                            \
    } while (0)

#define OPAL_DATATYPE_MYPACK_AVAILABLE(TYPE, ALIGN, NAME, unused)                             \
    do {                                                                                      \
        OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT(src, dest, cando_count, blocklen, TYPE, ALIGN); \
        success = true;                                                                       \
    } while (0)

#define OPAL_DATATYPE_MYPACK_NOTAVAIL(NAME, unused) \
    do {                                            \
        success = false;                            \
    } while (0)

    bool success = false;
    switch (id) {
    case OPAL_DATATYPE_INT1:
        // The below macro should expand to
        // OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT(src, dest, cando_count, stride, blocklen, int8_t);
        // by using OPAL_DATATYPE_HANDLE_* where it finds that INT1 means int8_t etc
        OPAL_DATATYPE_MYPACK(INT1);
        break;
    case OPAL_DATATYPE_INT2:
        OPAL_DATATYPE_MYPACK(INT2);
        break;
    case OPAL_DATATYPE_INT4:
        OPAL_DATATYPE_MYPACK(INT4);
        break;
    case OPAL_DATATYPE_INT8:
        OPAL_DATATYPE_MYPACK(INT8);
        break;
    case OPAL_DATATYPE_INT16:
        OPAL_DATATYPE_MYPACK(INT16);
        break;
    case OPAL_DATATYPE_UINT1:
        OPAL_DATATYPE_MYPACK(UINT1);
        break;
    case OPAL_DATATYPE_UINT2:
        OPAL_DATATYPE_MYPACK(UINT2);
        break;
    case OPAL_DATATYPE_UINT4:
        OPAL_DATATYPE_MYPACK(UINT4);
        break;
    case OPAL_DATATYPE_UINT8:
        OPAL_DATATYPE_MYPACK(UINT8);
        break;
    case OPAL_DATATYPE_UINT16:
        OPAL_DATATYPE_MYPACK(UINT16);
        break;
    case OPAL_DATATYPE_FLOAT2:
        OPAL_DATATYPE_MYPACK(FLOAT2);
        break;
    case OPAL_DATATYPE_FLOAT4:
        OPAL_DATATYPE_MYPACK(FLOAT4);
        break;
    case OPAL_DATATYPE_FLOAT8:
        OPAL_DATATYPE_MYPACK(FLOAT8);
        break;
    case OPAL_DATATYPE_FLOAT12:
        OPAL_DATATYPE_MYPACK(FLOAT12);
        break;
    case OPAL_DATATYPE_FLOAT16:
        OPAL_DATATYPE_MYPACK(FLOAT16);
        break;
    case OPAL_DATATYPE_SHORT_FLOAT_COMPLEX:
        OPAL_DATATYPE_MYPACK(SHORT_FLOAT_COMPLEX);
        break;
    case OPAL_DATATYPE_FLOAT_COMPLEX:
        OPAL_DATATYPE_MYPACK(FLOAT_COMPLEX);
        break;
    case OPAL_DATATYPE_DOUBLE_COMPLEX:
        OPAL_DATATYPE_MYPACK(DOUBLE_COMPLEX);
        break;
    case OPAL_DATATYPE_LONG_DOUBLE_COMPLEX:
        OPAL_DATATYPE_MYPACK(LONG_DOUBLE_COMPLEX);
        break;
    case OPAL_DATATYPE_FLOAT128_COMPLEX:
        OPAL_DATATYPE_MYPACK(FLOAT128_COMPLEX);
        break;
    case OPAL_DATATYPE_BOOL:
        OPAL_DATATYPE_MYPACK(BOOL);
        break;
    case OPAL_DATATYPE_WCHAR:
        OPAL_DATATYPE_MYPACK(WCHAR);
        break;
    default:
        return OPAL_ERROR;
    }
    if (!success) {
        return OPAL_ERROR;
    }

    *rtn_src = src;
    *rtn_dest = dest;
    return OPAL_SUCCESS;
}
#endif
