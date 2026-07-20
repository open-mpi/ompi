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

/*
 * The unrolled BLOCKLEN_ONE..EIGHT handlers below (and the blocklen 1..8
 * dispatch in the UNCHECKED movers) hand-code the inline copy region.  Keep
 * that in lock-step with the named constant so callers that branch on it (for
 * example an upper-layer consolidation mask) stay correct: if the inline region
 * ever grows, this assertion fires until the handlers follow.
 */
_Static_assert(OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN == 8,
               "inline pack/unpack handlers cover blocklen 1..8; update them and this "
               "assertion together with OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN");

/*
 * The unrolled copy bodies below take the source/destination advance as statements (ADV_SRC,
 * ADV_DEST) rather than a fixed stride, so a single set of bodies serves both the aligned
 * fast path (element-unit advance -> scaled-index addressing) and the rare sub-multiple-extent
 * path (byte-exact advance).  _src, _dest and the loop counter i come from the enclosing mover.
 */
#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_ONE(ADV_SRC, ADV_DEST) \
    {                                                                 \
        for (; i; i--) {                                              \
            *_dest = *_src;                                           \
            ADV_SRC;                                                  \
            ADV_DEST;                                                 \
        }                                                             \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_TWO(ADV_SRC, ADV_DEST) \
    {                                                                 \
        for (; i > 1; i -= 2) {                                       \
            _dest[0] = _src[0];                                       \
            _dest[1] = _src[1];                                       \
            ADV_SRC;                                                  \
            ADV_DEST;                                                 \
        }                                                             \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_THREE(ADV_SRC, ADV_DEST) \
    {                                                                   \
        for (; i > 2; i -= 3) {                                         \
            _dest[0] = _src[0];                                         \
            _dest[1] = _src[1];                                         \
            _dest[2] = _src[2];                                         \
            ADV_SRC;                                                    \
            ADV_DEST;                                                   \
        }                                                               \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FOUR(ADV_SRC, ADV_DEST) \
    {                                                                  \
        for (; i > 3; i -= 4) {                                        \
            _dest[0] = _src[0];                                        \
            _dest[1] = _src[1];                                        \
            _dest[2] = _src[2];                                        \
            _dest[3] = _src[3];                                        \
            ADV_SRC;                                                   \
            ADV_DEST;                                                  \
        }                                                              \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FIVE(ADV_SRC, ADV_DEST) \
    {                                                                  \
        for (; i > 4; i -= 5) {                                        \
            _dest[0] = _src[0];                                        \
            _dest[1] = _src[1];                                        \
            _dest[2] = _src[2];                                        \
            _dest[3] = _src[3];                                        \
            _dest[4] = _src[4];                                        \
            ADV_SRC;                                                   \
            ADV_DEST;                                                  \
        }                                                              \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SIX(ADV_SRC, ADV_DEST) \
    {                                                                 \
        for (; i > 5; i -= 6) {                                       \
            _dest[0] = _src[0];                                       \
            _dest[1] = _src[1];                                       \
            _dest[2] = _src[2];                                       \
            _dest[3] = _src[3];                                       \
            _dest[4] = _src[4];                                       \
            _dest[5] = _src[5];                                       \
            ADV_SRC;                                                  \
            ADV_DEST;                                                 \
        }                                                             \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SEVEN(ADV_SRC, ADV_DEST) \
    {                                                                   \
        for (; i > 6; i -= 7) {                                         \
            _dest[0] = _src[0];                                         \
            _dest[1] = _src[1];                                         \
            _dest[2] = _src[2];                                         \
            _dest[3] = _src[3];                                         \
            _dest[4] = _src[4];                                         \
            _dest[5] = _src[5];                                         \
            _dest[6] = _src[6];                                         \
            ADV_SRC;                                                    \
            ADV_DEST;                                                   \
        }                                                               \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_EIGHT(ADV_SRC, ADV_DEST) \
    {                                                                   \
        for (; i > 7; i -= 8) {                                         \
            _dest[0] = _src[0];                                         \
            _dest[1] = _src[1];                                         \
            _dest[2] = _src[2];                                         \
            _dest[3] = _src[3];                                         \
            _dest[4] = _src[4];                                         \
            _dest[5] = _src[5];                                         \
            _dest[6] = _src[6];                                         \
            _dest[7] = _src[7];                                         \
            ADV_SRC;                                                    \
            ADV_DEST;                                                   \
        }                                                               \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_RESIDUAL_DATA() \
    {                                                 \
        for (; i > 0; i--) {                          \
            *_dest++ = *_src++;                       \
        }                                             \
    }

/*
 * Dispatch the blocklen 1..8 unrolled bodies with a caller-supplied advance. ADV_SRC/ADV_DEST are
 * statements evaluated at the end of every block iteration; the enclosing mover selects element-
 * unit or byte-exact advance for the source and destination as appropriate.
 */
#define OPAL_DATATYPE_PREDEFINED_BLOCKLEN_DISPATCH(ADV_SRC, ADV_DEST)      \
    if (1 == blocklen) {                                                  \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_ONE(ADV_SRC, ADV_DEST);    \
    } else if (2 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_TWO(ADV_SRC, ADV_DEST);    \
    } else if (3 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_THREE(ADV_SRC, ADV_DEST);  \
    } else if (4 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FOUR(ADV_SRC, ADV_DEST);   \
    } else if (5 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_FIVE(ADV_SRC, ADV_DEST);   \
    } else if (6 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SIX(ADV_SRC, ADV_DEST);    \
    } else if (7 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_SEVEN(ADV_SRC, ADV_DEST);  \
    } else if (8 == blocklen) {                                          \
        OPAL_DATATYPE_PACK_PREDEFINED_BLOCKLEN_EIGHT(ADV_SRC, ADV_DEST);  \
    }

#define OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT_UNCHECKED(src_base, dest_base, count, blocklen, type) \
    {                                                                                               \
        register unsigned long i = count;                                                           \
        type *_src = (type *) src_base;                                                             \
        type *_dest = (type *) dest_base;                                                           \
        if (0 == (elem->extent % (ptrdiff_t) sizeof(type))) {                                       \
            /* Common case: the gappy source extent is a whole number of elements, so advance in    \
             * element units.  This keeps the stride in a scaled-index addressing mode instead of   \
             * a serialized byte pointer recompute, which matters a lot on x86.  The packed          \
             * destination is contiguous, hence a stride of blocklen elements. */                   \
            const ptrdiff_t src_stride = elem->extent / (ptrdiff_t) sizeof(type);                   \
            OPAL_DATATYPE_PREDEFINED_BLOCKLEN_DISPATCH(_src += src_stride,                          \
                                                       _dest += (ptrdiff_t) (blocklen));           \
        } else {                                                                                    \
            /* Rare: the extent is not a whole number of elements (only sub-size-aligned complex    \
             * types reach here with a multi-block count).  Advance the source byte-wise so a        \
             * non-multiple extent is never truncated by an element-unit stride; the unrolled        \
             * bodies are still used for the copy. */                                               \
            const ptrdiff_t src_stride = elem->extent;                                               \
            OPAL_DATATYPE_PREDEFINED_BLOCKLEN_DISPATCH(                                             \
                _src = (type *) ((unsigned char *) _src + src_stride),                             \
                _dest += (ptrdiff_t) (blocklen));                                                   \
        }                                                                                           \
        OPAL_DATATYPE_PACK_PREDEFINED_RESIDUAL_DATA()                                               \
        src_base = (unsigned char *) _src;                                                          \
        dest_base = (unsigned char *) _dest;                                                        \
    }

/*
 * Validate the alignment needed for typed accesses before entering the shared mover. Callers that
 * already performed these checks may use OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT_UNCHECKED directly.
 */
#define OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT(src_base, dest_base, count, blocklen, type, align) \
    {                                                                                            \
        if (((uintptr_t) src_base) % (align) || ((uintptr_t) dest_base) % (align)                \
            || (elem->extent % (align) && cando_count > blocklen)) {                             \
            return OPAL_ERROR;                                                                   \
        }                                                                                        \
        OPAL_DATATYPE_PACK_PREDEFINED_ELEMENT_UNCHECKED(src_base, dest_base, count, blocklen,    \
                                                         type);                                  \
    }

#define OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT_UNCHECKED(src_base, dest_base, count, blocklen, type) \
    {                                                                                                 \
        type *_src = (type *) src_base;                                                               \
        type *_dest = (type *) dest_base;                                                             \
        register unsigned long i = count;                                                             \
        if (0 == (elem->extent % (ptrdiff_t) sizeof(type))) {                                         \
            /* Mirror of pack: the packed source is contiguous and the destination strides by a      \
             * whole number of elements, so advance in element units for scaled-index addressing. */ \
            const ptrdiff_t dest_stride = elem->extent / (ptrdiff_t) sizeof(type);                    \
            OPAL_DATATYPE_PREDEFINED_BLOCKLEN_DISPATCH(_src += (ptrdiff_t) (blocklen),               \
                                                       _dest += dest_stride);                        \
        } else {                                                                                      \
            /* Rare: the destination extent is not a whole number of elements (only sub-size-aligned \
             * complex types).  Advance the destination byte-wise so a non-multiple extent is         \
             * honored exactly; the unrolled bodies are still used for the copy. */                  \
            const ptrdiff_t dest_stride = elem->extent;                                               \
            OPAL_DATATYPE_PREDEFINED_BLOCKLEN_DISPATCH(                                              \
                _src += (ptrdiff_t) (blocklen),                                                       \
                _dest = (type *) ((unsigned char *) _dest + dest_stride));                          \
        }                                                                                             \
        OPAL_DATATYPE_PACK_PREDEFINED_RESIDUAL_DATA()                                                 \
        src_base = (unsigned char *) _src;                                                            \
        dest_base = (unsigned char *) _dest;                                                          \
    }

/* Validate typed-access alignment before entering the unchecked unpack mover. */
#define OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT(src_base, dest_base, count, blocklen, type, align)      \
    {                                                                                                   \
        if (((uintptr_t) src_base) % (align) || ((uintptr_t) dest_base) % (align)                       \
            || (elem->extent % (align) && cando_count > blocklen)) {                                    \
            return OPAL_ERROR;                                                                          \
        }                                                                                               \
        OPAL_DATATYPE_UNPACK_PREDEFINED_ELEMENT_UNCHECKED(src_base, dest_base, count, blocklen, type);  \
    }

__opal_attribute_always_inline__ static inline int
opal_datatype_unpack_predefined_element(unsigned char **rtn_src, unsigned char **rtn_dest,
                                        size_t cando_count, const ddt_elem_desc_t *elem)
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

__opal_attribute_always_inline__ static inline int
opal_datatype_pack_predefined_element(unsigned char **rtn_src, unsigned char **rtn_dest,
                                      size_t cando_count, const ddt_elem_desc_t *elem)
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
