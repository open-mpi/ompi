/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_DATATYPE_ACCELERATOR_COPY_H_HAS_BEEN_INCLUDED
#define OPAL_DATATYPE_ACCELERATOR_COPY_H_HAS_BEEN_INCLUDED

#include "opal_config.h"

#include <assert.h>
#include <stddef.h>

#include "opal/datatype/opal_convertor.h"

/**
 * Copy @count elements of @elem_size bytes between an accelerator (device) buffer and a
 * host buffer, honoring the block/gap structure of one datatype element. Every copy goes
 * through pConvertor->cbmemcpy, so the same routine serves both the pack and the unpack
 * direction; the caller selects the direction by choosing which of @from / @to is the
 * strided (extent) side and which is the contiguous (packed) side. @elem_size is only ever
 * multiplied into byte counts, so no per-size specialization is needed. Returns @count.
 */
static inline size_t opal_datatype_accelerator_copy_by_size(opal_convertor_t *pConvertor,
                                                            size_t elem_size, size_t count,
                                                            size_t blocklen, size_t elem_count,
                                                            char **from, size_t from_len,
                                                            ptrdiff_t from_extent, char **to,
                                                            size_t to_len, ptrdiff_t to_extent)
{
    size_t block_count = 0, leftover;
    char *_from = *from, *_to = *to;

    (void) from_len;
    (void) to_len;

    assert(0 != elem_size);
    assert(0 != blocklen);

    if (0 == count) {
        return 0;
    }

    if ((1 == blocklen) || ((1 < elem_count) && (blocklen <= count))) {
        const size_t block_bytes = blocklen * elem_size;

        block_count = count / blocklen;
        if ((from_extent == (ptrdiff_t) block_bytes) && (to_extent == (ptrdiff_t) block_bytes)) {
            pConvertor->cbmemcpy(_to, _from, block_count * block_bytes, pConvertor);
            _to += block_count * block_bytes;
            _from += block_count * block_bytes;
        } else {
            for (size_t i = 0; i < block_count; i++) {
                pConvertor->cbmemcpy(_to, _from, block_bytes, pConvertor);
                _to += to_extent;
                _from += from_extent;
            }
        }
    }

    leftover = count - block_count * blocklen;
    if (0 != leftover) {
        const size_t leftover_bytes = leftover * elem_size;

        pConvertor->cbmemcpy(_to, _from, leftover_bytes, pConvertor);
        _to += leftover_bytes;
        _from += leftover_bytes;
    }

    *from = _from;
    *to = _to;
    return count;
}

#endif /* OPAL_DATATYPE_ACCELERATOR_COPY_H_HAS_BEEN_INCLUDED */
