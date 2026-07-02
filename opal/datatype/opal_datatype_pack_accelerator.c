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

#include "opal_config.h"

#include <assert.h>
#include <stddef.h>

#include "opal/datatype/opal_convertor_internal.h"
#include "opal/datatype/opal_datatype_internal.h"

int32_t opal_pack_accelerator_simple(opal_convertor_t *pConvertor, struct iovec *iov,
                                     uint32_t *out_size, size_t *max_data);

typedef size_t (*opal_datatype_accelerator_copy_function_t)(
    opal_convertor_t *pConvertor, size_t count, size_t blocklen, size_t elem_count, char **from,
    size_t from_len, ptrdiff_t from_extent, char **to, size_t to_len, ptrdiff_t to_extent);

static size_t opal_datatype_accelerator_copy_by_size(opal_convertor_t *pConvertor,
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

#define OPAL_DATATYPE_ACCELERATOR_COPY_BYTES(SIZE)                                             \
    static size_t opal_datatype_accelerator_copy_bytes_##SIZE(                                 \
        opal_convertor_t *pConvertor, size_t count, size_t blocklen, size_t elem_count,        \
        char **from, size_t from_len, ptrdiff_t from_extent, char **to, size_t to_len,         \
        ptrdiff_t to_extent)                                                                   \
    {                                                                                          \
        return opal_datatype_accelerator_copy_by_size(pConvertor, (SIZE), count, blocklen,     \
                                                      elem_count, from, from_len, from_extent, \
                                                      to, to_len, to_extent);                  \
    }

OPAL_DATATYPE_ACCELERATOR_COPY_BYTES(1)
OPAL_DATATYPE_ACCELERATOR_COPY_BYTES(2)
OPAL_DATATYPE_ACCELERATOR_COPY_BYTES(4)
OPAL_DATATYPE_ACCELERATOR_COPY_BYTES(8)
OPAL_DATATYPE_ACCELERATOR_COPY_BYTES(16)

static opal_datatype_accelerator_copy_function_t
    opal_datatype_accelerator_copy_functions[17] = {
        [1] = opal_datatype_accelerator_copy_bytes_1,
        [2] = opal_datatype_accelerator_copy_bytes_2,
        [4] = opal_datatype_accelerator_copy_bytes_4,
        [8] = opal_datatype_accelerator_copy_bytes_8,
        [16] = opal_datatype_accelerator_copy_bytes_16,
    };

static inline opal_datatype_accelerator_copy_function_t
opal_datatype_accelerator_copy_function_for_size(size_t elem_size)
{
    if ((elem_size < (sizeof(opal_datatype_accelerator_copy_functions)
                      / sizeof(opal_datatype_accelerator_copy_functions[0])))
        && (NULL != opal_datatype_accelerator_copy_functions[elem_size])) {
        return opal_datatype_accelerator_copy_functions[elem_size];
    }
    return NULL;
}

static inline size_t opal_pack_accelerator_predefined_data(opal_convertor_t *pConvertor,
                                                           const dt_elem_desc_t *pElem,
                                                           size_t *count,
                                                           unsigned char **memory,
                                                           unsigned char **packed,
                                                           size_t *space)
{
    const ddt_elem_desc_t *elem = &pElem->elem;
    const size_t elem_size = opal_datatype_basicDatatypes[elem->common.type]->size;
    const size_t block_bytes = elem_size * elem->blocklen;
    size_t copy_count = *count, copied;
    opal_datatype_accelerator_copy_function_t copy_function;
    char *from, *to;

    assert(0 != elem_size);
    assert(0 != elem->blocklen);
    assert(0 == (copy_count % elem->blocklen));

    /* Accelerator copies never split a datatype block. */
    if (copy_count > (*space / elem_size)) {
        copy_count = (*space / block_bytes) * elem->blocklen;
    }
    if (0 == copy_count) {
        return 0;
    }

    from = (char *) (*memory + elem->disp);
    to = (char *) *packed;
    copy_function = opal_datatype_accelerator_copy_function_for_size(elem_size);
    if (NULL == copy_function) {
        copied = opal_datatype_accelerator_copy_by_size(
            pConvertor, elem_size, copy_count, elem->blocklen, elem->count, &from, *space,
            elem->extent, &to, *space, block_bytes);
    } else {
        copied = copy_function(pConvertor, copy_count, elem->blocklen, elem->count, &from, *space,
                               elem->extent, &to, *space, block_bytes);
    }

    assert(copied == copy_count);
    *count -= copied;
    *memory = (unsigned char *) from - elem->disp;
    *space -= copied * elem_size;
    *packed = (unsigned char *) to;
    return copied * elem_size;
}

int32_t opal_pack_accelerator_simple(opal_convertor_t *pConvertor, struct iovec *iov,
                                     uint32_t *out_size, size_t *max_data)
{
    dt_stack_t *pStack;
    uint32_t pos_desc;
    size_t count_desc;
    size_t total_packed = 0;
    dt_elem_desc_t *description;
    dt_elem_desc_t *pElem;
    const opal_datatype_t *pData = pConvertor->pDesc;
    unsigned char *conv_ptr, *iov_ptr;
    size_t iov_len_local;
    uint32_t iov_count;

    description = pConvertor->use_desc->desc;
    pStack = pConvertor->pStack + pConvertor->stack_pos;
    pos_desc = pStack->index;
    conv_ptr = pConvertor->pBaseBuf + pStack->disp;
    count_desc = pStack->count;
    pStack--;
    pConvertor->stack_pos--;
    pElem = &(description[pos_desc]);

    for (iov_count = 0; iov_count < (*out_size); iov_count++) {
        iov_ptr = (unsigned char *) iov[iov_count].iov_base;
        iov_len_local = iov[iov_count].iov_len;

        while (1) {
            while (pElem->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            process_data:
                if (0 == opal_pack_accelerator_predefined_data(pConvertor, pElem, &count_desc,
                                                               &conv_ptr, &iov_ptr,
                                                               &iov_len_local)) {
                    goto complete_loop;
                }
                if (0 != count_desc) {
                    goto complete_loop;
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                pos_desc++;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
            if (OPAL_DATATYPE_END_LOOP == pElem->elem.common.type) {
            process_end_loop:
                if (0 == --(pStack->count)) {
                    if (0 == pConvertor->stack_pos) {
                        *out_size = iov_count;
                        goto complete_loop;
                    }
                    pConvertor->stack_pos--;
                    pStack--;
                    pos_desc++;
                } else {
                    pos_desc = pStack->index + 1;
                    if (-1 == pStack->index) {
                        pStack->disp += (pData->ub - pData->lb);
                    } else {
                        assert(OPAL_DATATYPE_LOOP == description[pStack->index].loop.common.type);
                        pStack->disp += description[pStack->index].loop.extent;
                    }
                }
                conv_ptr = pConvertor->pBaseBuf + pStack->disp;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
            if (OPAL_DATATYPE_LOOP == pElem->elem.common.type) {
            process_loop:
                PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, OPAL_DATATYPE_LOOP,
                           count_desc, pStack->disp);
                pos_desc++;
                UPDATE_INTERNAL_COUNTERS(description, pos_desc, pElem, count_desc);
            }
        }

    complete_loop:
        iov[iov_count].iov_len -= iov_len_local;
        total_packed += iov[iov_count].iov_len;
    }

    *max_data = total_packed;
    pConvertor->bConverted += total_packed;
    *out_size = iov_count;
    if (pConvertor->bConverted == pConvertor->remote_size) {
        pConvertor->flags |= CONVERTOR_COMPLETED;
        return 1;
    }

    PUSH_STACK(pStack, pConvertor->stack_pos, pos_desc, pElem->elem.common.type, count_desc,
               conv_ptr - pConvertor->pBaseBuf);
    return 0;
}
