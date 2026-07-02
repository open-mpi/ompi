/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2019 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2006 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/util/output.h"

#ifdef HAVE_ALLOCA_H
#    include <alloca.h>
#endif

/*
 * The loop-unrolling limits are a provisional cost model derived from FLOAT4 pack measurements on
 * an Apple M3 Pro. Revisit both limits after collecting equivalent data on other architectures and
 * for unpack and heterogeneous conversion. Keeping them here makes the temporary policy explicit.
 */
enum {
    OPAL_DATATYPE_OPT_MAX_DESC_GROWTH = 10,
    OPAL_DATATYPE_OPT_LOOP_UNROLL_MAX_ITEMS = 8,
    OPAL_DATATYPE_OPT_LOOP_UNROLL_MAX_DATA_BYTES = 128
};

typedef struct {
    ptrdiff_t disp;
    ptrdiff_t length;
} opal_datatype_opt_region_t;

/*
 * Advance over one logical item in a loop body. A nested loop occupies several
 * descriptor entries but is a single item at the enclosing loop level.
 */
static uint32_t opal_datatype_opt_next_item(const dt_elem_desc_t *desc, int32_t pos_desc,
                                            uint32_t item)
{
    if (OPAL_DATATYPE_LOOP == desc[pos_desc + item].elem.common.type) {
        return item + desc[pos_desc + item].loop.items + 1;
    }

    return item + 1;
}

/*
 * Select the measured unrolling factor for a noncontiguous loop containing only DATA descriptors.
 * Each DATA mover must remain below the measured byte limit. The factor grows the body to at most
 * eight entries and retains at least two loop iterations; any remainder is emitted afterward.
 */
static uint32_t opal_datatype_opt_loop_unroll_factor(const dt_elem_desc_t *desc, int32_t pos_desc)
{
    const ddt_loop_desc_t *loop = &desc[pos_desc].loop;
    const ddt_endloop_desc_t *end_loop = &desc[pos_desc + loop->items].end_loop;
    uint32_t body_items, factor, loop_factor;

    if ((4 > loop->loops) || (2 > loop->items)
        || (loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)
        || (OPAL_DATATYPE_END_LOOP != end_loop->common.type)) {
        return 1;
    }

    body_items = loop->items - 1;
    if (OPAL_DATATYPE_OPT_LOOP_UNROLL_MAX_ITEMS < body_items) {
        return 1;
    }
    for (uint32_t item = 0; item < body_items; ++item) {
        const ddt_elem_desc_t *elem = &desc[pos_desc + item + 1].elem;
        size_t type_size, block_bytes;

        if (!(elem->common.flags & OPAL_DATATYPE_FLAG_DATA)) {
            return 1;
        }
        type_size = opal_datatype_basicDatatypes[elem->common.type]->size;
        if ((0 == type_size) || (0 == elem->blocklen)
            || (elem->blocklen > OPAL_DATATYPE_OPT_LOOP_UNROLL_MAX_DATA_BYTES / type_size)) {
            return 1;
        }
        block_bytes = elem->blocklen * type_size;
        if (elem->count > OPAL_DATATYPE_OPT_LOOP_UNROLL_MAX_DATA_BYTES / block_bytes) {
            return 1;
        }
    }

    factor = OPAL_DATATYPE_OPT_LOOP_UNROLL_MAX_ITEMS / body_items;
    loop_factor = loop->loops / 2;
    factor = factor < loop_factor ? factor : loop_factor;
    return 1 < factor ? factor : 1;
}

/* Return true when a loop body contains DATA entries but no nested LOOP descriptor. */
static bool opal_datatype_opt_loop_is_innermost(const dt_elem_desc_t *desc, int32_t pos_desc)
{
    const ddt_loop_desc_t *loop = &desc[pos_desc].loop;

    for (uint32_t item = 1; item < loop->items; ++item) {
        if (OPAL_DATATYPE_LOOP == desc[pos_desc + item].elem.common.type) {
            return false;
        }
    }
    return true;
}

/*
 * Account for the DATA entries introduced by eligible loops before allocating the optimized
 * description. The normal optimizer reserves twice the input length for fusion; unrolling needs
 * this additional exact growth because one input DATA entry can become several output entries.
 */
static size_t opal_datatype_opt_loop_unroll_growth(const dt_type_desc_t *input_desc,
                                                   uint32_t optimization_mask)
{
    size_t growth = 0;

    if (!(optimization_mask & OPAL_DATATYPE_OPTIMIZE_LOOP_UNROLL)) {
        return 0;
    }

    for (size_t pos = 0; pos < input_desc->used; ++pos) {
        if (OPAL_DATATYPE_LOOP == input_desc->desc[pos].elem.common.type) {
            const ddt_loop_desc_t *loop = &input_desc->desc[pos].loop;
            uint32_t factor = opal_datatype_opt_loop_unroll_factor(input_desc->desc, (int32_t) pos);

            if (1 < factor) {
                size_t body_items = loop->items - 1;
                size_t replicas = factor + loop->loops % factor - 1;
                size_t extra;

                if (body_items > (SIZE_MAX - growth) / replicas) {
                    return SIZE_MAX;
                }
                extra = body_items * replicas;
                growth += extra;
            }
        }
    }
    return growth;
}

/*
 * Replace one cheap, flat DATA loop with a loop over unrolled groups followed by straight-line
 * residual groups. Displacements remain relative to the enclosing loop or datatype, preserving
 * typemap order without flattening the surrounding description.
 */
static void opal_datatype_opt_emit_unrolled_loop(dt_elem_desc_t **pElemDesc, int32_t *nbElems,
                                                 const dt_elem_desc_t *desc, int32_t pos_desc,
                                                 uint32_t factor)
{
    const ddt_loop_desc_t *loop = &desc[pos_desc].loop;
    const ddt_endloop_desc_t *end_loop = &desc[pos_desc + loop->items].end_loop;
    const uint32_t body_items = loop->items - 1;
    const uint32_t iterations = loop->loops / factor;
    const uint32_t tail = loop->loops % factor;
    const uint32_t unrolled_items = body_items * factor;

    CREATE_LOOP_START(*pElemDesc, iterations, unrolled_items + 1, loop->extent * factor,
                      loop->common.flags);
    (*pElemDesc)++;
    (*nbElems)++;
    for (uint32_t iteration = 0; iteration < factor; ++iteration) {
        for (uint32_t item = 0; item < body_items; ++item) {
            const ddt_elem_desc_t *elem = &desc[pos_desc + item + 1].elem;
            uint16_t elem_flags = OPAL_DATATYPE_FLAG_BASIC
                                  | (elem->common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED);

            CREATE_ELEM(*pElemDesc, elem->common.type, elem_flags, elem->blocklen, elem->count,
                        elem->disp + (ptrdiff_t) iteration * loop->extent, elem->extent);
            (*pElemDesc)++;
            (*nbElems)++;
        }
    }
    CREATE_LOOP_END(*pElemDesc, unrolled_items + 1, end_loop->first_elem_disp,
                    end_loop->size * factor, end_loop->common.flags);
    (*pElemDesc)++;
    (*nbElems)++;

    for (uint32_t iteration = 0; iteration < tail; ++iteration) {
        ptrdiff_t displacement = (ptrdiff_t) (iterations * factor + iteration) * loop->extent;

        for (uint32_t item = 0; item < body_items; ++item) {
            const ddt_elem_desc_t *elem = &desc[pos_desc + item + 1].elem;
            uint16_t elem_flags = OPAL_DATATYPE_FLAG_BASIC
                                  | (elem->common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED);

            CREATE_ELEM(*pElemDesc, elem->common.type, elem_flags, elem->blocklen, elem->count,
                        elem->disp + displacement, elem->extent);
            (*pElemDesc)++;
            (*nbElems)++;
        }
    }
}

/*
 * Identify the first and last copy fragments produced by one full datatype
 * instance.  The count-boundary optimization only needs these two fragments:
 * if the last fragment ends exactly where the first fragment of the next full
 * datatype starts, convertor setup can profitably consolidate count > 1.
 */
__opal_attribute_always_inline__ static inline bool
opal_datatype_opt_find_copy_boundaries(const dt_type_desc_t *type_desc,
                                       opal_datatype_opt_region_t *first,
                                       opal_datatype_opt_region_t *last)
{
    const dt_elem_desc_t *desc = type_desc->desc;
    dt_stack_t *stack;
    size_t stack_length = type_desc->used + 1;
    int32_t stack_pos = -1;
    int32_t pos = 0;

    first->length = 0;
    last->length = 0;
    if ((NULL == desc) || (0 == type_desc->used)) {
        return false;
    }

    stack = (dt_stack_t *) alloca(stack_length * sizeof(*stack));
    while (pos < (int32_t) type_desc->used) {
        const dt_elem_desc_t *entry = &desc[pos];
        ptrdiff_t base_disp = (0 <= stack_pos) ? stack[stack_pos].disp : 0;
        ptrdiff_t first_disp;
        int32_t next_pos;

        if (entry->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            first_disp = base_disp + entry->elem.disp;
            last->length = (ptrdiff_t) entry->elem.blocklen
                           * (ptrdiff_t) opal_datatype_basicDatatypes[entry->elem.common.type]->size;
            last->disp = first_disp + (ptrdiff_t) (entry->elem.count - 1) * entry->elem.extent;
            next_pos = pos + 1;
            goto record_boundary_copy;
        }
        if ((OPAL_DATATYPE_LOOP == entry->elem.common.type)
            && (entry->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)) {
            const ddt_endloop_desc_t *end_loop = &desc[pos + entry->loop.items].end_loop;

            first_disp = base_disp + end_loop->first_elem_disp;
            last->disp = first_disp + (ptrdiff_t) (entry->loop.loops - 1) * entry->loop.extent;
            last->length = end_loop->size;
            next_pos = pos + entry->loop.items + 1;
            if (entry->loop.common.flags & OPAL_DATATYPE_FLAG_NO_GAPS) {
                last->disp = first_disp;
                last->length *= entry->loop.loops;
            }
            goto record_boundary_copy;
        }
        if (OPAL_DATATYPE_LOOP == entry->elem.common.type) {
            assert((size_t) (stack_pos + 1) < stack_length);
            ++stack_pos;
            stack[stack_pos].index = pos;
            stack[stack_pos].count = entry->loop.loops;
            stack[stack_pos].disp = base_disp;
            ++pos;
            continue;
        }
        assert(OPAL_DATATYPE_END_LOOP == entry->elem.common.type);
        {
            const ddt_loop_desc_t *loop;

            assert(0 <= stack_pos);
            loop = &desc[stack[stack_pos].index].loop;
            if (0 == --stack[stack_pos].count) {
                --stack_pos;
                ++pos;
            } else {
                uint32_t completed = loop->loops - stack[stack_pos].count;

                /*
                 * The middle iterations cannot change the first or final
                 * fragment.  Visit the second iteration only when it is also
                 * the final one; otherwise jump directly to the last.
                 */
                if ((1 == completed) && (2 < loop->loops)) {
                    ptrdiff_t parent_disp = (0 < stack_pos) ? stack[stack_pos - 1].disp : 0;

                    stack[stack_pos].count = 1;
                    stack[stack_pos].disp = parent_disp + (ptrdiff_t) (loop->loops - 1) * loop->extent;
                } else {
                    stack[stack_pos].disp += loop->extent;
                }
                pos = stack[stack_pos].index + 1;
            }
            continue;
        }

    record_boundary_copy:
        if (0 == first->length) {
            first->disp = first_disp;
            first->length = last->length;
        }
        pos = next_pos;
    }

    return 0 != first->length;
}

/*
 * Cache whether adjacent datatype instances expose a copy fragment that spans
 * the count boundary.  This is descriptor shape metadata, so recompute it from
 * the optimized descriptor selected by the optimizer and clear any stale value.
 */
static void opal_datatype_opt_update_count_boundary(opal_datatype_t *pData,
                                                    const dt_type_desc_t *type_desc)
{
    opal_datatype_opt_region_t first, last;

    pData->flags &= ~OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE;
    if ((pData->flags & OPAL_DATATYPE_FLAG_NO_GAPS) || (0 == pData->size)) {
        return;
    }
    if (!opal_datatype_opt_find_copy_boundaries(type_desc, &first, &last)) {
        return;
    }

    if (last.disp + last.length == first.disp + (pData->ub - pData->lb)) {
        pData->flags |= OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE;
    }
}

/*
 * Estimate the number of copy ranges generated by the descriptor.
 */
static size_t opal_datatype_opt_count_range_groups_desc(const dt_elem_desc_t *desc,
                                                        int32_t start, int32_t end)
{
    size_t ranges = 0;

    for (int32_t pos = start; pos < end;) {
        const dt_elem_desc_t *entry = &desc[pos];

        if (entry->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            ranges += entry->elem.count;
            ++pos;
        } else if (OPAL_DATATYPE_LOOP == entry->elem.common.type) {
            const ddt_loop_desc_t *loop = &entry->loop;
            size_t loop_ranges;

            if (loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                loop_ranges = 1;
            } else {
                loop_ranges = opal_datatype_opt_count_range_groups_desc(desc, pos + 1,
                                                                        pos + loop->items);
            }
            ranges += loop_ranges * loop->loops;
            pos += loop->items + 1;
        } else {
            ++pos;
        }
    }

    return ranges;
}

static size_t opal_datatype_opt_count_range_groups(const dt_type_desc_t *type_desc)
{
    if ((NULL == type_desc->desc) || (0 == type_desc->used)) {
        return 0;
    }

    return opal_datatype_opt_count_range_groups_desc(type_desc->desc, 0, type_desc->used);
}

static void opal_datatype_opt_free_desc(dt_type_desc_t *type_desc)
{
    free(type_desc->desc);
    type_desc->desc = NULL;
    type_desc->length = 0;
    type_desc->used = 0;
}

static void opal_datatype_opt_set_fake_end_loop(dt_type_desc_t *type_desc,
                                                ptrdiff_t first_elem_disp,
                                                size_t size)
{
    ddt_endloop_desc_t *end_loop = &type_desc->desc[type_desc->used].end_loop;

    end_loop->common.type = OPAL_DATATYPE_END_LOOP;
    end_loop->common.flags = 0;
    end_loop->items = type_desc->used;
    end_loop->first_elem_disp = first_elem_disp;
    end_loop->size = size;
}

static ptrdiff_t opal_datatype_commit_description(opal_datatype_t *pData)
{
    ptrdiff_t first_elem_disp = 0;

    pData->flags |= OPAL_DATATYPE_FLAG_COMMITTED;

    /* We have to compute the displacement of the first non loop item in the
     * description.
     */
    if (0 != pData->size) {
        int index;
        dt_elem_desc_t *pElem = pData->desc.desc;

        index = GET_FIRST_NON_LOOP(pElem);
        assert(pElem[index].elem.common.flags & OPAL_DATATYPE_FLAG_DATA);
        first_elem_disp = pElem[index].elem.disp;
    }

    /* let's add a fake element at the end just to avoid useless comparaisons
     * in pack/unpack functions.
     */
    opal_datatype_opt_set_fake_end_loop(&pData->desc, first_elem_disp, pData->size);

    return first_elem_disp;
}

static uint32_t opal_datatype_opt_count_loop_markers(const dt_type_desc_t *type_desc)
{
    uint32_t loops = 0;

    for (size_t i = 0; i < type_desc->used; ++i) {
        if ((OPAL_DATATYPE_LOOP == type_desc->desc[i].elem.common.type)
            || (OPAL_DATATYPE_END_LOOP == type_desc->desc[i].elem.common.type)) {
            ++loops;
        }
    }

    return loops;
}

/*
 * Emit one data descriptor into the optimized description, shifted by
 * disp_delta. The CREATE_ELEM macro preserves the existing element-collapse
 * behavior used elsewhere in this optimizer.
 */
static void opal_datatype_opt_emit_elem(dt_elem_desc_t **pElemDesc, int32_t *nbElems,
                                        const ddt_elem_desc_t *elem, ptrdiff_t disp_delta)
{
    CREATE_ELEM(*pElemDesc, elem->common.type, elem->common.flags, elem->blocklen, elem->count,
                elem->disp + disp_delta, elem->extent);
    (*pElemDesc)++;
    (*nbElems)++;
}

/*
 * Copy a raw descriptor range from a loop body into the optimized description.
 * Data displacements, and nested loop end first-element displacements, are
 * shifted when the range is moved to a later logical iteration.
 */
static void opal_datatype_opt_emit_desc_range(dt_elem_desc_t **pElemDesc, int32_t *nbElems,
                                              const dt_elem_desc_t *desc, int32_t pos_desc,
                                              uint32_t start, uint32_t end,
                                              ptrdiff_t disp_delta)
{
    for (uint32_t i = start; i < end; ++i) {
        **pElemDesc = desc[pos_desc + i];
        if ((*pElemDesc)->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            (*pElemDesc)->elem.common.flags = OPAL_DATATYPE_FLAG_BASIC;
            (*pElemDesc)->elem.disp += disp_delta;
        } else if (OPAL_DATATYPE_END_LOOP == (*pElemDesc)->elem.common.type) {
            (*pElemDesc)->end_loop.first_elem_disp += disp_delta;
        }
        (*pElemDesc)++;
        (*nbElems)++;
    }
}

/*
 * Normalize an element whose repeated blocks are actually contiguous into one
 * block. Boundary fusion only handles single contiguous fragments.
 */
static void opal_datatype_opt_collapse_elem(ddt_elem_desc_t *elem)
{
    ptrdiff_t elem_size = (ptrdiff_t) elem->blocklen
                          * (ptrdiff_t) opal_datatype_basicDatatypes[elem->common.type]->size;

    if ((1 < elem->count) && (elem->extent == elem_size)) {
        elem->blocklen *= elem->count;
        elem->extent *= elem->count;
        elem->count = 1;
    }
}

static bool opal_datatype_opt_is_aligned(ptrdiff_t value, size_t alignment)
{
    return 0 == ((uintptr_t) value & (alignment - 1));
}

static uint16_t opal_datatype_opt_promoted_uint_type(ptrdiff_t disp, ptrdiff_t extent, uint32_t count,
                                                     ptrdiff_t bytes)
{
    static const uint16_t candidates[] = {OPAL_DATATYPE_UINT8, OPAL_DATATYPE_UINT4, OPAL_DATATYPE_UINT2};

    for (size_t i = 0; i < sizeof(candidates) / sizeof(candidates[0]); ++i) {
        uint16_t type = candidates[i];
        size_t type_size = opal_datatype_basicDatatypes[type]->size;

        if (opal_datatype_basicDatatypes[type]->flags & OPAL_DATATYPE_FLAG_UNAVAILABLE) {
            continue;
        }
        if (0 != ((size_t) bytes % type_size)) {
            continue;
        }
        if (!opal_datatype_opt_is_aligned(disp, type_size)) {
            continue;
        }
        if ((1 < count) && !opal_datatype_opt_is_aligned(extent, type_size)) {
            continue;
        }
        return type;
    }

    return OPAL_DATATYPE_UINT1;
}

static uint64_t opal_datatype_opt_type_mask(uint16_t type)
{
    if (64 <= type) {
        return 0;
    }

    return UINT64_C(1) << type;
}

static uint64_t opal_datatype_opt_type_pair_mask(uint16_t type1, uint16_t type2)
{
    return opal_datatype_opt_type_mask(type1) | opal_datatype_opt_type_mask(type2);
}

static uint16_t opal_datatype_opt_promoted_type(ptrdiff_t disp, ptrdiff_t extent, uint32_t count,
                                                ptrdiff_t bytes,
                                                uint64_t type_mask)
{
    uint16_t selected_type = OPAL_DATATYPE_UNAVAILABLE;
    size_t selected_size = 0;

    if (!opal_datatype_optimize_preserve_type) {
        return OPAL_DATATYPE_UINT1;
    }

    /*
     * Prefer one of the original participating types, using the widest type that fits the merged byte range and
     * alignment.
     */
    for (uint16_t type = OPAL_DATATYPE_FIRST_TYPE; type < OPAL_DATATYPE_UNAVAILABLE; ++type) {
        size_t type_size;

        if (0 == (type_mask & opal_datatype_opt_type_mask(type))) {
            continue;
        }
        if (opal_datatype_basicDatatypes[type]->flags & OPAL_DATATYPE_FLAG_UNAVAILABLE) {
            continue;
        }

        type_size = opal_datatype_basicDatatypes[type]->size;
        if (0 != ((size_t) bytes % type_size)) {
            continue;
        }
        if (!opal_datatype_opt_is_aligned(disp, type_size)) {
            continue;
        }
        if ((1 < count) && !opal_datatype_opt_is_aligned(extent, type_size)) {
            continue;
        }
        if (type_size > selected_size) {
            selected_type = type;
            selected_size = type_size;
        }
    }

    if (OPAL_DATATYPE_UNAVAILABLE != selected_type) {
        return selected_type;
    }

    return opal_datatype_opt_promoted_uint_type(disp, extent, count, bytes);
}

/*
 * Mixed-type contiguous regions cannot keep the original typemap in the optimized descriptor. Keep them
 * homogeneous-only, but preserve as much copy width as the byte layout allows by reusing one of the
 * original types when possible. Neutral unsigned integer types are only a fallback.
 */
static void opal_datatype_opt_set_mixed_region(ddt_elem_desc_t *elem, ptrdiff_t bytes, uint32_t count,
                                               ptrdiff_t disp, ptrdiff_t extent,
                                               uint64_t type_mask)
{
    uint16_t type = opal_datatype_opt_promoted_type(disp, extent, count, bytes, type_mask);
    size_t type_size = opal_datatype_basicDatatypes[type]->size;

    elem->common.type = type;
    elem->common.flags = OPAL_DATATYPE_FLAG_BASIC | OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED;
    elem->blocklen = bytes / type_size;
    elem->count = count;
    elem->disp = disp;
    elem->extent = extent;
}

static bool opal_datatype_opt_item_as_elem(const dt_elem_desc_t *desc, int32_t pos_desc,
                                           uint32_t item, ddt_elem_desc_t *elem);

/*
 * Summarize a contiguous nested loop as one data-like element so that an enclosing loop can test adjacency
 * across its iteration boundary. If every item in the loop body resolves to the same predefined type, keep
 * that type in the summary. Mixed or opaque regions are promoted to the widest participating type allowed by
 * the block size and alignment, and are marked restricted for homogeneous-only use.
 */
static bool opal_datatype_opt_compress_contiguous_loop(const dt_elem_desc_t *desc, int32_t pos_desc,
                                                       ddt_elem_desc_t *elem)
{
    const ddt_loop_desc_t *loop = &desc[pos_desc].loop;
    const ddt_endloop_desc_t *end_loop = &desc[pos_desc + loop->items].end_loop;
    uint16_t common_type = OPAL_DATATYPE_UNAVAILABLE;
    uint16_t common_flags = OPAL_DATATYPE_FLAG_BASIC;
    uint64_t type_mask = 0;
    size_t common_blocklen = 0;
    bool homogeneous = true;
    bool have_item = false;

    if (!(loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)) {
        return false;
    }

    for (uint32_t i = 1; i < loop->items; i = opal_datatype_opt_next_item(desc, pos_desc, i)) {
        ddt_elem_desc_t current;

        have_item = true;
        if (!opal_datatype_opt_item_as_elem(desc, pos_desc, i, &current)) {
            homogeneous = false;
            break;
        }
        type_mask |= opal_datatype_opt_type_mask(current.common.type);

        if (OPAL_DATATYPE_UNAVAILABLE == common_type) {
            common_type = current.common.type;
            common_blocklen = current.blocklen;
            common_flags |= current.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED;
            continue;
        }

        if (common_type != current.common.type) {
            homogeneous = false;
            break;
        }
        common_blocklen += current.blocklen;
        common_flags |= current.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED;
    }

    if (!have_item) {
        return false;
    }

    if (homogeneous) {
        size_t type_size = opal_datatype_basicDatatypes[common_type]->size;

        if ((0 == type_size) || (0 != end_loop->size % type_size)
            || (end_loop->size != common_blocklen * type_size)) {
            homogeneous = false;
        } else {
            elem->common.type = common_type;
            elem->common.flags = common_flags;
            elem->blocklen = end_loop->size / type_size;
        }
    }

    if (!homogeneous) {
        opal_datatype_opt_set_mixed_region(elem, end_loop->size, loop->loops, end_loop->first_elem_disp,
                                           loop->extent, type_mask);
    }

    if (homogeneous) {
        elem->count = loop->loops;
        elem->extent = loop->extent;
        elem->disp = end_loop->first_elem_disp;
    }
    opal_datatype_opt_collapse_elem(elem);
    return true;
}

/*
 * Convert one enclosing-loop body item to a single contiguous copy fragment.
 * This is used only for the first and last body items, because those are the
 * fragments that can merge across adjacent loop iterations.
 */
static bool opal_datatype_opt_item_as_elem(const dt_elem_desc_t *desc, int32_t pos_desc,
                                           uint32_t item,
                                           ddt_elem_desc_t *elem)
{
    if (desc[pos_desc + item].elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
        *elem = desc[pos_desc + item].elem;
        elem->common.flags = OPAL_DATATYPE_FLAG_BASIC;
        opal_datatype_opt_collapse_elem(elem);
        return (1 == elem->count);
    }

    if (OPAL_DATATYPE_LOOP == desc[pos_desc + item].elem.common.type) {
        return opal_datatype_opt_compress_contiguous_loop(desc, pos_desc + item, elem)
               && (1 == elem->count);
    }

    return false;
}

/*
 * Fuse the last fragment of one loop iteration with the first fragment of the
 * next iteration when they are byte-adjacent. Different basic types use the
 * widest participating copy type compatible with the merged layout.
 */
static bool opal_datatype_opt_fuse_tail_head(opal_datatype_t *pData,
                                             const ddt_elem_desc_t *tail,
                                             const ddt_elem_desc_t *head,
                                             ptrdiff_t head_disp_delta,
                                             uint32_t repeat_count,
                                             ptrdiff_t repeat_extent,
                                             ddt_elem_desc_t *fused)
{
    ptrdiff_t tail_size, head_size;

    if ((1 != tail->count) || (1 != head->count)) {
        return false;
    }

    tail_size = (ptrdiff_t) tail->blocklen
                * (ptrdiff_t) opal_datatype_basicDatatypes[tail->common.type]->size;
    head_size = (ptrdiff_t) head->blocklen
                * (ptrdiff_t) opal_datatype_basicDatatypes[head->common.type]->size;
    if (tail->disp + tail_size != head->disp + head_disp_delta) {
        return false;
    }

    *fused = *tail;
    fused->count = 1;
    fused->extent = tail_size + head_size;
    if (tail->common.type == head->common.type) {
        fused->common.flags = OPAL_DATATYPE_FLAG_BASIC
                              | ((tail->common.flags | head->common.flags)
                                 & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED);
        fused->blocklen += head->blocklen;
    } else {
        uint64_t type_mask = opal_datatype_opt_type_pair_mask(tail->common.type, head->common.type);

        opal_datatype_opt_set_mixed_region(fused, tail_size + head_size, repeat_count, tail->disp,
                                           repeat_extent, type_mask);
    }
    if (fused->common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED) {
        pData->flags |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
    }

    return true;
}

/*
 * Try to reduce copy fragments for non-contiguous loops where the end of one
 * iteration touches the beginning of the next. The generated representation is:
 *
 *   first iteration without the final item
 *   (final item + next first item, then the remaining next-iteration items) x N-1
 *   final item from the last iteration
 *
 * The transformation preserves typemap order and falls back unless the first
 * and final loop-body items are each representable as a single copy fragment.
 */
static bool opal_datatype_optimize_loop_boundary(opal_datatype_t *pData,
                                                 const dt_elem_desc_t *desc,
                                                 int32_t pos_desc,
                                                 dt_elem_desc_t **pElemDesc, int32_t *nbElems)
{
    const ddt_loop_desc_t *loop = &desc[pos_desc].loop;
    const ddt_endloop_desc_t *end_loop = &desc[pos_desc + loop->items].end_loop;
    const ddt_elem_desc_t *first, *last;
    ddt_elem_desc_t first_elem, last_elem, fused;
    uint32_t first_item = 1, after_first_item, last_item = 0, item_count = 0;
    uint32_t steady_items;

    if ((loop->loops < 2) || (loop->items <= 2)) {
        return false;
    }

    for (uint32_t i = first_item; i < loop->items;
         i = opal_datatype_opt_next_item(desc, pos_desc, i)) {
        if ((OPAL_DATATYPE_LOOP != desc[pos_desc + i].elem.common.type)
            && !(desc[pos_desc + i].elem.common.flags & OPAL_DATATYPE_FLAG_DATA)) {
            return false;
        }
        last_item = i;
        ++item_count;
    }

    if ((item_count < 2) || (0 == last_item)) {
        return false;
    }

    after_first_item = opal_datatype_opt_next_item(desc, pos_desc, first_item);
    if (!opal_datatype_opt_item_as_elem(desc, pos_desc, first_item, &first_elem)
        || !opal_datatype_opt_item_as_elem(desc, pos_desc, last_item, &last_elem)) {
        return false;
    }

    first = &first_elem;
    last = &last_elem;
    if (!opal_datatype_opt_fuse_tail_head(pData, last, first, loop->extent, loop->loops - 1,
                                          loop->extent, &fused)) {
        return false;
    }

    /*
     * Convert
     *   [E0, E1, ..., En] x count
     * where En(i) is byte-adjacent to E0(i + 1), into:
     *   E0, E1, ..., E(n-1)
     *   [En + next E0, next E1, ..., next E(n-1)] x (count - 1)
     *   final En
     * This triage is done at the loop-body item level, so middle items can be
     * either data entries or nested loops. The first and last items must be
     * representable as single contiguous copy fragments before they are fused.
     */
    opal_datatype_opt_emit_desc_range(pElemDesc, nbElems, desc, pos_desc, first_item, last_item, 0);

    if (2 == item_count) {
        CREATE_ELEM(*pElemDesc, fused.common.type, fused.common.flags, fused.blocklen,
                    loop->loops - 1, fused.disp, loop->extent);
        (*pElemDesc)++;
        (*nbElems)++;
    } else {
        steady_items = last_item - after_first_item + 2;
        CREATE_LOOP_START(*pElemDesc, loop->loops - 1, steady_items, loop->extent,
                          loop->common.flags);
        (*pElemDesc)++;
        (*nbElems)++;

        CREATE_ELEM(*pElemDesc, fused.common.type, fused.common.flags, fused.blocklen, 1,
                    fused.disp, fused.extent);
        (*pElemDesc)++;
        (*nbElems)++;
        opal_datatype_opt_emit_desc_range(pElemDesc, nbElems, desc, pos_desc, after_first_item,
                                          last_item, loop->extent);
        CREATE_LOOP_END(*pElemDesc, steady_items, fused.disp, end_loop->size,
                        loop->common.flags);
        (*pElemDesc)++;
        (*nbElems)++;
    }

    opal_datatype_opt_emit_elem(pElemDesc, nbElems, &last_elem,
                                (ptrdiff_t) (loop->loops - 1) * loop->extent);
    return true;
}

static int32_t opal_datatype_optimize_short(opal_datatype_t *pData,
                                            const dt_type_desc_t *input_desc,
                                            size_t count, dt_type_desc_t *output_desc,
                                            uint32_t optimization_mask,
                                            bool enable_loop_boundary,
                                            bool top_loop_boundary_only,
                                            bool *loop_boundary_expanded,
                                            bool *reevaluate)
{
    const dt_elem_desc_t *desc = input_desc->desc;
    dt_elem_desc_t *pElemDesc;
    dt_stack_t *pOrigStack, *pStack; /* pointer to the position on the stack */
    bool *innermost_stack;
    size_t stack_length = input_desc->used + 2;
    int32_t pos_desc = 0; /* actual position in the description of the derived datatype */
    int32_t stack_pos = 0;
    int32_t nbElems = 0;
    ptrdiff_t total_disp = 0;
    ddt_elem_desc_t last = {.common.flags = 0xFFFF /* all on */, .count = 0, .disp = 0}, compress;
    ddt_elem_desc_t current_elem;
    const ddt_elem_desc_t *current;
    size_t unroll_growth;

    if (NULL != loop_boundary_expanded) {
        *loop_boundary_expanded = false;
    }
    if (NULL != reevaluate) {
        *reevaluate = false;
    }

    /* The parallel scope markers share the existing stack allocation and add no allocator call to
     * MPI_Pack's temporary-datatype path. A DATA fusion requests reevaluation only in the active
     * innermost loop; removing a child loop lets the next pass reconsider its parent. */
    pOrigStack = pStack = (dt_stack_t *) malloc(stack_length
                                                * (sizeof(dt_stack_t) + sizeof(bool)));
    if (NULL == pOrigStack) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    innermost_stack = (bool *) (pOrigStack + stack_length);
    innermost_stack[0] = false;
    SAVE_STACK(pStack, -1, 0, count, 0);

    unroll_growth = opal_datatype_opt_loop_unroll_growth(input_desc, optimization_mask);
    if ((SIZE_MAX == unroll_growth)
        || (input_desc->used > (SIZE_MAX - unroll_growth - 1) / 2)) {
        free(pOrigStack);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    output_desc->length = 2 * input_desc->used + unroll_growth
                          + 1 /* for the fake OPAL_DATATYPE_END_LOOP at the end */;
    output_desc->desc = pElemDesc = (dt_elem_desc_t *) malloc(sizeof(dt_elem_desc_t)
                                                              * output_desc->length);
    if (NULL == output_desc->desc) {
        free(pOrigStack);
        output_desc->length = 0;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    output_desc->used = 0;

    assert(OPAL_DATATYPE_END_LOOP == desc[input_desc->used].elem.common.type);

    while (stack_pos >= 0) {
        if (OPAL_DATATYPE_END_LOOP
            == desc[pos_desc].elem.common.type) { /* end of the current loop */
            const ddt_endloop_desc_t *end_loop = &(desc[pos_desc].end_loop);
            if (0 != last.count) {
                CREATE_ELEM(pElemDesc, last.common.type,
                            OPAL_DATATYPE_FLAG_BASIC
                                | (last.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                            last.blocklen,
                            last.count, last.disp, last.extent);
                pElemDesc++;
                nbElems++;
                last.count = 0;
            }
            CREATE_LOOP_END(pElemDesc, nbElems - pStack->index + 1, /* # of elems in this loop */
                            end_loop->first_elem_disp, end_loop->size, end_loop->common.flags);
            if (--stack_pos >= 0) { /* still something to do ? */
                ddt_loop_desc_t *pStartLoop = &(output_desc->desc[pStack->index - 1].loop);
                pStartLoop->items = pElemDesc->end_loop.items;
                total_disp = pStack->disp; /* update the displacement position */
            }
            pElemDesc++;
            nbElems++;
            pStack--; /* go down one position on the stack */
            pos_desc++;
            continue;
        }
        if (OPAL_DATATYPE_LOOP == desc[pos_desc].elem.common.type) {
            const ddt_loop_desc_t *loop = &(desc[pos_desc].loop);
            int index = GET_FIRST_NON_LOOP(&(desc[pos_desc]));

            if (loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                if (opal_datatype_opt_compress_contiguous_loop(desc, pos_desc, &compress)) {
                    /* Removing an inner loop can make its parent eligible for loop-level fusion. */
                    if (NULL != reevaluate) {
                        *reevaluate = true;
                    }
                    if (compress.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED) {
                        pData->flags |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                    }
                    /**
                     * The current loop has been compressed and can now be treated as if it
                     * was a data element. We can now look if it can be fused with last,
                     * as done in the fusion of 2 elements below. Let's use the same code.
                     */
                    pos_desc += loop->items + 1;
                    current = &compress;
                    goto fuse_loops;
                }
            }

            /**
             * If the content of the loop is not contiguous there is little we can do
             * that would not incur significant optimization cost and still be beneficial
             * in reducing the number of memcpy during pack/unpack.
             */

            if (0 != last.count) { /* Generate the pending element */
                CREATE_ELEM(pElemDesc, last.common.type,
                            OPAL_DATATYPE_FLAG_BASIC
                                | (last.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                            last.blocklen,
                            last.count, last.disp, last.extent);
                pElemDesc++;
                nbElems++;
                last.count = 0;
                last.common.type = OPAL_DATATYPE_LOOP;
            }

            /*
             * Fully expand short innermost loops. Three-DATA, two-iteration bodies are included
             * because fusion can reduce an unrolled body to that shape; removing its loop exposes
             * the iteration boundary and lets subsequent passes canonicalize the parent.
             */
            if ((loop->items <= 4) && (loop->loops <= 2)
                && opal_datatype_opt_loop_is_innermost(desc, pos_desc)) {
                /* The parent loop must be reconsidered after these loop markers disappear. */
                if (NULL != reevaluate) {
                    *reevaluate = true;
                }
                ptrdiff_t elem_displ = 0;
                for (uint32_t i = 0; i < loop->loops; i++) {
                    for (uint32_t j = 0; j < (loop->items - 1); j++) {
                        current = &desc[pos_desc + index + j].elem;
                        /* Carry the optimizer's per-element TYPE_CHANGED marker onto the expanded
                         * copies. It records that this element's predefined type was changed while
                         * preserving byte layout, which lets the heterogeneous path decide whether
                         * the optimized description is usable; dropping it here would hide that from
                         * every element produced by full loop expansion. */
                        CREATE_ELEM(pElemDesc, current->common.type,
                                    OPAL_DATATYPE_FLAG_BASIC
                                        | (current->common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                                    current->blocklen, current->count, current->disp + elem_displ,
                                    current->extent);
                        pElemDesc++;
                        nbElems++;
                    }
                    elem_displ += loop->extent;
                }
                pos_desc += loop->items + 1;
                goto complete_loop;
            }

            /*
             * Non-contiguous loops may still expose mergeable copy fragments at
             * loop iteration boundaries. Boundary fusion reduces the number of
             * memcpy fragments per iteration, so try it before the weaker unroll
             * transform (which only trims END_LOOP bookkeeping) and before
             * preserving the loop as-is.
             */
            if (enable_loop_boundary
                && (optimization_mask & OPAL_DATATYPE_OPTIMIZE_LOOP_BOUNDARY)
                && (!top_loop_boundary_only || (0 == stack_pos))
                && opal_datatype_optimize_loop_boundary(pData, desc, pos_desc, &pElemDesc,
                                                        &nbElems)) {
                if (NULL != loop_boundary_expanded) {
                    *loop_boundary_expanded = true;
                }
                pos_desc += loop->items + 1;
                goto complete_loop;
            }

            /*
             * Last resort before keeping the loop verbatim: cheap flat DATA loops
             * benefit from fewer END_LOOP visits. Unrolling is attempted only after
             * boundary fusion has declined this loop, since fusing the iteration
             * boundary removes actual copies while unrolling only shortens the loop
             * bookkeeping. Keep a shortened loop and emit non-divisible iterations
             * afterward so arbitrary loop counts preserve their exact typemap order.
             */
            {
                uint32_t unroll_factor =
                    (optimization_mask & OPAL_DATATYPE_OPTIMIZE_LOOP_UNROLL)
                        ? opal_datatype_opt_loop_unroll_factor(desc, pos_desc)
                        : 1;
                if (1 < unroll_factor) {
                    opal_datatype_opt_emit_unrolled_loop(&pElemDesc, &nbElems, desc, pos_desc,
                                                         unroll_factor);
                    pos_desc += loop->items + 1;
                    goto complete_loop;
                }
            }

            CREATE_LOOP_START(pElemDesc, loop->loops, loop->items, loop->extent,
                              loop->common.flags);
            pElemDesc++;
            nbElems++;
            PUSH_STACK(pStack, stack_pos, nbElems, OPAL_DATATYPE_LOOP, loop->loops, total_disp);
            innermost_stack[stack_pos] = opal_datatype_opt_loop_is_innermost(desc, pos_desc);
            pos_desc++;
            DDT_DUMP_STACK(pStack, stack_pos, desc, "advance loops");

        complete_loop:
            total_disp = pStack->disp; /* update the displacement */
            continue;
        }
        while (desc[pos_desc].elem.common.flags
               & OPAL_DATATYPE_FLAG_DATA) { /* go over all basic datatype elements */
            current_elem = desc[pos_desc].elem;
            current_elem.common.flags = OPAL_DATATYPE_FLAG_BASIC;
            current = &current_elem;
            pos_desc++; /* point to the next element as current points to the current one */

        fuse_loops:
            if (0 == last.count) { /* first data of the datatype */
                last = *current;
                continue; /* next data */
            }
            /* can we merge it in order to decrease count */
            if ((ptrdiff_t) last.blocklen
                    * (ptrdiff_t) opal_datatype_basicDatatypes[last.common.type]->size
                == last.extent) {
                last.extent *= last.count;
                last.blocklen *= last.count;
                last.count = 1;
            }

            ptrdiff_t last_block_size =
                (ptrdiff_t) last.blocklen
                * (ptrdiff_t) opal_datatype_basicDatatypes[last.common.type]->size;
            ptrdiff_t current_block_size =
                (ptrdiff_t) current->blocklen
                * (ptrdiff_t) opal_datatype_basicDatatypes[current->common.type]->size;

            /* are the two elements compatible: aka they have very similar values and they
             * can be merged together by increasing the count, and/or changing the extent.
             */
            if (last_block_size == current_block_size) {
                bool mixed_types = (last.common.type != current->common.type);
                ptrdiff_t merged_extent = last.extent;
                uint32_t merged_count = last.count + current->count;
                bool can_merge = false;

                if (((last.extent * (ptrdiff_t) last.count + last.disp) == current->disp)
                    && ((1 == current->count) || (last.extent == current->extent))) {
                    can_merge = true;
                } else if ((1 == last.count)
                           && ((1 == current->count)
                               || ((last.disp + current->extent) == current->disp))) {
                    /* Ignore a count-1 extent, or use current's extent if it lands on current's displacement. */
                    merged_extent = (1 == current->count) ? current->disp - last.disp : current->extent;
                    can_merge = true;
                }

                if (can_merge) {
                    /* Two DATA descriptors become one; the new entry may merge again next pass. */
                    if ((NULL != reevaluate) && innermost_stack[stack_pos]) {
                        *reevaluate = true;
                    }
                    if (mixed_types) {
                        uint64_t type_mask =
                            opal_datatype_opt_type_pair_mask(last.common.type, current->common.type);

                        opal_datatype_opt_set_mixed_region(&last, last_block_size, merged_count, last.disp,
                                                           merged_extent, type_mask);
                        pData->flags |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                    } else {
                        last.common.flags |= current->common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED;
                        last.extent = merged_extent;
                        last.count = merged_count;
                    }
                    continue;
                }
                /* try other optimizations */
            }
            /* are the elements fusionable such that we can fusion the last blocklen of one with the
             * first blocklen of the other.
             */
            bool expands_inline_sequence =
                (1 < last.count) && (1 < current->count)
                && (last.blocklen <= OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN)
                && (current->blocklen <= OPAL_DATATYPE_PREDEFINED_MAX_INLINE_BLOCKLEN);

            /* Fusing two counted inline blocks replaces two DATA descriptors with a prefix, fused
             * block, and suffix. Keep the two-entry form because it requires fewer inline mover
             * dispatches. Fusion remains enabled if either side uses the generic copy path, where
             * combining adjacent boundary blocks can reduce the copy overhead. */
            if (!expands_inline_sequence
                && (optimization_mask & OPAL_DATATYPE_OPTIMIZE_ADJACENT_FUSION)
                && ((ptrdiff_t) (last.disp + (last.count - 1) * last.extent + last_block_size)
                    == current->disp)) {
                bool reduces_entries = (1 == last.count) && (1 == current->count);
                ptrdiff_t fused_extent = last.extent + current->extent;

                /* Counted prefixes or suffixes retain their descriptor, so only the one-to-one
                 * case exposes a smaller DATA sequence that merits another optimization pass. */
                if (reduces_entries && (NULL != reevaluate) && innermost_stack[stack_pos]) {
                    *reevaluate = true;
                }

                if (last.count != 1) {
                    CREATE_ELEM(pElemDesc, last.common.type,
                                OPAL_DATATYPE_FLAG_BASIC
                                    | (last.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                                last.blocklen, last.count - 1, last.disp, last.extent);
                    pElemDesc++;
                    nbElems++;
                    last.disp += (last.count - 1) * last.extent;
                    last.count = 1;
                }
                if (last.common.type == current->common.type) {
                    last.common.flags |= current->common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED;
                    last.blocklen += current->blocklen;
                } else {
                    uint64_t type_mask =
                        opal_datatype_opt_type_pair_mask(last.common.type, current->common.type);

                    opal_datatype_opt_set_mixed_region(&last, last_block_size + current_block_size, 1,
                                                       last.disp, fused_extent, type_mask);
                    pData->flags |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                }
                last.extent = fused_extent;
                if (current->count != 1) {
                    CREATE_ELEM(pElemDesc, last.common.type,
                                OPAL_DATATYPE_FLAG_BASIC
                                    | (last.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                                last.blocklen, last.count, last.disp, last.extent);
                    pElemDesc++;
                    nbElems++;
                    last = *current;
                    last.count -= 1;
                    last.disp += last.extent;
                }
                continue;
            }
            CREATE_ELEM(pElemDesc, last.common.type,
                        OPAL_DATATYPE_FLAG_BASIC
                            | (last.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                        last.blocklen,
                        last.count, last.disp, last.extent);
            pElemDesc++;
            nbElems++;
            last = *current;
        }
    }

    if (0 != last.count) {
        CREATE_ELEM(pElemDesc, last.common.type,
                    OPAL_DATATYPE_FLAG_BASIC
                        | (last.common.flags & OPAL_DATATYPE_OPTIMIZED_TYPE_CHANGED),
                    last.blocklen,
                    last.count, last.disp, last.extent);
        pElemDesc++;
        nbElems++;
    }
    /* cleanup the stack */
    output_desc->used = nbElems - 1; /* except the last fake END_LOOP */
    free(pOrigStack);
    return OPAL_SUCCESS;
}

/*
 * Run the short-description optimizer until loop-boundary expansion, DATA fusion, and loop
 * removal stop exposing new opportunities. A fusion can synthesize an entry that only becomes
 * mergeable on the next pass, while removing an inner loop can make its parent loop eligible for
 * optimization. Unrolling alone deliberately does not request a restart because it changes loop
 * execution without reducing the descriptor structure.
 *
 * Loop-boundary candidates must still reduce the estimated number of copy ranges and stay within
 * the descriptor-growth limit. A non-expanding optimization of the original input is converged
 * under the same fusion and loop-removal rule, then retained as the baseline when expansion is not
 * profitable.
 *
 * optimization_mask lets the caller suppress transforms whose runtime cost is
 * unfavorable for a specific pack/unpack path.  If top_loop_boundary_only is
 * true, expansion is allowed only on the outer loop in input_desc; this is used
 * when the input wraps an already-optimized datatype body.
 */
static int32_t opal_datatype_optimize_short_restart(opal_datatype_t *pData,
                                                    const dt_type_desc_t *input_desc,
                                                    dt_type_desc_t *output_desc,
                                                    uint32_t optimization_mask,
                                                    bool top_loop_boundary_only)
{
    dt_type_desc_t baseline = {0};
    dt_type_desc_t candidate = {0};
    dt_type_desc_t next = {0};
    const size_t growth_limit = input_desc->used * OPAL_DATATYPE_OPT_MAX_DESC_GROWTH;
    const uint32_t initial_flags = pData->flags & ~OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE;
    uint32_t baseline_flags, candidate_flags;
    size_t baseline_ranges, candidate_ranges;
    bool boundary_expanded, expanded = false, reevaluate = false;
    int32_t rc;

    pData->flags = initial_flags;
    rc = opal_datatype_optimize_short(pData, input_desc, 1, &candidate, optimization_mask, true,
                                      top_loop_boundary_only, &expanded, &reevaluate);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }
    candidate_flags = pData->flags;
    boundary_expanded = expanded;

    if (!expanded && !reevaluate) {
        *output_desc = candidate;
        opal_datatype_opt_update_count_boundary(pData, output_desc);
        return OPAL_SUCCESS;
    }
    candidate_ranges = opal_datatype_opt_count_range_groups(&candidate);

    while (expanded || reevaluate) {
        bool next_expanded = false, next_reevaluate = false;
        size_t next_ranges;

        if (candidate.used > growth_limit) {
            break;
        }

        pData->flags = initial_flags
                       | (candidate_flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED);
        rc = opal_datatype_optimize_short(pData, &candidate, 1, &next, optimization_mask, true,
                                          top_loop_boundary_only, &next_expanded,
                                          &next_reevaluate);
        if (OPAL_SUCCESS != rc) {
            opal_datatype_opt_free_desc(&candidate);
            pData->flags = initial_flags;
            return rc;
        }
        next_ranges = opal_datatype_opt_count_range_groups(&next);
        /* A structural reduction is useful even when it combines several ranges into one counted
         * descriptor. Boundary expansion without such a reduction keeps its stricter range test. */
        if ((next.used > growth_limit)
            || (next_expanded && !next_reevaluate && (next_ranges >= candidate_ranges))) {
            opal_datatype_opt_free_desc(&next);
            break;
        }

        boundary_expanded |= next_expanded;
        candidate_flags = pData->flags;
        opal_datatype_opt_free_desc(&candidate);
        candidate = next;
        next = (dt_type_desc_t) {0};
        candidate_ranges = next_ranges;
        expanded = next_expanded;
        reevaluate = next_reevaluate;
    }

    /* No loop-boundary expansion survived, so the converged fusion result needs no baseline. */
    if (!boundary_expanded) {
        *output_desc = candidate;
        pData->flags = initial_flags
                       | (candidate_flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED);
        opal_datatype_opt_update_count_boundary(pData, output_desc);
        return OPAL_SUCCESS;
    }

    pData->flags = initial_flags;
    reevaluate = false;
    rc = opal_datatype_optimize_short(pData, input_desc, 1, &baseline, optimization_mask, false,
                                      top_loop_boundary_only, NULL, &reevaluate);
    if (OPAL_SUCCESS != rc) {
        opal_datatype_opt_free_desc(&candidate);
        pData->flags = initial_flags;
        return rc;
    }
    baseline_flags = pData->flags;

    while (reevaluate) {
        bool next_reevaluate = false;

        pData->flags = initial_flags
                       | (baseline_flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED);
        rc = opal_datatype_optimize_short(pData, &baseline, 1, &next, optimization_mask, false,
                                          top_loop_boundary_only, NULL, &next_reevaluate);
        if (OPAL_SUCCESS != rc) {
            opal_datatype_opt_free_desc(&baseline);
            opal_datatype_opt_free_desc(&candidate);
            pData->flags = initial_flags;
            return rc;
        }
        if (next.used > growth_limit) {
            opal_datatype_opt_free_desc(&next);
            break;
        }

        baseline_flags = pData->flags;
        opal_datatype_opt_free_desc(&baseline);
        baseline = next;
        next = (dt_type_desc_t) {0};
        reevaluate = next_reevaluate;
    }
    baseline_ranges = opal_datatype_opt_count_range_groups(&baseline);

    if ((candidate.used <= growth_limit) && (candidate_ranges < baseline_ranges)) {
        opal_datatype_opt_free_desc(&baseline);
        *output_desc = candidate;
        pData->flags = initial_flags
                       | (candidate_flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED);
    } else {
        opal_datatype_opt_free_desc(&candidate);
        *output_desc = baseline;
        pData->flags = initial_flags
                       | (baseline_flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED);
    }
    opal_datatype_opt_update_count_boundary(pData, output_desc);

    return OPAL_SUCCESS;
}

int32_t opal_datatype_optimize_from_contiguous(opal_datatype_t *pData,
                                               const opal_datatype_t *oldType,
                                               size_t count,
                                               uint32_t optimization_mask)
{
    const dt_type_desc_t *body_desc;
    dt_type_desc_t input_desc = {0};
    dt_type_desc_t optimized_desc = {0};
    ptrdiff_t extent = oldType->ub - oldType->lb;
    ptrdiff_t first_elem_disp = 0;
    uint16_t loop_flags = (uint16_t) ((oldType->flags & 0xffffu)
                                      & ~OPAL_DATATYPE_FLAG_COMMITTED);
    uint32_t loop_markers;
    bool need_commit = !(pData->flags & OPAL_DATATYPE_FLAG_COMMITTED);
    int rc;

    /*
     * This helper is for datatypes created by contiguous(count, oldType).
     * Keep pData->desc as the real contiguous-constructor description so the
     * datatype still carries the full original layout information.  Only
     * pData->opt_desc is built from the already-optimized oldType body.
     */
    /*
     * The temporary input below wraps oldType's optimized body in a single
     * LOOP entry, whose repetition count is stored in ddt_loop_desc_t.loops
     * as a uint32_t.  If count does not fit there, leave pData unchanged
     * instead of truncating the loop count.
     */
    if ((count < 2) || (UINT32_MAX < count)) {
        return OPAL_SUCCESS;
    }

    body_desc = (0 != oldType->opt_desc.used) ? &oldType->opt_desc : &oldType->desc;
    if ((0 == body_desc->used) || (NULL == body_desc->desc)) {
        return OPAL_SUCCESS;
    }

    input_desc.used = body_desc->used + 2;
    input_desc.length = input_desc.used + 1;
    input_desc.desc = (dt_elem_desc_t *) malloc(sizeof(dt_elem_desc_t) * input_desc.length);
    if (NULL == input_desc.desc) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /*
     * Build a temporary optimization input equivalent to:
     *
     *   LOOP count times over oldType->opt_desc
     *
     * Restrict loop-boundary expansion to this outer loop.  The wrapped body
     * came from an already optimized datatype, so re-expanding its internal
     * loop boundaries would only redo work that has already been accepted or
     * rejected for oldType.
     */
    CREATE_LOOP_START(&input_desc.desc[0], count, body_desc->used + 1, extent, loop_flags);
    memcpy(&input_desc.desc[1], body_desc->desc, sizeof(dt_elem_desc_t) * body_desc->used);

    if (0 != oldType->size) {
        int index = GET_FIRST_NON_LOOP(input_desc.desc);

        assert(input_desc.desc[index].elem.common.flags & OPAL_DATATYPE_FLAG_DATA);
        first_elem_disp = input_desc.desc[index].elem.disp;
    }
    CREATE_LOOP_END(&input_desc.desc[input_desc.used - 1], body_desc->used + 1,
                    first_elem_disp, oldType->size, loop_flags);
    opal_datatype_opt_set_fake_end_loop(&input_desc, first_elem_disp, pData->size);

    rc = opal_datatype_optimize_short_restart(pData, &input_desc, &optimized_desc,
                                              optimization_mask, true);
    free(input_desc.desc);
    if (OPAL_SUCCESS != rc) {
        return rc;
    }

    /*
     * pData->opt_desc is built from oldType's optimized descriptor, so it
     * inherits any homogeneous-only restriction already present there.
     */
    pData->flags |= oldType->flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED;

    if ((NULL != pData->opt_desc.desc) && (pData->opt_desc.desc != pData->desc.desc)) {
        opal_datatype_opt_free_desc(&pData->opt_desc);
    }
    pData->opt_desc = optimized_desc;
    if (0 != pData->opt_desc.used) {
        opal_datatype_opt_set_fake_end_loop(&pData->opt_desc, first_elem_disp, pData->size);
        loop_markers = opal_datatype_opt_count_loop_markers(&pData->opt_desc);
        if (loop_markers > pData->loops) {
            pData->loops = loop_markers;
        }
    }
    if (need_commit) {
        (void) opal_datatype_commit_description(pData);
    }

    return OPAL_SUCCESS;
}

/*
 * Follow the basic pack traversal without copying data. Consecutive copy
 * regions are compared and the descriptor entry that could absorb its
 * predecessor is reported. Large vectors and loops only need their boundary
 * iterations; contiguous no-gap loops are already one effective copy region.
 */
static void opal_datatype_report_missed_optimizations(opal_datatype_t *pData)
{
    const dt_type_desc_t *type_desc = &pData->desc;
    const dt_elem_desc_t *desc;
    size_t stack_length = pData->loops + 1;
    dt_stack_t *stack;
    int32_t stack_pos = -1;
    int32_t pos = 0;
    bool compare_previous = true;
    struct {
        bool valid;
        uint32_t type;
        int32_t desc_index;
        uint32_t block_index;
        ptrdiff_t disp;
        ptrdiff_t end;
    } previous = {0};

    if (pData->flags & OPAL_DATATYPE_FLAG_NO_GAPS) {
        return;
    }

    /* Inspect the DDT representation used by pack when one is available. */
    if (0 != pData->opt_desc.used) {
        type_desc = &pData->opt_desc;
    }

    if (0 == type_desc->used) {
        return;
    }
    stack = (dt_stack_t *) alloca(stack_length * sizeof(*stack));
    desc = type_desc->desc;
    /* Resolve every visited descriptor entry into the memory regions that pack would copy. */
    while (pos < (int32_t) type_desc->used) {
        const dt_elem_desc_t *entry = &desc[pos];
        ptrdiff_t base_disp = (0 <= stack_pos) ? stack[stack_pos].disp : 0;
        ptrdiff_t first_disp;
        ptrdiff_t copy_extent;
        ptrdiff_t copy_size;
        uint32_t copy_count;
        uint32_t copy_type;
        int32_t next_pos;

        if (entry->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            /* A data entry is a strided vector; only its boundary blocks affect neighboring entries. */
            copy_count = entry->elem.count;
            copy_type = entry->elem.common.type;
            first_disp = base_disp + entry->elem.disp;
            copy_extent = entry->elem.extent;
            copy_size = (ptrdiff_t) entry->elem.blocklen
                        * (ptrdiff_t) opal_datatype_basicDatatypes[entry->elem.common.type]->size;
            next_pos = pos + 1;
            goto record_report_copy;
        }
        if ((OPAL_DATATYPE_LOOP == entry->elem.common.type)
            && (entry->loop.common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)) {
            const ddt_endloop_desc_t *end_loop = &desc[pos + entry->loop.items].end_loop;

            /* Pack handles a contiguous loop directly, so its body does not need to be traversed. */
            copy_count = entry->loop.loops;
            copy_type = OPAL_DATATYPE_UINT1;
            first_disp = base_disp + end_loop->first_elem_disp;
            copy_extent = entry->loop.extent;
            copy_size = end_loop->size;
            next_pos = pos + entry->loop.items + 1;
            if (entry->loop.common.flags & OPAL_DATATYPE_FLAG_NO_GAPS) {
                copy_size *= copy_count;
                copy_count = 1;
            }
            goto record_report_copy;
        }
        if (OPAL_DATATYPE_LOOP == entry->elem.common.type) {
            /* Enter a noncontiguous body using the same descriptor stack shape as the convertor. */
            assert((size_t) (stack_pos + 1) < stack_length);
            ++stack_pos;
            stack[stack_pos].index = pos;
            stack[stack_pos].count = entry->loop.loops;
            stack[stack_pos].disp = base_disp;
            ++pos;
            continue;
        }
        assert(OPAL_DATATYPE_END_LOOP == entry->elem.common.type);
        {
            const ddt_loop_desc_t *loop;

            assert(0 <= stack_pos);
            loop = &desc[stack[stack_pos].index].loop;
            if (0 == --stack[stack_pos].count) {
                --stack_pos;
                ++pos;
            } else {
                uint32_t completed = loop->loops - stack[stack_pos].count;

                /* Two iterations expose cross-iteration merges; jump directly to the final one afterward. */
                if ((2 == completed) && (3 < loop->loops)) {
                    ptrdiff_t parent_disp = (0 < stack_pos) ? stack[stack_pos - 1].disp : 0;

                    stack[stack_pos].count = 1;
                    stack[stack_pos].disp = parent_disp + (ptrdiff_t) (loop->loops - 1) * loop->extent;
                    compare_previous = false;
                } else {
                    stack[stack_pos].disp += loop->extent;
                }
                pos = stack[stack_pos].index + 1;
            }
            continue;
        }

    record_report_copy:
        if (0 != copy_count) {
            /* The first copy checks the preceding entry; the last preserves the next boundary. */
            uint32_t blocks[2] = {0, copy_count - 1};
            uint32_t block_count = (1 < copy_count) ? 2 : 1;

            for (uint32_t i = 0; i < block_count; ++i) {
                uint32_t block = blocks[i];
                ptrdiff_t disp = first_disp + (ptrdiff_t) block * copy_extent;
                ptrdiff_t end = disp + copy_size;

                if ((1 == i) && (2 < copy_count)) {
                    /* Skipped middle blocks make the first and last copies nonconsecutive. */
                    compare_previous = false;
                }
                if (compare_previous && previous.valid && (previous.end == disp)) {
#if OPAL_ENABLE_DEBUG
                    const char *kind = (previous.type == copy_type) ? "same-type" : "byte-wise";

                    opal_output(0,
                                "datatype %p %s description missed %s merge: desc %d block %u "
                                "[%ld, %ld) %s followed by desc %d block %u [%ld, %ld) %s",
                                (void *) pData,
                                type_desc == &pData->desc ? "original" : "optimized", kind,
                                previous.desc_index,
                                previous.block_index, (long) previous.disp, (long) previous.end,
                                opal_datatype_basicDatatypes[previous.type]->name, pos, block,
                                (long) disp, (long) end,
                                opal_datatype_basicDatatypes[copy_type]->name);
#endif
                }
                previous.valid = true;
                previous.type = copy_type;
                previous.desc_index = pos;
                previous.block_index = block;
                previous.disp = disp;
                previous.end = end;
                compare_previous = true;
            }
        }
        pos = next_pos;
    }
}

int32_t opal_datatype_commit(opal_datatype_t *pData)
{
    dt_type_desc_t optimized_desc = {0};
    ptrdiff_t first_elem_disp;

    if (pData->flags & OPAL_DATATYPE_FLAG_COMMITTED) {
        return OPAL_SUCCESS;
    }
    first_elem_disp = opal_datatype_commit_description(pData);

    /* If there is no datatype description how can we have an optimized description ? */
    if (0 == pData->desc.used) {
        pData->opt_desc.length = 0;
        pData->opt_desc.desc = NULL;
        pData->opt_desc.used = 0;
        return OPAL_SUCCESS;
    }

    /* If the data is contiguous is useless to generate an optimized version. */
    /*if( pData->size == (pData->true_ub - pData->true_lb) ) return OPAL_SUCCESS; */

    (void) opal_datatype_optimize_short_restart(pData, &pData->desc, &optimized_desc,
                                                OPAL_DATATYPE_OPTIMIZE_ALL, false);
    pData->opt_desc = optimized_desc;
    if (0 != pData->opt_desc.used) {
        /* let's add a fake element at the end just to avoid useless comparaisons
         * in pack/unpack functions.
         */
        opal_datatype_opt_set_fake_end_loop(&pData->opt_desc, first_elem_disp, pData->size);
    }
    if (opal_datatype_check_missed_optimizations) {
        opal_datatype_report_missed_optimizations(pData);
    }
    return OPAL_SUCCESS;
}
