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

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "opal/datatype/opal_convertor.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"
#include "opal/util/output.h"

/*
 * Return the number of payload bytes described by one block of a data element.
 * This intentionally ignores elem->count; callers that need one contiguous
 * copy fragment collapse or reject multi-count elements before using it.
 */
static ptrdiff_t opal_datatype_opt_elem_size(const ddt_elem_desc_t *elem)
{
    return (ptrdiff_t) elem->blocklen
           * (ptrdiff_t) opal_datatype_basicDatatypes[elem->common.type]->size;
}

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
    if ((1 < elem->count) && (elem->extent == opal_datatype_opt_elem_size(elem))) {
        elem->blocklen *= elem->count;
        elem->extent *= elem->count;
        elem->count = 1;
    }
}

static bool opal_datatype_opt_item_as_elem(const dt_elem_desc_t *desc, int32_t pos_desc,
                                           uint32_t item, ddt_elem_desc_t *elem);

/*
 * Summarize a contiguous nested loop as one data-like element so that an
 * enclosing loop can test adjacency across its iteration boundary. If every
 * item in the loop body resolves to the same predefined type, keep that type
 * in the summary. Only mixed or opaque regions are represented as UINT1 and
 * marked restricted for homogeneous-only use.
 */
static bool opal_datatype_opt_compress_contiguous_loop(const dt_elem_desc_t *desc,
                                                       int32_t pos_desc,
                                                       ddt_elem_desc_t *elem)
{
    const ddt_loop_desc_t *loop = &desc[pos_desc].loop;
    const ddt_endloop_desc_t *end_loop = &desc[pos_desc + loop->items].end_loop;
    uint16_t common_type = OPAL_DATATYPE_UNAVAILABLE;
    uint16_t common_flags = OPAL_DATATYPE_FLAG_BASIC;
    size_t common_blocklen = 0;
    bool homogeneous = true;
    bool have_item = false;

    if (!(loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)) {
        return false;
    }

    for (uint32_t i = 1; i < loop->items;
         i = opal_datatype_opt_next_item(desc, pos_desc, i)) {
        ddt_elem_desc_t current;

        have_item = true;
        if (!opal_datatype_opt_item_as_elem(desc, pos_desc, i, &current)) {
            homogeneous = false;
            break;
        }

        if (OPAL_DATATYPE_UNAVAILABLE == common_type) {
            common_type = current.common.type;
            common_blocklen = current.blocklen;
            common_flags |= current.common.flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
            continue;
        }

        if (common_type != current.common.type) {
            homogeneous = false;
            break;
        }
        common_blocklen += current.blocklen;
        common_flags |= current.common.flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
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
        elem->common.type = OPAL_DATATYPE_UINT1;
        elem->common.flags = OPAL_DATATYPE_FLAG_BASIC | OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
        elem->blocklen = end_loop->size;
    }

    elem->count = loop->loops;
    elem->extent = loop->extent;
    elem->disp = end_loop->first_elem_disp;
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
 * next iteration when they are byte-adjacent. Different basic types are fused as
 * UINT1 and force the optimized description to be restricted.
 */
static bool opal_datatype_opt_fuse_tail_head(opal_datatype_t *pData,
                                             const ddt_elem_desc_t *tail,
                                             const ddt_elem_desc_t *head,
                                             ptrdiff_t head_disp_delta,
                                             ddt_elem_desc_t *fused)
{
    ptrdiff_t tail_size, head_size;

    if ((1 != tail->count) || (1 != head->count)) {
        return false;
    }

    tail_size = opal_datatype_opt_elem_size(tail);
    head_size = opal_datatype_opt_elem_size(head);
    if (tail->disp + tail_size != head->disp + head_disp_delta) {
        return false;
    }

    *fused = *tail;
    fused->count = 1;
    fused->extent = tail_size + head_size;
    if (tail->common.type == head->common.type) {
        fused->common.flags = OPAL_DATATYPE_FLAG_BASIC
                              | ((tail->common.flags | head->common.flags)
                                 & OPAL_DATATYPE_OPTIMIZED_RESTRICTED);
        fused->blocklen += head->blocklen;
    } else {
        fused->common.type = OPAL_DATATYPE_UINT1;
        fused->common.flags = OPAL_DATATYPE_FLAG_BASIC | OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
        fused->blocklen = tail_size + head_size;
    }
    if (fused->common.flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED) {
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
    if (!opal_datatype_opt_fuse_tail_head(pData, last, first, loop->extent, &fused)) {
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

static int32_t opal_datatype_optimize_short(opal_datatype_t *pData, size_t count,
                                            dt_type_desc_t *pTypeDesc)
{
    dt_elem_desc_t *pElemDesc;
    dt_stack_t *pOrigStack, *pStack; /* pointer to the position on the stack */
    int32_t pos_desc = 0; /* actual position in the description of the derived datatype */
    int32_t stack_pos = 0;
    int32_t nbElems = 0;
    ptrdiff_t total_disp = 0;
    ddt_elem_desc_t last = {.common.flags = 0xFFFF /* all on */, .count = 0, .disp = 0}, compress;
    ddt_elem_desc_t *current;

    pOrigStack = pStack = (dt_stack_t *) malloc(sizeof(dt_stack_t) * (pData->loops + 2));
    SAVE_STACK(pStack, -1, 0, count, 0);

    pTypeDesc->length = 2 * pData->desc.used
                        + 1 /* for the fake OPAL_DATATYPE_END_LOOP at the end */;
    pTypeDesc->desc = pElemDesc = (dt_elem_desc_t *) malloc(sizeof(dt_elem_desc_t)
                                                            * pTypeDesc->length);
    pTypeDesc->used = 0;

    assert(OPAL_DATATYPE_END_LOOP == pData->desc.desc[pData->desc.used].elem.common.type);

    while (stack_pos >= 0) {
        if (OPAL_DATATYPE_END_LOOP
            == pData->desc.desc[pos_desc].elem.common.type) { /* end of the current loop */
            ddt_endloop_desc_t *end_loop = &(pData->desc.desc[pos_desc].end_loop);
            if (0 != last.count) {
                CREATE_ELEM(pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC, last.blocklen,
                            last.count, last.disp, last.extent);
                pElemDesc++;
                nbElems++;
                last.count = 0;
            }
            CREATE_LOOP_END(pElemDesc, nbElems - pStack->index + 1, /* # of elems in this loop */
                            end_loop->first_elem_disp, end_loop->size, end_loop->common.flags);
            if (--stack_pos >= 0) { /* still something to do ? */
                ddt_loop_desc_t *pStartLoop = &(pTypeDesc->desc[pStack->index - 1].loop);
                pStartLoop->items = pElemDesc->end_loop.items;
                total_disp = pStack->disp; /* update the displacement position */
            }
            pElemDesc++;
            nbElems++;
            pStack--; /* go down one position on the stack */
            pos_desc++;
            continue;
        }
        if (OPAL_DATATYPE_LOOP == pData->desc.desc[pos_desc].elem.common.type) {
            ddt_loop_desc_t *loop = (ddt_loop_desc_t *) &(pData->desc.desc[pos_desc]);
            int index = GET_FIRST_NON_LOOP(&(pData->desc.desc[pos_desc]));

            if (loop->common.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS) {
                if (opal_datatype_opt_compress_contiguous_loop(pData->desc.desc, pos_desc,
                                                               &compress)) {
                    if (compress.common.flags & OPAL_DATATYPE_OPTIMIZED_RESTRICTED) {
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
                CREATE_ELEM(pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC, last.blocklen,
                            last.count, last.disp, last.extent);
                pElemDesc++;
                nbElems++;
                last.count = 0;
                last.common.type = OPAL_DATATYPE_LOOP;
            }

            /* Can we unroll the loop? */
            if ((loop->items <= 3) && (loop->loops <= 2)) {
                ptrdiff_t elem_displ = 0;
                for (uint32_t i = 0; i < loop->loops; i++) {
                    for (uint32_t j = 0; j < (loop->items - 1); j++) {
                        current = &pData->desc.desc[pos_desc + index + j].elem;
                        CREATE_ELEM(pElemDesc, current->common.type, current->common.flags,
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
             * loop iteration boundaries. Try that before preserving the loop as-is.
             */
            if (opal_datatype_optimize_loop_boundary(pData, pData->desc.desc, pos_desc,
                                                     &pElemDesc, &nbElems)) {
                pos_desc += loop->items + 1;
                goto complete_loop;
            }

            CREATE_LOOP_START(pElemDesc, loop->loops, loop->items, loop->extent,
                              loop->common.flags);
            pElemDesc++;
            nbElems++;
            PUSH_STACK(pStack, stack_pos, nbElems, OPAL_DATATYPE_LOOP, loop->loops, total_disp);
            pos_desc++;
            DDT_DUMP_STACK(pStack, stack_pos, pData->desc.desc, "advance loops");

        complete_loop:
            total_disp = pStack->disp; /* update the displacement */
            continue;
        }
        while (pData->desc.desc[pos_desc].elem.common.flags
               & OPAL_DATATYPE_FLAG_DATA) { /* go over all basic datatype elements */
            current = &pData->desc.desc[pos_desc].elem;
            pos_desc++; /* point to the next element as current points to the current one */

        fuse_loops:
            if (0 == last.count) { /* first data of the datatype */
                last = *current;
                continue; /* next data */
            } else {      /* can we merge it in order to decrease count */
                if ((ptrdiff_t) last.blocklen
                        * (ptrdiff_t) opal_datatype_basicDatatypes[last.common.type]->size
                    == last.extent) {
                    last.extent *= last.count;
                    last.blocklen *= last.count;
                    last.count = 1;
                }
            }

            /* are the two elements compatible: aka they have very similar values and they
             * can be merged together by increasing the count, and/or changing the extent.
             */
            if ((last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size)
                == (current->blocklen * opal_datatype_basicDatatypes[current->common.type]->size)) {
                ddt_elem_desc_t save = last; /* safekeep the type and blocklen */
                if (last.common.type != current->common.type) {
                    last.blocklen *= opal_datatype_basicDatatypes[last.common.type]->size;
                    last.common.type   = OPAL_DATATYPE_UINT1;
                    last.common.flags |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                    pData->flags      |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                }

                if ((last.extent * (ptrdiff_t) last.count + last.disp) == current->disp) {
                    if (1 == current->count) {
                        last.count++;
                        continue;
                    }
                    if (last.extent == current->extent) {
                        last.count += current->count;
                        continue;
                    }
                }
                if (1 == last.count) {
                    /* we can ignore the extent of the element with count == 1 and merge them
                     * together if their displacements match */
                    if (1 == current->count) {
                        last.extent = current->disp - last.disp;
                        last.count++;
                        continue;
                    }
                    /* can we compute a matching displacement ? */
                    if ((last.disp + current->extent) == current->disp) {
                        last.extent = current->extent;
                        last.count = current->count + last.count;
                        continue;
                    }
                }
                last.blocklen = save.blocklen;
                last.common.type = save.common.type;
                /* try other optimizations */
            }
            /* are the elements fusionable such that we can fusion the last blocklen of one with the
             * first blocklen of the other.
             */
            if ((ptrdiff_t)(last.disp + (last.count - 1) * last.extent
                            + last.blocklen * opal_datatype_basicDatatypes[last.common.type]->size)
                == current->disp) {
                if (last.count != 1) {
                    CREATE_ELEM(pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                last.blocklen, last.count - 1, last.disp, last.extent);
                    pElemDesc++;
                    nbElems++;
                    last.disp += (last.count - 1) * last.extent;
                    last.count = 1;
                }
                if (last.common.type == current->common.type) {
                    last.blocklen += current->blocklen;
                } else {
                    last.blocklen = ((last.blocklen
                                      * opal_datatype_basicDatatypes[last.common.type]->size)
                                     + (current->blocklen
                                        * opal_datatype_basicDatatypes[current->common.type]
                                              ->size));
                    last.common.type   = OPAL_DATATYPE_UINT1;
                    last.common.flags |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                    pData->flags      |= OPAL_DATATYPE_OPTIMIZED_RESTRICTED;
                }
                last.extent += current->extent;
                if (current->count != 1) {
                    CREATE_ELEM(pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC,
                                last.blocklen, last.count, last.disp, last.extent);
                    pElemDesc++;
                    nbElems++;
                    last = *current;
                    last.count -= 1;
                    last.disp += last.extent;
                }
                continue;
            }
            CREATE_ELEM(pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC, last.blocklen,
                        last.count, last.disp, last.extent);
            pElemDesc++;
            nbElems++;
            last = *current;
        }
    }

    if (0 != last.count) {
        CREATE_ELEM(pElemDesc, last.common.type, OPAL_DATATYPE_FLAG_BASIC, last.blocklen,
                    last.count, last.disp, last.extent);
        pElemDesc++;
        nbElems++;
    }
    /* cleanup the stack */
    pTypeDesc->used = nbElems - 1; /* except the last fake END_LOOP */
    free(pOrigStack);
    return OPAL_SUCCESS;
}

/*
 * Follow the basic pack traversal without copying data. Consecutive copy
 * regions are compared and the descriptor entry that could absorb its
 * predecessor is reported. Large vectors and loops only need their boundary
 * iterations; contiguous no-gap loops are already one effective copy region.
 */
static void opal_datatype_check_missed_merges(opal_datatype_t *pData)
{
    const dt_type_desc_t *type_desc = &pData->desc;
    const dt_elem_desc_t *desc;
    const char *desc_name = "original";
    dt_stack_t static_stack[DT_STATIC_STACK_SIZE];
    dt_stack_t *stack = static_stack;
    size_t stack_length = pData->loops;
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
    } first = {0}, previous = {0};

    if (pData->flags & OPAL_DATATYPE_FLAG_NO_GAPS) {
        return;
    }

    /* Inspect the DDT representation used by pack when one is available. */
    if (0 != pData->opt_desc.used) {
        type_desc = &pData->opt_desc;
        desc_name = "optimized";
    }

    if (0 == type_desc->used) {
        return;
    }
    /* Match the convertor's fixed stack and allocate only for unusually deep loop nesting. */
    if (DT_STATIC_STACK_SIZE < stack_length) {
        stack = (dt_stack_t *) malloc(stack_length * sizeof(*stack));
        if (NULL == stack) {
            return;
        }
    }

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
            copy_size = opal_datatype_opt_elem_size(&entry->elem);
            next_pos = pos + 1;
        } else if ((OPAL_DATATYPE_LOOP == entry->elem.common.type)
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
        } else if (OPAL_DATATYPE_LOOP == entry->elem.common.type) {
            if (0 == entry->loop.loops) {
                pos += entry->loop.items + 1;
                continue;
            }
            /* Enter a noncontiguous body using the same descriptor stack shape as the convertor. */
            assert((size_t) (stack_pos + 1) < stack_length);
            ++stack_pos;
            stack[stack_pos].index = pos;
            stack[stack_pos].count = entry->loop.loops;
            stack[stack_pos].disp = base_disp;
            ++pos;
            continue;
        } else if (OPAL_DATATYPE_END_LOOP == entry->elem.common.type) {
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
        } else {
            ++pos;
            continue;
        }

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
                    const char *kind = (previous.type == copy_type) ? "same-type" : "byte-wise";

                    opal_output(0,
                                "datatype %p %s description missed %s merge: desc %d block %u "
                                "[%ld, %ld) %s followed by desc %d block %u [%ld, %ld) %s",
                                (void *) pData, desc_name, kind, previous.desc_index,
                                previous.block_index, (long) previous.disp, (long) previous.end,
                                opal_datatype_basicDatatypes[previous.type]->name, pos, block,
                                (long) disp, (long) end, opal_datatype_basicDatatypes[copy_type]->name);
                }
                if (!first.valid) {
                    first.valid = true;
                    first.type = copy_type;
                    first.desc_index = pos;
                    first.block_index = block;
                    first.disp = disp;
                    first.end = end;
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

    /* The final and first regions are consecutive only when the convertor processes count > 1. */
    if (first.valid && previous.valid && !(pData->flags & OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE)) {
        ptrdiff_t datatype_extent = pData->ub - pData->lb;
        ptrdiff_t next_disp = first.disp + datatype_extent;

        if (previous.end == next_disp) {
            const char *kind = (previous.type == first.type) ? "same-type" : "byte-wise";

            pData->flags |= OPAL_DATATYPE_FLAG_COUNT_OPTIMIZABLE;
            opal_output(0,
                        "datatype %p %s description can optimize %s merge across datatype count boundary: "
                        "desc %d block %u [%ld, %ld) %s followed by desc %d block %u [%ld, %ld) %s",
                        (void *) pData, desc_name, kind, previous.desc_index, previous.block_index,
                        (long) previous.disp, (long) previous.end,
                        opal_datatype_basicDatatypes[previous.type]->name, first.desc_index,
                        first.block_index, (long) next_disp, (long) (first.end + datatype_extent),
                        opal_datatype_basicDatatypes[first.type]->name);
        }
    }
    if (static_stack != stack) {
        free(stack);
    }
}

int32_t opal_datatype_commit(opal_datatype_t *pData)
{
    ddt_endloop_desc_t *pLast = &(pData->desc.desc[pData->desc.used].end_loop);
    ptrdiff_t first_elem_disp = 0;

    if (pData->flags & OPAL_DATATYPE_FLAG_COMMITTED) {
        return OPAL_SUCCESS;
    }
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
    pLast->common.type = OPAL_DATATYPE_END_LOOP;
    pLast->common.flags = 0;
    pLast->items = pData->desc.used;
    pLast->first_elem_disp = first_elem_disp;
    pLast->size = pData->size;

    /* If there is no datatype description how can we have an optimized description ? */
    if (0 == pData->desc.used) {
        pData->opt_desc.length = 0;
        pData->opt_desc.desc = NULL;
        pData->opt_desc.used = 0;
        return OPAL_SUCCESS;
    }

    /* If the data is contiguous is useless to generate an optimized version. */
    /*if( pData->size == (pData->true_ub - pData->true_lb) ) return OPAL_SUCCESS; */

    (void) opal_datatype_optimize_short(pData, 1, &(pData->opt_desc));
    if (0 != pData->opt_desc.used) {
        /* let's add a fake element at the end just to avoid useless comparaisons
         * in pack/unpack functions.
         */
        pLast = &(pData->opt_desc.desc[pData->opt_desc.used].end_loop);
        pLast->common.type = OPAL_DATATYPE_END_LOOP;
        pLast->common.flags = 0;
        pLast->items = pData->opt_desc.used;
        pLast->first_elem_disp = first_elem_disp;
        pLast->size = pData->size;
    }
    opal_datatype_check_missed_merges(pData);
    return OPAL_SUCCESS;
}
