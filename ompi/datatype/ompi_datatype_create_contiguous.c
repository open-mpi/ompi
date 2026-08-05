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
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2026      Stony Brook University.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/datatype/ompi_datatype_internal.h"
#include "mpi.h"

#include <stdbool.h>

int32_t ompi_datatype_create_contiguous( size_t count, const ompi_datatype_t* oldType,
                                         ompi_datatype_t** newType )
{
    ompi_datatype_t* pdt;

    if( (0 == count) || (0 == oldType->super.size) ) {
        return ompi_datatype_duplicate( &ompi_mpi_datatype_null.dt, newType);
    }

    pdt = ompi_datatype_create( oldType->super.desc.used + 2 );
    opal_datatype_add( &(pdt->super), &(oldType->super), count, 0, (oldType->super.ub - oldType->super.lb) );
    *newType = pdt;
    return OMPI_SUCCESS;
}

/*
 * Walk a descriptor interval and report whether it contains DATA entries with
 * blocklen values handled by the homogeneous typed inline-copy path.  When
 * require_counted is true, only count > 1 entries match; those are the entries
 * adjacent-fragment fusion can split into several count == 1 entries.
 */
static bool ompi_datatype_desc_has_small_blocks(const dt_elem_desc_t *desc, size_t start,
                                                size_t end, bool require_counted)
{
    /*
     * Descriptor loop bodies are stored inline between LOOP and END_LOOP
     * markers.  A linear scan therefore visits nested DATA entries without
     * recursion; the loop markers themselves do not affect this predicate.
     */
    for (size_t pos = start; pos < end; ++pos) {
        const dt_elem_desc_t *entry = &desc[pos];

        if (entry->elem.common.flags & OPAL_DATATYPE_FLAG_DATA) {
            if ((entry->elem.blocklen < 9) && (!require_counted || (1 < entry->elem.count))) {
                return true;
            }
        }
    }

    return false;
}

/*
 * Build the optimizer mask used when MPI_Pack/MPI_Unpack consolidate
 * (count, oldType) into a temporary contiguous datatype.  The default is to
 * allow all optimizations, then remove the transforms that were measured to
 * hurt the homogeneous CPU path for very small block lengths.
 */
static uint32_t ompi_datatype_consolidate_optimization_mask(const ompi_datatype_t *oldType)
{
    const dt_type_desc_t *desc = (0 != oldType->super.opt_desc.used)
                                     ? &oldType->super.opt_desc
                                     : &oldType->super.desc;
    uint32_t optimization_mask = OPAL_DATATYPE_OPTIMIZE_ALL;

    /*
     * blocklen < 9 is the fixed code-generated inline region in
     * pack_predefined_data().  Around those small copies the extra DATA
     * traversal can cost more than the larger memcpy regions save.
     * Loop-boundary expansion is therefore disabled for any small block.
     * Adjacent-fragment fusion is disabled only when a small block has count > 1,
     * because that transform can turn one compact counted entry into several count == 1 entries.
     */
    if ((NULL != desc->desc) && (0 != desc->used)
        && ompi_datatype_desc_has_small_blocks(desc->desc, 0, desc->used, false)) {
        optimization_mask &= ~OPAL_DATATYPE_OPTIMIZE_LOOP_BOUNDARY;
        if (ompi_datatype_desc_has_small_blocks(desc->desc, 0, desc->used, true)) {
            optimization_mask &= ~OPAL_DATATYPE_OPTIMIZE_ADJACENT_FUSION;
        }
    }

    return optimization_mask;
}

/*
 * MPI_Pack and MPI_Unpack sometimes see a costly (count, datatype) pair
 * where each datatype instance is processed separately.  When the pair is
 * large enough and the datatype is not already a contiguous no-gap layout,
 * build an equivalent temporary datatype and let the convertor process it as
 * (1, larger_datatype).  The specialized optimizer builds the temporary
 * datatype's optimized descriptor from the original already-optimized
 * descriptor, avoiding the full commit/optimization pass over the expanded
 * contiguous description.
 *
 * On success *newType is always valid.  If it is oldType, the caller does not
 * own it.  If it differs from oldType, the caller owns the temporary datatype
 * and must destroy it after use.
 */
int32_t ompi_datatype_consolidate_create(MPI_Count count, const ompi_datatype_t *oldType,
                                         ompi_datatype_t **newType)
{
    ompi_datatype_t *consolidated;
    size_t dtsize;
    uint32_t optimization_mask;
    MPI_Aint lb, extent;
    int rc;

    *newType = (ompi_datatype_t *) oldType;
    if ((0 >= count) || (count < ompi_datatype_consolidate_threshold)) {
        return OMPI_SUCCESS;
    }
    /* A no-gap datatype is already contiguous across counts, so rebuilding it cannot help. */
    if (oldType->super.flags & OPAL_DATATYPE_FLAG_NO_GAPS) {
        return OMPI_SUCCESS;
    }

    opal_datatype_type_size(&oldType->super, &dtsize);
    if (0 == dtsize) {
        return OMPI_SUCCESS;
    }
    rc = ompi_datatype_get_extent(oldType, &lb, &extent);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    if ((oldType->super.flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)
        && ((MPI_Aint) dtsize == extent)) {
        return OMPI_SUCCESS;
    }

    /*
     * The mask is relevant only when we are actually going to build a
     * temporary datatype.  Keep it after the cheap exits so predefined and
     * contiguous no-gap datatypes keep the direct MPI_Pack/MPI_Unpack path.
     */
    optimization_mask = ompi_datatype_consolidate_optimization_mask(oldType);

    rc = ompi_datatype_create_contiguous((size_t) count, oldType, &consolidated);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }

    rc = opal_datatype_optimize_from_contiguous(&consolidated->super, &oldType->super,
                                                (size_t) count, optimization_mask);
    if (OMPI_SUCCESS != rc) {
        ompi_datatype_destroy(&consolidated);
        return rc;
    }
    /*
     * The OPAL helper leaves the temporary datatype uncommitted if it cannot
     * build the cheap optimized descriptor.  In that case, keep using oldType
     * instead of paying for a full commit here.
     */
    if (!(consolidated->super.flags & OPAL_DATATYPE_FLAG_COMMITTED)) {
        ompi_datatype_destroy(&consolidated);
        return OMPI_SUCCESS;
    }

    *newType = consolidated;
    return OMPI_SUCCESS;
}
