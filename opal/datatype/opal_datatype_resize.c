/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/datatype/opal_datatype_internal.h"

int32_t opal_datatype_resize(opal_datatype_t *type, ptrdiff_t lb, ptrdiff_t extent)
{
    type->lb = lb;
    type->ub = lb + extent;

    type->flags &= ~OPAL_DATATYPE_FLAG_NO_GAPS;
    /* MPI_Type_create_resized installs explicit lower- and upper-bound markers on the
     * type.  Per the MPI standard these markers define the extent, propagate into any
     * enclosing derived datatype, and suppress the alignment epsilon.  This is the
     * bound-marker *concept*, which MPI-3.0 retained through MPI_Type_create_resized; it
     * is independent of the removed MPI_LB/MPI_UB pseudo-type *handles*, which stay
     * unavailable at the MPI layer unless the build opts into MPI-1 compatibility. */
    type->flags |= OPAL_DATATYPE_FLAG_USER_LB;
    type->flags |= OPAL_DATATYPE_FLAG_USER_UB;
    if ((extent == (ptrdiff_t) type->size) && (type->flags & OPAL_DATATYPE_FLAG_CONTIGUOUS)) {
        type->flags |= OPAL_DATATYPE_FLAG_NO_GAPS;
    }
    return OPAL_SUCCESS;
}
