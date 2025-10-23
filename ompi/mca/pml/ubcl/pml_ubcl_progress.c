/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2024 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_progress.c
 *
 * UBCL PML progress related functions
 *
 * Functions parameters and return values defined in ompi/mca/pml/pml.h.
 */

#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"

#include <ubcl_api.h>

/**
 * Forward to communication modules. Could use some weight for priority given
 * frequency of call with no event.
 */
int mca_pml_ubcl_progress(void)
{
    if (0 == mca_pml_ubcl_component.nprocs) {
        //return OMPI_ERROR;
        return OMPI_SUCCESS;
    }

    ubcl_progress();
    return OMPI_SUCCESS;
}
