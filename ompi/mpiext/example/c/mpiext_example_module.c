/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/*
 * This file contains the implementation of the OMPI_Progress
 * initialize / finalize hooks.  It has no file naming convention, and
 * generally contains whatever the extension needs it to.
 */

#include "ompi_config.h"

#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpiext/mpiext.h"
#include "ompi/mpiext/example/c/mpiext_example_c.h"

/* 
 * The init/fini functions and the component struct are not required,
 * but optional.  If an extension would like to have init/fini, in
 * addition to providing the hooks below, adding the line in
 * configure.m4 (documented in example's configure.m4) is also
 * required.
 */
static int example_init(void)
{
    printf("example mpiext init\n");
    return OMPI_SUCCESS;
}

static int example_fini(void)
{
    printf("example mpiext fini\n");
    return OMPI_SUCCESS;
}

/*
 * Similar to Open MPI components, a well-known struct provides
 * function pointers to the extension's init/fini hooks.  The struct
 * must be a global symbol of the form ompi_mpiext_<ext_name> and be
 * of type ompi_mpiext_component_t.
 */
ompi_mpiext_component_t ompi_mpiext_example = {
    example_init,
    example_fini
};
