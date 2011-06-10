/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpiext/mpiext.h"
#include "ompi/mpiext/example/mpiext_example_c.h"

static const char FUNC_NAME[] = "OMPI_Progress";

/* 
 * The init/fini functions and the component struct are not required,
 * but optional.  If an extension would like to have init/fini, in
 * addition to providing the hooks below, adding the line in
 * configure.m4 (documented in example's configure.m4) is also
 * required.
*/
static int
example_init(void)
{
    printf("example mpiext init\n");
    return OMPI_SUCCESS;
}

static int
example_fini(void)
{
    printf("example mpiext fini\n");
    return OMPI_SUCCESS;
}

ompi_mpiext_component_t ompi_mpiext_example = {
    example_init,
    example_fini
};


int OMPI_Progress(int count) 
{
    printf("Count = %d!\n", count);

    return MPI_SUCCESS;
}

