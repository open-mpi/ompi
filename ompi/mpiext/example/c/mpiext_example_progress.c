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
 * This file contains the C implementation of the OMPI_Progress
 * function.  It has no file naming convention, and generally contains
 * whatever the extension needs it to.
 */

#include "ompi_config.h"

#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/mpiext/mpiext.h"
#include "ompi/mpiext/example/c/mpiext_example_c.h"

static const char FUNC_NAME[] = "OMPI_Progress";

/*
 * Global variable from this extension
 */
int OMPI_Example_global = 42;

/*
 * Just to make the extension "interesting", we pass in an integer and
 * an MPI handle.
 */
int OMPI_Progress(int count, MPI_Comm comm) 
{
    char name[MPI_MAX_OBJECT_NAME];
    int len;

    /* Just as an example, get the name of the communicator and print
       it out.  Use the PMPI name when possible so that these
       invocations don't show up in profiling tools. */
#if OMPI_ENABLE_MPI_PROFILING
    PMPI_Comm_get_name(comm, name, &len);
#else
    MPI_Comm_get_name(comm, name, &len);
#endif

    printf("Count = %d, comm = %s\n", count, name);

    return MPI_SUCCESS;
}

