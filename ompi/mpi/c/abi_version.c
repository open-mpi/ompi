/*
 * Copyright (c) 2023 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>

#include "opal/util/show_help.h"
#include "ompi/runtime/ompi_spc.h"
#include "ompi/mpi/c/bindings.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/constants.h"
#ifdef OMPI_NO_MPI_PROTOTYPES
#include "ompi/mpi/c/abi.h"
#endif

int MPI_Abi_version(int *abi_major, int *abi_minor)
{
    /* 0.1 */
    *abi_major = 0;
    *abi_minor = 1;
    return MPI_SUCCESS;
}
