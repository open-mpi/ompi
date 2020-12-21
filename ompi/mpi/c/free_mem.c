/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "opal/mca/mpool/mpool.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Free_mem = PMPI_Free_mem
#endif
#define MPI_Free_mem PMPI_Free_mem
#endif

static const char FUNC_NAME[] = "MPI_Free_mem";


int MPI_Free_mem(void *baseptr)
{
    /* Per these threads:

         https://www.open-mpi.org/community/lists/devel/2007/07/1977.php
         https://www.open-mpi.org/community/lists/devel/2007/07/1979.php

       If you call MPI_ALLOC_MEM with a size of 0, you get NULL
       back.  So don't consider a NULL==baseptr an error. */
    if (NULL != baseptr && OMPI_SUCCESS != mca_mpool_base_free(baseptr)) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM, FUNC_NAME);
    }

    return MPI_SUCCESS;
}

