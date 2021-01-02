/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
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
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/info/info.h"
#include "opal/mca/mpool/mpool.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Alloc_mem = PMPI_Alloc_mem
#endif
#define MPI_Alloc_mem PMPI_Alloc_mem
#endif

static const char FUNC_NAME[] = "MPI_Alloc_mem";


int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
{
    opal_cstring_t *info_str = NULL;
    const char *mpool_hints = NULL;

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (size < 0 || NULL == baseptr) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_ARG,
                                          FUNC_NAME);
        } else if (NULL == info || ompi_info_is_freed(info)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_INFO,
                                          FUNC_NAME);
        }
    }

    /* Per these threads:

         https://www.open-mpi.org/community/lists/devel/2007/07/1977.php
         https://www.open-mpi.org/community/lists/devel/2007/07/1979.php

       If you call MPI_ALLOC_MEM with a size of 0, you get NULL
       back .*/
    if (0 == size) {
        *((void **) baseptr) = NULL;
        return MPI_SUCCESS;
    }

    if (MPI_INFO_NULL != info) {
        int flag;
        (void) ompi_info_get (info, "mpool_hints", &info_str, &flag);
        if (flag) {
            mpool_hints = info_str->string;
        }
    }

    *((void **) baseptr) = mca_mpool_base_alloc ((size_t) size, (struct opal_info_t*)info,
                                                 mpool_hints);

    if (NULL != info_str) {
        OBJ_RELEASE(info_str);
    }

    if (NULL == *((void **) baseptr)) {
        return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_NO_MEM,
                                      FUNC_NAME);
    }

    /* All done */
    return MPI_SUCCESS;
}

