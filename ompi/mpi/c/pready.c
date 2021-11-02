/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/mca/part/part.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/runtime/ompi_spc.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Pready = PMPI_Pready
#endif
#define MPI_Pready PMPI_Pready
#endif

static const char FUNC_NAME[] = "MPI_Pready";


int MPI_Pready(int partition, MPI_Request request)
{
    int rc;

    SPC_RECORD(OMPI_SPC_PREADY, 1);

    if (MPI_PARAM_CHECK) {
        rc = OMPI_SUCCESS;

        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (NULL == request || OMPI_REQUEST_PART != request->req_type) {
            rc = MPI_ERR_REQUEST;
        }
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    rc = mca_part.part_pready(partition, partition, request);
    OMPI_ERRHANDLER_RETURN(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
}
