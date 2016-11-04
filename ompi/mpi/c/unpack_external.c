/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2016 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Unpack_external = PMPI_Unpack_external
#endif
#define MPI_Unpack_external PMPI_Unpack_external
#endif

static const char FUNC_NAME[] = "MPI_Unpack_external";


int MPI_Unpack_external (const char datarep[], const void *inbuf, MPI_Aint insize,
                         MPI_Aint *position, void *outbuf, int outcount,
                         MPI_Datatype datatype)
{
    int rc = MPI_SUCCESS;

    MEMCHECKER(
        memchecker_datatype(datatype);
        memchecker_call(&opal_memchecker_base_isdefined, outbuf, outcount, datatype);
    );

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if ((NULL == inbuf) || (NULL == position)) {  /* outbuf can be MPI_BOTTOM */
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_ARG, FUNC_NAME);
        } else if (outcount < 0) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COUNT, FUNC_NAME);
        }
        OMPI_CHECK_DATATYPE_FOR_RECV(rc, datatype, outcount);
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
        OMPI_CHECK_USER_BUFFER(rc, outbuf, datatype, outcount);
        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
    }

    OPAL_CR_ENTER_LIBRARY();

    rc = ompi_datatype_unpack_external(datarep, inbuf, insize,
                                       position, outbuf, outcount,
                                       datatype);

    OPAL_CR_EXIT_LIBRARY();

    OMPI_ERRHANDLER_RETURN((OMPI_SUCCESS == rc) ? OMPI_SUCCESS : OMPI_ERROR,
                           MPI_COMM_WORLD, rc, FUNC_NAME);
}
