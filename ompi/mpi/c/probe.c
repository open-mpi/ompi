/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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
#include "ompi/mca/pml/pml.h"
#include "ompi/memchecker.h"
#include "ompi/request/request.h"

#if OPAL_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Probe = PMPI_Probe
#endif

#if OMPI_PROFILING_DEFINES
#include "ompi/mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Probe";


int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status)
{
    int rc;

    MEMCHECKER(
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (((tag < 0) && (tag != MPI_ANY_TAG)) || (tag > mca_pml.pml_max_tag)) {
            rc = MPI_ERR_TAG;
        } else if (ompi_comm_invalid(comm)) {
            rc = MPI_ERR_COMM;
        } else if ((source != MPI_ANY_SOURCE) &&
                   (MPI_PROC_NULL != source) &&
                   ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Probe");
    }

    if (MPI_PROC_NULL == source) {
        if (MPI_STATUS_IGNORE != status) {
            *status = ompi_request_empty.req_status;
            /*
             * Per MPI-1, the MPI_ERROR field is not defined for single-completion calls
             */
            MEMCHECKER(
                opal_memchecker_base_mem_undefined(&status->MPI_ERROR, sizeof(int));
            );
        }
        return MPI_SUCCESS;
    }

    OPAL_CR_ENTER_LIBRARY();

    rc = MCA_PML_CALL(probe(source, tag, comm, status));
    /*
     * Per MPI-1, the MPI_ERROR field is not defined for single-completion calls
     */
    MEMCHECKER(
        opal_memchecker_base_mem_undefined(&status->MPI_ERROR, sizeof(int));
    );

    OMPI_ERRHANDLER_RETURN(rc, comm, rc, "MPI_Probe");
}
