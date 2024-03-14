/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2023      Jeffrey M. Squyres.  All rights reserved.
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
#include "ompi/mca/pml/pml.h"
#include "ompi/memchecker.h"
#include "ompi/request/request.h"
#include "ompi/runtime/ompi_spc.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Recv = PMPI_Recv
#endif
#define MPI_Recv PMPI_Recv
#endif

static const char FUNC_NAME[] = "MPI_Recv";


int MPI_Recv(void *buf, int count, MPI_Datatype type, int source,
             int tag, MPI_Comm comm, MPI_Status *status)
{
    int rc = MPI_SUCCESS;

    SPC_RECORD(OMPI_SPC_RECV, 1);

    MEMCHECKER(
        memchecker_datatype(type);
        memchecker_call(&opal_memchecker_base_isaddressable, buf, count, type);
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        OMPI_CHECK_DATATYPE_FOR_RECV(rc, type, count);
        OMPI_CHECK_USER_BUFFER(rc, buf, type, count);

        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_COMM, FUNC_NAME);
        } else if (((tag < 0) && (tag != MPI_ANY_TAG)) || (tag > mca_pml.pml_max_tag)) {
            rc = MPI_ERR_TAG;
        } else if ((source != MPI_ANY_SOURCE) &&
                   (MPI_PROC_NULL != source) &&
                   ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        }

        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

#if OPAL_ENABLE_FT_MPI
    /*
     * An early check, so as to return early if we are communicating with
     * a failed process. This is not absolutely necessary since we will
     * check for this, and other, error conditions during the completion
     * call in the PML.
     */
    if( OPAL_UNLIKELY(!ompi_comm_iface_p2p_check_proc(comm, source, &rc)) ) {
        if (MPI_STATUS_IGNORE != status) {
            status->MPI_SOURCE = source;
            status->MPI_TAG    = tag;
        }
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }
#endif

    if (MPI_PROC_NULL == source) {
        if (MPI_STATUS_IGNORE != status) {
            OMPI_COPY_STATUS(status, ompi_request_empty.req_status, false);
            /*
             * Per MPI-1, the MPI_ERROR field is not defined for single-completion calls
             */
            MEMCHECKER(
                opal_memchecker_base_mem_undefined(&status->MPI_ERROR, sizeof(int));
            );
        }
        return MPI_SUCCESS;
    }

    rc = MCA_PML_CALL(recv(buf, count, type, source, tag, comm, status));
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}
