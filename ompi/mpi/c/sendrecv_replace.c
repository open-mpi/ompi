/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2008 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2015-2021 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
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
#include "ompi/mca/pml/pml.h"
#include "ompi/proc/proc.h"
#include "ompi/memchecker.h"
#include "ompi/runtime/ompi_spc.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak MPI_Sendrecv_replace = PMPI_Sendrecv_replace
#endif
#define MPI_Sendrecv_replace PMPI_Sendrecv_replace
#endif

static const char FUNC_NAME[] = "MPI_Sendrecv_replace";


int MPI_Sendrecv_replace(void * buf, int count, MPI_Datatype datatype,
                         int dest, int sendtag, int source, int recvtag,
                         MPI_Comm comm, MPI_Status *status)

{
    ompi_request_t* req;
    int rc = MPI_SUCCESS;
#if OPAL_ENABLE_FT_MPI
    int rcs = MPI_SUCCESS;
#endif

    SPC_RECORD(OMPI_SPC_SENDRECV_REPLACE, 1);

    MEMCHECKER(
               memchecker_datatype(datatype);
               memchecker_call(&opal_memchecker_base_isdefined, buf, count, datatype);
               memchecker_comm(comm);
               );

    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        OMPI_CHECK_DATATYPE_FOR_RECV(rc, datatype, count);

        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_NOHANDLE_INVOKE(MPI_ERR_COMM, FUNC_NAME);
        } else if (dest != MPI_PROC_NULL && ompi_comm_peer_invalid(comm, dest)) {
            rc = MPI_ERR_RANK;
        } else if (sendtag < 0 || sendtag > mca_pml.pml_max_tag) {
            rc = MPI_ERR_TAG;
        } else if (source != MPI_PROC_NULL && source != MPI_ANY_SOURCE && ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        } else if (((recvtag < 0) && (recvtag !=  MPI_ANY_TAG)) || (recvtag > mca_pml.pml_max_tag)) {
            rc = MPI_ERR_TAG;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

#if OPAL_ENABLE_FT_MPI
    /*
     * The final call to Sendrecv will check for process failures inside
     * So no need to check here.
     */
#endif /* OPAL_ENABLE_FT_MPI */

    /* simple case */
    if ( source == MPI_PROC_NULL || dest == MPI_PROC_NULL || count == 0 ) {
        rc = PMPI_Sendrecv(buf, count, datatype, dest, sendtag, buf, count, datatype, source, recvtag, comm, status);

        return rc;
    }

    /**
     * If we look for an optimal solution, then we should receive the data into a temporary buffer
     * and once the send completes we would unpack back into the original buffer. However, if the
     * sender is unknown, this approach can only be implementing by receiving with the recv datatype
     * (potentially non-contiguous) and thus the allocated memory will be larger than the size of the
     * datatype. A simpler, but potentially less efficient approach is to work on the data we have
     * control of, aka the sent data, and pack it into a contiguous buffer before posting the receive.
     * Once the send completes, we free it.
     */
    opal_convertor_t convertor;
    unsigned char packed_data[2048];
    struct iovec iov = { .iov_base = packed_data, .iov_len = sizeof(packed_data) };
    size_t packed_size, max_data;
    uint32_t iov_count;
    ompi_proc_t* proc = ompi_comm_peer_lookup(comm, dest);
    if(proc == NULL) {
        rc = MPI_ERR_RANK;
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }

    /* initialize convertor to pack send buffer */
    OBJ_CONSTRUCT(&convertor, opal_convertor_t);
    opal_convertor_copy_and_prepare_for_send( proc->super.proc_convertor, &(datatype->super),
                                              count, buf, 0, &convertor );

    /* setup a temporary buffer to send */
    opal_convertor_get_packed_size( &convertor, &packed_size );
    if( packed_size > sizeof(packed_data) ) {
        rc = PMPI_Alloc_mem(packed_size, MPI_INFO_NULL, &iov.iov_base);
        if(OMPI_SUCCESS != rc) {
            rc = OMPI_ERR_OUT_OF_RESOURCE;
            goto cleanup_and_return;
        }
        iov.iov_len = packed_size;
    }
    max_data = packed_size;
    iov_count = 1;
    (void)opal_convertor_pack(&convertor, &iov, &iov_count, &max_data);

    /* receive into the buffer */
    rc = MCA_PML_CALL(irecv(buf, count, datatype,
                            source, recvtag, comm, &req));
    if(OMPI_SUCCESS != rc) {
        goto cleanup_and_return;
    }

    /* send from the temporary buffer */
    rc = MCA_PML_CALL(send(iov.iov_base, packed_size, MPI_PACKED, dest,
                           sendtag, MCA_PML_BASE_SEND_STANDARD, comm));
#if OPAL_ENABLE_FT_MPI
    /* If ULFM is enabled we need to wait for the posted receive to
     * complete, hence we cannot return here */
    rcs = rc;
#else
    if(OMPI_SUCCESS != rc) {
        goto cleanup_and_return;
    }
#endif  /* OPAL_ENABLE_FT_MPI */

    rc = ompi_request_wait(&req, status);
#if OPAL_ENABLE_FT_MPI
    /* Sendrecv_replace never returns ERR_PROC_FAILED_PENDING because it is
     * blocking. Lets complete now that irecv and promote the error
     * to ERR_PROC_FAILED */
    if( OPAL_UNLIKELY(MPI_ERR_PROC_FAILED_PENDING == rc) ) {
        ompi_request_cancel(req);
        ompi_request_wait(&req, MPI_STATUS_IGNORE);
        rc = MPI_ERR_PROC_FAILED;
    }
#endif

#if OPAL_ENABLE_FT_MPI
    if( OPAL_UNLIKELY(MPI_SUCCESS != rcs && MPI_SUCCESS == rc) ) {
        rc = rcs;
    }
#endif

 cleanup_and_return:

    /* release resources */
    if(packed_size > sizeof(packed_data)) {
        PMPI_Free_mem(iov.iov_base);
    }
    OBJ_DESTRUCT(&convertor);

    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}
