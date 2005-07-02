/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
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

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "communicator/communicator.h"
#include "datatype/datatype.h"
#include "datatype/convertor.h"
#include "errhandler/errhandler.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Sendrecv_replace = PMPI_Sendrecv_replace
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Sendrecv_replace";


int MPI_Sendrecv_replace(void * buf, int count, MPI_Datatype datatype,
                         int dest, int sendtag, int source, int recvtag,
                         MPI_Comm comm, MPI_Status *status) 

{
    int rc;
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
        } else if (datatype == MPI_DATATYPE_NULL) {
            rc = MPI_ERR_TYPE;
        } else if (dest != MPI_PROC_NULL && ompi_comm_peer_invalid(comm, dest)) {
            rc = MPI_ERR_RANK;
        } else if (sendtag < 0 || sendtag > MPI_TAG_UB_VALUE) {
            rc = MPI_ERR_TAG;
        } else if (source != MPI_PROC_NULL && source != MPI_ANY_SOURCE && ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        } else if (((recvtag < 0) && (recvtag !=  MPI_ANY_TAG)) || (recvtag > MPI_TAG_UB_VALUE)) {
            rc = MPI_ERR_TAG;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }
 
    /* simple case */
    if ( source == MPI_PROC_NULL || dest == MPI_PROC_NULL || count == 0 ) {
        return MPI_Sendrecv(buf,count,datatype,dest,sendtag,buf,count,datatype,source,recvtag,comm,status);
    } else {

        ompi_convertor_t convertor;
        struct iovec iov;
        unsigned char recv_data[2048];
        size_t packed_size, max_data;
        uint32_t iov_count;
        int free_after;
        ompi_status_public_t recv_status;
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm,dest);
        if(proc == NULL) {
            rc = MPI_ERR_RANK;
            OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
        }

        /* initialize convertor to unpack recv buffer */
        OBJ_CONSTRUCT(&convertor, ompi_convertor_t);
        ompi_convertor_copy_and_prepare_for_recv( proc->proc_convertor, datatype,
                                                  count, buf, &convertor );

        /* setup a buffer for recv */
        ompi_convertor_get_packed_size( &convertor, &packed_size );
        if( packed_size > sizeof(recv_data) ) {
            iov.iov_base = (caddr_t)malloc(packed_size);
            if(iov.iov_base == NULL) {
                OMPI_ERRHANDLER_RETURN(OMPI_ERR_OUT_OF_RESOURCE, comm, MPI_ERR_BUFFER, FUNC_NAME);
            }
        } else {
            iov.iov_base = (caddr_t)recv_data;
        }

        /* recv into temporary buffer */
        rc = MPI_Sendrecv( buf, count, datatype, dest, sendtag, iov.iov_base, packed_size, 
                           MPI_BYTE, source, recvtag, comm, &recv_status );
        if (rc != MPI_SUCCESS) {
            if(packed_size > sizeof(recv_data))
                free(iov.iov_base);
            OBJ_DESTRUCT(&convertor);
            OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
        }

        /* unpack into users buffer */
        iov.iov_len = recv_status._count;
        iov_count = 1;
        max_data = recv_status._count;
        ompi_convertor_unpack(&convertor, &iov, &iov_count, &max_data, &free_after);

        /* return status to user */
        if(status != MPI_STATUS_IGNORE) {
            *status = recv_status;
        }

        /* release resources */
        if(packed_size > sizeof(recv_data)) {
            free(iov.iov_base);
        }
        OBJ_DESTRUCT(&convertor);
        return MPI_SUCCESS;
    }
}

