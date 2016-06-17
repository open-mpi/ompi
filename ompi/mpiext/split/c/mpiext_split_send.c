/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#include "ompi/mpiext/split/c/mpiext_split_c.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak OMPI_Split_send = POMPI_Split_send
#endif
#define OMPI_Split_send POMPI_Split_send
#endif

static const char FUNC_NAME[] = "OMPI_Split_send";


int OMPI_Split_send(const void *buf, int count, MPI_Datatype type, int dest,
                   int tag, MPI_Comm comm)
{
    int rc = MPI_SUCCESS;
    opal_convertor_t convertor;
    size_t offset;
    size_t size;

    MEMCHECKER(
        memchecker_datatype(type);
        memchecker_call(&opal_memchecker_base_isdefined, buf, count, type);
        memchecker_comm(comm);
    );

    if ( MPI_PARAM_CHECK ) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (ompi_comm_invalid(comm)) {
            return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_COMM, FUNC_NAME);
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
        } else if (tag < 0 || tag > mca_pml.pml_max_tag) {
            rc = MPI_ERR_TAG;
        } else if (ompi_comm_peer_invalid(comm, dest) &&
                   (MPI_PROC_NULL != dest)) {
            rc = MPI_ERR_RANK;
        } else {
            OMPI_CHECK_DATATYPE_FOR_SEND(rc, type, count);
            OMPI_CHECK_USER_BUFFER(rc, buf, type, count);
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, FUNC_NAME);
    }

    if (MPI_PROC_NULL == dest) {
        return MPI_SUCCESS;
    }

    assert(count > 0);
    if (count > 0) {
        ompi_proc_t* proc = ompi_comm_peer_lookup(comm,dest);
        OBJ_CONSTRUCT(&convertor, opal_convertor_t);
        convertor.stack_pos = -1;
        /* We will create a convertor specialized for the        */
        /* remote architecture and prepared with the type.       */
        opal_convertor_copy_and_prepare_for_send(
                           proc->super.proc_convertor,
                           &(type->super),
                           count,
                           buf,
                           0,
                           &convertor );
        opal_convertor_get_packed_size( &convertor, &size );
    }
    size = size / 2;
    offset = 0;
    opal_convertor_set_position(&convertor, &offset);
    OPAL_CR_ENTER_LIBRARY();
    rc = MCA_PML_CALL(csend(&convertor, &size, dest, tag, MCA_PML_BASE_SEND_STANDARD, comm));
    if (OMPI_SUCCESS != rc) {
        OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
    }
    offset += size;
    opal_convertor_set_position(&convertor, &offset);
    rc = MCA_PML_CALL(csend(&convertor, &size, dest, tag, MCA_PML_BASE_SEND_STANDARD, comm));
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, FUNC_NAME);
}
