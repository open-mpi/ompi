/*
 * $HEADERS$
 */
#include "lam_config.h"
#include <stdio.h>

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Bsend = PMPI_Bsend
#endif


int MPI_Bsend(void *buf, int count, MPI_Datatype type, int dest, int tag, MPI_Comm comm) 
{
    int rc;
    if (dest == MPI_PROC_NULL) {
        return MPI_SUCCESS;
    }
   
    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        if ( LAM_MPI_INVALID_STATE ) {
            rc = MPI_ERR_INTERN;
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
        } else if (type == MPI_DATATYPE_NULL) {
            rc = MPI_ERR_TYPE;
        } else if (tag < 0 || tag > MPI_TAG_UB_VALUE) {
            rc = MPI_ERR_TAG;
        } else if (lam_comm_invalid(comm)) {
            rc = MPI_ERR_COMM;
        } else if (lam_comm_peer_invalid(comm, dest)) {
            rc = MPI_ERR_RANK;
        }
        LAM_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Bsend");
    }

    rc = mca_pml.pml_send(buf, count, type, dest, tag, MCA_PML_BASE_SEND_BUFFERED, comm);
    LAM_ERRHANDLER_RETURN(rc, comm, rc, "MPI_Bsend");
}

