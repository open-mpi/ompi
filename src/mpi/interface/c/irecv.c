/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "mpi/runtime/runtime.h"
#include "mpi/interface/c/bindings.h"
#include "mca/mpi/pml/pml.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Irecv = PMPI_Irecv
#endif

int MPI_Irecv(void *buf, int count, MPI_Datatype type, int source,
              int tag, MPI_Comm comm, MPI_Request *request) 
{
    if (source == MPI_PROC_NULL) {
        return mca_pml.pml_null(request);
    }

    if ( MPI_PARAM_CHECK ) {
        int rc = MPI_SUCCESS;
        if (lam_mpi_finalized) {
            rc = MPI_ERR_INTERN;
        } else if (count < 0) {
            rc = MPI_ERR_COUNT;
#if 0
        } else if (type == MPI_DATATYPE_NULL) {
            rc = MPI_ERR_TYPE;
#endif
        } else if (tag < 0 || tag > MPI_TAG_UB_VALUE) {
            rc = MPI_ERR_TAG;
        } else if (lam_comm_invalid(comm)) {
            rc = MPI_ERR_COMM;
        } else if (source != MPI_ANY_SOURCE && source != MPI_PROC_NULL && lam_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        }
        if (rc != MPI_SUCCESS) {
            return rc;
        }
    }

    return mca_pml.pml_irecv(buf,count,type,source,tag,comm,request);
}

