/*
 * $HEADER$
 */

#include "lam_config.h"

#include "mpi.h"
#include "runtime/runtime.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"


#if LAM_HAVE_WEAK_SYMBOLS && LAM_PROFILING_DEFINES
#pragma weak MPI_Irecv = PMPI_Irecv
#endif

int MPI_Irecv(void *buf, int count, MPI_Datatype type, int source,
              int tag, MPI_Comm comm, MPI_Request *request) 
{
    int rc;
    if (source == MPI_PROC_NULL) {
        return mca_pml.pml_null(request);
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
        } else if (source != MPI_ANY_SOURCE && 
                   source != MPI_PROC_NULL &&  
                   lam_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        }
        LAM_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Irecv");
    }

    rc = mca_pml.pml_irecv(buf,count,type,source,tag,comm,request);
    LAM_ERRHANDLER_RETURN(rc, comm, rc, "MPI_Irecv");
}

