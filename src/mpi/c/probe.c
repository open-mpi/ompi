/*
 * $HEADERS$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "mca/pml/pml.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Probe = PMPI_Probe
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Probe";


OMPI_EXPORT
int MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status) 
{
    int rc;
    if (source == MPI_PROC_NULL) {
        if (status) {
            status->MPI_SOURCE = MPI_PROC_NULL;
            status->MPI_TAG = MPI_ANY_TAG;
            status->MPI_ERROR = MPI_SUCCESS;
            status->_count = 0;
        }
        return MPI_SUCCESS;
    }

    if ( MPI_PARAM_CHECK ) {
        rc = MPI_SUCCESS;
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
        if (tag < 0 || tag > MPI_TAG_UB_VALUE) {
            rc = MPI_ERR_TAG;
        } else if (ompi_comm_invalid(comm)) {
            rc = MPI_ERR_COMM;
        } else if (source != MPI_ANY_SOURCE && ompi_comm_peer_invalid(comm, source)) {
            rc = MPI_ERR_RANK;
        }
        OMPI_ERRHANDLER_CHECK(rc, comm, rc, "MPI_Probe");
    }

    rc = mca_pml.pml_probe(source, tag, comm, status);
    OMPI_ERRHANDLER_RETURN(rc, comm, rc, "MPI_Probe");
}
