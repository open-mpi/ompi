/*
 * $HEADER$
 */
#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>

#include "mpi.h"
#include "mpi/c/bindings.h"
#include "util/show_help.h"
#include "util/proc_info.h"
#include "runtime/runtime.h"
#include "communicator/communicator.h"
#include "errhandler/errhandler.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"

#if OMPI_HAVE_WEAK_SYMBOLS && OMPI_PROFILING_DEFINES
#pragma weak MPI_Abort = PMPI_Abort
#endif

#if OMPI_PROFILING_DEFINES
#include "mpi/c/profile/defines.h"
#endif

static const char FUNC_NAME[] = "MPI_Abort";


int MPI_Abort(MPI_Comm comm, int errorcode) 
{
    mca_ns_base_jobid_t jobid;

    /* Don't even bother checking comm and errorcode values for
       errors */

    if (MPI_PARAM_CHECK) {
        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
    }
    
    /* Kill everyone in the job.  We may make this better someday to
       actually loop over ompi_rte_kill_proc() to only kill the procs
       in comm, and additionally to somehow use errorcode. */

    jobid = ompi_name_server.get_jobid(ompi_process_info.name);
    ompi_rte_kill_job(jobid, 0);

    /* If we return from this, then the selected PCM was unable to
       kill the job (and the rte printed an error message).  So just
       die die die. */

    abort();
}
