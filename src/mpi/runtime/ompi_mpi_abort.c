/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <unistd.h>

#include "communicator/communicator.h"
#include "util/show_help.h"
#include "util/proc_info.h"
#include "runtime/runtime.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "event/event.h"

#if HAVE_SIGNAL_H
#include <signal.h>
#endif

int
ompi_mpi_abort(struct ompi_communicator_t* comm,
               int errcode,
               bool kill_remote_of_intercomm)
{
    mca_ns_base_jobid_t jobid;
    int ret;
    
    /* XXX - Should probably publish the error code somewhere */

    /* Kill everyone in the job.  We may make this better someday to
       actually loop over ompi_rte_kill_proc() to only kill the procs
       in comm, and additionally to somehow use errorcode. */

    jobid = ompi_name_server.get_jobid(ompi_rte_get_self());
    ret = ompi_rte_terminate_job(jobid, 0);

    if (OMPI_SUCCESS == ret) {
        while (1) {
            /* We should never really get here, since
               ompi_rte_terminate_job shouldn't return until the job
               is actually dead.  But just in case there are some
               race conditions, keep progressing the event loop until
               we get killed */
            if (!OMPI_HAVE_THREADS || ompi_event_progress_thread()) {
                ompi_event_loop(0);
            } else {
                sleep(1000);
            }
        }
    } else {
        /* If ret isn't OMPI_SUCCESS, then the rest of the job is
        still running.  But we can't really do anything about that, so
        just exit and let it become Somebody Elses Problem. */
        abort();
    }

    return OMPI_SUCCESS;
}
