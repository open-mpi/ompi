/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
    
    /* Kill everyone in the job.  We may make this better someday to
       actually loop over ompi_rte_kill_proc() to only kill the procs
       in comm, and additionally to somehow use errorcode. */

    jobid = ompi_name_server.get_jobid(ompi_rte_get_self());
    ret = ompi_rte_kill_job(jobid, SIGTERM, errcode, 0);

    if (1 /* BWB - fix me */) {
        while (1) {
            /* we successfully started the kill.  Just sit around and
               wait to be slaughtered.  run the event loop if we
               should */
            if (!OMPI_HAVE_THREADS || ompi_event_progress_thread()) {
                ompi_event_loop(0);
            } else {
                sleep(1000);
            }
        }
    } else {
        /* If we return from this, then the selected PCM was unable to
           kill the job (and the rte printed an error message).  So
           just die die die. */
        abort();
    }

    return OMPI_SUCCESS;
}
