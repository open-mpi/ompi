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

static
int
abort_procs(ompi_proc_t **procs, int proc_count, 
            mca_ns_base_jobid_t my_jobid)
{
    int i;
    int ret = OMPI_SUCCESS;
    int killret;
    mca_ns_base_jobid_t jobid;

    for (i = 0 ; i < proc_count ; ++i) {
        jobid = ompi_name_server.get_jobid(&(procs[i]->proc_name));
        if (jobid == my_jobid) continue;

        killret = ompi_rte_terminate_job(jobid, 0);
        if (OMPI_SUCCESS != killret) ret = killret;
    }

    return ret;
}


int
ompi_mpi_abort(struct ompi_communicator_t* comm,
               int errcode,
               bool kill_remote_of_intercomm)
{
    mca_ns_base_jobid_t my_jobid;
    int ret;
    
    /* BWB - XXX - Should probably publish the error code somewhere */

    /* Kill everyone in the job.  We may make this better someday to
       actually loop over ompi_rte_kill_proc() to only kill the procs
       in comm, and additionally to somehow use errorcode. */

    my_jobid = ompi_name_server.get_jobid(ompi_rte_get_self());

    /* kill everyone in the remote group execpt our jobid, if requested */
    if (kill_remote_of_intercomm && OMPI_COMM_IS_INTER(comm)) {
        abort_procs(comm->c_remote_group->grp_proc_pointers,
                    comm->c_remote_group->grp_proc_count,
                    my_jobid);
    }

    /* kill everyone in the local group, except our jobid */
    abort_procs(comm->c_local_group->grp_proc_pointers,
                comm->c_local_group->grp_proc_count,
                my_jobid);

    ret = ompi_rte_terminate_job(my_jobid, 0);

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
