/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_EXECINFO_H
#include <execinfo.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/event/event.h"
#include "opal/util/show_help.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rmgr/rmgr.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"

#if HAVE_SIGNAL_H
#include <signal.h>
#endif

static
int
abort_procs(ompi_proc_t **procs, int proc_count, 
            orte_jobid_t my_jobid)
{
    int i;
    int ret = OMPI_SUCCESS;
    int killret=OMPI_SUCCESS;
    orte_jobid_t jobid;

    for (i = 0 ; i < proc_count ; ++i) {
        if (ORTE_SUCCESS != (ret = orte_ns.get_jobid(&jobid, &(procs[i]->proc_name)))) {
            return ret;
        }
        if (jobid == my_jobid) continue;

        killret = orte_rmgr.terminate_job(jobid);

        if (OMPI_SUCCESS != killret) ret = killret;
    }

    return ret;
}


int
ompi_mpi_abort(struct ompi_communicator_t* comm,
               int errcode,
               bool kill_remote_of_intercomm)
{
    orte_jobid_t my_jobid;
    int ret = OMPI_SUCCESS;
    char hostname[MAXHOSTNAMELEN];
    pid_t pid;
    
    /* Corner case: if we're being called as a result of the
       OMPI_ERR_INIT_FINALIZE macro (meaning that this is before
       MPI_INIT or after MPI_FINALIZE), then just abort nothing MPI or
       ORTE has been setup yet. */

    if (!ompi_mpi_initialized || ompi_mpi_finalized) {
        exit(errcode);
    }

    /* If we're going to print anything, get the hostname and PID of
       this process */

    if (ompi_mpi_abort_print_stack ||
        0 != ompi_mpi_abort_delay) {
        gethostname(hostname, sizeof(hostname));
        pid = getpid();
    }

    /* Should we print a stack trace? */

    if (ompi_mpi_abort_print_stack) {
#if OMPI_WANT_PRETTY_PRINT_STACKTRACE && ! defined(__WINDOWS__) && defined(HAVE_BACKTRACE)
        int i;
        int trace_size;
        void *trace[32];
        char **messages = (char **)NULL;

        trace_size = backtrace(trace, 32);
        messages = backtrace_symbols(trace, trace_size);

        for (i = 0; i < trace_size; ++i) {
            fprintf(stderr, "[%s:%d] [%d] func:%s\n", hostname, (int) pid, 
                    i, messages[i]);
            fflush(stderr);
        }
#endif
    }

    /* Should we wait for a while before aborting? */

    if (0 != ompi_mpi_abort_delay) {
        if (ompi_mpi_abort_delay < 0) {
            fprintf(stderr ,"[%s:%d] Looping forever in MPI abort\n",
                    hostname, (int) pid);
            fflush(stderr);
            while (1) { 
                sleep(5); 
            }
        } else {
            fprintf(stderr, "[%s:%d] Delaying for %d seconds in MPI_abort\n",
                    hostname, (int) pid, ompi_mpi_abort_delay);
            do {
                sleep(1);
            } while (--ompi_mpi_abort_delay > 0);
        }
    }

    /* BWB - XXX - Should probably publish the error code somewhere */

    /* Kill everyone in the job.  We may make this better someday to
       actually loop over ompi_rte_kill_proc() to only kill the procs
       in comm, and additionally to somehow use errorcode. */

    if (ORTE_SUCCESS != (ret = orte_ns.get_jobid(&my_jobid, 
                                                 orte_process_info.my_name))) {
        /* What else can you do? */
        exit(errcode);
    }

    /* kill everyone in the remote group execpt our jobid, if
       requested */
    if (kill_remote_of_intercomm && OMPI_COMM_IS_INTER(comm)) {
        abort_procs(comm->c_remote_group->grp_proc_pointers,
                    comm->c_remote_group->grp_proc_count,
                    my_jobid);
    }

    /* kill everyone in the local group, except our jobid. */
    abort_procs(comm->c_local_group->grp_proc_pointers,
                comm->c_local_group->grp_proc_count,
                my_jobid);

    ret = orte_rmgr.terminate_job(my_jobid);

    if (OMPI_SUCCESS == ret) {
        while (1) {
            /* We should never really get here, since
               ompi_rte_terminate_job shouldn't return until the job
               is actually dead.  But just in case there are some
               race conditions, keep progressing the event loop until
               we get killed */
            if (!OMPI_ENABLE_PROGRESS_THREADS || opal_event_progress_thread()) {
                opal_event_loop(0);
            } else {
                sleep(1000);
            }
        }
    } else {
        /* If ret isn't OMPI_SUCCESS, then the rest of the job is
        still running.  But we can't really do anything about that, so
        just exit and let it become Somebody Elses Problem. */
        exit(errcode);
    }

    return OMPI_SUCCESS;
}
