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
 * Copyright (c) 2006-2007 Cisco Systems, Inc.  All rights reserved.
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
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include "opal/mca/backtrace/backtrace.h"
#include "orte/util/proc_info.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/params.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/sys_info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/proc/proc.h"
#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"

static bool have_been_invoked = false;

int
ompi_mpi_abort(struct ompi_communicator_t* comm,
               int errcode,
               bool kill_remote_of_intercomm)
{
    int count = 0, i, ret = OMPI_SUCCESS;
    char *host, hostname[MAXHOSTNAMELEN];
    pid_t pid = 0;
    orte_process_name_t *abort_procs;
    orte_std_cntr_t nabort_procs;

    /* Protection for recursive invocation */
    if (have_been_invoked) {
        return OMPI_SUCCESS;
    }
    have_been_invoked = true;

    /* If ORTE is initialized, use its nodename.  Otherwise, call
       gethostname. */

    if (orte_initialized) {
        host = orte_system_info.nodename;
    } else {
        gethostname(hostname, sizeof(hostname));
        host = hostname;
    }
    pid = getpid();

    /* Corner case: if we're being called as a result of the
       OMPI_ERR_INIT_FINALIZE macro (meaning that this is before
       MPI_INIT or after MPI_FINALIZE), then just abort nothing MPI or
       ORTE has been setup yet. */

    if (!ompi_mpi_initialized || ompi_mpi_finalized) {
        if (orte_initialized) {
            orte_errmgr.error_detected(errcode, NULL);
        }
    }

    /* Should we print a stack trace? */

    if (ompi_mpi_abort_print_stack) {
        char **messages;
        int len, i;

        if (OMPI_SUCCESS == opal_backtrace_buffer(&messages, &len)) {
            for (i = 0; i < len; ++i) {
                fprintf(stderr, "[%s:%d] [%d] func:%s\n", host, (int) pid, 
                        i, messages[i]);
                fflush(stderr);
            }
            free(messages);
        } else {
            /* This will print an message if it's unable to print the
               backtrace, so we don't need an additional "else" clause
               if opal_backtrace_print() is not supported. */
            opal_backtrace_print(stderr);
        }
    }

    /* Should we wait for a while before aborting? */

    if (0 != ompi_mpi_abort_delay) {
        if (ompi_mpi_abort_delay < 0) {
            fprintf(stderr ,"[%s:%d] Looping forever (MCA parameter mpi_abort_delay is < 0)\n",
                    host, (int) pid);
            fflush(stderr);
            while (1) { 
                sleep(5); 
            }
        } else {
            fprintf(stderr, "[%s:%d] Delaying for %d seconds before aborting\n",
                    host, (int) pid, ompi_mpi_abort_delay);
            do {
                sleep(1);
            } while (--ompi_mpi_abort_delay > 0);
        }
    }

    /* If ORTE isn't setup yet, then don't even try killing everyone.
       Sorry, Charlie... */

    if (!orte_initialized) {
        fprintf(stderr, "[%s:%d] Abort before MPI_INIT completed successfully; not able to guarantee that all other processes were killed!\n",
                host, (int) pid);
        exit(errcode);
    }

    /* abort local procs in the communicator.  If the communicator is
       an intercommunicator AND the abort has explicitly requested
       that we abort the remote procs, then do that as well. */
    nabort_procs = ompi_comm_size(comm);

    if (kill_remote_of_intercomm) {
        /* ompi_comm_remote_size() returns 0 if not an intercomm, so
           this is cool */
        nabort_procs += ompi_comm_remote_size(comm);
    }

    abort_procs = (orte_process_name_t*)malloc(sizeof(orte_process_name_t) * nabort_procs);
    if (NULL == abort_procs) {
        /* quick clean orte and get out */
        orte_errmgr.error_detected(errcode, 
                                   "Abort unable to malloc memory to kill procs", 
                                   NULL);
    }

    /* put all the local procs in the abort list */
    for (i = 0 ; i < ompi_comm_size(comm) ; ++i) {
        if (ORTE_EQUAL != orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                 &comm->c_local_group->grp_proc_pointers[i]->proc_name,
                                 orte_process_info.my_name)) {
            assert(count <= nabort_procs);
            abort_procs[count++] = comm->c_local_group->grp_proc_pointers[i]->proc_name;
        } else {
            /* don't terminate me just yet */
            nabort_procs--;
        }
    }

    /* if requested, kill off remote procs too */
    if (kill_remote_of_intercomm) {
        for (i = 0 ; i < ompi_comm_remote_size(comm) ; ++i) {
            if (ORTE_EQUAL != orte_ns.compare_fields(ORTE_NS_CMP_ALL, 
                                     &comm->c_remote_group->grp_proc_pointers[i]->proc_name,
                                     orte_process_info.my_name)) {
                assert(count <= nabort_procs);
                abort_procs[count++] =
                    comm->c_remote_group->grp_proc_pointers[i]->proc_name;
            } else {
                /* don't terminate me just yet */
                nabort_procs--;
            }
        }
    }

    if (nabort_procs > 0) {
        ret = orte_errmgr.abort_procs_request(abort_procs, nabort_procs);
        if (OMPI_SUCCESS != ret) {
            orte_errmgr.error_detected(ret, 
                                       "Open MPI failed to abort procs as requested (%d). Exiting.",
                                       ret, NULL);
        }
    }

    /* now that we've aborted everyone else, gracefully die. */
    orte_errmgr.error_detected(errcode, NULL);
    
    return OMPI_SUCCESS;
}
