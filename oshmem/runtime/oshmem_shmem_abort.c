/*
 *  Copyright (c) 2013      Mellanox Technologies, Inc.
 *                          All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"

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
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"

#include "oshmem/runtime/params.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/constants.h"
#include "oshmem/proc/proc.h"

static bool have_been_invoked = false;

int oshmem_shmem_abort(int errcode)
{
    char *host, hostname[MAXHOSTNAMELEN];
    pid_t pid = 0;

    /* Protection for recursive invocation */
    if (have_been_invoked) {
        return OSHMEM_SUCCESS;
    }
    have_been_invoked = true;

    /* If ORTE is initialized, use its nodename.  Otherwise, call
     gethostname. */

    if (orte_initialized) {
        host = orte_process_info.nodename;
    } else {
        gethostname(hostname, sizeof(hostname));
        host = hostname;
    }
    pid = getpid();

    orte_show_help("help-shmem-api.txt",
                   "shmem-abort",
                   true,
                   ORTE_PROC_MY_NAME->vpid,
                   pid,
                   host,
                   errcode);

    /* Should we print a stack trace?  Not aggregated because they
     might be different on all processes. */
    if (ompi_mpi_abort_print_stack) {
        char **messages;
        int len, i;

        if (OSHMEM_SUCCESS == opal_backtrace_buffer(&messages, &len)) {
            for (i = 0; i < len; ++i) {
                fprintf(stderr,
                        "[%s:%d] [%d] func:%s\n",
                        host,
                        (int) pid,
                        i,
                        messages[i]);
                fflush(stderr);
            }
            free(messages);
        } else {
            /* This will print an message if it's unable to print the
             backtrace, so we don't need an additional "else" clause
             if opal_backtrace_print() is not supported. */
            opal_backtrace_print(stderr, NULL, 1);
        }
    }

    if (!orte_initialized || !oshmem_shmem_initialized) {
        if (orte_show_help_is_available()) {
            /* TODO help message from SHMEM not from MPI is needed*/
            orte_show_help("help-shmem-runtime.txt",
                           "oshmem shmem abort:cannot guarantee all killed",
                           true,
                           host,
                           (int) pid);
        } else {
            fprintf(stderr,
                    "[%s:%d] Local abort completed successfully; not able to aggregate error messages, and not able to guarantee that all other processes were killed!\n",
                    host,
                    (int) pid);
        }
        oshmem_shmem_aborted = true;
        exit(errcode);
    }

    /* abort local procs in the communicator.  If the communicator is
     an intercommunicator AND the abort has explicitly requested
     that we abort the remote procs, then do that as well. */

    oshmem_shmem_aborted = true;
    /* now that we've aborted everyone else, gracefully die. */

    orte_errmgr.abort(errcode, NULL );

    return OSHMEM_SUCCESS;
}
