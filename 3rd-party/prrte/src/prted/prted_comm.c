/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009      Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2019 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <time.h>

#include "src/event/event-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/pmix/pmix-internal.h"
#include "src/prted/pmix/pmix_server.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_path.h"
#include "src/util/pmix_environ.h"

#include "src/util/name_fns.h"
#include "src/util/nidmap.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/oob/base/base.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/plm/plm.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"

#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/prte_wait.h"
#include "src/runtime/runtime.h"

#include "src/prted/prted.h"

/*
 * Globals
 */
static char *get_prted_comm_cmd_str(int command);

static void _notify_release(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lk = (prte_pmix_lock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(status);

    PRTE_PMIX_WAKEUP_THREAD(lk);
}

static pmix_pointer_array_t *procs_prev_ordered_to_terminate = NULL;

void prte_daemon_recv(int status, pmix_proc_t *sender,
                      pmix_data_buffer_t *buffer,
                      prte_rml_tag_t tag, void *cbdata)
{
    prte_daemon_cmd_flag_t command;
    int ret;
    int32_t n;
    int32_t signal;
    pmix_nspace_t job;
    pmix_data_buffer_t data, *answer;
    prte_job_t *jdata;
    pmix_proc_t proc, *pptr;
    int32_t i, num_replies;
    pmix_pointer_array_t procarray;
    prte_proc_t *proct;
    char *cmd_str = NULL;
    pmix_pointer_array_t *procs_to_kill = NULL;
    int32_t num_procs, num_new_procs = 0, p;
    prte_proc_t *cur_proc = NULL, *prev_proc = NULL;
    bool found = false;
    bool compressed;
    FILE *fp;
    char gscmd[256], path[1035], *pathptr;
    char string[256], *string_ptr = string;
    char *coprocessors;
    prte_pmix_lock_t lk;
    pmix_proc_t pname;
    pmix_byte_object_t pbo;
    pmix_topology_t ptopo;
    char *tmp;
    pmix_info_t info[4];
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    /* unpack the command */
    n = 1;
    ret = PMIx_Data_unpack(NULL, buffer, &command, &n, PMIX_UINT8);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        return;
    }

    cmd_str = get_prted_comm_cmd_str(command);
    PMIX_OUTPUT_VERBOSE((1, prte_debug_output,
                         "%s prted:comm:process_commands() Processing Command: %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), cmd_str));
    free(cmd_str);
    cmd_str = NULL;

    /* now process the command locally */
    switch (command) {

        /****    NULL    ****/
    case PRTE_DAEMON_NULL_CMD:
        ret = PRTE_SUCCESS;
        break;

        /****    KILL_LOCAL_PROCS   ****/
    case PRTE_DAEMON_KILL_LOCAL_PROCS:
        num_replies = 0;

        /* construct the pointer array */
        PMIX_CONSTRUCT(&procarray, pmix_pointer_array_t);
        pmix_pointer_array_init(&procarray, num_replies, PRTE_GLOBAL_ARRAY_MAX_SIZE, 16);

        /* unpack the proc names into the array */
        while (PMIX_SUCCESS == (ret = PMIx_Data_unpack(NULL, buffer, &proc, &n, PMIX_PROC))) {
            proct = PMIX_NEW(prte_proc_t);
            PMIX_LOAD_PROCID(&proct->name, proc.nspace, proc.rank);

            pmix_pointer_array_add(&procarray, proct);
            num_replies++;
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != ret) {
            PMIX_ERROR_LOG(ret);
            goto KILL_PROC_CLEANUP;
        }

        if (0 == num_replies) {
            /* kill everything */
            if (PRTE_SUCCESS != (ret = prte_odls.kill_local_procs(NULL))) {
                PRTE_ERROR_LOG(ret);
            }
            break;
        } else {
            /* kill the procs */
            if (PRTE_SUCCESS != (ret = prte_odls.kill_local_procs(&procarray))) {
                PRTE_ERROR_LOG(ret);
            }
        }

        /* cleanup */
    KILL_PROC_CLEANUP:
        for (i = 0; i < procarray.size; i++) {
            if (NULL != (proct = (prte_proc_t *) pmix_pointer_array_get_item(&procarray, i))) {
                free(proct);
            }
        }
        PMIX_DESTRUCT(&procarray);
        break;

        /****    SIGNAL_LOCAL_PROCS   ****/
    case PRTE_DAEMON_SIGNAL_LOCAL_PROCS:
        /* unpack the jobid */
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &job, &n, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            goto CLEANUP;
        }

        /* look up job data object */
        jdata = prte_get_job_data_object(job);

        /* get the signal */
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &signal, &n, PMIX_INT32);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            goto CLEANUP;
        }

        /* Convert SIGTSTP to SIGSTOP so we can suspend a.out */
        if (SIGTSTP == signal) {
            if (prte_debug_daemons_flag) {
                pmix_output(0, "%s prted_cmd: converted SIGTSTP to SIGSTOP before delivering",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            }
            signal = SIGSTOP;
            if (NULL != jdata) {
                jdata->state |= PRTE_JOB_STATE_SUSPENDED;
            }
        } else if (SIGCONT == signal && NULL != jdata) {
            jdata->state &= ~PRTE_JOB_STATE_SUSPENDED;
        }

        if (prte_debug_daemons_flag) {
            pmix_output(0, "%s prted_cmd: received signal_local_procs, delivering signal %d",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), signal);
        }

        /* signal them */
        if (PRTE_SUCCESS != (ret = prte_odls.signal_local_procs(NULL, signal))) {
            PRTE_ERROR_LOG(ret);
        }
        break;

        /****    ADD_LOCAL_PROCS   ****/
    case PRTE_DAEMON_ADD_LOCAL_PROCS:
    case PRTE_DAEMON_DVM_ADD_PROCS:
        if (prte_debug_daemons_flag) {
            pmix_output(0, "%s prted_cmd: received add_local_procs",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        }

        /* launch the processes */
        if (PRTE_SUCCESS != (ret = prte_odls.launch_local_procs(buffer))) {
            PMIX_OUTPUT_VERBOSE((1, prte_debug_output,
                                 "%s prted:comm:add_procs failed to launch on error %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_ERROR_NAME(ret)));
        }
        break;

    case PRTE_DAEMON_ABORT_PROCS_CALLED:
        if (prte_debug_daemons_flag) {
            pmix_output(0, "%s prted_cmd: received abort_procs report",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        }

        /* Number of processes */
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &num_procs, &n, PMIX_INT32);
        if (PMIX_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            goto CLEANUP;
        }

        /* Retrieve list of processes */
        procs_to_kill = PMIX_NEW(pmix_pointer_array_t);
        pmix_pointer_array_init(procs_to_kill, num_procs, INT32_MAX, 2);

        /* Keep track of previously terminated, so we don't keep ordering the
         * same processes to die.
         */
        if (NULL == procs_prev_ordered_to_terminate) {
            procs_prev_ordered_to_terminate = PMIX_NEW(pmix_pointer_array_t);
            pmix_pointer_array_init(procs_prev_ordered_to_terminate, num_procs + 1, INT32_MAX, 8);
        }

        num_new_procs = 0;
        for (i = 0; i < num_procs; ++i) {
            cur_proc = PMIX_NEW(prte_proc_t);

            n = 1;
            ret = PMIx_Data_unpack(NULL, buffer, &(cur_proc->name), &n, PMIX_PROC);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                goto CLEANUP;
            }

            /* See if duplicate */
            found = false;
            for (p = 0; p < procs_prev_ordered_to_terminate->size; ++p) {
                if (NULL
                    == (prev_proc = (prte_proc_t *)
                            pmix_pointer_array_get_item(procs_prev_ordered_to_terminate, p))) {
                    continue;
                }
                if (PMIX_CHECK_PROCID(&cur_proc->name, &prev_proc->name)) {
                    found = true;
                    break;
                }
            }

            PMIX_OUTPUT_VERBOSE(
                (2, prte_debug_output,
                 "%s prted:comm:abort_procs Application %s requests term. of %s (%2d of %2d) %3s.",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender),
                 PRTE_NAME_PRINT(&(cur_proc->name)), i, num_procs, (found ? "Dup" : "New")));

            /* If not a duplicate, then add to the to_kill list */
            if (!found) {
                pmix_pointer_array_add(procs_to_kill, (void *) cur_proc);
                PMIX_RETAIN(cur_proc);
                pmix_pointer_array_add(procs_prev_ordered_to_terminate, (void *) cur_proc);
                num_new_procs++;
            }
        }

        /*
         * Send the request to terminate
         */
        if (num_new_procs > 0) {
            PMIX_OUTPUT_VERBOSE((2, prte_debug_output,
                                 "%s prted:comm:abort_procs Terminating application requested "
                                 "processes (%2d / %2d).",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_new_procs, num_procs));
            prte_plm.terminate_procs(procs_to_kill);
        } else {
            PMIX_OUTPUT_VERBOSE((2, prte_debug_output,
                                 "%s prted:comm:abort_procs No new application processes to "
                                 "terminating from request (%2d / %2d).",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), num_new_procs, num_procs));
        }

        break;

        /****    DEFINE PSET    ****/
    case PRTE_DAEMON_DEFINE_PSET:
        // get pset name
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &cmd_str, &n, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        // get number of target procs
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &num_procs, &n, PMIX_INT32);
        if (PMIX_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        // create space for them
        pptr = PMIx_Proc_create(num_procs);
        if (NULL == pptr) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            goto CLEANUP;
        }
        // unpack the targets
        n = num_procs;
        ret = PMIx_Data_unpack(NULL, buffer, pptr, &n, PMIX_PROC);
        if (PMIX_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            goto CLEANUP;
        }
        // define the pset
        ret = PMIx_server_define_process_set(pptr, num_procs, cmd_str);
        free(cmd_str);
        cmd_str = NULL;
        PMIx_Proc_free(pptr, num_procs);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
        }
        break;

        /****    EXIT COMMAND    ****/
    case PRTE_DAEMON_EXIT_CMD:
        if (prte_debug_daemons_flag) {
            pmix_output(0, "%s prted_cmd: received exit cmd", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        }
        jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            return;
        }
        /* kill the local procs */
        prte_odls.kill_local_procs(NULL);
        // ensure daemons know we were ordered to terminate
        prte_prteds_term_ordered = true;
        /* if all my routes and local children are gone, then terminate ourselves */
        if (0 == (ret = pmix_list_get_size(&prte_rml_base.children))) {
            for (i = 0; i < prte_local_children->size; i++) {
                proct = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
                if (NULL != proct && PRTE_FLAG_TEST(proct, PRTE_PROC_FLAG_ALIVE)) {
                    /* at least one is still alive */
                    if (prte_debug_daemons_flag) {
                        pmix_output(0, "%s prted_cmd: exit cmd, but proc %s is alive",
                                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                    PRTE_NAME_PRINT(&proct->name));
                    }
                    return;
                }
            }
            /* call our appropriate exit procedure */
            if (prte_debug_daemons_flag) {
                pmix_output(0, "%s prted_cmd: all routes and children gone - exiting",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
            }
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
        } else if (prte_debug_daemons_flag) {
            pmix_output(0, "%s prted_cmd: exit cmd, %d routes still exist",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), ret);
        }
        return;

        /****    HALT VM COMMAND    ****/
    case PRTE_DAEMON_HALT_VM_CMD:
        if (prte_debug_daemons_flag) {
            pmix_output(0, "%s prted_cmd: received halt_vm cmd",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        }
        /* this is an abnormal termination */
        prte_abnormal_term_ordered = true;

        jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            return;
        }
        /* kill the local procs */
        prte_odls.kill_local_procs(NULL);
        /* any tools attached to us will have done so via PMIx, so
         * let's provide them with a friendly "job end" notification */
        PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
        PMIX_INFO_LOAD(&info[1], PMIX_EVENT_AFFECTED_PROC, &prte_process_info.myproc, PMIX_PROC);
        PMIX_INFO_LOAD(&info[2], "prte.notify.donotloop", NULL, PMIX_BOOL);
        PMIX_INFO_LOAD(&info[3], PMIX_EVENT_DO_NOT_CACHE, NULL, PMIX_BOOL);
        PRTE_PMIX_CONSTRUCT_LOCK(&lk);
        ret = PMIx_Notify_event(PMIX_EVENT_JOB_END, &prte_process_info.myproc,
                                PMIX_RANGE_SESSION, info, 4, _notify_release, &lk);
        PRTE_PMIX_WAIT_THREAD(&lk);
        PRTE_PMIX_DESTRUCT_LOCK(&lk);
        // ensure daemons know we were ordered to terminate
        prte_prteds_term_ordered = true;
        if (PRTE_PROC_IS_MASTER) {
            /* if all my routes and local children are gone, then terminate ourselves */
            if (0 == pmix_list_get_size(&prte_rml_base.children)) {
                for (i = 0; i < prte_local_children->size; i++) {
                    proct = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
                    if (NULL != proct && PRTE_FLAG_TEST(proct, PRTE_PROC_FLAG_ALIVE)) {
                        /* at least one is still alive */
                        return;
                    }
                }
                /* call our appropriate exit procedure */
                if (prte_debug_daemons_flag) {
                    pmix_output(0, "%s prted_cmd: all routes and children gone - exiting",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
                }
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            }
        } else {
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
        }
        return;

        /****     DVM CLEANUP JOB COMMAND    ****/
    case PRTE_DAEMON_DVM_CLEANUP_JOB_CMD:
        /* unpack the jobid */
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &job, &n, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            goto CLEANUP;
        }

        /* look up job data object */
        if (NULL == (jdata = prte_get_job_data_object(job))) {
            /* we can safely ignore this request as the job
             * was already cleaned up, or it was a tool */
            goto CLEANUP;
        }
        /* if would be rare, but a very fast terminating job could conceivably
         * reach here prior to the spawn requestor being notified of spawn */
        ret = prte_plm_base_spawn_response(PMIX_SUCCESS, jdata);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
        }

        PRTE_PMIX_CONSTRUCT_LOCK(&lk);
        PMIx_server_deregister_nspace(job, _notify_release, &lk);
        PRTE_PMIX_WAIT_THREAD(&lk);
        PRTE_PMIX_DESTRUCT_LOCK(&lk);

        /* cleanup any pending server ops */
        PMIX_LOAD_PROCID(&pname, job, PMIX_RANK_WILDCARD);
        prte_pmix_server_clear(&pname);

        PMIX_RELEASE(jdata);
        break;

        /****     REPORT TOPOLOGY COMMAND    ****/
    case PRTE_DAEMON_REPORT_TOPOLOGY_CMD:
        PMIX_DATA_BUFFER_CONSTRUCT(&data);
        /* pack the topology signature */
        ret = PMIx_Data_pack(NULL, &data, &prte_topo_signature, 1, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_DESTRUCT(&data);
            goto CLEANUP;
        }
        /* pack the topology */
        ptopo.source = "hwloc";
        ptopo.topology = prte_hwloc_topology;
        ret = PMIx_Data_pack(NULL, &data, &ptopo, 1, PMIX_TOPO);
        if (PMIX_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_DESTRUCT(&data);
            goto CLEANUP;
        }

        /* detect and add any coprocessors */
        coprocessors = prte_hwloc_base_find_coprocessors(prte_hwloc_topology);
        ret = PMIx_Data_pack(NULL, &data, &coprocessors, 1, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
        }
        if (NULL != coprocessors) {
            free(coprocessors);
        }
        /* see if I am on a coprocessor */
        coprocessors = prte_hwloc_base_check_on_coprocessor();
        ret = PMIx_Data_pack(NULL, &data, &coprocessors, 1, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
        }
        if (NULL != coprocessors) {
            free(coprocessors);
        }
        PMIX_DATA_BUFFER_CREATE(answer);
        if (PMIx_Data_compress((uint8_t *) data.base_ptr, data.bytes_used, (uint8_t **) &pbo.bytes,
                               &pbo.size)) {
            /* the data was compressed - mark that we compressed it */
            compressed = true;
        } else {
            /* mark that it was not compressed */
            compressed = false;
            pbo.bytes = data.base_ptr;
            pbo.size = data.bytes_used;
            data.base_ptr = NULL;
            data.bytes_used = 0;
        }
        PMIX_DATA_BUFFER_DESTRUCT(&data);
        ret = PMIx_Data_pack(NULL, answer, &compressed, 1, PMIX_BOOL);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            PMIX_DATA_BUFFER_RELEASE(answer);
            goto CLEANUP;
        }
        /* pack the payload */
        ret = PMIx_Data_pack(NULL, answer, &pbo, 1, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            PMIX_DATA_BUFFER_RELEASE(answer);
            goto CLEANUP;
        }
        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
        /* send the data */
        PRTE_RML_SEND(ret, sender->rank, answer, PRTE_RML_TAG_TOPOLOGY_REPORT);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(answer);
        }
        break;

    case PRTE_DAEMON_GET_STACK_TRACES:
        /* prep the response */
        PMIX_DATA_BUFFER_CREATE(answer);
        pathptr = path;

        /* unpack the jobid */
        n = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &job, &n, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            goto CLEANUP;
        }

        // Try to find the "gstack" executable.  Failure to find the
        // executable will be handled below, because the receiver
        // expects to have the process name, hostname, and PID in the
        // buffer before finding an error message.
        char *gstack_exec;
        gstack_exec = pmix_find_absolute_path("gstack");

        /* we have to at least include the nspace of this job
         * in the reply to ensure the DVM master knows which
         * job we are talking about */
        tmp = (char *) job;
        if (PMIX_SUCCESS != PMIx_Data_pack(NULL, answer, &tmp, 1, PMIX_STRING)) {
            if (NULL != gstack_exec) {
                free(gstack_exec);
            }
            break;
        }

        /* hit each local process with a gstack command */
        for (i = 0; i < prte_local_children->size; i++) {
            proct = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
            if (NULL != proct &&
                PRTE_FLAG_TEST(proct, PRTE_PROC_FLAG_ALIVE) &&
                PMIX_CHECK_NSPACE(proct->name.nspace, job)) {
                PMIX_DATA_BUFFER_CONSTRUCT(&data);
                if (PMIX_SUCCESS != PMIx_Data_pack(NULL, &data, &proct->name, 1, PMIX_PROC) ||
                    PMIX_SUCCESS != PMIx_Data_pack(NULL, &data, &proct->node->name, 1, PMIX_STRING) ||
                    PMIX_SUCCESS != PMIx_Data_pack(NULL, &data, &proct->pid, 1, PMIX_PID)) {
                    PMIX_DATA_BUFFER_DESTRUCT(&data);
                    break;
                }

                // If we were able to find the gstack executable,
                // above, then run the command here.
                fp = NULL;
                if (NULL != gstack_exec) {
                    (void) snprintf(gscmd, sizeof(gscmd), "%s %lu", gstack_exec,
                                    (unsigned long) proct->pid);
                    fp = popen(gscmd, "r");
                }

                // If either we weren't able to find or run the gstack
                // exectuable, send back a nice error message here.
                if (NULL == gstack_exec || NULL == fp) {
                    (void) snprintf(string, sizeof(string),
                                    "Failed to %s \"%s\" on %s to obtain stack traces",
                                    (NULL == gstack_exec) ? "find" : "run",
                                    (NULL == gstack_exec) ? "gstack" : gstack_exec,
                                    proct->node->name);
                    if (PMIX_SUCCESS == PMIx_Data_pack(NULL, &data, &string_ptr, 1, PMIX_STRING)) {
                        ret = PMIx_Data_unload(&data, &pbo);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_DATA_BUFFER_DESTRUCT(&data);
                            break;
                        }
                        PMIx_Data_pack(NULL, answer, &pbo, 1, PMIX_BYTE_OBJECT);
                    }
                    PMIX_DATA_BUFFER_DESTRUCT(&data);
                    break;
                }
                /* Read the output a line at a time and pack it for transmission */
                memset(path, 0, sizeof(path));
                while (fgets(path, sizeof(path) - 1, fp) != NULL) {
                    if (PMIX_SUCCESS != PMIx_Data_pack(NULL, &data, &pathptr, 1, PMIX_STRING)) {
                        PMIX_DATA_BUFFER_DESTRUCT(&data);
                        break;
                    }
                    memset(path, 0, sizeof(path));
                }
                /* close */
                pclose(fp);
                /* transfer this load */
                ret = PMIx_Data_unload(&data, &pbo);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_DESTRUCT(&data);
                    break;
                }
                if (PMIX_SUCCESS != PMIx_Data_pack(NULL, answer, &pbo, 1, PMIX_BYTE_OBJECT)) {
                    PMIX_DATA_BUFFER_DESTRUCT(&data);
                    break;
                }
                PMIX_DATA_BUFFER_DESTRUCT(&data);
            }
        }
        if (NULL != gstack_exec) {
            free(gstack_exec);
        }
        /* always send our response */
        PRTE_RML_SEND(ret, PRTE_PROC_MY_HNP->rank, answer, PRTE_RML_TAG_STACK_TRACE);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(answer);
        }
        break;

    default:
        PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
    }

CLEANUP:
    return;
}

static char *get_prted_comm_cmd_str(int command)
{
    switch (command) {
    case PRTE_DAEMON_KILL_LOCAL_PROCS:
        return strdup("PRTE_DAEMON_KILL_LOCAL_PROCS");
    case PRTE_DAEMON_SIGNAL_LOCAL_PROCS:
        return strdup("PRTE_DAEMON_SIGNAL_LOCAL_PROCS");
    case PRTE_DAEMON_ADD_LOCAL_PROCS:
        return strdup("PRTE_DAEMON_ADD_LOCAL_PROCS");

    case PRTE_DAEMON_EXIT_CMD:
        return strdup("PRTE_DAEMON_EXIT_CMD");
    case PRTE_DAEMON_PROCESS_AND_RELAY_CMD:
        return strdup("PRTE_DAEMON_PROCESS_AND_RELAY_CMD");
    case PRTE_DAEMON_NULL_CMD:
        return strdup("NULL");

    case PRTE_DAEMON_HALT_VM_CMD:
        return strdup("PRTE_DAEMON_HALT_VM_CMD");

    case PRTE_DAEMON_ABORT_PROCS_CALLED:
        return strdup("PRTE_DAEMON_ABORT_PROCS_CALLED");

    case PRTE_DAEMON_DVM_ADD_PROCS:
        return strdup("PRTE_DAEMON_DVM_ADD_PROCS");

    case PRTE_DAEMON_GET_STACK_TRACES:
        return strdup("PRTE_DAEMON_GET_STACK_TRACES");

    case PRTE_DAEMON_GET_MEMPROFILE:
        return strdup("PRTE_DAEMON_GET_MEMPROFILE");

    case PRTE_DAEMON_DVM_CLEANUP_JOB_CMD:
        return strdup("PRTE_DAEMON_DVM_CLEANUP_JOB_CMD");

    default:
        return strdup("Unknown Command!");
    }
}
