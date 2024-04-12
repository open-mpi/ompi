/* -*- C -*-
 *
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "prte_config.h"

#include <string.h>
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif

#include "src/mca/mca.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_environ.h"

#include "constants.h"
#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/ras/base/base.h"
#include "src/rml/rml.h"
#include "src/mca/schizo/base/base.h"
#include "src/mca/state/state.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_quit.h"
#include "src/util/error_strings.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"
#include "types.h"

#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/mca/plm/plm.h"
#include "src/mca/plm/plm_types.h"

static bool recv_issued = false;

int prte_plm_base_comm_start(void)
{
    if (recv_issued) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:receive start comm", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_PLM,
                  PRTE_RML_PERSISTENT, prte_plm_base_recv, NULL);
    if (PRTE_PROC_IS_MASTER) {
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_PRTED_CALLBACK,
                      PRTE_RML_PERSISTENT, prte_plm_base_daemon_callback, NULL);
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_REPORT_REMOTE_LAUNCH,
                      PRTE_RML_PERSISTENT, prte_plm_base_daemon_failed, NULL);
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_TOPOLOGY_REPORT,
                      PRTE_RML_PERSISTENT, prte_plm_base_daemon_topology, NULL);
    }
    recv_issued = true;

    return PRTE_SUCCESS;
}

int prte_plm_base_comm_stop(void)
{
    if (!recv_issued) {
        return PRTE_SUCCESS;
    }

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:receive stop comm", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_PLM);
    if (PRTE_PROC_IS_MASTER) {
        PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_PRTED_CALLBACK);
        PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_REPORT_REMOTE_LAUNCH);
        PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_TOPOLOGY_REPORT);
    }
    recv_issued = false;

    return PRTE_SUCCESS;
}

/* process incoming messages in order of receipt */
void prte_plm_base_recv(int status, pmix_proc_t *sender,
                        pmix_data_buffer_t *buffer,
                        prte_rml_tag_t tag, void *cbdata)
{
    prte_plm_cmd_flag_t command;
    int32_t count;
    pmix_nspace_t job;
    prte_job_t *jdata, *parent, jb;
    pmix_data_buffer_t *answer;
    pmix_rank_t vpid;
    prte_proc_t *proc;
    prte_proc_state_t state;
    prte_exit_code_t exit_code;
    int32_t rc = PRTE_SUCCESS, ret;
    prte_app_context_t *app, *child_app;
    pmix_proc_t name, *nptr;
    pid_t pid;
    bool debugging, found;
    int i, room, *rmptr = &room;
    char **env;
    char *prefix_dir, *tmp;
    pmix_rank_t tgt, *tptr;
    pmix_value_t pidval = PMIX_VALUE_STATIC_INIT;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:receive processing msg", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    count = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &command, &count, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto CLEANUP;
    }

    switch (command) {
    case PRTE_PLM_ALLOC_JOBID_CMD:
        /* set default return value */
        PMIX_LOAD_NSPACE(job, NULL);

        /* unpack the room number of the request so we can return it to them */
        count = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &room, &count, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto CLEANUP;
        }
        /* the new nspace is our base nspace with an "@N" extension */
        pmix_asprintf(&tmp, "%s@%u", prte_plm_globals.base_nspace, prte_plm_globals.next_jobid);
        PMIX_LOAD_NSPACE(job, tmp);
        free(tmp);
        prte_plm_globals.next_jobid++;
        PMIX_CONSTRUCT(&jb, prte_job_t);

        /* setup the response */
        PMIX_DATA_BUFFER_CREATE(answer);

        /* pack the status to be returned */
        rc = PMIx_Data_pack(NULL, answer, &rc, 1, PMIX_INT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }

        /* pack the jobid */
        rc = PMIx_Data_pack(NULL, answer, &job, 1, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }

        /* pack the room number of the request */
        rc = PMIx_Data_pack(NULL, answer, &room, 1, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }

        /* send the response back to the sender */
        PRTE_RML_SEND(ret, sender->rank, answer, PRTE_RML_TAG_JOBID_RESP);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(answer);
        }
        break;

    case PRTE_PLM_LAUNCH_JOB_CMD:
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:receive job launch command from %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender)));

        /* unpack the job object */
        count = 1;
        rc = prte_job_unpack(buffer, &jdata);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            goto ANSWER_LAUNCH;
        }

        /* record the sender so we know who to respond to */
        PMIX_LOAD_PROCID(&jdata->originator, sender->nspace, sender->rank);

        /* assign a schizo module */
        if (NULL == jdata->personality) {
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&jdata->personality, "prte");
        }
        tmp = PMIX_ARGV_JOIN_COMPAT(jdata->personality, ',');
        jdata->schizo = (struct prte_schizo_base_module_t*)prte_schizo_base_detect_proxy(tmp);
        if (NULL == jdata->schizo) {
            pmix_show_help("help-schizo-base.txt", "no-proxy", true, prte_tool_basename, tmp);
            free(tmp);
            rc = PRTE_ERR_NOT_FOUND;
            goto ANSWER_LAUNCH;
        }
        free(tmp);

        /* get the name of the actual spawn parent - i.e., the proc that actually
         * requested the spawn */
        if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCH_PROXY, (void **) &nptr, PMIX_PROC)) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            rc = PRTE_ERR_NOT_FOUND;
            goto ANSWER_LAUNCH;
        }

        /* get the parent's job object */
        if (NULL != (parent = prte_get_job_data_object(nptr->nspace)) &&
            !PMIX_CHECK_NSPACE(parent->nspace, PRTE_PROC_MY_NAME->nspace)) {
            /* link the spawned job to the spawner */
            PMIX_RETAIN(jdata);
            pmix_list_append(&parent->children, &jdata->super);
            /* connect the launcher as well */
            if (PMIX_NSPACE_INVALID(parent->launcher)) {
                /* we are an original spawn */
                PMIX_LOAD_NSPACE(jdata->launcher, nptr->nspace);
            } else {
                PMIX_LOAD_NSPACE(jdata->launcher, parent->launcher);
            }
            /* if the prefix was set in the parent's job, we need to transfer
             * that prefix to the child's app_context so any further launch of
             * orteds can find the correct binary. There always has to be at
             * least one app_context in both parent and child, so we don't
             * need to check that here. However, be sure not to overwrite
             * the prefix if the user already provided it!
             */
            app = (prte_app_context_t *) pmix_pointer_array_get_item(parent->apps, 0);
            child_app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, 0);
            if (NULL != app && NULL != child_app) {
                prefix_dir = NULL;
                if (prte_get_attribute(&app->attributes, PRTE_APP_PREFIX_DIR,
                                       (void **) &prefix_dir, PMIX_STRING)
                    && !prte_get_attribute(&child_app->attributes, PRTE_APP_PREFIX_DIR, NULL,
                                           PMIX_STRING)) {
                    prte_set_attribute(&child_app->attributes, PRTE_APP_PREFIX_DIR,
                                       PRTE_ATTR_GLOBAL, prefix_dir, PMIX_STRING);
                }
                if (NULL != prefix_dir) {
                    free(prefix_dir);
                }
            }
        }
        PMIX_PROC_RELEASE(nptr);

        /* if the user asked to forward any envars, cycle through the app contexts
         * in the comm_spawn request and add them
         */
        if (NULL != prte_forwarded_envars) {
            for (i = 0; i < jdata->apps->size; i++) {
                app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i);
                if (NULL == app) {
                    continue;
                }
                env = pmix_environ_merge(prte_forwarded_envars, app->env);
                PMIX_ARGV_FREE_COMPAT(app->env);
                app->env = env;
            }
        }

        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:receive adding hosts",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

        /* process any add-hostfile and add-host options that were provided */
        if (PRTE_SUCCESS != (rc = prte_ras_base_add_hosts(jdata))) {
            PRTE_ERROR_LOG(rc);
            goto ANSWER_LAUNCH;
        }

        if (NULL != parent && !PRTE_FLAG_TEST(parent, PRTE_JOB_FLAG_TOOL)) {
            if (NULL == parent->bookmark) {
                /* find the sender's node in the job map */
                proc = (prte_proc_t *) pmix_pointer_array_get_item(parent->procs, sender->rank);
                if (NULL != proc) {
                    /* set the bookmark so the child starts from that place - this means
                     * that the first child process could be co-located with the proc
                     * that called comm_spawn, assuming slots remain on that node. Otherwise,
                     * the procs will start on the next available node
                     */
                    jdata->bookmark = proc->node;
                }
            } else {
                jdata->bookmark = parent->bookmark;
            }
        }

        if (!prte_dvm_ready) {
            pmix_pointer_array_add(prte_cache, jdata);
            return;
        }

        /* launch it */
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:receive calling spawn",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        if (PRTE_SUCCESS != (rc = prte_plm.spawn(jdata))) {
            PRTE_ERROR_LOG(rc);
            goto ANSWER_LAUNCH;
        }
        break;
    ANSWER_LAUNCH:
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:receive - error on launch: %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), rc));

        /* setup the response */
        PMIX_DATA_BUFFER_CREATE(answer);

        /* pack the error code to be returned */
        rc = PMIx_Data_pack(NULL, answer, &rc, 1, PMIX_INT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }

        /* pack an invalid jobid */
        PMIX_LOAD_NSPACE(job, NULL);
        rc = PMIx_Data_pack(NULL, answer, &job, 1, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }

        /* pack the room number of the request */
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_ROOM_NUM, (void **) &rmptr, PMIX_INT)) {
            rc = PMIx_Data_pack(NULL, answer, &room, 1, PMIX_INT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
        }

        /* send the response back to the sender */
        PRTE_RML_SEND(ret, sender->rank, answer, PRTE_RML_TAG_LAUNCH_RESP);
        if (PRTE_SUCCESS != ret) {
            PRTE_ERROR_LOG(ret);
            PMIX_DATA_BUFFER_RELEASE(answer);
        }
        break;

    case PRTE_PLM_UPDATE_PROC_STATE:
            pmix_output_verbose(5, prte_plm_base_framework.framework_output,
                                "\n\n%s plm:base:receive update proc state command from %s\n\n",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender));
        count = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &job, &count, PMIX_PROC_NSPACE);
        while (PMIX_SUCCESS == rc) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                "%s plm:base:receive got update_proc_state for job %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(job)));

            PMIX_LOAD_NSPACE(name.nspace, job);
            /* get the job object */
            jdata = prte_get_job_data_object(job);
            count = 1;
            rc = PMIx_Data_unpack(NULL, buffer, &vpid, &count, PMIX_PROC_RANK);
            while (PMIX_SUCCESS == rc) {
                if (PMIX_RANK_INVALID == vpid) {
                    /* flag indicates that this job is complete - move on */
                    break;
                }
                name.rank = vpid;
                /* unpack the pid */
                count = 1;
                rc = PMIx_Data_unpack(NULL, buffer, &pid, &count, PMIX_PID);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                /* unpack the state */
                count = 1;
                rc = PMIx_Data_unpack(NULL, buffer, &state, &count, PMIX_UINT32);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                /* unpack the exit code */
                count = 1;
                rc = PMIx_Data_unpack(NULL, buffer, &exit_code, &count, PMIX_INT32);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    goto CLEANUP;
                }

                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s plm:base:receive got update_proc_state for vpid %u pid %d state %s exit_code %d",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), vpid, (int)pid, prte_proc_state_to_str(state),
                                     (int) exit_code));

                if (NULL != jdata) {
                    /* get the proc data object */
                    proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, vpid);
                    if (NULL == proc) {
                        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FORCED_EXIT);
                        goto CLEANUP;
                    }
                    /* NEVER update the proc state before activating the state machine - let
                     * the state cbfunc update it as it may need to compare this
                     * state against the prior proc state */
                    proc->pid = pid;
                    proc->exit_code = exit_code;
                    PRTE_ACTIVATE_PROC_STATE(&name, state);
                }
                /* get entry from next rank */
                rc = PMIx_Data_unpack(NULL, buffer, &vpid, &count, PMIX_PROC_RANK);
            }
            /* prepare for next job */
            count = 1;
            rc = PMIx_Data_unpack(NULL, buffer, &job, &count, PMIX_PROC_NSPACE);
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            rc = prte_pmix_convert_status(rc);
        } else {
            rc = PRTE_SUCCESS;
        }
        break;

    case PRTE_PLM_READY_FOR_DEBUG_CMD:
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                            "%s plm:base:receive ready for debug command from %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender)));
        count = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &job, &count, PMIX_PROC_NSPACE);
        while (PMIX_SUCCESS == rc) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                "%s plm:base:receive got ready for debug for job %s",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(job)));

            PMIX_LOAD_NSPACE(name.nspace, job);
            /* get the job object */
            jdata = prte_get_job_data_object(job);
            debugging = false;
            if (prte_get_attribute(&jdata->attributes, PRTE_JOB_STOP_ON_EXEC, NULL, PMIX_BOOL) ||
                prte_get_attribute(&jdata->attributes, PRTE_JOB_STOP_IN_INIT, NULL, PMIX_BOOL) ||
                prte_get_attribute(&jdata->attributes, PRTE_JOB_STOP_IN_APP, NULL, PMIX_BOOL)) {
                debugging = true;
            }
            count = 1;
            rc = PMIx_Data_unpack(NULL, buffer, &vpid, &count, PMIX_PROC_RANK);
            while (PMIX_SUCCESS == rc) {
                if (PMIX_RANK_INVALID == vpid) {
                    /* flag indicates that this job is complete - move on */
                    break;
                }
                name.rank = vpid;
                /* unpack the pid */
                count = 1;
                rc = PMIx_Data_unpack(NULL, buffer, &pid, &count, PMIX_PID);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    goto CLEANUP;
                }

                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                     "%s plm:base:receive got ready for debug for vpid %u",
                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), vpid));

                /* get the proc data object */
                proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, vpid);
                if (NULL == proc) {
                    PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FORCED_EXIT);
                    goto CLEANUP;
                }
                /* NEVER update the proc state before activating the state machine - let
                 * the state cbfunc update it as it may need to compare this
                 * state against the prior proc state */
                proc->pid = pid;
                tptr = &tgt;
                if (debugging) {
                    jdata->num_ready_for_debug++;
                }
                /* get entry from next rank */
                rc = PMIx_Data_unpack(NULL, buffer, &vpid, &count, PMIX_PROC_RANK);
            }
            if (debugging && jdata->num_ready_for_debug == jdata->num_procs) {
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_READY_FOR_DEBUG);
            }
            /* prepare for next job */
            count = 1;
            rc = PMIx_Data_unpack(NULL, buffer, &job, &count, PMIX_PROC_NSPACE);
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            rc = prte_pmix_convert_status(rc);
        } else {
            rc = PRTE_SUCCESS;
        }
        break;

    case PRTE_PLM_REGISTERED_CMD:
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                            "%s plm:base:receive registered command from %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender)));
        count = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &job, &count, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto CLEANUP;
        }
        PMIX_LOAD_NSPACE(name.nspace, job);
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                            "%s plm:base:receive got registered for job %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(job)));
        /* get the job object */
        if (NULL == (jdata = prte_get_job_data_object(job))) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            rc = PRTE_ERR_NOT_FOUND;
            goto CLEANUP;
        }
        count = 1;
        while (PRTE_SUCCESS == PMIx_Data_unpack(NULL, buffer, &vpid, &count, PMIX_PROC_RANK)) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s plm:base:receive got registered for vpid %u",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), vpid));
            proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, vpid);
            if (NULL == proc) {
                PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FORCED_EXIT);
                goto CLEANUP;
            }
            proc->state = PRTE_PROC_STATE_REGISTERED;
            jdata->num_reported++;
            count = 1;
        }
        if (jdata->num_reported == jdata->num_procs) {
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_REGISTERED);
        }
        break;

    case PRTE_PLM_LOCAL_LAUNCH_COMP_CMD:
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                            "%s plm:base:receive local launch complete command from %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender)));
        count = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &job, &count, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto CLEANUP;
        }
        PMIX_LOAD_NSPACE(name.nspace, job);
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                            "%s plm:base:receive got local launch complete for job %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(job)));
        /* get the job object */
        if (NULL == (jdata = prte_get_job_data_object(job))) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            rc = PRTE_ERR_NOT_FOUND;
            goto CLEANUP;
        }
        count = 1;
        while (PRTE_SUCCESS == PMIx_Data_unpack(NULL, buffer, &vpid, &count, PMIX_PROC_RANK)) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s plm:base:receive got local launch complete for vpid %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_VPID_PRINT(vpid)));
            proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, vpid);
            if (NULL == proc) {
                PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FORCED_EXIT);
                goto CLEANUP;
            }
            /* unpack the pid */
            count = 1;
            rc = PMIx_Data_unpack(NULL, buffer, &pid, &count, PMIX_PID);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                goto CLEANUP;
            }
            proc->pid = pid;
            pidval.type = PMIX_PID;
            pidval.data.pid = pid;
            /* store the PID for later retrieval */
            rc = PMIx_Store_internal(&proc->name, PMIX_PROC_PID, &pidval);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            /* unpack the state */
            count = 1;
            rc = PMIx_Data_unpack(NULL, buffer, &state, &count, PMIX_UINT32);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                goto CLEANUP;
            }
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s plm:base:receive got local launch complete for vpid %u state %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), vpid,
                                 prte_proc_state_to_str(state)));
            /* if the child failed to start, unpack its exit code */
            if (PRTE_PROC_STATE_RUNNING != state) {
                count = 1;
                rc = PMIx_Data_unpack(NULL, buffer, &exit_code, &count, PMIX_INT32);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    goto CLEANUP;
                }
                proc->exit_code = exit_code;
            }
            proc->state = state;
            jdata->num_launched++;
            count = 1;
        }
        /* record that we heard back from a daemon during app launch */
        jdata->num_daemons_reported++;
        found = prte_get_attribute(&jdata->attributes, PRTE_JOB_SHOW_PROGRESS, NULL, PMIX_BOOL);
        if (found) {
            if (0 == jdata->num_daemons_reported % 100 ||
                jdata->num_daemons_reported == prte_process_info.num_daemons) {
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_REPORT_PROGRESS);
            }
        }
        if (1 == jdata->num_launched) {
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_STARTED);
        }
        if (jdata->num_launched == jdata->num_procs) {
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_RUNNING);
        }
        break;

    default:
        PRTE_ERROR_LOG(PRTE_ERR_VALUE_OUT_OF_BOUNDS);
        rc = PRTE_ERR_VALUE_OUT_OF_BOUNDS;
        break;
    }

CLEANUP:
    /* see if an error occurred - if so, wakeup the HNP so we can exit */
    if (PRTE_PROC_IS_MASTER && PRTE_SUCCESS != rc) {
        jdata = NULL;
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
    }

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:receive done processing commands",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
}

/* where HNP messages come */
void prte_plm_base_receive_process_msg(int fd, short event, void *data)
{
    PRTE_HIDE_UNUSED_PARAMS(fd, event, data);
    assert(0);
}
