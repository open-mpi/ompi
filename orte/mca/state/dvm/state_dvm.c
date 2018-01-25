/*
 * Copyright (c) 2015-2018 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>

#include "opal/util/output.h"
#include "opal/mca/pmix/pmix.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/filem/filem.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/regx/regx.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/session_dir.h"
#include "orte/util/threads.h"
#include "orte/runtime/orte_quit.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/state/state.h"
#include "orte/mca/state/base/base.h"
#include "orte/mca/state/base/state_private.h"
#include "state_dvm.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

/* local functions */
static void init_complete(int fd, short args, void *cbdata);
static void vm_ready(int fd, short args, void *cbata);
static void check_complete(int fd, short args, void *cbdata);
static void cleanup_job(int fd, short args, void *cbdata);

/******************
 * DVM module - used when mpirun is persistent
 ******************/
orte_state_base_module_t orte_state_dvm_module = {
    init,
    finalize,
    orte_state_base_activate_job_state,
    orte_state_base_add_job_state,
    orte_state_base_set_job_state_callback,
    orte_state_base_set_job_state_priority,
    orte_state_base_remove_job_state,
    orte_state_base_activate_proc_state,
    orte_state_base_add_proc_state,
    orte_state_base_set_proc_state_callback,
    orte_state_base_set_proc_state_priority,
    orte_state_base_remove_proc_state
};

static void dvm_notify(int sd, short args, void *cbdata);

/* defined default state machine sequence - individual
 * plm's must add a state for launching daemons
 */
static orte_job_state_t launch_states[] = {
    ORTE_JOB_STATE_INIT,
    ORTE_JOB_STATE_INIT_COMPLETE,
    ORTE_JOB_STATE_ALLOCATE,
    ORTE_JOB_STATE_ALLOCATION_COMPLETE,
    ORTE_JOB_STATE_DAEMONS_LAUNCHED,
    ORTE_JOB_STATE_DAEMONS_REPORTED,
    ORTE_JOB_STATE_VM_READY,
    ORTE_JOB_STATE_MAP,
    ORTE_JOB_STATE_MAP_COMPLETE,
    ORTE_JOB_STATE_SYSTEM_PREP,
    ORTE_JOB_STATE_LAUNCH_APPS,
    ORTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE,
    ORTE_JOB_STATE_RUNNING,
    ORTE_JOB_STATE_REGISTERED,
    /* termination states */
    ORTE_JOB_STATE_TERMINATED,
    ORTE_JOB_STATE_NOTIFY_COMPLETED,
    ORTE_JOB_STATE_NOTIFIED,
    ORTE_JOB_STATE_ALL_JOBS_COMPLETE
};
static orte_state_cbfunc_t launch_callbacks[] = {
    orte_plm_base_setup_job,
    init_complete,
    orte_ras_base_allocate,
    orte_plm_base_allocation_complete,
    orte_plm_base_daemons_launched,
    orte_plm_base_daemons_reported,
    vm_ready,
    orte_rmaps_base_map_job,
    orte_plm_base_mapping_complete,
    orte_plm_base_complete_setup,
    orte_plm_base_launch_apps,
    orte_state_base_local_launch_complete,
    orte_plm_base_post_launch,
    orte_plm_base_registered,
    check_complete,
    dvm_notify,
    cleanup_job,
    orte_quit
};

static orte_proc_state_t proc_states[] = {
    ORTE_PROC_STATE_RUNNING,
    ORTE_PROC_STATE_REGISTERED,
    ORTE_PROC_STATE_IOF_COMPLETE,
    ORTE_PROC_STATE_WAITPID_FIRED,
    ORTE_PROC_STATE_TERMINATED
};
static orte_state_cbfunc_t proc_callbacks[] = {
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs,
    orte_state_base_track_procs
};

static void force_quit(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* give us a chance to stop the orteds */
    orte_plm.terminate_orteds();
    OBJ_RELEASE(caddy);
}

/************************
 * API Definitions
 ************************/
static int init(void)
{
    int i, rc;
    int num_states;

    /* setup the state machines */
    OBJ_CONSTRUCT(&orte_job_states, opal_list_t);
    OBJ_CONSTRUCT(&orte_proc_states, opal_list_t);

    /* setup the job state machine */
    num_states = sizeof(launch_states) / sizeof(orte_job_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_job_state(launch_states[i],
                                                           launch_callbacks[i],
                                                           ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    /* add the termination response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_DAEMONS_TERMINATED,
                                                       orte_quit, ORTE_SYS_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add a default error response */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_FORCED_EXIT,
                                                       force_quit, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    /* add callback to report progress, if requested */
    if (ORTE_SUCCESS != (rc = orte_state.add_job_state(ORTE_JOB_STATE_REPORT_PROGRESS,
                                                       orte_state_base_report_progress, ORTE_ERROR_PRI))) {
        ORTE_ERROR_LOG(rc);
    }
    if (5 < opal_output_get_verbosity(orte_state_base_framework.framework_output)) {
        orte_state_base_print_job_state_machine();
    }

    /* populate the proc state machine to allow us to
     * track proc lifecycle changes
     */
    num_states = sizeof(proc_states) / sizeof(orte_proc_state_t);
    for (i=0; i < num_states; i++) {
        if (ORTE_SUCCESS != (rc = orte_state.add_proc_state(proc_states[i],
                                                            proc_callbacks[i],
                                                            ORTE_SYS_PRI))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    if (5 < opal_output_get_verbosity(orte_state_base_framework.framework_output)) {
        orte_state_base_print_proc_state_machine();
    }

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    opal_list_item_t *item;

    /* cleanup the proc state machine */
    while (NULL != (item = opal_list_remove_first(&orte_proc_states))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&orte_proc_states);

    return ORTE_SUCCESS;
}

static void files_ready(int status, void *cbdata)
{
    orte_job_t *jdata = (orte_job_t*)cbdata;

    if (ORTE_SUCCESS != status) {
        ORTE_FORCED_TERMINATE(status);
        return;
    } else {
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP);
    }
}

static void init_complete(int sd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    ORTE_ACQUIRE_OBJECT(caddy);

    /* nothing to do here but move along - if it is the
     * daemon job, then next step is allocate */
    ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_ALLOCATE);
    OBJ_RELEASE(caddy);
}

static void vm_ready(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    int rc;
    opal_buffer_t *buf;
    orte_daemon_cmd_flag_t command = ORTE_DAEMON_DVM_NIDMAP_CMD;
    orte_grpcomm_signature_t *sig;
    opal_buffer_t *wireup;
    orte_job_t *jptr;
    orte_proc_t *dmn;
    opal_byte_object_t bo, *boptr;
    int8_t flag;
    int32_t numbytes, v;
    char *nidmap;
    opal_list_t *modex;
    opal_value_t *val, *kv;

    ORTE_ACQUIRE_OBJECT(caddy);

    /* if this is my job, then we are done */
    if (ORTE_PROC_MY_NAME->jobid == caddy->jdata->jobid) {
        /* if there is only one daemon in the job, then there
         * is just a little bit to do */
        if (1 == orte_process_info.num_procs) {
            if (!orte_nidmap_communicated) {
                if (ORTE_SUCCESS != (rc = orte_regx.nidmap_create(orte_node_pool, &orte_node_regex))) {
                    ORTE_ERROR_LOG(rc);
                    return;
                }
                orte_nidmap_communicated = true;
            }
        } else {
            /* send the daemon map to every daemon in this DVM - we
             * do this here so we don't have to do it for every
             * job we are going to launch */
            buf = OBJ_NEW(opal_buffer_t);
            opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD);
            /* if we couldn't provide the allocation regex on the orted
             * cmd line, then we need to provide all the info here */
            if (!orte_nidmap_communicated) {
                if (ORTE_SUCCESS != (rc = orte_regx.nidmap_create(orte_node_pool, &nidmap))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(buf);
                    return;
                }
                orte_nidmap_communicated = true;
            } else {
                nidmap = NULL;
            }
            opal_dss.pack(buf, &nidmap, 1, OPAL_STRING);
            if (NULL != nidmap) {
                free(nidmap);
            }
            /* provide the info on the capabilities of each node */
            if (!orte_node_info_communicated) {
                flag = 1;
                opal_dss.pack(buf, &flag, 1, OPAL_INT8);
                if (ORTE_SUCCESS != (rc = orte_regx.encode_nodemap(buf))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(buf);
                    return;
                }
                orte_node_info_communicated = true;
                /* get wireup info for daemons */
                jptr = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
                wireup = OBJ_NEW(opal_buffer_t);
                for (v=0; v < jptr->procs->size; v++) {
                    if (NULL == (dmn = (orte_proc_t*)opal_pointer_array_get_item(jptr->procs, v))) {
                        continue;
                    }
                    val = NULL;
                    if (opal_pmix.legacy_get()) {
                        if (OPAL_SUCCESS != (rc = opal_pmix.get(&dmn->name, OPAL_PMIX_PROC_URI, NULL, &val)) || NULL == val) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(buf);
                            OBJ_RELEASE(wireup);
                            return;
                        } else {
                            /* pack the name of the daemon */
                            if (ORTE_SUCCESS != (rc = opal_dss.pack(wireup, &dmn->name, 1, ORTE_NAME))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(buf);
                                OBJ_RELEASE(wireup);
                                return;
                            }
                            /* pack the URI */
                           if (ORTE_SUCCESS != (rc = opal_dss.pack(wireup, &val->data.string, 1, OPAL_STRING))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(buf);
                                OBJ_RELEASE(wireup);
                                return;
                            }
                            OBJ_RELEASE(val);
                        }
                    } else {
                        if (OPAL_SUCCESS != (rc = opal_pmix.get(&dmn->name, NULL, NULL, &val)) || NULL == val) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(buf);
                            OBJ_RELEASE(wireup);
                            return;
                        } else {
                            /* pack the name of the daemon */
                            if (ORTE_SUCCESS != (rc = opal_dss.pack(wireup, &dmn->name, 1, ORTE_NAME))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(buf);
                                OBJ_RELEASE(wireup);
                                return;
                            }
                            /* the data is returned as a list of key-value pairs in the opal_value_t */
                            if (OPAL_PTR != val->type) {
                                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                                OBJ_RELEASE(buf);
                                OBJ_RELEASE(wireup);
                                return;
                            }
                            modex = (opal_list_t*)val->data.ptr;
                            numbytes = (int32_t)opal_list_get_size(modex);
                            if (ORTE_SUCCESS != (rc = opal_dss.pack(wireup, &numbytes, 1, OPAL_INT32))) {
                                ORTE_ERROR_LOG(rc);
                                OBJ_RELEASE(buf);
                                OBJ_RELEASE(wireup);
                                return;
                            }
                            OPAL_LIST_FOREACH(kv, modex, opal_value_t) {
                                if (ORTE_SUCCESS != (rc = opal_dss.pack(wireup, &kv, 1, OPAL_VALUE))) {
                                    ORTE_ERROR_LOG(rc);
                                    OBJ_RELEASE(buf);
                                    OBJ_RELEASE(wireup);
                                    return;
                                }
                            }
                            OPAL_LIST_RELEASE(modex);
                            OBJ_RELEASE(val);
                        }
                    }
                }
                /* put it in a byte object for xmission */
                opal_dss.unload(wireup, (void**)&bo.bytes, &numbytes);
                /* pack the byte object - zero-byte objects are fine */
                bo.size = numbytes;
                boptr = &bo;
                if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &boptr, 1, OPAL_BYTE_OBJECT))) {
                    ORTE_ERROR_LOG(rc);
                    OBJ_RELEASE(wireup);
                    OBJ_RELEASE(buf);
                    return;
                }
                /* release the data since it has now been copied into our buffer */
                if (NULL != bo.bytes) {
                    free(bo.bytes);
                }
                OBJ_RELEASE(wireup);
            } else {
                flag = 0;
                opal_dss.pack(buf, &flag, 1, OPAL_INT8);
            }

            /* goes to all daemons */
            sig = OBJ_NEW(orte_grpcomm_signature_t);
            sig->signature = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
            sig->signature[0].jobid = ORTE_PROC_MY_NAME->jobid;
            sig->signature[0].vpid = ORTE_VPID_WILDCARD;
            if (ORTE_SUCCESS != (rc = orte_grpcomm.xcast(sig, ORTE_RML_TAG_DAEMON, buf))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buf);
                OBJ_RELEASE(sig);
                ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
                return;
            }
            OBJ_RELEASE(buf);
        }
        /* notify that the vm is ready */
        fprintf(stdout, "DVM ready\n");
        OBJ_RELEASE(caddy);
        return;
    }

    /* progress the job */
    caddy->jdata->state = ORTE_JOB_STATE_VM_READY;

    /* position any required files */
    if (ORTE_SUCCESS != orte_filem.preposition_files(caddy->jdata, files_ready, caddy->jdata)) {
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
    }

    /* cleanup */
    OBJ_RELEASE(caddy);
}

static void check_complete(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_proc_t *proc;
    int i;
    orte_node_t *node;
    orte_job_map_t *map;
    orte_std_cntr_t index;
    char *rtmod;

    ORTE_ACQUIRE_OBJECT(caddy);
    jdata = caddy->jdata;

    opal_output_verbose(2, orte_state_base_framework.framework_output,
                        "%s state:dvm:check_job_complete on job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid));

    if (NULL == jdata || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        /* just check to see if the daemons are complete */
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                             "%s state:dvm:check_job_complete - received NULL job, checking daemons",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        rtmod = orte_rml.get_routed(orte_mgmt_conduit);
        if (0 == orte_routed.num_routes(rtmod)) {
            /* orteds are done! */
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                 "%s orteds complete - exiting",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            if (NULL == jdata) {
                jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            }
            ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_DAEMONS_TERMINATED);
            OBJ_RELEASE(caddy);
            return;
        }
        OBJ_RELEASE(caddy);
        return;
    }

    /* mark the job as terminated, but don't override any
     * abnormal termination flags
     */
    if (jdata->state < ORTE_JOB_STATE_UNTERMINATED) {
        jdata->state = ORTE_JOB_STATE_TERMINATED;
    }

    /* tell the IOF that the job is complete */
    if (NULL != orte_iof.complete) {
        orte_iof.complete(jdata);
    }

    /* tell the PMIx subsystem the job is complete */
    if (NULL != opal_pmix.server_deregister_nspace) {
        opal_pmix.server_deregister_nspace(jdata->jobid, NULL, NULL);
    }

    /* Release the resources used by this job. Since some errmgrs may want
     * to continue using resources allocated to the job as part of their
     * fault recovery procedure, we only do this once the job is "complete".
     * Note that an aborted/killed job -is- flagged as complete and will
     * therefore have its resources released. We need to do this after
     * we call the errmgr so that any attempt to restart the job will
     * avoid doing so in the exact same place as the current job
     */
    if (NULL != jdata->map) {
        map = jdata->map;
        for (index = 0; index < map->nodes->size; index++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, index))) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                 "%s state:dvm releasing procs from node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            for (i = 0; i < node->procs->size; i++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                    continue;
                }
                if (proc->name.jobid != jdata->jobid) {
                    /* skip procs from another job */
                    continue;
                }
                node->slots_inuse--;
                node->num_procs--;
                OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                     "%s state:dvm releasing proc %s from node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name), node->name));
                /* set the entry in the node array to NULL */
                opal_pointer_array_set_item(node->procs, i, NULL);
                /* release the proc once for the map entry */
                OBJ_RELEASE(proc);
            }
            /* set the node location to NULL */
            opal_pointer_array_set_item(map->nodes, index, NULL);
            /* maintain accounting */
            OBJ_RELEASE(node);
            /* flag that the node is no longer in a map */
            ORTE_FLAG_UNSET(node, ORTE_NODE_FLAG_MAPPED);
        }
        OBJ_RELEASE(map);
        jdata->map = NULL;
    }

    if (ORTE_FLAG_TEST(jdata, ORTE_JOB_FLAG_DEBUGGER_DAEMON)) {
        /* this was a debugger daemon. notify that a debugger has detached */
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_DEBUGGER_DETACH);
    } else if (jdata->state != ORTE_JOB_STATE_NOTIFIED) {
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                             "%s state:dvm:check_job_completed state is terminated - activating notify",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_NOTIFY_COMPLETED);
        /* mark the job as notified */
        jdata->state = ORTE_JOB_STATE_NOTIFIED;
    }

    OBJ_RELEASE(caddy);
}

static void cleanup_job(int sd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;

    ORTE_ACQUIRE_OBJECT(caddy);
    jdata = caddy->jdata;

    /* remove this object from the job array */
    opal_hash_table_set_value_uint32(orte_job_data, jdata->jobid, NULL);

    OBJ_RELEASE(caddy);
}

typedef struct {
    opal_list_t *info;
    orte_job_t *jdata;
} mycaddy_t;

static void notify_complete(int status, void *cbdata)
{
    mycaddy_t *mycaddy = (mycaddy_t*)cbdata;

    OPAL_LIST_RELEASE(mycaddy->info);
    ORTE_ACTIVATE_JOB_STATE(mycaddy->jdata, ORTE_JOB_STATE_NOTIFIED);
    OBJ_RELEASE(mycaddy->jdata);
    free(mycaddy);
}

static void dvm_notify(int sd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata = caddy->jdata;
    orte_proc_t *pptr=NULL;
    int ret;
    opal_buffer_t *reply;
    orte_daemon_cmd_flag_t command;
    orte_grpcomm_signature_t *sig;
    bool notify = true;
    opal_list_t *info;
    opal_value_t *val;
    opal_process_name_t pname, *proc, pnotify;
    mycaddy_t *mycaddy;

    /* see if there was any problem */
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_ABORTED_PROC, (void**)&pptr, OPAL_PTR) && NULL != pptr) {
        ret = pptr->exit_code;
    /* or whether we got cancelled by the user */
    } else if (orte_get_attribute(&jdata->attributes, ORTE_JOB_CANCELLED, NULL, OPAL_BOOL)) {
        ret = ORTE_ERR_JOB_CANCELLED;
    } else {
        ret = ORTE_SUCCESS;
    }

    if (0 == ret && orte_get_attribute(&jdata->attributes, ORTE_JOB_SILENT_TERMINATION, NULL, OPAL_BOOL)) {
        notify = false;
    }
    /* if the jobid matches that of the requestor, then don't notify */
    proc = &pnotify;
    if (orte_get_attribute(&jdata->attributes, ORTE_JOB_LAUNCH_PROXY, (void**)&proc, OPAL_NAME)) {
        if (pnotify.jobid == jdata->jobid) {
            notify = false;
        }
    }

    if (notify) {
        /* the source is the job that terminated */
        pname.jobid = jdata->jobid;
        pname.vpid = OPAL_VPID_WILDCARD;

        info = OBJ_NEW(opal_list_t);
        /* ensure this only goes to the job terminated event handler */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_EVENT_NON_DEFAULT);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(info, &val->super);
        /* tell the server not to cache the event as subsequent jobs
         * do not need to know about it */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_EVENT_DO_NOT_CACHE);
        val->type = OPAL_BOOL;
        val->data.flag = true;
        opal_list_append(info, &val->super);
        /* provide the status */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_JOB_TERM_STATUS);
        val->type = OPAL_STATUS;
        val->data.status = ret;
        opal_list_append(info, &val->super);
        /* tell the requestor which job or proc  */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_PROCID);
        val->type = OPAL_NAME;
        val->data.name.jobid = jdata->jobid;
        if (NULL != pptr) {
            val->data.name.vpid = pptr->name.vpid;
        } else {
            val->data.name.vpid = ORTE_VPID_WILDCARD;
        }
        opal_list_append(info, &val->super);
        /* pass along the proc to be notified */
        val = OBJ_NEW(opal_value_t);
        val->key = strdup(OPAL_PMIX_EVENT_CUSTOM_RANGE);
        val->type = OPAL_NAME;
        val->data.name.jobid = pnotify.jobid;
        val->data.name.vpid = pnotify.vpid;
        opal_list_append(info, &val->super);
        /* setup the caddy */
        mycaddy = (mycaddy_t*)malloc(sizeof(mycaddy_t));
        mycaddy->info = info;
        OBJ_RETAIN(jdata);
        mycaddy->jdata = jdata;
        opal_pmix.server_notify_event(OPAL_ERR_JOB_TERMINATED, &pname,
                                      info, notify_complete, mycaddy);
    }

    /* now ensure that _all_ daemons know that this job has terminated so even
     * those that did not participate in it will know to cleanup the resources
     * they assigned to the job. This is necessary now that the mapping function
     * has been moved to the backend daemons - otherwise, non-participating daemons
     * retain the slot assignments on the participating daemons, and then incorrectly
     * map subsequent jobs thinking those nodes are still "busy" */
    reply = OBJ_NEW(opal_buffer_t);
    command = ORTE_DAEMON_DVM_CLEANUP_JOB_CMD;
    opal_dss.pack(reply, &command, 1, ORTE_DAEMON_CMD);
    opal_dss.pack(reply, &jdata->jobid, 1, ORTE_JOBID);
    sig = OBJ_NEW(orte_grpcomm_signature_t);
    sig->signature = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    sig->signature[0].jobid = ORTE_PROC_MY_NAME->jobid;
    sig->signature[0].vpid = ORTE_VPID_WILDCARD;
    orte_grpcomm.xcast(sig, ORTE_RML_TAG_DAEMON, reply);
    OBJ_RELEASE(reply);
    OBJ_RELEASE(sig);
}
