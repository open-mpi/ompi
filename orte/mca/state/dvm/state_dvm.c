/*
 * Copyright (c) 2015-2016 Intel, Inc. All rights reserved
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
#include "orte/mca/iof/iof.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/nidmap.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_quit.h"

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
    } else {
        ORTE_ACTIVATE_JOB_STATE(jdata, ORTE_JOB_STATE_MAP);
    }
}

static void init_complete(int sd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;

    /* nothing to do here but move along - if it is the
     * daemon job, then next step is allocate */
    if (caddy->jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        ORTE_ACTIVATE_JOB_STATE(caddy->jdata, ORTE_JOB_STATE_ALLOCATE);
    } else {
        /* next step - position any required files */
        if (ORTE_SUCCESS != orte_filem.preposition_files(caddy->jdata, files_ready, caddy->jdata)) {
            ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        }
    }
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
    opal_byte_object_t bo, *boptr;
    int8_t flag;
    int32_t numbytes;

    /* if this is my job, then we are done */
    if (ORTE_PROC_MY_NAME->jobid == caddy->jdata->jobid) {
        /* send the daemon map to every daemon in this DVM - we
         * do this here so we don't have to do it for every
         * job we are going to launch */
        buf = OBJ_NEW(opal_buffer_t);
        /* pack the "load nidmap" cmd */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            return;
        }
        /* construct a nodemap with everything in it */
        if (ORTE_SUCCESS != (rc = orte_util_encode_nodemap(&bo, false))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            return;
        }

        /* store it */
        boptr = &bo;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &boptr, 1, OPAL_BYTE_OBJECT))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buf);
            return;
        }
        /* release the data since it has now been copied into our buffer */
        free(bo.bytes);

        /* pack a flag indicating wiring info is provided */
        flag = 1;
        opal_dss.pack(buf, &flag, 1, OPAL_INT8);
        /* get wireup info for daemons per the selected routing module */
        wireup = OBJ_NEW(opal_buffer_t);
        if (ORTE_SUCCESS != (rc = orte_routed.get_wireup_info(wireup))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(wireup);
            OBJ_RELEASE(buf);
            return;
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
    orte_job_t *jdata = caddy->jdata;

    orte_proc_t *proc;
    int i;
    orte_node_t *node;
    orte_job_map_t *map;
    orte_std_cntr_t index;

    opal_output_verbose(2, orte_state_base_framework.framework_output,
                        "%s state:dvm:check_job_complete on job %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == jdata) ? "NULL" : ORTE_JOBID_PRINT(jdata->jobid));

    if (NULL == jdata || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        /* just check to see if the daemons are complete */
        OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                             "%s state:dvm:check_job_complete - received NULL job, checking daemons",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        if (0 == orte_routed.num_routes()) {
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
        opal_pmix.server_deregister_nspace(jdata->jobid);
    }

    /* Release the resources used by this job. Since some errmgrs may want
     * to continue using resources allocated to the job as part of their
     * fault recovery procedure, we only do this once the job is "complete".
     * Note that an aborted/killed job -is- flagged as complete and will
     * therefore have its resources released. We need to do this after
     * we call the errmgr so that any attempt to restart the job will
     * avoid doing so in the exact same place as the current job
     */
    if (NULL != jdata->map  && jdata->state == ORTE_JOB_STATE_TERMINATED) {
        map = jdata->map;
        for (index = 0; index < map->nodes->size; index++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, index))) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((2, orte_state_base_framework.framework_output,
                                 "%s releasing procs from node %s",
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
                                     "%s releasing proc %s from node %s",
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
    OBJ_RELEASE(caddy);
}
