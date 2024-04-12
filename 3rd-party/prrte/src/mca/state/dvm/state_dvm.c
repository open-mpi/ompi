/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      IBM Corporation.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#include "src/pmix/pmix-internal.h"
#include "src/prted/pmix/pmix_server.h"
#include "src/prted/pmix/pmix_server_internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/nidmap.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_output.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/filem/filem.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/odls_types.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_data_server.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/prte_wait.h"
#include "src/threads/pmix_threads.h"

#include "src/mca/state/base/base.h"
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
static void job_started(int fd, short args, void *cbata);
static void ready_for_debug(int fd, short args, void *cbata);

/******************
 * DVM module - used when mpirun is persistent
 ******************/
prte_state_base_module_t prte_state_dvm_module = {
    .init = init,
    .finalize = finalize,
    .activate_job_state = prte_state_base_activate_job_state,
    .add_job_state = prte_state_base_add_job_state,
    .set_job_state_callback = prte_state_base_set_job_state_callback,
    .remove_job_state = prte_state_base_remove_job_state,
    .activate_proc_state = prte_state_base_activate_proc_state,
    .add_proc_state = prte_state_base_add_proc_state,
    .set_proc_state_callback = prte_state_base_set_proc_state_callback,
    .remove_proc_state = prte_state_base_remove_proc_state
};

static void dvm_notify(int sd, short args, void *cbdata);

/* defined default state machine sequence - individual
 * plm's must add a state for launching daemons
 */
static prte_job_state_t launch_states[] = {
    PRTE_JOB_STATE_INIT,
    PRTE_JOB_STATE_INIT_COMPLETE,
    PRTE_JOB_STATE_ALLOCATE,
    PRTE_JOB_STATE_ALLOCATION_COMPLETE,
    PRTE_JOB_STATE_DAEMONS_LAUNCHED,
    PRTE_JOB_STATE_DAEMONS_REPORTED,
    PRTE_JOB_STATE_VM_READY,
    PRTE_JOB_STATE_MAP,
    PRTE_JOB_STATE_MAP_COMPLETE,
    PRTE_JOB_STATE_SYSTEM_PREP,
    PRTE_JOB_STATE_LAUNCH_APPS,
    PRTE_JOB_STATE_SEND_LAUNCH_MSG,
    PRTE_JOB_STATE_STARTED,
    PRTE_JOB_STATE_LOCAL_LAUNCH_COMPLETE,
    PRTE_JOB_STATE_READY_FOR_DEBUG,
    PRTE_JOB_STATE_RUNNING,
    PRTE_JOB_STATE_REGISTERED,
    /* termination states */
    PRTE_JOB_STATE_TERMINATED,
    PRTE_JOB_STATE_NOTIFY_COMPLETED,
    PRTE_JOB_STATE_NOTIFIED,
    PRTE_JOB_STATE_ALL_JOBS_COMPLETE
};

static prte_state_cbfunc_t launch_callbacks[] = {
    prte_plm_base_setup_job,
    init_complete,
    prte_ras_base_allocate,
    prte_plm_base_allocation_complete,
    prte_plm_base_daemons_launched,
    prte_plm_base_daemons_reported,
    vm_ready,
    prte_rmaps_base_map_job,
    prte_plm_base_mapping_complete,
    prte_plm_base_complete_setup,
    prte_plm_base_launch_apps,
    prte_plm_base_send_launch_msg,
    job_started,
    prte_state_base_local_launch_complete,
    ready_for_debug,
    prte_plm_base_post_launch,
    prte_plm_base_registered,
    check_complete,
    dvm_notify,
    cleanup_job,
    prte_quit
};

static prte_proc_state_t proc_states[] = {
    PRTE_PROC_STATE_RUNNING,
    PRTE_PROC_STATE_READY_FOR_DEBUG,
    PRTE_PROC_STATE_REGISTERED,
    PRTE_PROC_STATE_IOF_COMPLETE,
    PRTE_PROC_STATE_WAITPID_FIRED,
    PRTE_PROC_STATE_TERMINATED
};

static prte_state_cbfunc_t proc_callbacks[] = {
    prte_state_base_track_procs,
    prte_state_base_track_procs,
    prte_state_base_track_procs,
    prte_state_base_track_procs,
    prte_state_base_track_procs,
    prte_state_base_track_procs
};

static void force_quit(int fd, short args, void *cbdata)
{
    PRTE_HIDE_UNUSED_PARAMS(fd, args);
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;

    /* give us a chance to stop the orteds */
    prte_plm.terminate_orteds();
    PMIX_RELEASE(caddy);
}

/************************
 * Local variables
 ************************/
static bool terminate_dvm = false;
static bool dvm_terminated = false;


/************************
 * API Definitions
 ************************/
static int init(void)
{
    int i, rc;
    int num_states;

    /* setup the state machines */
    PMIX_CONSTRUCT(&prte_job_states, pmix_list_t);
    PMIX_CONSTRUCT(&prte_proc_states, pmix_list_t);

    /* setup the job state machine */
    num_states = sizeof(launch_states) / sizeof(prte_job_state_t);
    for (i = 0; i < num_states; i++) {
        if (PRTE_SUCCESS
            != (rc = prte_state.add_job_state(launch_states[i], launch_callbacks[i]))) {
            PRTE_ERROR_LOG(rc);
        }
    }
    /* add the termination response */
    rc = prte_state.add_job_state(PRTE_JOB_STATE_DAEMONS_TERMINATED, prte_quit);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }
    /* add a default error response */
    rc = prte_state.add_job_state(PRTE_JOB_STATE_FORCED_EXIT, force_quit);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }
    /* add callback to report progress, if requested */
    rc = prte_state.add_job_state(PRTE_JOB_STATE_REPORT_PROGRESS,
                                  prte_state_base_report_progress);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }
    if (5 < pmix_output_get_verbosity(prte_state_base_framework.framework_output)) {
        prte_state_base_print_job_state_machine();
    }

    /* populate the proc state machine to allow us to
     * track proc lifecycle changes
     */
    num_states = sizeof(proc_states) / sizeof(prte_proc_state_t);
    for (i = 0; i < num_states; i++) {
        rc = prte_state.add_proc_state(proc_states[i], proc_callbacks[i]);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
        }
    }
    if (5 < pmix_output_get_verbosity(prte_state_base_framework.framework_output)) {
        prte_state_base_print_proc_state_machine();
    }

    return PRTE_SUCCESS;
}

static int finalize(void)
{
    /* cleanup the state machines */
    PMIX_LIST_DESTRUCT(&prte_proc_states);
    PMIX_LIST_DESTRUCT(&prte_job_states);

    return PRTE_SUCCESS;
}

static void files_ready(int status, void *cbdata)
{
    prte_job_t *jdata = (prte_job_t *) cbdata;

    if (PRTE_SUCCESS != status) {
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FILES_POSN_FAILED);
    } else {
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_MAP);
    }
}

static void init_complete(int sd, short args, void *cbdata)
{
    PRTE_HIDE_UNUSED_PARAMS(sd, args);
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(caddy);

    /* need to go thru allocate step in case someone wants to
     * expand the DVM */
    PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_ALLOCATE);
    PMIX_RELEASE(caddy);
}

static void vm_ready(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    int rc, i;
    pmix_data_buffer_t buf;
    prte_grpcomm_signature_t sig;
    prte_job_t *jptr;
    prte_proc_t *dmn;
    int32_t v;
    pmix_value_t *val;
    pmix_status_t ret;
    PRTE_HIDE_UNUSED_PARAMS(fd, args, cbdata);

    PMIX_ACQUIRE_OBJECT(caddy);
    /* if this is my job, then we are done */
    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_LAUNCHED_DAEMONS, NULL, PMIX_BOOL)) {
        /* if there is more than one daemon in the job, then there
         * is just a little bit to do */
        if (!prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)
            && 1 < prte_process_info.num_daemons) {
            /* send the daemon map to every daemon in this DVM - we
             * do this here so we don't have to do it for every
             * job we are going to launch */
            PMIX_DATA_BUFFER_CONSTRUCT(&buf);
            rc = prte_util_nidmap_create(prte_node_pool, &buf);
            if (PRTE_SUCCESS != rc) {
                PRTE_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_DESTRUCT(&buf);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                return;
            }
            /* get wireup info for daemons */
            jptr = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
            for (v = 0; v < jptr->procs->size; v++) {
                if (NULL == (dmn = (prte_proc_t *) pmix_pointer_array_get_item(jptr->procs, v))) {
                    continue;
                }
                val = NULL;
                if (PMIX_SUCCESS != (ret = PMIx_Get(&dmn->name, PMIX_PROC_URI, NULL, 0, &val)) ||
                    NULL == val) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_DESTRUCT(&buf);
                    PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                    return;
                }
                rc = PMIx_Data_pack(NULL, &buf, &dmn->name, 1, PMIX_PROC);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_DESTRUCT(&buf);
                    PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                    return;
                }
                rc = PMIx_Data_pack(NULL, &buf, &val->data.string, 1, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_DESTRUCT(&buf);
                    PMIX_VALUE_RELEASE(val);
                    PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                    return;
                }
                PMIX_VALUE_RELEASE(val);
            }

            /* goes to all daemons */
            PMIX_PROC_CREATE(sig.signature, 1);
            PMIX_LOAD_PROCID(&sig.signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
            sig.sz = 1;
            if (PRTE_SUCCESS != (rc = prte_grpcomm.xcast(&sig, PRTE_RML_TAG_WIREUP, &buf))) {
                PRTE_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_DESTRUCT(&buf);
                PMIX_PROC_FREE(sig.signature, 1);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                return;
            }
            PMIX_DATA_BUFFER_DESTRUCT(&buf);
            PMIX_PROC_FREE(sig.signature, 1);
        }
    }
    if (PMIX_CHECK_NSPACE(PRTE_PROC_MY_NAME->nspace, caddy->jdata->nspace)) {
        prte_dvm_ready = true;
        /* notify that the vm is ready */
        if (0 > prte_state_base.parent_fd) {
            if (prte_state_base.ready_msg && prte_persistent) {
                fprintf(stdout, "DVM ready\n");
                fflush(stdout);
            }
        } else {
            char ok = 'K';
            write(prte_state_base.parent_fd, &ok, 1);
            close(prte_state_base.parent_fd);
            prte_state_base.parent_fd = -1;
        }
        for (i = 0; i < prte_cache->size; i++) {
            jptr = (prte_job_t *) pmix_pointer_array_get_item(prte_cache, i);
            if (NULL != jptr) {
                pmix_pointer_array_set_item(prte_cache, i, NULL);
                prte_plm.spawn(jptr);
            }
        }
        /* progress the job */
        caddy->jdata->state = PRTE_JOB_STATE_VM_READY;
        PMIX_RELEASE(caddy);
        return;
    }

    /* position any required files */
    if (PRTE_SUCCESS != prte_filem.preposition_files(caddy->jdata, files_ready, caddy->jdata)) {
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_FILES_POSN_FAILED);
    }
    PMIX_RELEASE(caddy);
}

static void job_started(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata = caddy->jdata;
    pmix_info_t *iptr;
    time_t timestamp;
    pmix_proc_t *nptr;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    /* if there is an originator for this job, notify them
     * that the first process of the job has been started */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DVM_JOB, NULL, PMIX_BOOL)) {
        /* dvm job => launch was requested by a TOOL, so we notify the launch proxy
         * and NOT the originator (as that would be us) */
        nptr = NULL;
        if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCH_PROXY, (void **) &nptr, PMIX_PROC)
            || NULL == nptr) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            return;
        }
        timestamp = time(NULL);
        PMIX_INFO_CREATE(iptr, 5);
        /* target this notification solely to that one tool */
        PMIX_INFO_LOAD(&iptr[0], PMIX_EVENT_CUSTOM_RANGE, nptr, PMIX_PROC);
        PMIX_PROC_RELEASE(nptr);
        /* pass the nspace of the spawned job */
        PMIX_INFO_LOAD(&iptr[1], PMIX_NSPACE, jdata->nspace, PMIX_STRING);
        /* not to be delivered to a default event handler */
        PMIX_INFO_LOAD(&iptr[2], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
        /* provide the timestamp */
        PMIX_INFO_LOAD(&iptr[3], PMIX_EVENT_TIMESTAMP, &timestamp, PMIX_TIME);
        PMIX_INFO_LOAD(&iptr[4], "prte.notify.donotloop", NULL, PMIX_BOOL);
        PMIx_Notify_event(PMIX_EVENT_JOB_START, &prte_process_info.myproc, PMIX_RANGE_CUSTOM, iptr,
                          5, NULL, NULL);
        PMIX_INFO_FREE(iptr, 5);
    }

    PMIX_RELEASE(caddy);
}

static void ready_for_debug(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata = caddy->jdata;
    pmix_proc_t *nptr;
    time_t timestamp;
    pmix_info_t *iptr;
    size_t ninfo;
    pmix_data_array_t darray;
    void *tinfo;
    pmix_status_t rc;
    int n;
    char *name;
    prte_app_context_t *app;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    /* launch was requested by a TOOL, so we notify the launch proxy
     * and NOT the originator (as that would be us) */
    nptr = NULL;
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCH_PROXY, (void **) &nptr, PMIX_PROC)
        || NULL == nptr) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        goto DONE;
    }
    timestamp = time(NULL);
    PMIX_INFO_LIST_START(tinfo);
    /* target this notification solely to that one tool */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_EVENT_CUSTOM_RANGE, nptr, PMIX_PROC);
    PMIX_PROC_RELEASE(nptr);
    /* pass the nspace of the job */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_NSPACE, jdata->nspace, PMIX_STRING);
    for (n=0; n < jdata->apps->size; n++) {
        app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, n);
        if (NULL == app) {
            continue;
        }
        /* if pset name was assigned, pass it */
       if (prte_get_attribute(&app->attributes, PRTE_APP_PSET_NAME, (void**) &name, PMIX_STRING)) {
           PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_PSET_NAME, name, PMIX_STRING);
           free(name);
        }
        /* pass the argv from each app */
        name = PMIX_ARGV_JOIN_COMPAT(app->argv, ' ');
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_APP_ARGV, name, PMIX_STRING);
        free(name);
    }

    /* not to be delivered to a default event handler */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    /* provide the timestamp */
    PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_EVENT_TIMESTAMP, &timestamp, PMIX_TIME);
    PMIX_INFO_LIST_ADD(rc, tinfo, "prte.notify.donotloop", NULL, PMIX_BOOL);
    PMIX_INFO_LIST_CONVERT(rc, tinfo, &darray);
    if (PMIX_ERR_EMPTY == rc) {
        iptr = NULL;
        ninfo = 0;
    } else if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_UPDATE_EXIT_STATUS(rc);
        PMIX_INFO_LIST_RELEASE(tinfo);
        PMIX_PROC_RELEASE(nptr);
        goto DONE;
    } else {
        iptr = (pmix_info_t *) darray.array;
        ninfo = darray.size;
    }
    PMIX_INFO_LIST_RELEASE(tinfo);

    PMIx_Notify_event(PMIX_READY_FOR_DEBUG, PRTE_PROC_MY_NAME, PMIX_RANGE_CUSTOM, iptr,
                      ninfo, NULL, NULL);
    PMIX_INFO_FREE(iptr, ninfo);

DONE:
    PMIX_RELEASE(caddy);
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lk = (prte_pmix_lock_t *) cbdata;

    PMIX_POST_OBJECT(lk);
    lk->status = prte_pmix_convert_status(status);
    PRTE_PMIX_WAKEUP_THREAD(lk);
}
static void lkcbfunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lk = (prte_pmix_lock_t *) cbdata;

    PMIX_POST_OBJECT(lk);
    lk->status = prte_pmix_convert_status(status);
    PRTE_PMIX_WAKEUP_THREAD(lk);
}
static void check_complete(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata, *jptr;
    prte_proc_t *proc;
    int i, rc;
    prte_node_t *node;
    prte_job_map_t *map;
    int32_t index;
    pmix_proc_t pname;
    prte_pmix_lock_t lock;
    uint8_t command = PRTE_PMIX_PURGE_PROC_CMD;
    pmix_data_buffer_t *buf;
    pmix_pointer_array_t procs;
    char *tmp;
    prte_timer_t *timer;
    prte_app_context_t *app;
    hwloc_obj_t obj;
    hwloc_obj_type_t type;
    hwloc_cpuset_t boundcpus, tgt;
    bool takeall;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);
    jdata = caddy->jdata;

    pmix_output_verbose(2, prte_state_base_framework.framework_output,
                        "%s state:dvm:check_job_complete on job %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        (NULL == jdata) ? "NULL" : PRTE_JOBID_PRINT(jdata->nspace));

    if (NULL != jdata &&
        prte_get_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT_EVENT, (void **) &timer, PMIX_POINTER)) {
        /* timer is an prte_timer_t object */
        prte_event_evtimer_del(timer->ev);
        PMIX_RELEASE(timer);
        prte_remove_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT_EVENT);
    }

    if (NULL == jdata || PMIX_CHECK_NSPACE(jdata->nspace, PRTE_PROC_MY_NAME->nspace)) {
        /* just check to see if the daemons are complete */
        PMIX_OUTPUT_VERBOSE(
            (2, prte_state_base_framework.framework_output,
             "%s state:dvm:check_job_complete - received NULL job, checking daemons",
             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        if (0 == pmix_list_get_size(&prte_rml_base.children)) {
            /* orteds are done! */
            PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                                 "%s prteds complete - exiting",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            if (NULL == jdata) {
                jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
            }
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            PMIX_RELEASE(caddy);
            prte_dvm_ready = false;
            return;
        }
        prte_plm.terminate_orteds();
        PMIX_RELEASE(caddy);
        return;
    }

    /* mark the job as terminated, but don't override any
     * abnormal termination flags
     */
    if (jdata->state < PRTE_JOB_STATE_UNTERMINATED) {
        jdata->state = PRTE_JOB_STATE_TERMINATED;
    }

    /* see if there was any problem */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, NULL, PMIX_POINTER)) {
        rc = prte_pmix_convert_rc(jdata->exit_code);
        /* or whether we got cancelled by the user */
    } else if (prte_get_attribute(&jdata->attributes, PRTE_JOB_CANCELLED, NULL, PMIX_BOOL)) {
        rc = prte_pmix_convert_rc(PRTE_ERR_JOB_CANCELLED);
    } else {
        rc = prte_pmix_convert_rc(jdata->exit_code);
    }

    /* if would be rare, but a very fast terminating job could conceivably
     * reach here prior to the spawn requestor being notified of spawn */
    rc = prte_plm_base_spawn_response(rc, jdata);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }

    /* cleanup any pending server ops */
    PMIX_LOAD_PROCID(&pname, jdata->nspace, PMIX_RANK_WILDCARD);
    prte_pmix_server_clear(&pname);

    /* cleanup the local procs as these are gone */
    for (i = 0; i < prte_local_children->size; i++) {
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i))) {
            continue;
        }
        /* if this child is part of the job... */
        if (PMIX_CHECK_NSPACE(proc->name.nspace, jdata->nspace)) {
            /* clear the entry in the local children */
            pmix_pointer_array_set_item(prte_local_children, i, NULL);
            PMIX_RELEASE(proc); // maintain accounting
        }
    }

    /* tell the IOF that the job is complete */
    if (NULL != prte_iof.complete) {
        prte_iof.complete(jdata);
    }

    /* tell the PMIx subsystem the job is complete */
    PRTE_PMIX_CONSTRUCT_LOCK(&lock);
    PMIx_server_deregister_nspace(pname.nspace, opcbfunc, &lock);
    PRTE_PMIX_WAIT_THREAD(&lock);
    PRTE_PMIX_DESTRUCT_LOCK(&lock);

    if (!prte_persistent) {
        /* update our exit status */
        PRTE_UPDATE_EXIT_STATUS(jdata->exit_code);
        /* if this is an abnormal termination, report it */
        if (jdata->state > PRTE_JOB_STATE_ERROR) {
            char *msg;
            msg = prte_dump_aborted_procs(jdata);
            if (NULL != msg) {
                pmix_byte_object_t bo;
                PMIX_BYTE_OBJECT_CONSTRUCT(&bo);
                bo.bytes = (char *) msg;
                bo.size = strlen(msg);
                PRTE_PMIX_CONSTRUCT_LOCK(&lock);
                rc = PMIx_server_IOF_deliver(&prte_process_info.myproc,
                                             PMIX_FWD_STDDIAG_CHANNEL,
                                             &bo, NULL, 0, lkcbfunc, (void *) &lock);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                } else {
                    /* wait for completion */
                    PRTE_PMIX_WAIT_THREAD(&lock);
                    if (PMIX_SUCCESS != lock.status) {
                        PMIX_ERROR_LOG(lock.status);
                    }
                }
                PRTE_PMIX_DESTRUCT_LOCK(&lock);
                free(msg);
            }
        }
        /* if all of the jobs we are running are done, then shut us down */
        for (i = 0; i < prte_job_data->size; i++) {
            jptr = (prte_job_t *) pmix_pointer_array_get_item(prte_job_data, i);
            if (NULL == jptr) {
                continue;
            }
            /* skip the daemon job */
            if (PMIX_CHECK_NSPACE(jptr->nspace, PRTE_PROC_MY_NAME->nspace)) {
                continue;
            }
            if (jptr->state < PRTE_JOB_STATE_TERMINATED) {
                /* still alive - finish processing this job's termination */
                goto release;
            }
        }

        /* Let the tools know that a job terminated before we shutdown */
        if (jdata->state != PRTE_JOB_STATE_NOTIFIED) {
            PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                                 "%s state:dvm:check_job_completed state is terminated - activating notify",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            terminate_dvm = true;  // flag that the DVM is to terminate
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_NOTIFY_COMPLETED);
            PMIX_RELEASE(caddy);
            return;
        }

        /* if we fell thru to this point, then nobody is still
         * alive except the daemons, so just shut us down */
        prte_plm.terminate_orteds();
        PMIX_RELEASE(caddy);
        return;
    }

    if (NULL != prte_data_server_uri) {
        /* tell the data server to purge any data from this nspace */
        PMIX_DATA_BUFFER_CREATE(buf);
        /* room number is ignored, but has to be included for pack sequencing */
        i = 0;
        rc = PMIx_Data_pack(NULL, buf, &i, 1, PMIX_INT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
            goto release;
        }
        rc = PMIx_Data_pack(NULL, buf, &command, 1, PMIX_UINT8);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
            goto release;
        }
        /* pack the nspace to be purged */
        pname.rank = PMIX_RANK_WILDCARD;
        rc = PMIx_Data_pack(NULL, buf, &pname, 1, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
            goto release;
        }
        /* send it to the data server */
        PRTE_RML_SEND(rc, PRTE_PROC_MY_NAME->rank, buf, PRTE_RML_TAG_DATA_SERVER);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(buf);
        }
    }

release:
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
        takeall = false;
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, NULL, PMIX_BOOL)) {
            type = HWLOC_OBJ_PU;
        } else {
            type = HWLOC_OBJ_CORE;
        }
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_PES_PER_PROC, NULL, PMIX_UINT16) ||
            PRTE_MAPPING_BYUSER == PRTE_GET_MAPPING_POLICY(map->mapping) ||
            PRTE_MAPPING_SEQ == PRTE_GET_MAPPING_POLICY(map->mapping)) {
            takeall = true;
        }
        boundcpus = hwloc_bitmap_alloc();
        for (index = 0; index < map->nodes->size; index++) {
            node = (prte_node_t *) pmix_pointer_array_get_item(map->nodes, index);
            if (NULL == node) {
                continue;
            }
            PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                                 "%s state:dvm releasing procs from node %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
            for (i = 0; i < node->procs->size; i++) {
                proc = (prte_proc_t *) pmix_pointer_array_get_item(node->procs, i);
                if (NULL == proc) {
                    continue;
                }
                if (!PMIX_CHECK_NSPACE(proc->name.nspace, jdata->nspace)) {
                    /* skip procs from another job */
                    continue;
                }
                app = (prte_app_context_t*) pmix_pointer_array_get_item(jdata->apps, proc->app_idx);
                if (!PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL) &&
                    !PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_TOOL)) {
                    node->slots_inuse--;
                    node->num_procs--;
                    node->next_node_rank--;
                }
                /* release the resources held by the proc - only the first
                 * cpu in the proc's cpuset was used to mark usage */
                if (NULL != proc->cpuset) {
                    if (0 != (rc = hwloc_bitmap_list_sscanf(boundcpus, proc->cpuset))) {
                        pmix_output(0, "hwloc_bitmap_sscanf returned %s for the string %s",
                                    prte_strerror(rc), proc->cpuset);
                        continue;
                    }
                    if (takeall) {
                        tgt = boundcpus;
                    } else {
                        /* we only want to restore the first CPU of whatever region
                         * the proc was bound to, so we have to first narrow the
                         * bitmap down to only that region */
                        hwloc_bitmap_andnot(prte_rmaps_base.available, boundcpus, node->available);
                        /* the set bits in the result are the bound cpus that are still
                         * marked as in-use */
                        obj = hwloc_get_obj_inside_cpuset_by_type(node->topology->topo,
                                                                  prte_rmaps_base.available, type, 0);
                        if (NULL == obj) {
                            pmix_output(0, "COULD NOT GET BOUND CPU FOR RESOURCE RELEASE");
                            continue;
                        }
#if HWLOC_API_VERSION < 0x20000
                        tgt = obj->allowed_cpuset;
#else
                        tgt = obj->cpuset;
#endif
                    }
                    hwloc_bitmap_or(node->available, node->available, tgt);
                }

                PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                                     "%s state:dvm releasing proc %s from node %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&proc->name), node->name));
                /* set the entry in the node array to NULL */
                pmix_pointer_array_set_item(node->procs, i, NULL);
                /* release the proc once for the map entry */
                PMIX_RELEASE(proc);
            }
            /* set the node location to NULL */
            pmix_pointer_array_set_item(map->nodes, index, NULL);
            /* maintain accounting */
            PMIX_RELEASE(node);
            /* flag that the node is no longer in a map */
            PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
        }
        hwloc_bitmap_free(boundcpus);
        PMIX_RELEASE(map);
        jdata->map = NULL;
    }

    /* if requested, check fd status for leaks */
    if (prte_state_base.run_fdcheck) {
        prte_state_base_check_fds(jdata);
    }

    /* if this job was a launcher, then we need to abort all of its
     * child jobs that might still be running */
    if (0 < pmix_list_get_size(&jdata->children)) {
        PMIX_CONSTRUCT(&procs, pmix_pointer_array_t);
        pmix_pointer_array_init(&procs, 1, INT_MAX, 1);
        PMIX_LIST_FOREACH(jptr, &jdata->children, prte_job_t)
        {
            proc = PMIX_NEW(prte_proc_t);
            PMIX_LOAD_PROCID(&proc->name, jptr->nspace, PMIX_RANK_WILDCARD);
            pmix_pointer_array_add(&procs, proc);
        }
        prte_plm.terminate_procs(&procs);
        for (i = 0; i < procs.size; i++) {
            if (NULL != (proc = (prte_proc_t *) pmix_pointer_array_get_item(&procs, i))) {
                PMIX_RELEASE(proc);
            }
        }
        PMIX_DESTRUCT(&procs);
    }

    if (jdata->state != PRTE_JOB_STATE_NOTIFIED) {
        PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                             "%s state:dvm:check_job_completed state is terminated - activating notify",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_NOTIFY_COMPLETED);
        /* mark the job as notified */
        jdata->state = PRTE_JOB_STATE_NOTIFIED;
    }

    PMIX_POST_OBJECT(jdata);
    PMIX_RELEASE(caddy);
}

static void cleanup_job(int sd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    if (terminate_dvm && !dvm_terminated) {
        dvm_terminated = true;
        prte_plm.terminate_orteds();
    }
    if (NULL != caddy->jdata) {
        PMIX_RELEASE(caddy->jdata);
    }
    PMIX_RELEASE(caddy);
}

static void dvm_notify(int sd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata = caddy->jdata;
    prte_proc_t *pptr = NULL;
    int rc;
    pmix_data_buffer_t *reply;
    prte_daemon_cmd_flag_t command;
    prte_grpcomm_signature_t sig;
    bool notify = true, flag;
    pmix_proc_t *proc, pnotify;
    pmix_info_t *info;
    size_t ninfo;
    pmix_proc_t pname;
    pmix_data_buffer_t pbkt;
    pmix_data_range_t range = PMIX_RANGE_SESSION;
    pmix_status_t code, ret;
    char *errmsg = NULL;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                         "%s state:dvm:dvm_notify called",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    /* see if there was any problem */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, (void **) &pptr, PMIX_POINTER)
        && NULL != pptr) {
        rc = jdata->exit_code;
        /* or whether we got cancelled by the user */
    } else if (prte_get_attribute(&jdata->attributes, PRTE_JOB_CANCELLED, NULL, PMIX_BOOL)) {
        rc = PRTE_ERR_JOB_CANCELLED;
    } else {
        rc = jdata->exit_code;
    }

    if (0 == rc &&
        prte_get_attribute(&jdata->attributes, PRTE_JOB_SILENT_TERMINATION, NULL, PMIX_BOOL)) {
        notify = false;
    }
    /* if the jobid matches that of the requestor, then don't notify */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCH_PROXY, (void **) &proc, PMIX_PROC)) {
        if (PMIX_CHECK_NSPACE(proc->nspace, jdata->nspace)) {
            notify = false;
        }
        PMIX_PROC_RELEASE(proc);
    }

    if (notify) {
        PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                             "%s state:dvm:dvm_notify notification requested",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        /* if it was an abnormal termination, then construct an appropriate
         * error message */
        if (PRTE_SUCCESS != rc) {
            errmsg = prte_dump_aborted_procs(jdata);
        }
        /* construct the info to be provided */
        if (NULL == errmsg) {
            ninfo = 3;
        } else {
            ninfo = 4;
        }
        PMIX_INFO_CREATE(info, ninfo);
        /* ensure this only goes to the job terminated event handler */
        flag = true;
        PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, &flag, PMIX_BOOL);
        /* provide the status */
        PMIX_INFO_LOAD(&info[1], PMIX_JOB_TERM_STATUS, &rc, PMIX_STATUS);
        /* tell the requestor which job or proc  */
        PMIX_LOAD_NSPACE(pname.nspace, jdata->nspace);
        if (NULL != pptr) {
            pname.rank = pptr->name.rank;
        } else {
            pname.rank = PMIX_RANK_WILDCARD;
        }
        PMIX_INFO_LOAD(&info[2], PMIX_EVENT_AFFECTED_PROC, &pname, PMIX_PROC);
        if (NULL != errmsg) {
            PMIX_INFO_LOAD(&info[3], PMIX_EVENT_TEXT_MESSAGE, errmsg, PMIX_STRING);
            free(errmsg);
        }

        /* pack the info for sending */
        PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);

        /* pack the status code */
        code = PMIX_EVENT_JOB_END;
        if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, &code, 1, PMIX_STATUS))) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_RELEASE(caddy);
            return;
        }
        /* pack the source - it cannot be me as that will cause
         * the pmix server to upcall the event back to me */
        PMIX_LOAD_PROCID(&pnotify, jdata->nspace, 0);
        if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, &pnotify, 1, PMIX_PROC))) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_RELEASE(caddy);
            return;
        }
        /* pack the range */
        if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, &range, 1, PMIX_DATA_RANGE))) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_RELEASE(caddy);
            return;
        }
        /* pack the number of infos */
        if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, &ninfo, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_RELEASE(caddy);
            return;
        }
        /* pack the infos themselves */
        if (PMIX_SUCCESS != (ret = PMIx_Data_pack(NULL, &pbkt, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_RELEASE(caddy);
            return;
        }
        PMIX_INFO_FREE(info, ninfo);

        /* insert into pmix_data_buffer_t */
        PMIX_DATA_BUFFER_CREATE(reply);
        /* we need to add a flag indicating this came from an invalid proc so that we will
         * inject it into our own PMIx server library */
        rc = PMIx_Data_pack(NULL, reply, &PRTE_NAME_INVALID->rank, 1, PMIX_PROC_RANK);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_DATA_BUFFER_RELEASE(reply);
            PMIX_RELEASE(caddy);
            return;
        }
        rc = PMIx_Data_copy_payload(reply, &pbkt);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);

        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(reply);
            PMIX_RELEASE(caddy);
            return;
        }

        /* we have to send the notification to all daemons so that
         * anyone watching for it can receive it */
        PMIX_PROC_CREATE(sig.signature, 1);
        PMIX_LOAD_PROCID(&sig.signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
        sig.sz = 1;
        if (PRTE_SUCCESS != (rc = prte_grpcomm.xcast(&sig, PRTE_RML_TAG_NOTIFICATION, reply))) {
            PRTE_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(reply);
            PMIX_PROC_FREE(sig.signature, 1);
            PMIX_RELEASE(caddy);
            return;
        }
        PMIX_OUTPUT_VERBOSE((2, prte_state_base_framework.framework_output,
                             "%s state:dvm:dvm_notify notification sent",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        PMIX_DATA_BUFFER_RELEASE(reply);
        /* maintain accounting */
        PMIX_PROC_FREE(sig.signature, 1);
    }

    if (prte_persistent) {
        /* now ensure that _all_ daemons know that this job has terminated so even
         * those that did not participate in it will know to cleanup the resources
         * they assigned to the job. This is necessary now that the mapping function
         * has been moved to the backend daemons - otherwise, non-participating daemons
         * retain the slot assignments on the participating daemons, and then incorrectly
         * map subsequent jobs thinking those nodes are still "busy" */
        PMIX_DATA_BUFFER_CREATE(reply);
        command = PRTE_DAEMON_DVM_CLEANUP_JOB_CMD;
        rc = PMIx_Data_pack(NULL, reply, &command, 1, PMIX_UINT8);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(reply);
            return;
        }
        rc = PMIx_Data_pack(NULL, reply, &jdata->nspace, 1, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(reply);
            return;
        }
        PMIX_PROC_CREATE(sig.signature, 1);
        PMIX_LOAD_PROCID(&sig.signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
        sig.sz = 1;
        prte_grpcomm.xcast(&sig, PRTE_RML_TAG_DAEMON, reply);
        PMIX_DATA_BUFFER_RELEASE(reply);
        PMIX_PROC_FREE(sig.signature, 1);
    }

    // We are done with our use of job data and have notified the other daemons
    if (notify) {
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_NOTIFIED);
    }

    PMIX_RELEASE(caddy);
}
