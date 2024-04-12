/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2018 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif

#include "src/class/pmix_hash_table.h"
#include "src/class/pmix_list.h"
#include "src/event/event-internal.h"
#include "src/include/hash_string.h"

#include "src/hwloc/hwloc-internal.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_fd.h"
#include "src/util/pmix_if.h"
#include "src/util/malloc.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"

#include "src/mca/errmgr/base/base.h"
#include "src/mca/filem/base/base.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/oob/base/base.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/plm/plm.h"
#include "src/mca/prtereachable/base/base.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rtc/base/base.h"
#include "src/mca/schizo/base/base.h"
#include "src/mca/state/base/base.h"
#include "src/mca/state/state.h"

#include "src/prted/pmix/pmix_server.h"
#include "src/rml/rml.h"

#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/prte_wait.h"
#include "src/runtime/runtime.h"

#include "src/mca/ess/base/base.h"
#include "src/mca/ess/ess.h"
#include "src/mca/ess/hnp/ess_hnp.h"

static int rte_init(int argc, char **argv);
static int rte_finalize(void);

prte_ess_base_module_t prte_ess_hnp_module = {
    .init = rte_init,
    .finalize = rte_finalize
};

static int rte_init(int argc, char **argv)
{
    int ret;
    char *error = NULL;
    char *contact_path;
    char *tmp;
    prte_job_t *jdata = NULL;
    prte_node_t *node;
    prte_proc_t *proc;
    prte_app_context_t *app;
    int idx;
    prte_topology_t *t;
    pmix_value_t pval;
    pmix_status_t pret;
    PRTE_HIDE_UNUSED_PARAMS(argc);

    /* run the prolog */
    if (PRTE_SUCCESS != (ret = prte_ess_base_std_prolog())) {
        error = "prte_ess_base_std_prolog";
        goto error;
    }

    /* get the local topology */
    if (NULL == prte_hwloc_topology) {
        if (PRTE_SUCCESS != (ret = prte_hwloc_base_get_topology())) {
            error = "topology discovery";
            goto error;
        }
    }

    /* open and setup the state machine */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_state_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        error = "prte_state_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_state_base_select())) {
        error = "prte_state_base_select";
        goto error;
    }

    /* open the errmgr */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_errmgr_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        error = "prte_errmgr_base_open";
        goto error;
    }

    /* Since we are the HNP, then responsibility for
     * defining the name falls to the PLM component for our
     * respective environment - hence, we have to open the PLM
     * first and select that component.
     */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_plm_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        error = "prte_plm_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_plm_base_select())) {
        error = "prte_plm_base_select";
        if (PRTE_ERR_FATAL == ret) {
            /* we already output a show_help - so keep down the verbage */
            ret = PRTE_ERR_SILENT;
        }
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_plm.set_hnp_name())) {
        error = "prte_plm_set_hnp_name";
        goto error;
    }

    /* get the job data object for the daemons */
    jdata = PMIX_NEW(prte_job_t);
    PMIX_LOAD_NSPACE(jdata->nspace, PRTE_PROC_MY_NAME->nspace);
    ret = prte_set_job_data_object(jdata);

    /* set the schizo personality to "prte" by default */
    jdata->schizo = (struct prte_schizo_base_module_t*)prte_schizo_base_detect_proxy("prte");
    if (NULL == jdata->schizo) {
        pmix_show_help("help-schizo-base.txt", "no-proxy", true, prte_tool_basename, "prte");
        error = "select personality";
        ret = PRTE_ERR_SILENT;
        goto error;
    }

    /* mark that the daemons have reported as we are the
     * only ones in the system right now, and we definitely
     * are running!
     */
    jdata->state = PRTE_JOB_STATE_DAEMONS_REPORTED;

    /* every job requires at least one app */
    app = PMIX_NEW(prte_app_context_t);
    app->app = strdup(argv[0]);
    app->argv = PMIX_ARGV_COPY_COMPAT(argv);
    app->job = (struct prte_job_t*)jdata;
    pmix_pointer_array_set_item(jdata->apps, 0, app);
    jdata->num_apps++;
    /* create and store a node object where we are */
    node = PMIX_NEW(prte_node_t);
    node->name = strdup(prte_process_info.nodename);
    node->index = PRTE_PROC_MY_NAME->rank;
    PRTE_FLAG_SET(node, PRTE_NODE_FLAG_LOC_VERIFIED);
    pmix_pointer_array_set_item(prte_node_pool, PRTE_PROC_MY_NAME->rank, node);

    /* create and store a proc object for us */
    proc = PMIX_NEW(prte_proc_t);
    PMIX_LOAD_PROCID(&proc->name, PRTE_PROC_MY_NAME->nspace, PRTE_PROC_MY_NAME->rank);
    proc->pid = prte_process_info.pid;
    proc->state = PRTE_PROC_STATE_RUNNING;
    PMIX_RETAIN(node); /* keep accounting straight */
    proc->node = node;
    pmix_pointer_array_set_item(jdata->procs, PRTE_PROC_MY_NAME->rank, proc);

    /* record that the daemon (i.e., us) is on this node
     * NOTE: we do not add the proc object to the node's
     * proc array because we are not an application proc.
     * Instead, we record it in the daemon field of the
     * node object
     */
    PMIX_RETAIN(proc); /* keep accounting straight */
    node->daemon = proc;
    PRTE_FLAG_SET(node, PRTE_NODE_FLAG_DAEMON_LAUNCHED);
    node->state = PRTE_NODE_STATE_UP;
    /* get our aliases - will include all the interface aliases captured in prte_init */
    node->aliases = PMIX_ARGV_COPY_COMPAT(prte_process_info.aliases);
    /* record that the daemon job is running */
    jdata->num_procs = 1;
    jdata->state = PRTE_JOB_STATE_RUNNING;
    /* obviously, we have "reported" */
    jdata->num_reported = 1;
    jdata->num_daemons_reported = 1;

    /* setup my session directory here as the OOB may need it */
    PMIX_OUTPUT_VERBOSE((2, prte_debug_output,
                         "%s setting up session dir with\n\ttmpdir: %s\n\thost %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         (NULL == prte_process_info.tmpdir_base) ? "UNDEF" : prte_process_info.tmpdir_base,
                         prte_process_info.nodename));
    /* create the directory tree */
    if (PRTE_SUCCESS != (ret = prte_session_dir(PRTE_PROC_MY_NAME))) {
        error = "prte_session_dir";
        goto error;
    }

    /* setup the PMIx server - we need this here in case the
     * communications infrastructure wants to register
     * information */
    if (PRTE_SUCCESS != (ret = pmix_server_init())) {
        /* the server code already barked, so let's be quiet */
        ret = PRTE_ERR_SILENT;
        error = "pmix_server_init";
        goto error;
    }
    /* Setup the communication infrastructure */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_prtereachable_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        PRTE_ERROR_LOG(ret);
        error = "prte_prtereachable_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_reachable_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_prtereachable_base_select";
        goto error;
    }
    /*
     * OOB Layer
     */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_oob_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        error = "prte_oob_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_oob_base_select())) {
        error = "prte_oob_base_select";
        goto error;
    }

    // set our RML address
    prte_oob_base_get_addr(&proc->rml_uri);
    prte_process_info.my_hnp_uri = strdup(proc->rml_uri);
    /* store it in the local PMIx repo for later retrieval */
    PMIX_VALUE_LOAD(&pval, proc->rml_uri, PMIX_STRING);
    if (PMIX_SUCCESS != (pret = PMIx_Store_internal(PRTE_PROC_MY_NAME, PMIX_PROC_URI, &pval))) {
        PMIX_ERROR_LOG(pret);
        ret = PRTE_ERROR;
        PMIX_VALUE_DESTRUCT(&pval);
        error = "store uri";
        goto error;
    }
    PMIX_VALUE_DESTRUCT(&pval);

    /*
     * Runtime Messaging Layer
     */
    prte_rml_open();

    /* it is now safe to start the pmix server */
    pmix_server_start();

    /*
     * Group communications
     */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_grpcomm_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        PRTE_ERROR_LOG(ret);
        error = "prte_grpcomm_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_grpcomm_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_grpcomm_base_select";
        goto error;
    }

    /* setup the error manager */
    if (PRTE_SUCCESS != (ret = prte_errmgr_base_select())) {
        error = "prte_errmgr_base_select";
        goto error;
    }

    if (0 < pmix_output_get_verbosity(prte_ess_base_framework.framework_output)) {
        pmix_output(0, "ALIASES FOR %s", node->name);
        if (NULL != node->aliases) {
            for (idx=0; NULL != node->aliases[idx]; idx++) {
                pmix_output(0, "\tALIAS: %s", node->aliases[idx]);
            }
        }
    }

    /* Now provide a chance for the PLM
     * to perform any module-specific init functions. This
     * needs to occur AFTER the communications are setup
     * as it may involve starting a non-blocking recv
     */
    if (PRTE_SUCCESS != (ret = prte_plm.init())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_plm_init";
        goto error;
    }
    /*
     * Setup the remaining resource
     * management and errmgr frameworks - application procs
     * and daemons do not open these frameworks as they only use
     * the hnp proxy support in the PLM framework.
     */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_ras_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        PRTE_ERROR_LOG(ret);
        error = "prte_ras_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_ras_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_ras_base_find_available";
        goto error;
    }
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_rmaps_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        PRTE_ERROR_LOG(ret);
        error = "prte_rmaps_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_rmaps_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_rmaps_base_find_available";
        goto error;
    }

    /* add the topology to the array of known topologies */
    t = PMIX_NEW(prte_topology_t);
    t->topo = prte_hwloc_topology;
    /* generate the signature */
    prte_topo_signature = prte_hwloc_base_get_topo_signature(prte_hwloc_topology);
    t->sig = strdup(prte_topo_signature);
    t->index = pmix_pointer_array_add(prte_node_topologies, t);
    node->topology = t;
    node->available = prte_hwloc_base_filter_cpus(prte_hwloc_topology);
    if (15 < pmix_output_get_verbosity(prte_ess_base_framework.framework_output)) {
        char *output = NULL;
        pmix_output(0, "%s Topology Info:", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        prte_hwloc_print(&output, "\t", prte_hwloc_topology);
        pmix_output(0, "%s", output);
        free(output);
    }

    /* Open/select the odls */
    ret = pmix_mca_base_framework_open(&prte_odls_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != ret) {
        PRTE_ERROR_LOG(ret);
        error = "prte_odls_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_odls_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_odls_base_select";
        goto error;
    }
    /* Open/select the rtc */
    ret = pmix_mca_base_framework_open(&prte_rtc_base_framework,
                                       PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PRTE_SUCCESS != ret) {
        PRTE_ERROR_LOG(ret);
        error = "prte_rtc_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_rtc_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_rtc_base_select";
        goto error;
    }

    /* set the pmix_output hnp file location to be in the
     * proc-specific session directory. */
    pmix_asprintf(&tmp, "%s/%s", jdata->session_dir,
                              PMIX_RANK_PRINT(PRTE_PROC_MY_NAME->rank));
    pmix_output_set_output_file_info(tmp, "output-", NULL, NULL);
    free(tmp);

    /* setup I/O forwarding system - must come after we init routes */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_iof_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        PRTE_ERROR_LOG(ret);
        error = "prte_iof_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_iof_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_iof_base_select";
        goto error;
    }
    /* setup the FileM */
    if (PRTE_SUCCESS
        != (ret = pmix_mca_base_framework_open(&prte_filem_base_framework,
                                               PMIX_MCA_BASE_OPEN_DEFAULT))) {
        PRTE_ERROR_LOG(ret);
        error = "prte_filem_base_open";
        goto error;
    }
    if (PRTE_SUCCESS != (ret = prte_filem_base_select())) {
        PRTE_ERROR_LOG(ret);
        error = "prte_filem_base_select";
        goto error;
    }

    return PRTE_SUCCESS;

error:
    if (PRTE_ERR_SILENT != ret && !prte_report_silent_errors) {
        pmix_show_help("help-prte-runtime.txt", "prte_init:startup:internal-failure", true, error,
                       PRTE_ERROR_NAME(ret), ret);
    }
    if (NULL != jdata) {
        /* remove our session directory tree */
        PMIX_RELEASE(jdata);
    }
    return PRTE_ERR_SILENT;
}

static int rte_finalize(void)
{
    char *contact_path;
    prte_job_t *jdata;

    /* first stage shutdown of the errmgr, deregister the handler but keep
     * the required facilities until the rml and oob are offline */
    prte_errmgr.finalize();

    /* close frameworks */
    (void) pmix_mca_base_framework_close(&prte_filem_base_framework);
    (void) pmix_mca_base_framework_close(&prte_grpcomm_base_framework);
    (void) pmix_mca_base_framework_close(&prte_iof_base_framework);
    (void) pmix_mca_base_framework_close(&prte_plm_base_framework);
    if (!prte_abnormal_term_ordered) {
        /* make sure our local procs are dead */
        prte_odls.kill_local_procs(NULL);
    }
    (void) pmix_mca_base_framework_close(&prte_rtc_base_framework);
    (void) pmix_mca_base_framework_close(&prte_odls_base_framework);
    prte_rml_close();
    (void) pmix_mca_base_framework_close(&prte_oob_base_framework);
    (void) pmix_mca_base_framework_close(&prte_prtereachable_base_framework);
    (void) pmix_mca_base_framework_close(&prte_errmgr_base_framework);
    (void) pmix_mca_base_framework_close(&prte_state_base_framework);

    free(prte_topo_signature);

    /* shutdown the pmix server */
    pmix_server_finalize();
    /* output any lingering stdout/err data */
    fflush(stdout);
    fflush(stderr);

    return PRTE_SUCCESS;
}
