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
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_alias.h"
#include "src/mca/mca.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/state/state.h"

#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"
#include "src/mca/plm/plm.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public pmix_mca_base_module_t struct.
 */

#include "src/mca/plm/base/static-components.h"

/*
 * Global variables for use within PLM frameworks
 */
prte_plm_globals_t prte_plm_globals = {
    .base_nspace = NULL,
    .next_jobid = 0,
    .daemonlaunchstart = {0, 0},
    .tree_spawn_cmd = PMIX_DATA_BUFFER_STATIC_INIT,
    .daemon_nodes_assigned_at_launch = true,
    .node_regex_threshold = 0,
    .daemon1_has_reported = false,
    .cache = NULL
};

/*
 * The default module
 */
static int local_init(void);
static int local_spawn(prte_job_t *jdata);
static int term_orteds(void);

prte_plm_base_module_t prte_plm = {
    .init = local_init,
    .set_hnp_name = prte_plm_base_set_hnp_name,
    .spawn = local_spawn,
    .remote_spawn = NULL,
    .terminate_job = prte_plm_base_prted_terminate_job,
    .terminate_orteds = term_orteds,
    .terminate_procs = prte_plm_base_prted_kill_local_procs,
    .signal_job = prte_plm_base_prted_signal_local_procs,
    .finalize = NULL
};

static int mca_plm_base_register(pmix_mca_base_register_flag_t flags)
{
    PRTE_HIDE_UNUSED_PARAMS(flags);

    prte_plm_globals.node_regex_threshold = 1024;
    (void) pmix_mca_base_framework_var_register(&prte_plm_base_framework,
                                                "node_regex_threshold",
                                                "Only pass the node regex on the orted command line if smaller than this threshold",
                                                PMIX_MCA_BASE_VAR_TYPE_SIZE_T,
                                                &prte_plm_globals.node_regex_threshold);

    /* Note that we break abstraction rules here by listing a
     specific PLM here in the base.  This is necessary, however,
     due to extraordinary circumstances:

     1. In PRRTE v2.0, we want to rename the "rsh" PLM to be
        "ssh" to more closely represent its usage.

     2. The MCA aliasing mechanism was therefore ported from
        OMPI for this purpose. Both the component itself and
        all of its MCA vars are aliased.

     3. However -- at least as currently implemented -- by the time
     individual components are registered, it's too late to make
     aliases.  Hence, if we want to preserve the name "rsh" for
     some sembalance of backwards compatibility (and we do!), we
     have to register "rsh" as an "alias for ssh" up here in
     the PLM base, before any PLM components are registered.

     This is why we tolerate this abstraction break up here in the
     PLM component base. */
    (void) pmix_mca_base_alias_register("prte", "plm", "ssh", "rsh", PMIX_MCA_BASE_ALIAS_FLAG_NONE);
    return PRTE_SUCCESS;
}

static int prte_plm_base_close(void)
{
    int rc;

    /* Close the selected component */
    if (NULL != prte_plm.finalize) {
        prte_plm.finalize();
    }

    /* if we are the HNP, then stop our receive */
    if (PRTE_PROC_IS_MASTER) {
        if (PRTE_SUCCESS != (rc = prte_plm_base_comm_stop())) {
            PRTE_ERROR_LOG(rc);
            return rc;
        }
    }

    if (NULL != prte_plm_globals.base_nspace) {
        free(prte_plm_globals.base_nspace);
    }
    while (NULL != pmix_list_remove_first(&prte_plm_globals.daemon_cache)); // do not release list items!
    PMIX_DESTRUCT(&prte_plm_globals.daemon_cache);

    return pmix_mca_base_framework_components_close(&prte_plm_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int prte_plm_base_open(pmix_mca_base_open_flag_t flags)
{
    /* init the next jobid */
    prte_plm_globals.next_jobid = 1;

    /* default to assigning daemons to nodes at launch */
    prte_plm_globals.daemon_nodes_assigned_at_launch = true;

    PMIX_CONSTRUCT(&prte_plm_globals.daemon_cache, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_plm_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, plm, NULL, mca_plm_base_register, prte_plm_base_open,
                                prte_plm_base_close, prte_plm_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);


// the LOCAL ONLY plm module
static void launch_daemons(int fd, short args, void *cbdata);

static int local_init(void)
{
    int rc;

    rc = prte_state.add_job_state(PRTE_JOB_STATE_LAUNCH_DAEMONS, launch_daemons);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }

    /* start the recvs */
    if (PRTE_SUCCESS != (rc = prte_plm_base_comm_start())) {
        PRTE_ERROR_LOG(rc);
    }

    /* we assign daemon nodes at launch */
    prte_plm_globals.daemon_nodes_assigned_at_launch = true;

    return rc;
}

static int local_spawn(prte_job_t *jdata)
{
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
        /* this is a restart situation - skip to the mapping stage */
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_MAP);
    } else {
        /* new job - set it up */
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_INIT);
    }
    return PRTE_SUCCESS;
}

static int term_orteds(void)
{
    int rc;

    if (PRTE_SUCCESS != (rc = prte_plm_base_prted_exit(PRTE_DAEMON_EXIT_CMD))) {
        PRTE_ERROR_LOG(rc);
    }

    return rc;
}

static void launch_daemons(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *state = (prte_state_caddy_t *) cbdata;
    prte_job_t *daemons;
    int rc;
    prte_job_map_t *map = NULL;
    prte_node_t *node;

    /* setup the virtual machine */
    daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    if (PRTE_SUCCESS != (rc = prte_plm_base_setup_virtual_machine(state->jdata))) {
        PRTE_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_FAILED_TO_START);
        PMIX_RELEASE(state);
        return;
    }

    /* if we don't want to launch, then don't attempt to
     * launch the daemons - the user really wants to just
     * look at the proposed process map
     */
    if (prte_get_attribute(&daemons->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        /* set the state to indicate the daemons reported - this
         * will trigger the daemons_reported event and cause the
         * job to move to the following step
         */
        state->jdata->state = PRTE_JOB_STATE_DAEMONS_LAUNCHED;
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
        PMIX_RELEASE(state);
        return;
    }

    /* Get the map for this job */
    if (NULL == (map = daemons->map)) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_FAILED_TO_START);
        PMIX_RELEASE(state);
        return;
    }

    if (0 == map->num_new_daemons) {
        if (!prte_managed_allocation || prte_set_slots_override) {
            // set the number of slots on our node
            node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
            prte_plm_base_set_slots(node);
            state->jdata->total_slots_alloc = node->slots;
        } else {
            /* for managed allocations, the total slots allocated is fixed at time of allocation */
            state->jdata->total_slots_alloc = prte_ras_base.total_slots_alloc;
        }

        // check for topology limitations
        prte_rmaps_base.require_hwtcpus = !prte_hwloc_base_core_cpus(node->topology->topo);

        // display the allocation, if requested
        if (prte_get_attribute(&state->jdata->attributes, PRTE_JOB_DISPLAY_ALLOC, NULL, PMIX_BOOL)) {
            prte_ras_base_display_alloc(state->jdata);
        }

        /* jump to mapping */
        state->jdata->state = PRTE_JOB_STATE_VM_READY;
        PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
        PMIX_RELEASE(state);
        return;
    }

    // otherwise, this is an error
    pmix_show_help("help-plm-base.txt", "no-available-pls", true);
    PRTE_ACTIVATE_JOB_STATE(state->jdata, PRTE_JOB_STATE_FAILED_TO_START);
    PMIX_RELEASE(state);
}
