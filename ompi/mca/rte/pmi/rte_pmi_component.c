/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include "opal/runtime/opal_params.h"
#include "opal/mca/common/pmi/common_pmi.h"

#include "opal/mca/hwloc/base/base.h"
#include "opal/runtime/opal.h"
#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"
#include "ompi/debuggers/debuggers.h"

#include "rte_pmi.h"
#include "rte_pmi_internal.h"

/*
 * Public string showing the component version number
 */
const char *ompi_rte_pmi_component_version_string =
    "OMPI pmi rte MCA component version " OMPI_VERSION;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

const ompi_rte_component_t mca_rte_pmi_component = {
    {
        OMPI_RTE_BASE_VERSION_1_0_0,

        /* Component name and version */
        "pmi",
        OMPI_MAJOR_VERSION,
        OMPI_MINOR_VERSION,
        OMPI_RELEASE_VERSION,

        /* Component open and close functions */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


ompi_process_info_t ompi_process_info;
bool ompi_rte_proc_is_bound = false;


int
ompi_rte_init(int *argc, char ***argv)
{
    int tmp, i, rank, size, ret;
    int *node_ranks;
    char *node_info;
    hwloc_obj_t root;
    hwloc_cpuset_t boundset, rootset;
    char *tmp_str, *error;

    // Initialize PMI
    int rc = mca_common_pmi_init (opal_pmi_version);
    
    if ( OPAL_SUCCESS != rc ) {
        return rc;
    }

    /* be kind, set line buffering */
    setvbuf(stdout, NULL, _IONBF, 0);

    ret = opal_init(argc, argv);
    if (OMPI_SUCCESS != ret) {
        return ret;
    }

    // Setup job name
    tmp = mca_common_pmi_appnum();
    ompi_rte_my_process_name.jobid = tmp;
    ompi_process_info.app_num = ompi_rte_my_process_name.jobid;
    ompi_process_info.pid = getpid();

    // Setup rank information
    rank = mca_common_pmi_rank();
    ompi_rte_my_process_name.vpid = rank;

    // Setup process groups size
    size = mca_common_pmi_size();
    ompi_process_info.num_procs = size;


    rc = mca_common_pmi_local_info(rank, &node_ranks, &tmp, &error);
    if( OPAL_SUCCESS != rc ){
        // FIX ME: maybe we somehow should use error message to
        // help user understand the reason of failure?
        return rc;
    }
    ompi_process_info.num_local_peers = tmp;
    for (i = 0 ; i < ompi_process_info.num_local_peers ; ++i) {
        if (rank == node_ranks[i]) {
            ompi_process_info.my_local_rank = i;
            ompi_process_info.my_node_rank = i;
            break;
        }
    }
    ompi_process_info.my_hnp_uri = NULL;
    ompi_process_info.peer_modex = 0;
    ompi_process_info.peer_init_barrier = 0;
    ompi_process_info.peer_fini_barrier = 0;
    ompi_process_info.job_session_dir = NULL; /* BWB: FIX ME */
    ompi_process_info.proc_session_dir = NULL; /* BWB: FIX ME */
    gethostname(ompi_process_info.nodename, sizeof(ompi_process_info.nodename));
    ompi_process_info.cpuset = NULL;

    /* setup hwloc */
    if (NULL == opal_hwloc_topology) {
        if (OPAL_SUCCESS != (ret = opal_hwloc_base_get_topology())) {
            return ret;
        }
    }
    root = hwloc_get_root_obj(opal_hwloc_topology);

    /* get our bindings */
    rootset = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, root);
    boundset = hwloc_bitmap_alloc();
    if (hwloc_get_cpubind(opal_hwloc_topology, boundset, 
                          HWLOC_CPUBIND_PROCESS) >= 0) {
        /* we are bound if the two cpusets are not equal, or if there
           is only ONE PU available to us */
        if (0 != hwloc_bitmap_compare(boundset, rootset) ||
            opal_hwloc_base_single_cpu(rootset) ||
            opal_hwloc_base_single_cpu(boundset)) {
            hwloc_bitmap_list_asprintf(&ompi_process_info.cpuset, boundset);
            ompi_rte_proc_is_bound = true;
        }
    }
    hwloc_bitmap_free(boundset);

    ret = ompi_rte_pmi_name_init();
    if (OMPI_SUCCESS != ret) return ret;

    ret = ompi_rte_pmi_db_init();
    if (OMPI_SUCCESS != ret) return ret;

    /* Fill in things the attributes want to know... */
    tmp = mca_common_pmi_universe();
    asprintf(&tmp_str, "%d", tmp);
    setenv("OMPI_UNIVERSE_SIZE", tmp_str, 1);
    free(tmp_str);

    /* BWB: FIX ME: Why is info looking at this instead of ompi_process_info.num_procs? */
    asprintf(&tmp_str, "%d", ompi_process_info.num_procs);
    setenv("OMPI_MCA_orte_ess_num_procs", tmp_str, 1);
    free(tmp_str);

    if (NULL != (tmp_str = (char*)hwloc_obj_get_info_by_name(root, "CPUType"))) {
        setenv("OMPI_MCA_orte_cpu_type", tmp_str, 1);
    }

    asprintf(&node_info, "%s,%d", 
             ompi_process_info.nodename,
             ompi_process_info.my_local_rank);
    ret = ompi_rte_db_store(OMPI_PROC_MY_NAME, OMPI_DB_RTE_INFO, node_info, OPAL_STRING);
    if (OMPI_SUCCESS != ret) return ret;
    free(node_info);

    return OMPI_SUCCESS;
}


int
ompi_rte_finalize(void)
{
    ompi_rte_pmi_db_fini();
    ompi_rte_pmi_name_fini();
    mca_common_pmi_finalize();
    opal_finalize();
    return OMPI_SUCCESS;
}


void
ompi_rte_wait_for_debugger(void)
{
    if (1 != MPIR_being_debugged) {
        return;
    }

    /* if we are being debugged, then we need to find
     * the correct plug-ins
     */
    ompi_debugger_setup_dlls();

    /* spin until debugger attaches and releases us */
    while (MPIR_debug_gate == 0) {
#if defined(HAVE_USLEEP)
        usleep(100000); /* microseconds */
#else
        sleep(1);       /* seconds */
#endif
    }
}
