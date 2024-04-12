/*
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/hwloc/hwloc-internal.h"
#include "src/include/constants.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/schizo/schizo.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_tsd.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_show_help.h"
#include "src/util/prte_cmd_line.h"

/*
 * Globals
 */
bool prte_hwloc_base_inited = false;
hwloc_topology_t prte_hwloc_topology = NULL;
hwloc_cpuset_t prte_hwloc_my_cpuset = NULL;
prte_hwloc_base_map_t prte_hwloc_base_map = PRTE_HWLOC_BASE_MAP_NONE;
prte_hwloc_base_mbfa_t prte_hwloc_base_mbfa = PRTE_HWLOC_BASE_MBFA_WARN;
prte_binding_policy_t prte_hwloc_default_binding_policy = 0;
char *prte_hwloc_default_cpu_list = NULL;
char *prte_hwloc_base_topo_file = NULL;
int prte_hwloc_base_output = -1;
bool prte_hwloc_default_use_hwthread_cpus = false;
bool prte_hwloc_synthetic_topo = false;

hwloc_obj_type_t prte_hwloc_levels[] = {
    HWLOC_OBJ_MACHINE,
    HWLOC_OBJ_NUMANODE,
    HWLOC_OBJ_PACKAGE,
    HWLOC_OBJ_L3CACHE,
    HWLOC_OBJ_L2CACHE,
    HWLOC_OBJ_L1CACHE,
    HWLOC_OBJ_CORE,
    HWLOC_OBJ_PU
};

static char *prte_hwloc_base_binding_policy = NULL;
static int verbosity = 0;
static char *default_cpu_list = NULL;
static bool bind_to_core = false;
static bool bind_to_socket = false;
static char *enum_values = NULL;

int prte_hwloc_base_register(void)
{
    int ret;
    char *ptr;

    /* debug output */
    ret = pmix_mca_base_var_register("prte", "hwloc", "base", "verbose", "Debug verbosity",
                                     PMIX_MCA_BASE_VAR_TYPE_INT,
                                     &verbosity);
    pmix_mca_base_var_register_synonym(ret, "opal", "hwloc", "base", "verbose",
                                       PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    if (0 < verbosity) {
        prte_hwloc_base_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(prte_hwloc_base_output, verbosity);
    }

    /* handle some deprecated options */
    prte_hwloc_default_use_hwthread_cpus = false;
    (void) pmix_mca_base_var_register("prte", "hwloc", "base", "use_hwthreads_as_cpus",
                                      "Use hardware threads as independent cpus",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &prte_hwloc_default_use_hwthread_cpus);

    (void) pmix_mca_base_var_register("prte", "hwloc", "base", "bind_to_core",
                                      "Bind processes to cores",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &bind_to_core);

    (void) pmix_mca_base_var_register("prte", "hwloc", "base", "bind_to_socket",
                                      "Bind processes to sockets",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &bind_to_socket);

    /* hwloc_base_mbind_policy */

    prte_hwloc_base_map = PRTE_HWLOC_BASE_MAP_NONE;
    ret = pmix_mca_base_var_register("prte", "hwloc", "default", "mem_alloc_policy",
                                     "Default general memory allocations placement policy (this is not memory binding). "
                                     "\"none\" means that no memory policy is applied. \"local_only\" means that a process' "
                                     "memory allocations will be restricted to its local NUMA domain. "
                                     "If using direct launch, this policy will not be in effect until after PMIx_Init. "
                                     "Note that operating system paging policies are unaffected by this setting. For "
                                     "example, if \"local_only\" is used and local NUMA domain memory is exhausted, a new "
                                     "memory allocation may cause paging.",
                                     PMIX_MCA_BASE_VAR_TYPE_STRING,
                                     &enum_values);
    if (0 > ret) {
        return ret;
    }
    if (NULL != enum_values) {
        if (0 == strncasecmp(enum_values, "none", strlen("none"))) {
            prte_hwloc_base_map = PRTE_HWLOC_BASE_MAP_NONE;
        } else if (0 == strncasecmp(enum_values, "local_only", strlen("local_only"))) {
            prte_hwloc_base_map = PRTE_HWLOC_BASE_MAP_LOCAL_ONLY;
        } else {
            pmix_show_help("help-prte-hwloc-base.txt", "invalid binding_policy", true,
                           enum_values);
            return PRTE_ERR_SILENT;
        }
    }

    /* hwloc_base_bind_failure_action */
    enum_values = NULL;
    prte_hwloc_base_mbfa = PRTE_HWLOC_BASE_MBFA_WARN;
    ret = pmix_mca_base_var_register("prte", "hwloc", "default", "mem_bind_failure_action",
                                     "What PRTE will do if it explicitly tries to bind memory to a specific NUMA "
                                     "location, and fails.  Note that this is a different case than the general "
                                     "allocation policy described by mem_alloc_policy.  A value of \"silent\" "
                                     "means that PRTE will proceed without comment. A value of \"warn\" means that "
                                     "PRTE will warn the first time this happens, but allow the job to continue "
                                     "(possibly with degraded performance).  A value of \"error\" means that PRTE "
                                     "will abort the job if this happens.",
                                     PMIX_MCA_BASE_VAR_TYPE_STRING,
                                     &enum_values);
    if (0 > ret) {
        return ret;
    }
    if (NULL != enum_values) {
        if (0 == strncasecmp(enum_values, "silent", strlen("silent"))) {
            prte_hwloc_base_mbfa = PRTE_HWLOC_BASE_MBFA_SILENT;
        } else if (0 == strncasecmp(enum_values, "warn", strlen("warn"))) {
            prte_hwloc_base_mbfa = PRTE_HWLOC_BASE_MBFA_WARN;
        } else if (0 == strncasecmp(enum_values, "error", strlen("error"))) {
            prte_hwloc_base_mbfa = PRTE_HWLOC_BASE_MBFA_ERROR;
        } else {
            pmix_show_help("help-prte-hwloc-base.txt", "invalid binding_policy", true,
                           enum_values);
            return PRTE_ERR_SILENT;
        }
    }

    /* NOTE: for future developers and readers of this code, the binding policies are strictly
     * limited to none, hwthread, core, l1cache, l2cache, l3cache, package, and numa
     *
     * The default binding policy can be modified by any combination of the following:
     *    * overload-allowed - multiple processes can be bound to the same PU (core or HWT)
     *    * if-supported - perform the binding if it is supported by the OS, but do not
     *                     generate an error if it cannot be done
     */
    prte_hwloc_base_binding_policy = NULL;
    ret = pmix_mca_base_var_register("prte", "hwloc", "default", "binding_policy",
                                     "Default policy for binding processes. Allowed values: none, hwthread, core, l1cache, "
                                     "l2cache, "
                                     "l3cache, numa, package, (\"none\" is the default when oversubscribed, \"core\" is "
                                     "the default otherwise). Allowed "
                                     "colon-delimited qualifiers: "
                                     "overload-allowed, if-supported",
                                     PMIX_MCA_BASE_VAR_TYPE_STRING,
                                     &prte_hwloc_base_binding_policy);
    if (NULL == prte_hwloc_base_binding_policy) {
        if (bind_to_core) {
            prte_hwloc_base_binding_policy = "core";
        } else if (bind_to_socket) {
            prte_hwloc_base_binding_policy = "package";
        }
    }

    /* Allow specification of a default CPU list - a comma-delimited list of cpu ranges that
     * are the default PUs for this DVM. CPUs are to be specified as LOGICAL indices. If a
     * cpuset is provided, then all process placements and bindings will be constrained to the
     * identified CPUs. IN ESSENCE, THIS IS A USER-DEFINED "SOFT" CGROUP.
     *
     * Example: if the default binding policy is "core", then each process will be bound to the
     * first unused core underneath the topological object upon which it has been mapped. In other
     * words, if two processes are mapped to a given package, then the first process will be bound
     * to core0 of that package, and the second process will be bound to core1.
     *
     * If the cpuset specified that only cores 10, 12, and 14 were to be used, then the first
     * process would be bound to core10 and the second process would be bound to core12.
     *
     * If the default binding policy had been set to "package", and if cores 10, 12, and 14 are all
     * on the same package, then both processes would be bound to cores 10, 12, and 14. Note that
     * they would have been bound to all PUs on the package if the cpuset had not been given.
     *
     * If cores 10 and 12 are on package0, and core14 is on package1, then if the first process is
     * mapped to package0 and we are using a binding policy of "package", the first process would be
     * bound to core10 and core12. If the second process were mapped to package1, then it would be
     * bound only to core14 as that is the only PU in the cpuset that lies in package1.
     */
    default_cpu_list = NULL;
    ret = pmix_mca_base_var_register("prte", "hwloc", "default", "cpu_list",
                                     "Comma-separated list of ranges specifying logical cpus to be used by the DVM. "
                                     "Supported modifier:HWTCPUS (ranges specified in hwthreads) or CORECPUS "
                                     "(default: ranges specified in cores)",
                                     PMIX_MCA_BASE_VAR_TYPE_STRING,
                                     &default_cpu_list);


    if (NULL != default_cpu_list) {
        if (NULL != (ptr = strrchr(default_cpu_list, ':'))) {
            *ptr = '\0';
            prte_hwloc_default_cpu_list = strdup(default_cpu_list);
            ++ptr;
            if (0 == strcasecmp(ptr, "HWTCPUS")) {
                prte_hwloc_default_use_hwthread_cpus = true;
            } else if (0 == strcasecmp(ptr, "CORECPUS")) {
                prte_hwloc_default_use_hwthread_cpus = false;
            } else {
                pmix_show_help("help-prte-hwloc-base.txt", "bad-processor-type", true,
                               default_cpu_list, ptr);
                return PRTE_ERR_BAD_PARAM;
            }
        } else {
            prte_hwloc_default_cpu_list = strdup(default_cpu_list);
        }
    }

    prte_hwloc_base_topo_file = NULL;
    ret = pmix_mca_base_var_register("prte", "hwloc", "use", "topo_file",
                                     "Read local topology from file instead of directly sensing it",
                                     PMIX_MCA_BASE_VAR_TYPE_STRING,
                                     &prte_hwloc_base_topo_file);
    (void) pmix_mca_base_var_register_synonym(ret, "prte", "ras", "simulator", "topo_files",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);
    (void) pmix_mca_base_var_register_synonym(ret, "prte", "hwloc", "base", "use_topo_file",
                                              PMIX_MCA_BASE_VAR_SYN_FLAG_DEPRECATED);

    /* register parameters */
    return PRTE_SUCCESS;
}

int prte_hwloc_base_open(void)
{
    int rc;

    if (prte_hwloc_base_inited) {
        return PRTE_SUCCESS;
    }
    prte_hwloc_base_inited = true;

    /* check the provided default binding policy for correctness - specifically want to ensure
     * there are no disallowed qualifiers and setup the global param */
    if (PRTE_SUCCESS
        != (rc = prte_hwloc_base_set_binding_policy(NULL, prte_hwloc_base_binding_policy))) {
        return rc;
    }

    return PRTE_SUCCESS;
}

void prte_hwloc_base_close(void)
{
    if (!prte_hwloc_base_inited) {
        return;
    }

    if (NULL != prte_hwloc_default_cpu_list) {
        free(prte_hwloc_default_cpu_list);
    }

    /* destroy the topology */
    if (NULL != prte_hwloc_topology) {
        hwloc_topology_destroy(prte_hwloc_topology);
        prte_hwloc_topology = NULL;
    }

    /* All done */
    prte_hwloc_base_inited = false;
}

int prte_hwloc_base_set_default_binding(void *jd, void *opt)
{
    prte_job_t *jdata = (prte_job_t*)jd;
    prte_rmaps_options_t *options = (prte_rmaps_options_t*)opt;
    prte_mapping_policy_t mpol;

    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_PES_PER_PROC, NULL, PMIX_UINT16)) {
        /* bind to cpus */
        if (options->use_hwthreads || prte_rmaps_base.require_hwtcpus) {
            /* if we are using hwthread cpus, then bind to those */
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using byhwthread",
                                __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_HWTHREAD);
        } else {
            /* bind to core */
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using bycore", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_CORE);
        }
    } else if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_TOOL)) {
        /* tools are never bound */
        PRTE_SET_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_NONE);
    } else {
        /* if we are mapping by some object, then we default
         * to binding to that object */
        mpol = PRTE_GET_MAPPING_POLICY(jdata->map->mapping);
        if (PRTE_MAPPING_BYHWTHREAD == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using byhwthread", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_HWTHREAD);
        } else if (PRTE_MAPPING_BYCORE == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using bycore", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_CORE);
        } else if (PRTE_MAPPING_BYL1CACHE == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using byL1", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L1CACHE);
        } else if (PRTE_MAPPING_BYL2CACHE == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using byL2", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L2CACHE);
        } else if (PRTE_MAPPING_BYL3CACHE == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using byL3", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L3CACHE);
        } else if (PRTE_MAPPING_BYNUMA == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using bynuma",
                                __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_NUMA);
        } else if (PRTE_MAPPING_BYPACKAGE == mpol) {
            pmix_output_verbose(options->verbosity, options->stream,
                                "setdefaultbinding[%d] binding not given - using bypackage", __LINE__);
            PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_PACKAGE);
        } else if (PRTE_MAPPING_PELIST == mpol) {
            if (options->use_hwthreads) {
                /* if we are using hwthread cpus, then bind to those */
                pmix_output_verbose(options->verbosity, options->stream,
                                    "setdefaultbinding[%d] binding not given - using byhwthread for pe-list", __LINE__);
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding,
                                                PRTE_BIND_TO_HWTHREAD);
            } else {
                /* otherwise bind to core */
                pmix_output_verbose(options->verbosity, options->stream,
                                    "setdefaultbinding[%d] binding not given - using bycore for pe-list", __LINE__);
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding,
                                                PRTE_BIND_TO_CORE);
            }
        } else if (PRTE_MAPPING_PPR == mpol) {
            if (HWLOC_OBJ_MACHINE == options->maptype) {
                if (options->nprocs <= 2) {
                    /* we are mapping by node or some other non-object method */
                    if (options->use_hwthreads || prte_rmaps_base.require_hwtcpus) {
                        /* if we are using hwthread cpus, then bind to those */
                        pmix_output_verbose(options->verbosity, options->stream,
                                            "setdefaultbinding[%d] binding not given - using byhwthread", __LINE__);
                        PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding,
                                                        PRTE_BIND_TO_HWTHREAD);
                    } else {
                        /* otherwise bind to core */
                        pmix_output_verbose(options->verbosity, options->stream,
                                            "setdefaultbinding[%d] binding not given - using bycore", __LINE__);
                        PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding,
                                                        PRTE_BIND_TO_CORE);
                    }
                } else {
                    pmix_output_verbose(options->verbosity, options->stream,
                                        "setdefaultbinding[%d] binding not given - using bynuma",
                                        __LINE__);
                    PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_NUMA);
                }
            } else if (HWLOC_OBJ_PACKAGE == options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_PACKAGE);
            } else if (HWLOC_OBJ_NUMANODE== options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_NUMA);
#if HWLOC_API_VERSION < 0x20000
            } else if (HWLOC_OBJ_CACHE == options->maptype) {
                if (1 == options->cmaplvl) {
                    PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L1CACHE);
                } else if (2 == options->cmaplvl) {
                    PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L2CACHE);
                } else if (3 == options->cmaplvl) {
                    PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L3CACHE);
                }
#else
            } else if (HWLOC_OBJ_L1CACHE == options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L1CACHE);
            } else if (HWLOC_OBJ_L2CACHE == options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L2CACHE);
            } else if (HWLOC_OBJ_L3CACHE == options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_L3CACHE);
#endif
            } else if (HWLOC_OBJ_CORE == options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_CORE);
            } else if (HWLOC_OBJ_PU == options->maptype) {
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_HWTHREAD);
            }
        } else {
            if (options->nprocs <= 2) {
                /* we are mapping by node or some other non-object method */
                if (options->use_hwthreads || prte_rmaps_base.require_hwtcpus) {
                    /* if we are using hwthread cpus, then bind to those */
                    pmix_output_verbose(options->verbosity, options->stream,
                                        "setdefaultbinding[%d] binding not given - using byhwthread", __LINE__);
                    PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding,
                                                    PRTE_BIND_TO_HWTHREAD);
                } else {
                    /* otherwise bind to core */
                    pmix_output_verbose(options->verbosity, options->stream,
                                        "setdefaultbinding[%d] binding not given - using bycore", __LINE__);
                    PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding,
                                                    PRTE_BIND_TO_CORE);
                }
            } else {
                pmix_output_verbose(options->verbosity, options->stream,
                                    "setdefaultbinding[%d] binding not given - using bynuma",
                                    __LINE__);
                PRTE_SET_DEFAULT_BINDING_POLICY(jdata->map->binding, PRTE_BIND_TO_NUMA);
            }
        }
    }
    /* they might have set the overload-allowed flag while wanting PRRTE
     * to set the default binding - don't override it */
    if (!PRTE_BIND_OVERLOAD_SET(jdata->map->binding)) {
        if (PRTE_BIND_OVERLOAD_ALLOWED(prte_hwloc_default_binding_policy)) {
            jdata->map->binding |= PRTE_BIND_ALLOW_OVERLOAD;
        }
    }
    return PRTE_SUCCESS;
}

static bool fns_init = false;
static pmix_tsd_key_t print_tsd_key;
char *prte_hwloc_print_null = "NULL";

static void buffer_cleanup(void *value)
{
    int i;
    prte_hwloc_print_buffers_t *ptr;

    if (NULL != value) {
        ptr = (prte_hwloc_print_buffers_t *) value;
        for (i = 0; i < PRTE_HWLOC_PRINT_NUM_BUFS; i++) {
            free(ptr->buffers[i]);
        }
        free(ptr);
    }
}

prte_hwloc_print_buffers_t *prte_hwloc_get_print_buffer(void)
{
    prte_hwloc_print_buffers_t *ptr;
    int ret, i;

    if (!fns_init) {
        /* setup the print_args function */
        if (PRTE_SUCCESS != (ret = pmix_tsd_key_create(&print_tsd_key, buffer_cleanup))) {
            return NULL;
        }
        fns_init = true;
    }

    ret = pmix_tsd_getspecific(print_tsd_key, (void **) &ptr);
    if (PRTE_SUCCESS != ret)
        return NULL;

    if (NULL == ptr) {
        ptr = (prte_hwloc_print_buffers_t *) malloc(sizeof(prte_hwloc_print_buffers_t));
        for (i = 0; i < PRTE_HWLOC_PRINT_NUM_BUFS; i++) {
            ptr->buffers[i] = (char *) malloc((PRTE_HWLOC_PRINT_MAX_SIZE + 1) * sizeof(char));
        }
        ptr->cntr = 0;
        ret = pmix_tsd_setspecific(print_tsd_key, (void *) ptr);
    }

    return (prte_hwloc_print_buffers_t *) ptr;
}

char *prte_hwloc_base_print_locality(prte_hwloc_locality_t locality)
{
    prte_hwloc_print_buffers_t *ptr;
    int idx;

    ptr = prte_hwloc_get_print_buffer();
    if (NULL == ptr) {
        return prte_hwloc_print_null;
    }
    /* cycle around the ring */
    if (PRTE_HWLOC_PRINT_NUM_BUFS == ptr->cntr) {
        ptr->cntr = 0;
    }

    idx = 0;

    if (PRTE_PROC_ON_LOCAL_CLUSTER(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'C';
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_CU(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'C';
        ptr->buffers[ptr->cntr][idx++] = 'U';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_NODE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_PACKAGE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'S';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_NUMA(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = 'M';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_L3CACHE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = '3';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_L2CACHE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = '2';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_L1CACHE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'L';
        ptr->buffers[ptr->cntr][idx++] = '1';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_CORE(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'C';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (PRTE_PROC_ON_LOCAL_HWTHREAD(locality)) {
        ptr->buffers[ptr->cntr][idx++] = 'H';
        ptr->buffers[ptr->cntr][idx++] = 'w';
        ptr->buffers[ptr->cntr][idx++] = 't';
        ptr->buffers[ptr->cntr][idx++] = ':';
    }
    if (0 < idx) {
        ptr->buffers[ptr->cntr][idx - 1] = '\0';
    } else if (PRTE_PROC_NON_LOCAL & locality) {
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = 'O';
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = '\0';
    } else {
        /* must be an unknown locality */
        ptr->buffers[ptr->cntr][idx++] = 'U';
        ptr->buffers[ptr->cntr][idx++] = 'N';
        ptr->buffers[ptr->cntr][idx++] = 'K';
        ptr->buffers[ptr->cntr][idx++] = '\0';
    }

    return ptr->buffers[ptr->cntr];
}

int prte_hwloc_base_set_binding_policy(void *jdat, char *spec)
{
    int i;
    prte_binding_policy_t tmp;
    char **quals, *myspec, *ptr;
    prte_job_t *jdata = (prte_job_t *) jdat;

    /* set default */
    tmp = 0;

    /* binding specification */
    if (NULL == spec) {
        return PRTE_SUCCESS;
    }

    myspec = strdup(spec); // protect the input

    /* check for qualifiers */
    ptr = strchr(myspec, ':');
    if (NULL != ptr) {
        *ptr = '\0';
        ++ptr;
        quals = PMIX_ARGV_SPLIT_COMPAT(ptr, ':');
        for (i = 0; NULL != quals[i]; i++) {
            if (PMIX_CHECK_CLI_OPTION(quals[i], PRTE_CLI_IF_SUPP)) {
                tmp |= PRTE_BIND_IF_SUPPORTED;

            } else if (PMIX_CHECK_CLI_OPTION(quals[i], PRTE_CLI_OVERLOAD)) {
                tmp |= (PRTE_BIND_ALLOW_OVERLOAD | PRTE_BIND_OVERLOAD_GIVEN);

            } else if (PMIX_CHECK_CLI_OPTION(quals[i], PRTE_CLI_NOOVERLOAD)) {
                tmp = (tmp & ~PRTE_BIND_ALLOW_OVERLOAD);
                tmp |= PRTE_BIND_OVERLOAD_GIVEN;

            } else if (PMIX_CHECK_CLI_OPTION(quals[i], PRTE_CLI_REPORT)) {
                if (NULL == jdata) {
                    pmix_show_help("help-prte-rmaps-base.txt", "unsupported-default-modifier", true,
                                   "binding policy", quals[i]);
                    free(myspec);
                    return PRTE_ERR_SILENT;
                }
                prte_set_attribute(&jdata->attributes, PRTE_JOB_REPORT_BINDINGS, PRTE_ATTR_GLOBAL,
                                   NULL, PMIX_BOOL);

            } else {
                /* unknown option */
                pmix_show_help("help-prte-hwloc-base.txt", "unrecognized-modifier", true, spec);
                PMIX_ARGV_FREE_COMPAT(quals);
                free(myspec);
                return PRTE_ERR_BAD_PARAM;
            }
        }
        PMIX_ARGV_FREE_COMPAT(quals);
    }

    if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_NONE)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_NONE);

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_HWT)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_HWTHREAD);

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_CORE)) {
        if (prte_rmaps_base.require_hwtcpus) {
            PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_HWTHREAD);
        } else {
            PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_CORE);
        }

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_L1CACHE)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_L1CACHE);

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_L2CACHE)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_L2CACHE);

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_L3CACHE)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_L3CACHE);

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_NUMA)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_NUMA);

    } else if (PMIX_CHECK_CLI_OPTION(myspec, PRTE_CLI_PACKAGE)) {
        PRTE_SET_BINDING_POLICY(tmp, PRTE_BIND_TO_PACKAGE);

    } else {
        pmix_show_help("help-prte-hwloc-base.txt", "invalid binding_policy", true, "binding",
                       spec);
        free(myspec);
        return PRTE_ERR_BAD_PARAM;
    }
    free(myspec);

    if (NULL == jdata) {
        prte_hwloc_default_binding_policy = tmp;
    } else {
        if (NULL == jdata->map) {
            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
            return PRTE_ERR_BAD_PARAM;
        }
        jdata->map->binding = tmp;
    }
    return PRTE_SUCCESS;
}
