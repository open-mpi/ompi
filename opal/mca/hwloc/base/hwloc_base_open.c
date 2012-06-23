/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"

#include "opal/constants.h"
#include "opal/dss/dss.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/mca/hwloc/hwloc.h"
#include "opal/mca/hwloc/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */
#include "opal/mca/hwloc/base/static-components.h"


/*
 * Globals
 */
int opal_hwloc_base_output = -1;
opal_list_t opal_hwloc_base_components;
bool opal_hwloc_base_inited = false;
#if OPAL_HAVE_HWLOC
hwloc_topology_t opal_hwloc_topology=NULL;
hwloc_cpuset_t opal_hwloc_my_cpuset=NULL;
hwloc_cpuset_t opal_hwloc_base_given_cpus=NULL;
opal_hwloc_base_map_t opal_hwloc_base_map = OPAL_HWLOC_BASE_MAP_NONE;
opal_hwloc_base_mbfa_t opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_WARN;
opal_binding_policy_t opal_hwloc_binding_policy=0;
char *opal_hwloc_base_slot_list=NULL;
char *opal_hwloc_base_cpu_set=NULL;
bool opal_hwloc_report_bindings=false;
hwloc_obj_type_t opal_hwloc_levels[] = {
    HWLOC_OBJ_MACHINE,
    HWLOC_OBJ_NODE,
    HWLOC_OBJ_SOCKET,
    HWLOC_OBJ_CACHE,
    HWLOC_OBJ_CACHE,
    HWLOC_OBJ_CACHE,
    HWLOC_OBJ_CORE,
    HWLOC_OBJ_PU
};
bool opal_hwloc_use_hwthreads_as_cpus = false;
#endif


int opal_hwloc_base_open(void)
{
    if (opal_hwloc_base_inited) {
        return OPAL_SUCCESS;
    }
    opal_hwloc_base_inited = true;

#if OPAL_HAVE_HWLOC
    {
        int value, i;
        opal_data_type_t tmp;
        char *str_value;
        char **tmpvals, **quals;

        /* Debugging / verbose output */
        mca_base_param_reg_int_name("hwloc", "base_verbose", 
                                    "Verbosity level of the hwloc framework",
                                    false, false,
                                    0, &value);
        if (0 != value) {
            opal_hwloc_base_output = opal_output_open(NULL);
        } else {
            opal_hwloc_base_output = -1;
        }

        /* hwloc_base_mbind_policy */
        switch (opal_hwloc_base_map) {
        case OPAL_HWLOC_BASE_MAP_NONE:
            str_value = "none";
            break;
        case OPAL_HWLOC_BASE_MAP_LOCAL_ONLY:
            str_value = "local_only";
            break;
        }
        mca_base_param_reg_string_name("hwloc", "base_mem_alloc_policy",
                                       "Policy to determine where general memory allocations are placed after MPI_INIT (this is not memory binding). A value of \"none\" means that no memory policy is applied. A value of \"local_only\" means that all memory allocations will be restricted to the local NUMA node where each process is placed. Note that operating system paging policies are unaffected by this setting. For example, if \"local_only\" is used and local NUMA node memory is exhausted, a new memory allocation may cause paging.",
                                       false, false, str_value, &str_value);
        if (strcasecmp(str_value, "none") == 0) {
            opal_hwloc_base_map = OPAL_HWLOC_BASE_MAP_NONE;
        } else if (strcasecmp(str_value, "local_only") == 0 ||
                   strcasecmp(str_value, "local-only") == 0) {
            opal_hwloc_base_map = OPAL_HWLOC_BASE_MAP_LOCAL_ONLY;
        } else {
            char hostname[32];
            gethostname(hostname, sizeof(hostname));
            opal_show_help("help-opal-hwloc-base.txt", "invalid policy",
                           true, hostname, getpid(), str_value);
            free(str_value);
            return OPAL_ERR_BAD_PARAM;
        }
        free(str_value);
        
        /* hwloc_base_bind_failure_action */
        switch (opal_hwloc_base_mbfa) {
        case OPAL_HWLOC_BASE_MBFA_SILENT:
            str_value = "silent";
            break;
        case OPAL_HWLOC_BASE_MBFA_WARN:
            str_value = "warn";
            break;
        case OPAL_HWLOC_BASE_MBFA_ERROR:
            str_value = "error";
            break;
        }
        mca_base_param_reg_string_name("hwloc", "base_mem_bind_failure_action",
                                       "What Open MPI will do if it explicitly tries to bind memory to a specific NUMA location, and fails.  Note that this is a different case than the general allocation policy described by hwloc_base_alloc_policy.  A value of \"silent\" means that Open MPI will proceed without comment. A value of \"warn\" means that Open MPI will warn the first time this happens, but allow the job to continue (possibly with degraded performance).  A value of \"error\" means that Open MPI will abort the job if this happens.",
                                       false, false, str_value, &str_value);
        if (strcasecmp(str_value, "silent") == 0) {
            opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_SILENT;
        } else if (strcasecmp(str_value, "warn") == 0) {
            opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_WARN;
        } else if (strcasecmp(str_value, "error") == 0) {
            opal_hwloc_base_mbfa = OPAL_HWLOC_BASE_MBFA_ERROR;
        } else {
            char hostname[32];
            gethostname(hostname, sizeof(hostname));
            opal_show_help("help-opal-hwloc-base.txt", "invalid error action",
                           true, hostname, getpid(), str_value);
            free(str_value);
            return OPAL_ERR_BAD_PARAM;
        }
        free(str_value);
        
        /* binding specification */
        mca_base_param_reg_string_name("hwloc", "base_binding_policy",
                                       "Policy for binding processes [none (default) | hwthread | core | l1cache | l2cache | l3cache | socket | numa | board] (supported qualifiers: overload-allowed,if-supported)",
                                       false, false, NULL, &str_value);
        if (NULL == str_value) {
            opal_hwloc_binding_policy = OPAL_BIND_TO_NONE;
            /* mark that no binding policy was specified */
            opal_hwloc_binding_policy &= ~OPAL_BIND_GIVEN;
        } else if (0 == strncasecmp(str_value, "none", strlen("none"))) {
            opal_hwloc_binding_policy = OPAL_BIND_TO_NONE;
            opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
        } else {
            opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
            tmpvals = opal_argv_split(str_value, ':');
            if (1 < opal_argv_count(tmpvals)) {
                quals = opal_argv_split(tmpvals[1], ',');
                for (i=0; NULL != quals[i]; i++) {
                    if (0 == strcasecmp(quals[i], "if-supported")) {
                        opal_hwloc_binding_policy |= OPAL_BIND_IF_SUPPORTED;
                    } else if (0 == strcasecmp(quals[i], "overload-allowed")) {
                        opal_hwloc_binding_policy |= OPAL_BIND_ALLOW_OVERLOAD;
                    } else {
                        /* unknown option */
                        opal_output(0, "Unknown qualifier to orte_process_binding: %s", str_value);
                        return OPAL_ERR_BAD_PARAM;
                    }
                }
                opal_argv_free(quals);
            }
            if (0 == strcasecmp(tmpvals[0], "hwthread")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_HWTHREAD);
            } else if (0 == strcasecmp(tmpvals[0], "core")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_CORE);
            } else if (0 == strcasecmp(tmpvals[0], "l1cache")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_L1CACHE);
            } else if (0 == strcasecmp(tmpvals[0], "l2cache")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_L2CACHE);
            } else if (0 == strcasecmp(tmpvals[0], "l3cache")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_L3CACHE);
            } else if (0 == strcasecmp(tmpvals[0], "socket")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_SOCKET);
            } else if (0 == strcasecmp(tmpvals[0], "numa")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_NUMA);
            } else if (0 == strcasecmp(tmpvals[0], "board")) {
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_BOARD);
            } else {
                opal_show_help("help-opal-hwloc-base.txt", "unrecognized-policy", true, "binding", str_value);
                opal_argv_free(tmpvals);
                free(str_value);
                return OPAL_ERR_BAD_PARAM;
            }
            opal_argv_free(tmpvals);
        }
        free(str_value);

        /* backward compatibility */
        mca_base_param_reg_int_name("hwloc", "base_bind_to_core",
                                    "Bind processes to cores",
                                    false, false, (int)false, &value);
        if (value) {
            /* set binding policy to core - error if something else already set */
            if (OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy) &&
                OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy) != OPAL_BIND_TO_CORE) {
                /* error - cannot redefine the default ranking policy */
                opal_show_help("help-opal-hwloc-base.txt", "redefining-policy", true,
                               "core", opal_hwloc_base_print_binding(opal_hwloc_binding_policy));
                return OPAL_ERR_SILENT;
            }
            OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_CORE);
            opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
        }

        mca_base_param_reg_int_name("hwloc", "base_bind_to_socket",
                                    "Bind processes to sockets",
                                    false, false, (int)false, &value);
        if (value) {
            /* set binding policy to socket - error if something else already set */
            if (OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy) &&
                OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy) != OPAL_BIND_TO_SOCKET) {
                /* error - cannot redefine the default ranking policy */
                opal_show_help("help-opal-hwloc-base.txt", "redefining-policy", true,
                               "socket", opal_hwloc_base_print_binding(opal_hwloc_binding_policy));
                return OPAL_ERR_SILENT;
            }
            OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_SOCKET);
            opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
        }

        mca_base_param_reg_int_name("hwloc", "base_report_bindings",
                                    "Report bindings to stderr",
                                    false, false, (int)false, &value);
        opal_hwloc_report_bindings = OPAL_INT_TO_BOOL(value);

        /* did the user provide a slot list? */
        tmp = mca_base_param_reg_string_name("hwloc", "base_slot_list",
                                             "List of processor IDs to bind processes to [default=NULL]",
                                             false, false, NULL, &opal_hwloc_base_slot_list);
        if (NULL != opal_hwloc_base_slot_list) {
            /* if we already were given a policy, then this is an error */
            if (OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy)) {
                opal_show_help("help-opal-hwloc-base.txt", "redefining-policy", true,
                               "socket", opal_hwloc_base_print_binding(opal_hwloc_binding_policy));
                return OPAL_ERR_SILENT;
            }
            OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_CPUSET);
            opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
        }

        /* cpu allocation specification */
        mca_base_param_reg_string_name("hwloc", "base_cpu_set",
                                       "Comma-separated list of ranges specifying logical cpus allocated to this job [default: none]",
                                       false, false, NULL, &opal_hwloc_base_cpu_set);
        if (NULL != opal_hwloc_base_cpu_set) {
            if (!OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy)) {
                /* it is okay if a binding policy was already given - just ensure that
                 * we do bind to the given cpus if provided, otherwise this would be
                 * ignored if someone didn't also specify a binding policy
                 */
                OPAL_SET_BINDING_POLICY(opal_hwloc_binding_policy, OPAL_BIND_TO_CPUSET);
                opal_hwloc_binding_policy |= OPAL_BIND_GIVEN;
            }
        }

        /* to support tools such as ompi_info, add the components
         * to a list
         */
        OBJ_CONSTRUCT(&opal_hwloc_base_components, opal_list_t);
        if (OPAL_SUCCESS !=
            mca_base_components_open("hwloc", opal_hwloc_base_output,
                                     mca_hwloc_base_static_components,
                                     &opal_hwloc_base_components, true)) {
            return OPAL_ERROR;
        }

        /* declare hwthreads as independent cpus */
        mca_base_param_reg_int_name("hwloc", "base_use_hwthreads_as_cpus",
                                    "Use hardware threads as independent cpus",
                                    false, false, (int)false, &value);
        opal_hwloc_use_hwthreads_as_cpus = OPAL_INT_TO_BOOL(value);

        /* declare the hwloc data types */
        tmp = OPAL_HWLOC_TOPO;
        if (OPAL_SUCCESS != (value = opal_dss.register_type(opal_hwloc_pack,
                                                            opal_hwloc_unpack,
                                                            (opal_dss_copy_fn_t)opal_hwloc_copy,
                                                            (opal_dss_compare_fn_t)opal_hwloc_compare,
                                                            (opal_dss_size_fn_t)opal_hwloc_size,
                                                            (opal_dss_print_fn_t)opal_hwloc_print,
                                                            OPAL_DSS_STRUCTURED,
                                                            "OPAL_HWLOC_TOPO", &tmp))) {
            return value;
        }
    }
#endif

    return OPAL_SUCCESS;
}

#if OPAL_HAVE_HWLOC
static void obj_data_const(opal_hwloc_obj_data_t *ptr)
{
    ptr->available = NULL;
    ptr->npus = 0;
    ptr->idx = UINT_MAX;
}
static void obj_data_dest(opal_hwloc_obj_data_t *ptr)
{
    if (NULL != ptr->available) {
        hwloc_bitmap_free(ptr->available);
    }
}
OBJ_CLASS_INSTANCE(opal_hwloc_obj_data_t,
                   opal_object_t,
                   obj_data_const, obj_data_dest);

static void sum_const(opal_hwloc_summary_t *ptr)
{
    ptr->num_objs = 0;
    ptr->rtype = 0;
}
OBJ_CLASS_INSTANCE(opal_hwloc_summary_t,
                   opal_list_item_t,
                   sum_const, NULL);
static void topo_data_const(opal_hwloc_topo_data_t *ptr)
{
    ptr->available = NULL;
    OBJ_CONSTRUCT(&ptr->summaries, opal_list_t);
}
static void topo_data_dest(opal_hwloc_topo_data_t *ptr)
{
    opal_list_item_t *item;

    if (NULL != ptr->available) {
        hwloc_bitmap_free(ptr->available);
    }
    while (NULL != (item = opal_list_remove_first(&ptr->summaries))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&ptr->summaries);
}
OBJ_CLASS_INSTANCE(opal_hwloc_topo_data_t,
                   opal_object_t,
                   topo_data_const,
                   topo_data_dest);
#endif
