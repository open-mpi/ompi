/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 *
 * Copyright (c) 2012-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/mca/hwloc/hwloc.h"

#include "opal/util/argv.h"
#include "opal/class/opal_tree.h"

#include "orte/util/show_help.h"
#include "orte/util/error_strings.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

#include "orte/runtime/orte_globals.h"

#include "rmaps_lama.h"

#include MCA_timer_IMPLEMENTATION_HEADER


/*********************************
 * Module setup
 *********************************/
static int orte_rmaps_lama_map(orte_job_t *jdata);
orte_rmaps_base_module_t orte_rmaps_lama_module = {
    orte_rmaps_lama_map
};


/*********************************
 * Timer
 *********************************/
#define RMAPS_LAMA_TIMER_TOTAL           0
#define RMAPS_LAMA_TIMER_PARSE_PARAMS    1
#define RMAPS_LAMA_TIMER_BUILD_MAX_TREE  2
#define RMAPS_LAMA_TIMER_MAPPING         3
#define RMAPS_LAMA_TIMER_ORDERING        4
#define RMAPS_LAMA_TIMER_MAX             5

static double rmaps_lama_get_time(void);
static void   rmaps_lama_set_time(int idx, bool is_start);
static void   rmaps_lama_display_all_timers(void);
static void   rmaps_lama_clear_timers(void);
static void   rmaps_lama_display_indv_timer_core(double diff, char *str);

static double timer_start[RMAPS_LAMA_TIMER_MAX];
static double timer_end[RMAPS_LAMA_TIMER_MAX];
static double timer_accum[RMAPS_LAMA_TIMER_MAX];

#define RMAPS_LAMA_CLEAR_TIMERS()               \
    {                                           \
        if( rmaps_lama_timing_enabled ) {       \
            rmaps_lama_clear_timers();          \
        }                                       \
    }
#define RMAPS_LAMA_START_TIMER(idx)                  \
    {                                                \
        if( rmaps_lama_timing_enabled ) {            \
            rmaps_lama_set_time(idx, true);          \
        }                                            \
    }
#define RMAPS_LAMA_END_TIMER(idx)                     \
    {                                                 \
        if( rmaps_lama_timing_enabled ) {             \
            rmaps_lama_set_time(idx, false);          \
        }                                             \
    }
#define RMAPS_LAMA_DISPLAY_TIMERS()             \
    {                                           \
        if( rmaps_lama_timing_enabled ) {       \
            rmaps_lama_display_all_timers();    \
        }                                       \
    }


/*********************************
 * Structures & Defines
 *********************************/
static void rmaps_lama_hwloc_user_construct(rmaps_lama_hwloc_user_t *item);
static void rmaps_lama_hwloc_user_destruct(rmaps_lama_hwloc_user_t *item);

OBJ_CLASS_INSTANCE(rmaps_lama_hwloc_user_t,
                   opal_object_t,
                   rmaps_lama_hwloc_user_construct,
                   rmaps_lama_hwloc_user_destruct);                   


/*********************************
 * Globals
 *********************************/
/*
 * Mapping
 */
rmaps_lama_level_type_t *lama_mapping_layout = NULL;
static rmaps_lama_level_type_t *lama_mapping_layout_sort = NULL;
int lama_mapping_num_layouts = 0;

/*
 * Binding
 */
rmaps_lama_level_type_t  lama_binding_level  = LAMA_LEVEL_UNKNOWN;
static int lama_binding_num_levels = 0;

/*
 * MPPR
 */
rmaps_lama_level_info_t *lama_mppr_levels    = NULL;
int lama_mppr_num_levels = 0;

/*
 * Ordering
 */
static rmaps_lama_order_type_t lama_ordering = LAMA_ORDER_NATURAL;

/*
 * Homogeneous system optimization
 */
bool lama_mppr_max_tree_homogeneous_system = false;


/*********************************
 * Support Macros
 *********************************/


/*********************************
 * Support functions
 *********************************/
/*
 * Preprocess the command line arguments
 */
static int orte_rmaps_lama_process_params(orte_job_t *jdata);

/*
 * Mapping Support:
 * Core mapping function
 */
static int orte_rmaps_lama_map_core(orte_job_t *jdata);

/*
 * Mapping Support:
 * Recursive function for mapping process
 */
static int rmaps_lama_map_core_iter_level(orte_job_t *jdata,
                                          orte_app_context_t *cur_app_context,
                                          opal_list_t *node_list,
                                          orte_node_t **cur_mach_ptr,
                                          opal_tree_t *max_tree,
                                          int cur_level,
                                          int mach_level,
                                          int **pu_idx_ref,
                                          int **last_pu_idx_ref,
                                          int *num_mapped,
                                          int max_procs,
                                          int *iter_passes);

/*
 * Mapping Support:
 * Access the next machine in the node list
 */
static orte_node_t* get_next_machine(orte_job_t *jdata, opal_list_t *node_list,
                                     opal_list_item_t *cur_mach);

/*
 * Mapping Support:
 * Check the availability of the requested slot on the specified node
 */
static int check_node_availability(orte_node_t *cur_node,
                                   opal_tree_t *max_tree,
                                   int *pu_idx_ref,
                                   char **slot_list);

/*
 * Mapping Support:
 * Debugging PU display
 */
static void display_pu_ref(int *ref, int size, int rank, orte_proc_t *proc);
static char * pu_ref_to_str(int *ref, int size);

/*
 * Mapping Support:
 * Convert the process layout 'layer' to the sorted position for the PU
 */
static int convert_layer_to_sort_idx(rmaps_lama_level_type_t layer);

/*
 * MPPR Support:
 * Check to make sure a process can be placed on this resource given the
 * MPPR restrictions.
 */
static int rmaps_lama_check_mppr(orte_node_t *node,
                                 hwloc_obj_t *child_obj);
static int rmaps_lama_iter_mppr_parents(orte_node_t *node,
                                        hwloc_obj_t *child_obj,
                                        bool check_only);
static int rmaps_lama_iter_mppr_children(orte_node_t *node,
                                         hwloc_obj_t *child_obj,
                                         bool check_only);

/*
 * MPPR Support:
 * Increment parents of this child to account for a process being placed
 * on this resource.
 */
static int rmaps_lama_inc_mppr(orte_node_t *node,
                               hwloc_obj_t *child_obj);

/*
 * Mapping Support:
 * Return the native representation of the slot list
 */
static char * get_native_slot_list(orte_node_t *cur_node,
                                   hwloc_obj_t *pu_obj,
                                   int *put_idx_ref);

/*
 * Ordering Support:
 * Reorder sequentially
 */
static int rmaps_lama_ordering_sequential(orte_job_t *jdata);

/*
 * Map a single process to a specific node
 */
static int orte_rmaps_lama_map_process(orte_job_t *jdata,
                                       orte_node_t *node,
                                       int app_idx,
                                       orte_proc_t **proc);

/*********************************
 * Main Module function to map a job
 *********************************/
static int orte_rmaps_lama_map(orte_job_t *jdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    mca_base_component_t *loc_comp = &mca_rmaps_lama_component.base_version;

    RMAPS_LAMA_CLEAR_TIMERS();
    RMAPS_LAMA_START_TIMER(RMAPS_LAMA_TIMER_TOTAL);

    /*
     * Sanity Check:
     * If we are not the 'chosen' mapper, then exit here
     */
    if (NULL != jdata->map->req_mapper &&
        0 != strcasecmp(jdata->map->req_mapper, loc_comp->mca_component_name)) {
        /* a mapper has been specified, and it isn't me */
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: job %s not using lama mapper (using %s)",
                            ORTE_JOBID_PRINT(jdata->jobid),
                            jdata->map->req_mapper);
        return ORTE_ERR_TAKE_NEXT_OPTION;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Mapping job %s",
                        ORTE_JOBID_PRINT(jdata->jobid));

    /*
     * Identify this as the mapper responsible for this job
     */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(loc_comp->mca_component_name);

    /*
     * Start at the beginning...
     */
    jdata->num_procs = 0;

    /*
     * Process the command line arguments
     */
    RMAPS_LAMA_START_TIMER(RMAPS_LAMA_TIMER_PARSE_PARAMS);
    if( ORTE_SUCCESS != (ret = orte_rmaps_lama_process_params(jdata)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }
    RMAPS_LAMA_END_TIMER(RMAPS_LAMA_TIMER_PARSE_PARAMS);

    /*
     * Actually map the job
     */
    if( ORTE_SUCCESS != (ret = orte_rmaps_lama_map_core(jdata)) ) {
        ORTE_ERROR_LOG(ret);
        exit_status = ret;
        goto cleanup;
    }

    /*
     * All Done
     */

    RMAPS_LAMA_END_TIMER(RMAPS_LAMA_TIMER_TOTAL);
    RMAPS_LAMA_DISPLAY_TIMERS();


 cleanup:
    if( NULL != lama_mapping_layout ) {
        free(lama_mapping_layout);
        lama_mapping_layout = NULL;
    }

    if( NULL != lama_mapping_layout_sort ) {
        free(lama_mapping_layout_sort);
        lama_mapping_layout_sort = NULL;
    }

    if( NULL != lama_mppr_levels ) {
        free(lama_mppr_levels);
        lama_mppr_levels = NULL;
    }

    return exit_status;
}


/*********************************
 * User defined lookup structure for hwloc topology
 *********************************/
static void rmaps_lama_hwloc_user_construct(rmaps_lama_hwloc_user_t *item)
{
    item->node_mppr = OBJ_NEW(opal_pointer_array_t);
    opal_pointer_array_init(item->node_mppr,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE,
                            ORTE_GLOBAL_ARRAY_MAX_SIZE,
                            ORTE_GLOBAL_ARRAY_BLOCK_SIZE);
}

static void rmaps_lama_hwloc_user_destruct(rmaps_lama_hwloc_user_t *item)
{
    orte_std_cntr_t i;

    if( NULL != item->node_mppr ) {
        for(i = 0; i < item->node_mppr->size; ++i) {
            if( NULL != item->node_mppr->addr[i] ) {
                OBJ_RELEASE(item->node_mppr->addr[i]);
                item->node_mppr->addr[i] = NULL;
            }
        }
        OBJ_RELEASE(item->node_mppr);
        item->node_mppr = NULL;
    }
}


/*********************************
 * Command line parameter parsing functions
 *********************************/
static int orte_rmaps_lama_process_params(orte_job_t *jdata)
{
    int ret, i;
    char *type_str = NULL;

    /*
     * Process map/bind/order/mppr aliases.  It will print its own
     * error message if something went wrong.
     */
    if( ORTE_SUCCESS != (ret = rmaps_lama_process_alias_params(jdata) ) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    /*
     * Parse: Binding.  It will print its own error message if
     * something goes wrong.
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ----- Binding  : [%s]",
                        rmaps_lama_cmd_bind);
    if( ORTE_SUCCESS != (ret = rmaps_lama_parse_binding(rmaps_lama_cmd_bind,
                                                        &lama_binding_level,
                                                        &lama_binding_num_levels)) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( 10 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        type_str = lama_type_enum_to_str(lama_binding_level);
        opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: ----- Binding  : %*d x %10s",
                            MAX_BIND_DIGIT_LEN, lama_binding_num_levels, type_str);
        free(type_str);
        type_str = NULL;
    }
    /* Reset the binding option since we are going to do it ourselves */
    OPAL_SET_BINDING_POLICY(jdata->map->binding, OPAL_BIND_TO_NONE);

    /*
     * Parse: Mapping from Process Layout string.  It will print its
     * own error message if something goes wrong.
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ----- Mapping  : [%s]",
                        rmaps_lama_cmd_map);
    if( ORTE_SUCCESS != (ret = rmaps_lama_parse_mapping(rmaps_lama_cmd_map,
                                                        &lama_mapping_layout,
                                                        &lama_mapping_layout_sort,
                                                        &lama_mapping_num_layouts)) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( 10 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        for( i = 0; i < lama_mapping_num_layouts; ++i ) {
            type_str = lama_type_enum_to_str(lama_mapping_layout[i]);
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ----- Mapping  : (%d) %10s (%d vs %d)",
                                i, type_str,
                                lama_mapping_layout[i], lama_mapping_layout_sort[i]);
            free(type_str);
            type_str = NULL;
        }
    }

    /*
     * Parse: MPPR.  It will print its own error message if something
     * goes wrong.
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ----- MPPR     : [%s]",
                        rmaps_lama_cmd_mppr);
    if( ORTE_SUCCESS != (ret = rmaps_lama_parse_mppr(rmaps_lama_cmd_mppr,
                                                     &lama_mppr_levels,
                                                     &lama_mppr_num_levels)) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( 10 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        for( i = 0; i < lama_mppr_num_levels; ++i ) {
            type_str = lama_type_enum_to_str(lama_mppr_levels[i].type);
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ----- MPPR     : %*d at %10s",
                                MAX_BIND_DIGIT_LEN, lama_mppr_levels[i].max_resources, type_str);
            free(type_str);
            type_str = NULL;
        }
    }

    /*
     * Parse: Ordering
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ----- Ordering : [%s]",
                        rmaps_lama_cmd_ordering);
    if( ORTE_SUCCESS != (ret = rmaps_lama_parse_ordering(rmaps_lama_cmd_ordering,
                                                         &lama_ordering)) ) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }

    if( 10 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        if( LAMA_ORDER_NATURAL == lama_ordering ) {
            type_str = strdup("Natural");
        }
        else if( LAMA_ORDER_SEQ == lama_ordering ) {
            type_str = strdup("Sequential");
        }
        else {
            type_str = strdup("Unknown");
        }
        opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: ----- Ordering : %10s",
                            type_str);
        free(type_str);
        type_str = NULL;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");

    return ORTE_SUCCESS;
}


/*********************************
 * Support functions
 *********************************/
rmaps_lama_level_type_t lama_type_str_to_enum(char *param)
{
    if( 0 == strncmp(param, "n", strlen("n")) ) {
        return LAMA_LEVEL_MACHINE;
    }
    else if( 0 == strncmp(param, "b", strlen("b")) ) {
        return LAMA_LEVEL_BOARD;
    }
    else if( 0 == strncmp(param, "s", strlen("s")) ) {
        return LAMA_LEVEL_SOCKET;
    }
    else if( 0 == strncmp(param, "c", strlen("c")) ) {
        return LAMA_LEVEL_CORE;
    }
    else if( 0 == strncmp(param, "h", strlen("h")) ) {
        return LAMA_LEVEL_PU;
    }
    else if( 0 == strncmp(param, "L1", strlen("L1")) ) {
        return LAMA_LEVEL_CACHE_L1;
    }
    else if( 0 == strncmp(param, "L2", strlen("L2")) ) {
        return LAMA_LEVEL_CACHE_L2;
    }
    else if( 0 == strncmp(param, "L3", strlen("L3")) ) {
        return LAMA_LEVEL_CACHE_L3;
    }
    else if( 0 == strncmp(param, "N", strlen("N")) ) {
        return LAMA_LEVEL_NUMA;
    }

    return LAMA_LEVEL_UNKNOWN;
}

char * lama_type_enum_to_str(rmaps_lama_level_type_t param)
{
    if( LAMA_LEVEL_MACHINE == param ) {
        return strdup("Machine");
    }
    else if( LAMA_LEVEL_BOARD == param ) {
        return strdup("Board");
    }
    else if( LAMA_LEVEL_SOCKET == param ) {
        return strdup("Socket");
    }
    else if( LAMA_LEVEL_CORE == param ) {
        return strdup("Core");
    }
    else if( LAMA_LEVEL_PU == param ) {
        return strdup("Hw. Thread");
    }
    else if( LAMA_LEVEL_CACHE_L1 == param ) {
        return strdup("L1 Cache");
    }
    else if( LAMA_LEVEL_CACHE_L2 == param ) {
        return strdup("L2 Cache");
    }
    else if( LAMA_LEVEL_CACHE_L3 == param ) {
        return strdup("L3 Cache");
    }
    else if( LAMA_LEVEL_NUMA == param ) {
        return strdup("NUMA");
    }

    return strdup("Unknown");
}

/*********************************
 * Core Mapper function
 *********************************/
static int orte_rmaps_lama_map_core(orte_job_t *jdata)
{
    int ret, exit_status = ORTE_SUCCESS;
    int cur_app_idx = 0;
    int num_slots;
    orte_app_context_t *cur_app_context = NULL;
    orte_node_t *cur_mach = NULL;
    orte_node_t **cur_mach_ptr = NULL;
    orte_proc_t *proc = NULL;
    opal_list_t *node_list = NULL;
    opal_list_item_t *item = NULL;
    opal_tree_t *max_tree = NULL;
    int *pu_idx_ref = NULL;
    int *last_pu_idx_ref = NULL;
    int i, num_mapped, last_num_mapped, mach_level = -1;
    orte_std_cntr_t j;
    int max_procs_to_map;
    int iter_passes;
    char * last_level_str = NULL;
    bool initial_map = true;

    /*
     * Setup PU reference
     * Find the position of the 'machine'
     */
    pu_idx_ref = (int*)malloc(sizeof(int) * lama_mapping_num_layouts);
    if (NULL == pu_idx_ref) {
        return ORTE_ERROR;
    }
    last_pu_idx_ref = (int*)malloc(sizeof(int) * lama_mapping_num_layouts);
    if (NULL == last_pu_idx_ref) {
        free(pu_idx_ref);
        return ORTE_ERROR;
    }

    for( i = 0; i < lama_mapping_num_layouts; ++i ) {
        pu_idx_ref[i] = 0;
        last_pu_idx_ref[i] = -1;
        if( LAMA_LEVEL_MACHINE == lama_mapping_layout[i] ) {
            mach_level = i;
        }
    }

    /*
     * Foreach app context
     */
    for(cur_app_idx = 0; cur_app_idx < jdata->apps->size; ++cur_app_idx ) {
        if( NULL == (cur_app_context = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, cur_app_idx))) {
            continue;
        }

        /*
         * Get the list of nodes for this app_context.
         */
        node_list = OBJ_NEW(opal_list_t);
        ret = orte_rmaps_base_get_target_nodes(node_list,
                                               &num_slots,
                                               cur_app_context,
                                               jdata->map->mapping,
                                               initial_map, false);
        if(ORTE_SUCCESS != ret ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
        /* Flag that all subsequent requests should not reset the node->mapped flag */
        initial_map = false;

        /*
         * If a bookmark exists from some prior mapping, then start from there
         */
        cur_mach = (orte_node_t*)orte_rmaps_base_get_starting_point(node_list, jdata);

        /*
         * If the application did not specify the number of procs
         * then set it to the number of 'slots'
         * JJH: TODO: Revisit 'max_procs' calculation
         */
        if (0 == cur_app_context->num_procs) {
            cur_app_context->num_procs = num_slots;
        }
        max_procs_to_map = cur_app_context->num_procs;

        /*
         * Build the Max Tree
         */
        RMAPS_LAMA_START_TIMER(RMAPS_LAMA_TIMER_BUILD_MAX_TREE);
        max_tree = rmaps_lama_create_empty_max_tree();
        if( ORTE_SUCCESS != (ret = rmaps_lama_build_max_tree(jdata, node_list,
                                                             max_tree,
                                                             &lama_mppr_max_tree_homogeneous_system)) ) {
            exit_status = ret;
            goto cleanup;
        }
        RMAPS_LAMA_END_TIMER(RMAPS_LAMA_TIMER_BUILD_MAX_TREE);


        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Mapping:  -----------------------");
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: ---------------------------------");
        RMAPS_LAMA_START_TIMER(RMAPS_LAMA_TIMER_MAPPING);

        /*
         * Clear PU reference
         */
        for( i = 0; i < lama_mapping_num_layouts; ++i ) {
            pu_idx_ref[i] = 0;
        }

        /*
         * Mapping: Recursively loop over all levels
         */
        num_mapped = 0;
        last_num_mapped = 0;
        iter_passes = 0;
        cur_mach_ptr = (orte_node_t**)malloc(sizeof(orte_node_t*));
        *cur_mach_ptr = cur_mach;
        while( max_procs_to_map > num_mapped ) {
            ret = rmaps_lama_map_core_iter_level(jdata,
                                                 cur_app_context,
                                                 node_list,
                                                 cur_mach_ptr,
                                                 max_tree,
                                                 lama_mapping_num_layouts-1,
                                                 mach_level,
                                                 &pu_idx_ref,
                                                 &last_pu_idx_ref,
                                                 &num_mapped,
                                                 max_procs_to_map,
                                                 &iter_passes);
            if( ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }

            /*
             * We only get here (without finishing the mapping) if we are going to
             * start oversubscribing resources.
             */
            if( max_procs_to_map > num_mapped ) {
                if( !rmaps_lama_can_oversubscribe ) {
                    orte_show_help("help-orte-rmaps-lama.txt",
                                   "orte-rmaps-lama:oversubscribe",
                                   true,
                                   num_mapped, max_procs_to_map);
                    exit_status = ORTE_ERROR;
                    goto cleanup;
                } else {
                    rmaps_lama_am_oversubscribing = true;
                }
            }

            /*
             * Check to see if we have made any progress in the mapping loop
             */
            if( 0 < cur_app_idx && 2 == iter_passes ) {
                /*
                 * Give it another pass:
                 * This is an edge case when we are trying to restart from a
                 * bookmark left by a previous app context. If this app context
                 * is starting from exactly the beginning of the allocation
                 * then the recursive loop could return out here after the
                 * increment pass. This is indicated by (iter_passes = 2).
                 * Since no processes were mapped, we just try again.
                 */
            }
            else if( last_num_mapped == num_mapped ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "orte-rmaps-lama:no-resources-available",
                               true,
                               cur_app_idx,
                               num_mapped, max_procs_to_map,
                               (NULL == rmaps_lama_cmd_map      ? "[Not Provided]" : rmaps_lama_cmd_map),
                               (NULL == rmaps_lama_cmd_bind     ? "[Not Provided]" : rmaps_lama_cmd_bind),
                               (NULL == rmaps_lama_cmd_mppr     ? "[Not Provided]" : rmaps_lama_cmd_mppr),
                               (NULL == rmaps_lama_cmd_ordering ? "[Not Provided]" : rmaps_lama_cmd_ordering));
                exit_status = ORTE_ERROR;
                goto cleanup;
            } else {
                last_num_mapped = num_mapped;
            }
        }

        /*
         * Display Bookmark for debugging
         */
        last_level_str = pu_ref_to_str(last_pu_idx_ref, lama_mapping_num_layouts);
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Bookmark: --> Node %10s PU %10s",
                            jdata->bookmark->name, last_level_str);
        free(last_level_str);
        last_level_str = NULL;

        /*
         * Clenup for next iteration
         */
        if( NULL != node_list ) {
            while(NULL != (item = opal_list_remove_first(node_list))) {
                OBJ_RELEASE(item);
            }
            OBJ_RELEASE(node_list);
            node_list = NULL;
        }

        OBJ_RELEASE(max_tree);
        max_tree = NULL;
    }

    RMAPS_LAMA_END_TIMER(RMAPS_LAMA_TIMER_MAPPING);


    /*
     * Ordering
     */
    RMAPS_LAMA_START_TIMER(RMAPS_LAMA_TIMER_ORDERING);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    if( LAMA_ORDER_SEQ == lama_ordering ) {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Ordering: Sequential ------------");

        if( ORTE_SUCCESS != (ret = rmaps_lama_ordering_sequential(jdata)) ) {
            ORTE_ERROR_LOG(ret);
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Ordering: Natural ---------------");
#if 0
        /*
         * We compute our own vpids inline with the algorithm. So no need to use the
         * orte_rmaps_base_compute_vpids() function.
         */
#endif
    }
    RMAPS_LAMA_END_TIMER(RMAPS_LAMA_TIMER_ORDERING);


    /*
     * Display Mapping
     */
    if( 10 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        char *cpu_bitmap;
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: ---------------------------------");
        for( j = 0; j < jdata->procs->size; ++j) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, j))) {
                continue;
            }
            cpu_bitmap = NULL;
            orte_get_attribute(&proc->attributes, ORTE_PROC_CPU_BITMAP, (void**)&cpu_bitmap, OPAL_STRING);
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Ordering: Proc. %2d on Node %10s - Slot %s",
                                proc->name.vpid, proc->node->name, cpu_bitmap);
            if (NULL != cpu_bitmap) {
                free(cpu_bitmap);
            }
        }
    }


    /*
     * All done
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Finished ------------------------");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");


 cleanup:
    if( NULL != node_list ) {
        while(NULL != (item = opal_list_remove_first(node_list))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(node_list);
    }

    if( NULL != max_tree ) {
        OBJ_RELEASE(max_tree);
    }

    free(pu_idx_ref);
    free(last_pu_idx_ref);

    if( NULL != last_level_str ) {
        free(last_level_str);
    }

    return exit_status;
}

static int rmaps_lama_map_core_iter_level(orte_job_t *jdata,
                                          orte_app_context_t *cur_app_context,
                                          opal_list_t *node_list,
                                          orte_node_t **cur_mach_ptr,
                                          opal_tree_t *max_tree,
                                          int cur_level,
                                          int mach_level,
                                          int **pu_idx_ref,
                                          int **last_pu_idx_ref,
                                          int *num_mapped,
                                          int max_procs,
                                          int *iter_passes)
{
    int ret, exit_status = ORTE_SUCCESS;
    int i, j;
    opal_tree_item_t *tree_for_level = NULL;
    int max_subtree_arity = 0;
    char * level_str = NULL;
    char * last_level_str = NULL;
    char * slot_list = NULL;
    orte_proc_t *proc = NULL;
    int pu_idx = 0;

    /*
     * Find the current tree for this level
     * If it is the machine level, then we need to access the information from
     * the node list, not the max_tree.
     */
    if( cur_level != mach_level ) {
        tree_for_level = opal_tree_find_with(opal_tree_get_root(max_tree),
                                             &lama_mapping_layout[cur_level]);
        /*
         * We do not need subtree, but the arity of the subtree
         * JJH TODO: This should be an opal_tree function.
         */
        max_subtree_arity = 1; /* include self */
        while( NULL != (tree_for_level = opal_tree_get_next_sibling(tree_for_level)) ) {
            ++max_subtree_arity;
        }
    }
    else if( NULL == *cur_mach_ptr ) {
        *cur_mach_ptr = get_next_machine(jdata, node_list, (opal_list_item_t*)(*cur_mach_ptr));
    }

    pu_idx = convert_layer_to_sort_idx(lama_mapping_layout[cur_level]);
    level_str = lama_type_enum_to_str(lama_mapping_layout[cur_level]);

    /*
     * Do we need to advance to a bookmark
     */
    if( (*last_pu_idx_ref)[0] >= 0 && 0 == *iter_passes ) {
        /*
         * Display last mapped
         */
        last_level_str = pu_ref_to_str(*last_pu_idx_ref, lama_mapping_num_layouts);
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Bookmark: --> Last Mapped: Node %10s (bkmrk %10s) PU %10s - Level %2d",
                            (NULL == *cur_mach_ptr ? "(NULL)" : (*cur_mach_ptr)->name),
                            jdata->bookmark->name, last_level_str, (*last_pu_idx_ref)[pu_idx]);
        free(last_level_str);
        last_level_str = NULL;

        /*
         * Set the level starting point to the last known index
         */
        i = (*last_pu_idx_ref)[pu_idx];
    } else {
        i = 0;
    }


    /*
     * Loop over all siblings at this level
     * Initial condition above, Increment at bottom, Break check at bottom
     */
    while( 1 ) {
        /*
         * Define the PU index
         */
        (*pu_idx_ref)[pu_idx] = i;

        if( (*last_pu_idx_ref)[0] >= 0 && 0 == *iter_passes ) {
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Mapping: --> Level %2d: %10s (%2d) - I %2d - Arity %2d - %10s - Increment only",
                                cur_level+1,
                                level_str, pu_idx, i, max_subtree_arity,
                                (NULL == *cur_mach_ptr ? "" : (*cur_mach_ptr)->name));
        } else {
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Mapping: --> Level %2d: %10s (%2d) - I %2d - Arity %2d - %10s",
                                cur_level+1,
                                level_str, pu_idx, i, max_subtree_arity,
                                (NULL == *cur_mach_ptr ? "" : (*cur_mach_ptr)->name));
        }


        /*
         * If not the inner most loop, iterate to the next level down
         */
        if( cur_level > 0 ) {
            ret = rmaps_lama_map_core_iter_level(jdata,
                                                 cur_app_context,
                                                 node_list,
                                                 cur_mach_ptr,
                                                 max_tree,
                                                 cur_level - 1,
                                                 mach_level,
                                                 pu_idx_ref,
                                                 last_pu_idx_ref,
                                                 num_mapped,
                                                 max_procs,
                                                 iter_passes);
            if( ORTE_SUCCESS != ret ) {
                ORTE_ERROR_LOG(ret);
                exit_status = ret;
                goto cleanup;
            }
        }
        /*
         * If we are restarting the iteration from a previous bookmark then
         * the first pass through is a no-op mapping pass that just increments
         * the PU reference.
         * Called by innermost loop
         */
        else if( (*last_pu_idx_ref)[0] >= 0 && 0 == *iter_passes ) {
            *iter_passes += 1;
        }
        /*
         * Try to map at this location
         */
        else {
            /*
             * On first pass, make sure we increment this, just so we do not
             * accidentally think this is an increment pass.
             */
            if( 0 == *iter_passes ) {
                *iter_passes += 1;
            }

            /*
             * Display the PU ref for debugging
             */
            display_pu_ref(*pu_idx_ref, lama_mapping_num_layouts, *num_mapped, proc);


            /*
             * Check to see if this resource is available on this node.
             *
             * In a heterogeneous or otherwise non-uniformly restricted
             * environment we may iterate to a resource that is not
             * available either because it does not exist, or is not
             * available for allocation (off-lined, sub-node allocation).
             * Additionally, we need to check resource constrains expressed
             * in the MPPR and binding.
             */
            ret = check_node_availability((*cur_mach_ptr),
                                          max_tree,
                                          *pu_idx_ref,
                                          &slot_list);
            if( ORTE_SUCCESS != ret || NULL == slot_list ) {
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "mca:rmaps:hwtopo: Mapping: --> Level %2d: %s - INVALID/SKIP",
                                    cur_level+1,
                                    level_str);
                /*
                 * By not mapping here we just let the iterations continue
                 * until a suitable match is found or we have exhausted all
                 * possible locations to match and thus cannot map any more.
                 */
            }
            else {
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "mca:rmaps:lama: Mapping: --> Level %2d: %s - Slot List (%s)",
                                    cur_level+1,
                                    level_str, slot_list);

                /*
                 * Map this process onto the resource specified
                 * level_tree_objs[*] and cur_mach point to the specific resource
                 */
                proc = NULL;
                ret = orte_rmaps_lama_map_process(jdata,
                                                  (*cur_mach_ptr),
                                                  cur_app_context->idx,
                                                  &proc);
                if( ORTE_SUCCESS != ret ) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    goto bailout;
                }

                /*
                 * Set the binding for this process
                 */
                orte_set_attribute(&proc->attributes, ORTE_PROC_CPU_BITMAP, ORTE_ATTR_GLOBAL, slot_list, OPAL_STRING);
                /*
                 * Insert the proc into the 'native' ordering location.
                 */
                proc->name.vpid = jdata->num_procs;
                if (ORTE_SUCCESS != (ret = opal_pointer_array_set_item(jdata->procs,
                                                                       proc->name.vpid, proc))) {
                    ORTE_ERROR_LOG(ret);
                    exit_status = ret;
                    goto cleanup;
                }
                jdata->num_procs += 1;

                /*
                 * Save a bookmark so we can return here later if necessary
                 */
                for( j = 0; j < lama_mapping_num_layouts; ++j ) {
                    (*last_pu_idx_ref)[j] = (*pu_idx_ref)[j];
                }
                jdata->bookmark = (orte_node_t*)(*cur_mach_ptr);

                (*num_mapped)++;
            }
        }

        /*
         * Increment loop
         *
         * If we are binding, then we may need to advance the binding layer
         * by more than one.
         */
        if( cur_level != mach_level ) {
            if( lama_binding_level == lama_mapping_layout[cur_level] ) {
                i += lama_binding_num_levels;
            } else {
                ++i;
            }
        } else {
            /*
             * Note: Currently we do not allow for 'binding' to multiple machines
             * But keep the code just in case we want to play with 'stride' later
             */
            if( lama_binding_level == lama_mapping_layout[cur_level] && lama_binding_num_levels > 1) {
                opal_output(0, "mca:rmaps:lama: ERROR: Cannot bind to multiple machines - SHOULD NEVER HAPPEN: %s",
                            rmaps_lama_cmd_bind);
                exit_status = ORTE_ERROR;
                goto bailout;
#if 0
                for( j = 0; j < lama_binding_num_levels; ++j ) {
                    cur_mach = get_next_machine(jdata, node_list, (opal_list_item_t*)cur_mach);
                    if( NULL == cur_mach ) {
                        break;
                    }
                    ++i;
                }
#endif
            } else {
                *cur_mach_ptr = get_next_machine(jdata, node_list, (opal_list_item_t*)(*cur_mach_ptr));
                ++i;
            }
        }

        /*
         * Check if we are done mapping before iterating again
         */
        if( max_procs <= *num_mapped ) {
            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }

        /*
         * Check if we are done looping
         */
        if( cur_level != mach_level ) {
            if( i >= max_subtree_arity ) {
                break;
            }
        } else {
            if( NULL == *cur_mach_ptr ) {
                break;
            }
        }
    }


    /*
     * Sanity Check: Check if we are done mapping
     */
    if( max_procs <= *num_mapped ) {
        exit_status = ORTE_SUCCESS;
        goto cleanup;
    }

 cleanup:
    /*
     * If the outermost layer, the increment the number of iteration passes.
     */
    if( cur_level == lama_mapping_num_layouts-1 ) {
        *iter_passes += 1;
    }

 bailout:
    if( NULL != level_str ) {
        free(level_str);
        level_str = NULL;
    }

    if( NULL != slot_list ) {
        free(slot_list);
        slot_list = NULL;
    }

    return exit_status;
}

static orte_node_t* get_next_machine(orte_job_t *jdata, opal_list_t *node_list,
                                     opal_list_item_t *cur_mach)
{
    orte_node_t *next_mach = NULL;

    if( NULL == cur_mach ) {
        next_mach = (orte_node_t*)opal_list_get_first(node_list);
    }
    else if( opal_list_get_last(node_list) == cur_mach ) {
        next_mach = NULL;
    }
    else {
        next_mach = (orte_node_t*)opal_list_get_next(cur_mach);
    }

    return next_mach;
}

static int orte_rmaps_lama_map_process(orte_job_t *jdata,
                                       orte_node_t *node,
                                       int app_idx,
                                       orte_proc_t **proc)
{
    int ret;

    /*
     * Add this node to the map, but only once
     */
    if( !ORTE_FLAG_TEST(node, ORTE_NODE_FLAG_MAPPED) ) {
        if (ORTE_SUCCESS > (ret = opal_pointer_array_add(jdata->map->nodes, (void*)node))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        ORTE_FLAG_SET(node, ORTE_NODE_FLAG_MAPPED);
        OBJ_RETAIN(node);  /* maintain accounting on object */
        ++(jdata->map->num_nodes);
    }

    /*
     * Setup the process object
     */
    if (NULL == (*proc = orte_rmaps_base_setup_proc(jdata, node, app_idx))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    return ORTE_SUCCESS;
}

static int rmaps_lama_ordering_sequential(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_proc_t *proc = NULL, *swap = NULL;
    orte_std_cntr_t i, j;
    int cur_rank = 0;
    orte_node_t *cur_node = NULL;

    map = jdata->map;

    opal_output_verbose(15, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");

    /*
     * Assign the ranks sequentially
     */
    for( i = 0; i < map->nodes->size; ++i) {
        if (NULL == (cur_node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        for( j = 0; j < cur_node->procs->size; ++j) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(cur_node->procs, j))) {
                continue;
            }
            /* ignore procs from other jobs */
            if (proc->name.jobid != jdata->jobid) {
                continue;
            }

            opal_output_verbose(15, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Ordering: Rename Proc. %2d to %2d (Rev. %s)",
                                proc->name.vpid, cur_rank, proc->node->name);
            proc->name.vpid = cur_rank;
            ++cur_rank;
        }
    }

    /*
     * Fix the job structure ordering - Sort by new vpid
     * 
     * If we do not do this then the remote daemons assign the incorrect
     * ranks to the processes since they use the relative ordering in the
     * jdata->procs structure to determine vpids locally.
     *
     * JJH: Look at combining these loops with the loop in the core so we
     * JJH: do not have to iterate over the list two times
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    cur_rank = 0;
    for( j = 0; j < jdata->procs->size; ++j) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, j))) {
            continue;
        }

        opal_output_verbose(15, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Ordering: Proc. %2d on Node %s",
                            proc->name.vpid, proc->node->name);

        while((int)proc->name.vpid != cur_rank ) {
            swap = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->name.vpid);

            opal_pointer_array_set_item(jdata->procs, proc->name.vpid, proc);
            opal_pointer_array_set_item(jdata->procs, cur_rank,        swap);

            opal_output_verbose(15, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Ordering: \t SWAP Proc. %2d (%d) and Proc. %2d (%d)",
                                proc->name.vpid, cur_rank, swap->name.vpid, proc->name.vpid);
            proc = swap;
        }
        ++cur_rank;
    }

    return ORTE_SUCCESS;
}

static int convert_layer_to_sort_idx(rmaps_lama_level_type_t layer)
{
    int i;

    for(i = 0; i < lama_mapping_num_layouts; ++i ) {
        if( lama_mapping_layout_sort[i] == layer ) {
            return i;
        }
    }

    return 0;
}

static void display_pu_ref(int *ref, int size, int rank, orte_proc_t *proc)
{
    char *str = NULL;

    str = pu_ref_to_str(ref, size);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Mapping: PU Ref: %s [Rank %2d] Name: %s",
                        str, rank,
                        (NULL == proc ? "(null)" : ORTE_NAME_PRINT(&proc->name)));

    free(str);

    return;
}

static char * pu_ref_to_str(int *ref, int size)
{
    int i, idx;
    char *str = NULL;

    str = (char *)malloc(sizeof(char) * (2 * size));
    for(i = 0, idx = 0; i < size; ++i, idx += 2) {
        sprintf(&(str[idx]), "%2d", ref[i]);
    }

    return str;
}

static int check_node_availability(orte_node_t *cur_node,
                                   opal_tree_t *max_tree,
                                   int *pu_idx_ref,
                                   char **slot_list)
{
    int exit_status = ORTE_SUCCESS;
    int i;
    char * level_str = NULL;
    hwloc_obj_t *topo_child = NULL, *topo_parent, *topo_allocated;


    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Checking: Node (%s) -------------",
                        cur_node->name);
    opal_output_verbose(11, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");


    /*
     * Determine if the current node has the necessary hardware
     * as described by the PU index.
     * Find the hwloc object reference for the resource pointed to
     * by the PU index.
     * JJH TODO: If homogeneous system then this could be simplified.
     */
    topo_allocated = topo_parent  = (hwloc_obj_t*)malloc(sizeof(hwloc_obj_t) * 1);
    if (NULL == topo_parent) {
        return ORTE_ERROR;
    }
    *topo_parent = hwloc_get_obj_by_depth(cur_node->topology, 0, 0);
    for( i = 0; i < lama_mapping_num_layouts; ++i ) {
        /*
         * Skip 'machine' level
         */
        if( LAMA_LEVEL_MACHINE == lama_mapping_layout_sort[i] ) {
            continue;
        }
        /*
         * Skip 'board' level
         * JJH: HWLOC does not support BOARD at the moment
         */
        if( LAMA_LEVEL_BOARD == lama_mapping_layout_sort[i] ) {
            continue;
        }

        level_str = lama_type_enum_to_str(lama_mapping_layout_sort[i]);
        opal_output_verbose(11, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: Checking: %2d of %s",
                            pu_idx_ref[i], level_str);

        /*
         * Find the nth subtree matching the current key
         */
        topo_child = rmaps_lama_find_nth_subtree_match(cur_node->topology,
                                                       *topo_parent,
                                                       pu_idx_ref[i],
                                                       lama_mapping_layout_sort[i]);

        /*
         * If it does not exist, then this node is not capable of matching
         * so it is unavailable.
         */
        if( NULL == topo_child ) {
            opal_output_verbose(11, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Check failed: Node %s does not have a %10s %2d",
                                cur_node->name, level_str, pu_idx_ref[i]);
            exit_status = ORTE_ERROR;
            goto cleanup;
        }

        /*
         * Keep decending the tree
         */
        topo_parent = topo_child;
        free(level_str);
        level_str = NULL;
    }

    /*
     * We have sufficient hardware :)
     */


    /*
     * Return the native slot list to bind to
     * Internally checks the MPPR
     */
    *slot_list = get_native_slot_list(cur_node, topo_parent, pu_idx_ref);
    if( NULL == *slot_list ) {
        goto cleanup;
    }

 cleanup:
    if( NULL != level_str ) {
        free(level_str);
        level_str = NULL;
    }

    if( ORTE_SUCCESS != exit_status ) {
        if( NULL != *slot_list ) {
            free(*slot_list);
            *slot_list = NULL;
        }
    }

    free(topo_allocated);

    return exit_status;
}

static int rmaps_lama_check_mppr(orte_node_t *node,
                                 hwloc_obj_t *child_obj)
{
    int ret;

    /*
     * Optimization if no MPPR provided
     */
    if( NULL == lama_mppr_levels ) {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: No MPPR to check - Skip...");
        return ORTE_SUCCESS;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Check ---------------------------");
    /*
     * Check Parents (excluding self)
     */
    if( ORTE_SUCCESS != (ret = rmaps_lama_iter_mppr_parents(node, &(*child_obj)->parent, true)) ) {
        return ret;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Check ---------------------------");

    /*
     * Check Children (including self)
     */
    if( ORTE_SUCCESS != (ret = rmaps_lama_iter_mppr_children(node, child_obj, true)) ) {
        return ret;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Check ---------------------------");

    return ORTE_SUCCESS;
}

static int rmaps_lama_inc_mppr(orte_node_t *node,
                               hwloc_obj_t *child_obj)
{
    int ret;

    /*
     * Optimization if no MPPR provided
     */
    if( NULL == lama_mppr_levels ) {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: No MPPR to increment - Skip...");
        return ORTE_SUCCESS;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Inc   ---------------------------");
    /*
     * Increment Parents (excluding self)
     */
    if( ORTE_SUCCESS != (ret = rmaps_lama_iter_mppr_parents(node, &(*child_obj)->parent, false)) ) {
        return ret;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Inc   ---------------------------");

    /*
     * Increment Children (including self)
     */
    if( ORTE_SUCCESS != (ret = rmaps_lama_iter_mppr_children(node, child_obj, false)) ) {
        return ret;
    }

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Inc   ---------------------------");

    return ORTE_SUCCESS;
}

static int rmaps_lama_iter_mppr_parents(orte_node_t *node,
                                        hwloc_obj_t *child_obj,
                                        bool check_only)
{
    rmaps_lama_hwloc_user_t *hwloc_userdata = NULL;
    rmaps_lama_node_mppr_t *mppr_accounting = NULL;
    char str[128];

    /*
     * Basecase
     */
    if( NULL == *child_obj ) {
        return ORTE_SUCCESS;
    }

    /*
     * Check self
     */
    /*
     * Access MPPR info for this object
     */
    hwloc_userdata = (rmaps_lama_hwloc_user_t*)((opal_hwloc_topo_data_t*)(*child_obj)->userdata)->userdata;
    mppr_accounting = (rmaps_lama_node_mppr_t*)opal_pointer_array_get_item(hwloc_userdata->node_mppr, node->index);

    hwloc_obj_snprintf(str, sizeof(str), node->topology, *child_obj, "#", 0);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: %s: P [%2d] %10s - %20s - Max %3d , Cur %3d (Oversub.: %s / %s)",
                        (check_only ? "Checking " : "Increment"),
                        node->index, node->name, str,
                        mppr_accounting->max,
                        (check_only ? mppr_accounting->cur : mppr_accounting->cur + 1),
                        (rmaps_lama_am_oversubscribing ? "T" : "F"),
                        (rmaps_lama_can_oversubscribe ? "T" : "F") );

    /*
     * Check limits - Error on first to exceed
     */
    if( check_only ) {
        if( mppr_accounting->max >= 0 && !rmaps_lama_am_oversubscribing) {
            if( (mppr_accounting->cur)+1 > mppr_accounting->max ) {
                return ORTE_ERROR;
            }
        }
    }
    /*
     * Increment current number allocated below this level
     */
    else {
        mppr_accounting->cur += 1;
    }

    /*
     * Go to parent
     */
    return rmaps_lama_iter_mppr_parents(node, &((*child_obj)->parent), check_only);
}

static int rmaps_lama_iter_mppr_children(orte_node_t *node,
                                         hwloc_obj_t *child_obj,
                                         bool check_only)
{
    int ret;
    rmaps_lama_hwloc_user_t *hwloc_userdata = NULL;
    rmaps_lama_node_mppr_t *mppr_accounting = NULL;
    char str[128];
    int i;

    /*
     * Check self
     */
    /*
     * Access MPPR info for this object
     */
    hwloc_userdata = (rmaps_lama_hwloc_user_t*)((opal_hwloc_topo_data_t*)(*child_obj)->userdata)->userdata;
    mppr_accounting = (rmaps_lama_node_mppr_t*)opal_pointer_array_get_item(hwloc_userdata->node_mppr, node->index);

    hwloc_obj_snprintf(str, sizeof(str), node->topology, *child_obj, "#", 0);
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: %s: C [%2d] %10s - %20s - Max %3d , Cur %3d (Oversub.: %s / %s)",
                        (check_only ? "Checking " : "Increment"),
                        node->index, node->name, str,
                        mppr_accounting->max,
                        (check_only ? mppr_accounting->cur : mppr_accounting->cur + 1),
                        (rmaps_lama_am_oversubscribing ? "T" : "F"),
                        (rmaps_lama_can_oversubscribe ? "T" : "F") );

    /*
     * Check limits - Error on first to exceed
     */
    if( check_only ) {
        if( mppr_accounting->max >= 0 && !rmaps_lama_am_oversubscribing) {
            if( (mppr_accounting->cur)+1 > mppr_accounting->max ) {
                return ORTE_ERROR;
            }
        }
    }
    /*
     * Increment current number allocated below this level
     */
    else {
        mppr_accounting->cur += 1;
    }

    /*
     * Check all children
     */
    for(i = 0; i < (int)(*child_obj)->arity; ++i ) {
        if( ORTE_SUCCESS != (ret = rmaps_lama_iter_mppr_children(node, &((*child_obj)->children[i]), check_only)) ) {
            return ret;
        }
    }

    return ORTE_SUCCESS;
}


static char * get_native_slot_list(orte_node_t *cur_node, hwloc_obj_t *pu_obj, int *put_idx_ref)
{
    int i;
    char *slot_list = NULL;
    hwloc_obj_t *binding_parent = NULL;
    hwloc_obj_t *cur_parent = NULL;
    hwloc_cpuset_t binding_cpuset;
    hwloc_cpuset_t scratch_cpuset;
    char *type_str = NULL;

    /*
     * Sanity check
     */
    if( NULL == pu_obj ) {
        return NULL;
    }

    /*
     * Determine the cpumask to send to the backend for binding
     */

    /*
     * Iterate up the tree until we reach the binding parent
     */
    binding_parent = rmaps_lama_find_parent(cur_node->topology, pu_obj, lama_binding_level);
    if( NULL == binding_parent ) {
        return NULL;
    }

    /*
     * Iterate across cousins until we find enough resources or hit the node boundary
     */
    binding_cpuset = hwloc_bitmap_alloc();
    hwloc_bitmap_zero(binding_cpuset);

    scratch_cpuset = hwloc_bitmap_alloc();

    cur_parent = binding_parent;

    for(i = 0; i < lama_binding_num_levels; ++i) {
        /*
         * Check MPPR Availability
         */
        if( ORTE_SUCCESS != rmaps_lama_check_mppr(cur_node, cur_parent) ) {
            goto cleanup;
        }

        /*
         * Accumulate the bitmask
         * 
         * JJH: TODO: Add resource offline check (?)
         */
        hwloc_bitmap_zero(scratch_cpuset);
        /* JJH: Maybe use opal_hwloc_base_get_available_cpus(cur_node->topology, (*cur_parent)) ?
         * They do pretty much the same thing, but with more checks...
         */
        hwloc_bitmap_and(scratch_cpuset, (*cur_parent)->allowed_cpuset, (*cur_parent)->online_cpuset);
        hwloc_bitmap_or(binding_cpuset, scratch_cpuset, binding_cpuset);

#if 0
        {
            hwloc_obj_snprintf(str, sizeof(str), cur_node->topology, *cur_parent, "#", 0);
            printf("--> BINDING TO -- %-20s \t -- %2d of %2d -- %2d vs %2d\n",str,
                   i, lama_binding_level,
                   (*binding_parent)->logical_index, (*cur_parent)->logical_index);

            hwloc_bitmap_snprintf(str, sizeof(str), (*cur_parent)->allowed_cpuset );
            printf("-->            CPU A : %-20s\n", str);
            hwloc_bitmap_snprintf(str, sizeof(str), (*cur_parent)->online_cpuset );
            printf("-->            CPU B : %-20s\n", str);
            hwloc_bitmap_snprintf(str, sizeof(str), scratch_cpuset);
            printf("-->            CPU C : %-20s\n", str);
            hwloc_bitmap_snprintf(str, sizeof(str), binding_cpuset);
            printf("-->            CPU D : %-20s\n", str);
        }
#endif

        /*
         * Iterate to the next cousin.
         * If we exceed the boundary of the node, then send up an error.
         */
        if( (i+1) < lama_binding_num_levels && NULL == (*cur_parent)->next_cousin ) {
            type_str = lama_type_enum_to_str(lama_binding_level);
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Error: Not able to bind to %*d x %10s - Stopped at %*d",
                                MAX_BIND_DIGIT_LEN, lama_binding_num_levels,
                                type_str,
                                MAX_BIND_DIGIT_LEN, i);
            free(type_str);
            type_str = NULL;
            goto cleanup;
        }
        /*
         * Point to the next cousin
         */
        if( NULL != (*cur_parent)->next_cousin ) {
            cur_parent = &((*cur_parent)->next_cousin);
        }
    }

    /*
     * Account for the process placement in the MPPR
     * Assumes a previous check
     * We cannot do this in the loop, since if the MPPR check fails we would
     * need to roll back previous increments.
     */
    cur_parent = binding_parent;
    for(i = 0; i < lama_binding_num_levels; ++i) {
        /*
         * Account for the process placement in the MPPR
         * Assumes a previous check.
         */
        if( ORTE_SUCCESS != rmaps_lama_inc_mppr(cur_node, cur_parent) ) {
            goto cleanup;
        }

        /*
         * Point to the next cousin
         */
        if( NULL != (*cur_parent)->next_cousin ) {
            cur_parent = &((*cur_parent)->next_cousin);
        }
    }

    /*
     * Convert the cpuset to a slot_list for the remote daemon
     */
    hwloc_bitmap_list_asprintf(&slot_list, binding_cpuset);

 cleanup:
    hwloc_bitmap_free(scratch_cpuset);
    hwloc_bitmap_free(binding_cpuset);
    free(binding_parent);

    return slot_list;
}


/*********************************
 * Timer Support
 *********************************/
static double rmaps_lama_get_time(void)
{
    double wtime;

#if OPAL_TIMER_USEC_NATIVE
    wtime = (double)opal_timer_base_get_usec() / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif

    return wtime;
}

static void rmaps_lama_set_time(int idx, bool is_start)
{
    if(idx < RMAPS_LAMA_TIMER_MAX ) {
        if( is_start ) {
            timer_start[idx] = rmaps_lama_get_time();
        } else {
            timer_end[idx]   = rmaps_lama_get_time();
            timer_accum[idx] += timer_end[idx] - timer_start[idx];
        }
    }
}

static void rmaps_lama_display_all_timers(void)
{
    double diff = 0.0;
    double total = 0.0;
    char * label = NULL;

    opal_output(0,
                "mca:rmaps:lama: Timing: ---------------------------\n");

    /*
     * Timer: Parse Parameters
     */
    label = strdup("Parse Params");
    diff = timer_accum[RMAPS_LAMA_TIMER_PARSE_PARAMS];
    rmaps_lama_display_indv_timer_core(diff, label);
    free(label);
    total += diff;

    /*
     * Timer: Build Max Tree
     */
    label = strdup("Build Max Tree");
    diff = timer_accum[RMAPS_LAMA_TIMER_BUILD_MAX_TREE];
    rmaps_lama_display_indv_timer_core(diff, label);
    free(label);
    total += diff;

    /*
     * Timer: Mapping
     */
    label = strdup("Mapping");
    diff = timer_accum[RMAPS_LAMA_TIMER_MAPPING];
    rmaps_lama_display_indv_timer_core(diff, label);
    free(label);
    total += diff;

    /*
     * Timer: Ordering
     */
    label = strdup("Ordering");
    diff = timer_accum[RMAPS_LAMA_TIMER_ORDERING];
    rmaps_lama_display_indv_timer_core(diff, label);
    free(label);
    total += diff;

    /*
     * Timer: Total Overhead
     */
    label = strdup("Other Overhead");
    diff = timer_accum[RMAPS_LAMA_TIMER_TOTAL];
    rmaps_lama_display_indv_timer_core(diff - total, label);
    free(label);

    /*
     * Timer: Total
     */
    label = strdup("Total");
    diff = timer_accum[RMAPS_LAMA_TIMER_TOTAL];
    rmaps_lama_display_indv_timer_core(diff, label);
    free(label);

    opal_output(0,
                "mca:rmaps:lama: ---------------------------------");
}

static void rmaps_lama_clear_timers(void)
{
    int i;
    for(i = 0; i < RMAPS_LAMA_TIMER_MAX; ++i) {
        timer_start[i] = 0.0;
        timer_end[i]   = 0.0;
        timer_accum[i] = 0.0;
    }
}


static void rmaps_lama_display_indv_timer_core(double diff, char *str)
{
    double perc  = 0;
    double total = 0;

    total = timer_end[RMAPS_LAMA_TIMER_TOTAL] - timer_start[RMAPS_LAMA_TIMER_TOTAL];
    perc = (diff/total) * 100;

    opal_output(0,
                "mca:rmaps:lama: \t%-20s = %10.2f ms\t%6.2f %s\n",
                str, (diff * 1000), perc, "%");
    return;
}
