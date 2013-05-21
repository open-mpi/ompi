/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 *
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 * Resource Mapping 
 */
#ifndef ORTE_RMAPS_LAMA_H
#define ORTE_RMAPS_LAMA_H

#include "orte_config.h"

#include "opal/class/opal_tree.h"

#include "orte/mca/rmaps/rmaps.h"

BEGIN_C_DECLS

ORTE_MODULE_DECLSPEC extern orte_rmaps_base_component_t mca_rmaps_lama_component;

extern orte_rmaps_base_module_t orte_rmaps_lama_module;


/*********************************
 * Structures & Defines
 *********************************/
/*
 * JJH: Can we reuse the opal_hwloc_level_t data structure in
 *      opal/mca/hwloc/hwloc.h
 */
typedef enum {
    LAMA_LEVEL_MACHINE  =  0,
    LAMA_LEVEL_BOARD    =  1,
    LAMA_LEVEL_NUMA     =  2,
    LAMA_LEVEL_SOCKET   =  3,
    LAMA_LEVEL_CACHE_L3 =  4,
    LAMA_LEVEL_CACHE_L2 =  5,
    LAMA_LEVEL_CACHE_L1 =  6,
    LAMA_LEVEL_CORE     =  7,
    LAMA_LEVEL_PU       =  8,
    LAMA_LEVEL_UNKNOWN  =  9
} rmaps_lama_level_type_t;

typedef enum {
    LAMA_ORDER_NATURAL  =  0,
    LAMA_ORDER_SEQ      =  1
} rmaps_lama_order_type_t;

struct  rmaps_lama_level_info_t {
    rmaps_lama_level_type_t  type;
    int  max_resources;
};
typedef struct rmaps_lama_level_info_t rmaps_lama_level_info_t;

/*
 * Structure to attach to the hwloc tree
 * Accounting for mppr
 */
struct  rmaps_lama_hwloc_user_t {
    opal_object_t super;

    opal_pointer_array_t *node_mppr;
};
typedef struct rmaps_lama_hwloc_user_t rmaps_lama_hwloc_user_t;
OBJ_CLASS_DECLARATION(rmaps_lama_hwloc_user_t);

struct rmaps_lama_node_mppr_t {
    int max;
    int cur;
};
typedef struct rmaps_lama_node_mppr_t rmaps_lama_node_mppr_t;

rmaps_lama_level_type_t lama_type_str_to_enum(char *param);
char * lama_type_enum_to_str(rmaps_lama_level_type_t param);


/*********************************
 * Command Line Interface Parsing
 *********************************/
/*
 * User defined command line interface (CLI) arguments
 */
extern char * rmaps_lama_cmd_map;
extern char * rmaps_lama_cmd_bind;
extern char * rmaps_lama_cmd_mppr;
extern char * rmaps_lama_cmd_ordering;
extern bool rmaps_lama_timing_enabled;
extern bool rmaps_lama_can_oversubscribe;
extern bool rmaps_lama_am_oversubscribing;

/*
 * Internal representations of command line arguments
 */
extern int lama_mapping_num_layouts;
extern rmaps_lama_level_type_t *lama_mapping_layout;

extern rmaps_lama_level_type_t  lama_binding_level;

extern rmaps_lama_level_info_t *lama_mppr_levels;
extern int lama_mppr_num_levels;

/*
 * Homogeneous system optimization
 */
extern bool lama_mppr_max_tree_homogeneous_system;

/*
 * Maximum length of digits in CLI
 */
#define MAX_BIND_DIGIT_LEN 4

int rmaps_lama_process_alias_params(orte_job_t *jdata);

int rmaps_lama_parse_mapping(char *layout,
                             rmaps_lama_level_type_t **layout_types,
                             rmaps_lama_level_type_t **layout_types_sorted,
                             int *num_types);
int rmaps_lama_parse_binding(char *layout,
                             rmaps_lama_level_type_t *binding_level,
                             int *num_types);
int rmaps_lama_parse_mppr(char *layout,
                          rmaps_lama_level_info_t **mppr_levels,
                          int *num_types);
int rmaps_lama_parse_ordering(char *layout,
                              rmaps_lama_order_type_t *order);

bool rmaps_lama_ok_to_prune_level(rmaps_lama_level_type_t level);

/*********************************
 * Max Tree Structure
 *********************************/
struct rmaps_lama_max_tree_item_t {
    opal_tree_item_t tree_element;

    rmaps_lama_level_type_t type;
};
typedef struct rmaps_lama_max_tree_item_t rmaps_lama_max_tree_item_t;


/*
 * Union all topologies into the max tree
 */
int rmaps_lama_build_max_tree(orte_job_t *jdata, opal_list_t *node_list,
                              opal_tree_t * max_tree, bool *is_homogeneous);

/*
 * Find a matching subtree
 */
hwloc_obj_t * rmaps_lama_find_nth_subtree_match(hwloc_topology_t hwloc_topo,
                                                hwloc_obj_t parent_obj,
                                                int nth,
                                                rmaps_lama_level_type_t lama_key);
hwloc_obj_t * rmaps_lama_find_parent(hwloc_topology_t hwloc_topo,
                                     hwloc_obj_t *child_obj,
                                     rmaps_lama_level_type_t lama_key);

/*
 * Create Empty Tree
 */
opal_tree_t * rmaps_lama_create_empty_max_tree(void);

/*
 * Pretty Print
 */
void   rmaps_lama_max_tree_pretty_print_tree(opal_tree_t *tree);

END_C_DECLS

#endif /* ORTE_RMAPS_LAMA_H */
