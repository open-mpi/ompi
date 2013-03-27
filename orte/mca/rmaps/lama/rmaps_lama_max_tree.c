/*
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2012      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * Max Tree Support Functions
 *
 */
#include "rmaps_lama.h"

#include "orte/util/show_help.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

/*********************************
 * Max Tree Construction
 *********************************/
/*
 * Convert an hwloc tree to an opal_tree
 */
static int rmaps_lama_convert_hwloc_tree_to_opal_tree(opal_tree_t *opal_tree,
                                                      hwloc_topology_t *hwloc_topo);

/*
 * Convert an hwloc subtree to an opal subtree
 */
static int rmaps_lama_convert_hwloc_subtree(hwloc_obj_t obj,
                                            opal_tree_item_t *parent_item);

/*
 * Convert LAMA key to HWLOC key/depth
 */
static int rmaps_lama_convert_lama_key_to_hwloc_key(rmaps_lama_level_type_t lama_key,
                                                    hwloc_obj_type_t *hwloc_key, int *depth);

/*
 * Convert HWLOC key/depth to LAMA key
 */
static int rmaps_lama_convert_hwloc_key_to_lama_key(hwloc_obj_type_t hwloc_key, int depth,
                                                    rmaps_lama_level_type_t *lama_key);

/*
 * Compare two HWLOC topologies for similar structure
 */
static int rmaps_lama_hwloc_compare_topos(hwloc_topology_t *left, hwloc_topology_t *right);
static int rmaps_lama_hwloc_compare_subtrees(hwloc_obj_t left, hwloc_obj_t right);

/*
 * Merge two opal_trees
 */
static int rmaps_lama_merge_trees(opal_tree_t *src_tree, opal_tree_t *into_tree,
                                  opal_tree_item_t *src_parent, opal_tree_item_t *into_parent);

/*
 * Prune the max tree to just those levels specified
 */
static int rmaps_lama_prune_max_tree(opal_tree_t *max_tree, opal_tree_item_t *parent_item);

/*
 * Annotate the hwloc tree for MPPR accounting
 */
static int rmaps_lama_annotate_node_for_mppr(orte_node_t *node, hwloc_obj_t obj);

/*
 * Access the MPPR for the specified key
 */
static int rmaps_lama_get_mppr_for_key(orte_node_t *node, rmaps_lama_level_type_t lama_key);

/*
 * Recursive core of nth_subtree_match
 */
static int rmaps_lama_find_nth_subtree_match_core(hwloc_topology_t hwloc_topo,
                                                  hwloc_obj_t parent_obj,
                                                  int nth,
                                                  int *num_found,
                                                  hwloc_obj_type_t hwloc_key,
                                                  int depth,
                                                  hwloc_obj_t *cur_child);

static void rmaps_lama_max_tree_item_construct(rmaps_lama_max_tree_item_t *item)
{
    item->type = LAMA_LEVEL_UNKNOWN;
}


/*********************************
 * Max Tree Accessors/Functions
 *********************************/
OBJ_CLASS_INSTANCE(rmaps_lama_max_tree_item_t,
                   opal_tree_item_t,
                   rmaps_lama_max_tree_item_construct, NULL);

static int lama_max_tree_comp(opal_tree_item_t *item, void *key);
static int lama_max_tree_serialize(opal_tree_item_t *item, opal_buffer_t *buffer);
static int lama_max_tree_deserialize(opal_buffer_t *buffer, opal_tree_item_t **item);
static void * lama_max_tree_get_key(opal_tree_item_t *item);


/*********************************
 * Max Tree Pretty Print
 *********************************/
static char * rmaps_lama_max_tree_pretty_print_subtree_element_get(opal_tree_t *tree,
                                                                   opal_tree_item_t *parent,
                                                                   int level);
static void pretty_print_subtree(opal_tree_t *tree, opal_tree_item_t *parent, int level);
static void pretty_print_subtree_element(opal_tree_t *tree, opal_tree_item_t *parent, int level);


/*********************************
 * Function Defintions
 *********************************/
int rmaps_lama_build_max_tree(orte_job_t *jdata, opal_list_t *node_list,
                              opal_tree_t * max_tree, bool *is_homogeneous)
{
    int ret;
    opal_tree_t *tmp_tree = NULL;
    hwloc_topology_t topo, *last_topo = NULL;
    orte_node_t *cur_node = NULL;

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ----- Building the Max Tree...");
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");

    /*
     * Assume homogeneous system, unless otherwise noted
     */
    *is_homogeneous = true;

    /*
     * Process all other unique trees from remote daemons who are in
     * this allocation
     */
    for(cur_node  = (orte_node_t*)opal_list_get_first(node_list);
        cur_node != (orte_node_t*)opal_list_get_end(node_list);
        cur_node  = (orte_node_t*)opal_list_get_next(cur_node) ) {
        if (NULL == (topo = cur_node->topology)) {
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ----- No Tree Available: %s (skipping)", cur_node->name);
        }

        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: ----- Converting Remote Tree: %s", cur_node->name);

        /*
         * Convert to opal_tree
         */
        tmp_tree = rmaps_lama_create_empty_max_tree();
        rmaps_lama_convert_hwloc_tree_to_opal_tree(tmp_tree, &topo);
        if( 11 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
            rmaps_lama_max_tree_pretty_print_tree(tmp_tree);
        }

        /*
         * Compare the current and last topologies if we are still considering
         * this max tree to represent a homogeneous system.
         */
        if( *is_homogeneous ) {
            if( NULL == last_topo ) {
                last_topo = &topo;
            } else {
                if( 0 != rmaps_lama_hwloc_compare_topos(last_topo, &topo) ) {
                    *is_homogeneous = false;
                }
            }
        }

        /*
         * Prune the input tree so that is only contains levels that the user
         * asked for.
         */
        if( 11 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ---------------------------------");
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ----- Pruning input Tree...");
        }
        if( ORTE_SUCCESS != (ret = rmaps_lama_prune_max_tree(tmp_tree, opal_tree_get_root(tmp_tree))) ) {
            return ret;
        }
        if( 11 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ----- Input Tree... - Post Prune");
            rmaps_lama_max_tree_pretty_print_tree(tmp_tree);
        }

        /*
         * Merge into max_tree
         */
        if( opal_tree_is_empty(max_tree) ) {
            opal_tree_dup(tmp_tree, max_tree);
        } else {
            if( ORTE_SUCCESS != (ret = rmaps_lama_merge_trees(tmp_tree,
                                                              max_tree,
                                                              opal_tree_get_root(tmp_tree),
                                                              opal_tree_get_root(max_tree) ))) {
                return ret;
            }
        }

        /*
         * Release and move on...
         */
        OBJ_RELEASE(tmp_tree);
        tmp_tree = NULL;
    }


    /*
     * Fill out the MPPR accounting information for each node
     */
    for(cur_node  = (orte_node_t*)opal_list_get_first(node_list);
        cur_node != (orte_node_t*)opal_list_get_end(node_list);
        cur_node  = (orte_node_t*)opal_list_get_next(cur_node) ) {
        if( ORTE_SUCCESS != (ret = rmaps_lama_annotate_node_for_mppr(cur_node,
                                                                     hwloc_get_obj_by_depth(cur_node->topology, 0, 0))) ) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /*
     * JJH: NEEDS TESTING
     * Note: This check is in place, but not used at the moment due to lack of
     * system availability. Pending system availability and further testing,
     * just assume heterogeneous.
     */
    *is_homogeneous = false;

    /*
     * Display the final Max Tree
     */
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ----- Final Max Tree... - %s system",
                        (*is_homogeneous ? "Homogeneous" : "Heterogeneous") );
    if( 11 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        rmaps_lama_max_tree_pretty_print_tree(max_tree);
    }
    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: ---------------------------------");

    return ORTE_SUCCESS;
}

static int rmaps_lama_convert_hwloc_tree_to_opal_tree(opal_tree_t *opal_tree, hwloc_topology_t *hwloc_topo)
{
    hwloc_obj_t topo_root;

    if( 15 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        opal_output_verbose(15, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: ----- Converting Topology:");
        /* opal_dss.dump(0, opal_hwloc_topology, OPAL_HWLOC_TOPO); */
        opal_dss.dump(0, *hwloc_topo, OPAL_HWLOC_TOPO);
    }

    topo_root = hwloc_get_root_obj(*hwloc_topo);

    rmaps_lama_convert_hwloc_subtree(topo_root, 
                                     opal_tree_get_root(opal_tree));

    return ORTE_SUCCESS;
}

static int rmaps_lama_convert_hwloc_subtree(hwloc_obj_t obj, 
                                            opal_tree_item_t *parent_item)
{
    rmaps_lama_max_tree_item_t *max_tree_item = NULL;
    char * key_child_str = NULL;
    char * key_parent_str = NULL;

    while (obj) {
        /*
         * Create new tree item
         */
        max_tree_item = OBJ_NEW(rmaps_lama_max_tree_item_t);

        /*
         * Convert the HWLOC object to the LAMA key
         */
        rmaps_lama_convert_hwloc_key_to_lama_key(obj->type, 
                                                 obj->attr->cache.depth,
                                                 &(max_tree_item->type));

        /*
         * Append tree item to parent.  Unless it is the same as the
         * parent (L1 instruction vs data cache).  JJH: Newer versions
         * of hwloc can differentiate from the obj->attr->cache.type.
         */
        if( NULL != obj->parent &&
            obj->parent->type == obj->type && 
            obj->parent->attr->cache.depth == obj->attr->cache.depth ) {
            key_child_str  = lama_type_enum_to_str(max_tree_item->type);
            key_parent_str = lama_type_enum_to_str(((rmaps_lama_max_tree_item_t*)parent_item)->type);
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Warning: Identical level detected: "
                                "Child [%s] vs Parent [%s]",
                                key_child_str, key_parent_str);
            free(key_child_str);
            free(key_parent_str);

            /*
             * Add descendants if they exist
             */
            if (obj->first_child) {
                rmaps_lama_convert_hwloc_subtree(obj->first_child, 
                                                 parent_item);
            }
        } else {
            opal_tree_add_child(parent_item, &max_tree_item->tree_element);

            /*
             * Add descendants if they exist
             */
            if (obj->first_child) {
                rmaps_lama_convert_hwloc_subtree(obj->first_child, 
                                                 &max_tree_item->tree_element);
            }
        }

        /*
         * Advance to next sibling
         */
        obj = obj->next_sibling;
    }

    return ORTE_SUCCESS;
}

static int rmaps_lama_annotate_node_for_mppr(orte_node_t *node, hwloc_obj_t obj)
{
    rmaps_lama_hwloc_user_t *hwloc_userdata = NULL;
    rmaps_lama_node_mppr_t *mppr_accounting = NULL;
    rmaps_lama_level_type_t lama_key;
    opal_hwloc_topo_data_t *opal_hwloc_topo = NULL;
    int i;

    /*
     * Attach our user pointer to the topology, if it is not already there.
     * We will fill it in as needed later.
     *
     * Note: opal/mca/hwloc/base/hwloc_base_util.c attaches their own object
     * to the userdata. There is a pointer in that structure we can use without
     * interfering with what OPAL is trying to do.
     */
    if( NULL == obj->userdata ) {
        /* Some objects may not have topo data associated with them
         * JJH: This is memory leak :/ Fix.
         */
        obj->userdata = (void*)OBJ_NEW(opal_hwloc_topo_data_t);
    }
    if( NULL != obj->userdata ) {
        opal_hwloc_topo = (opal_hwloc_topo_data_t*)(obj->userdata);

        if( NULL == opal_hwloc_topo->userdata ) {
            hwloc_userdata = OBJ_NEW(rmaps_lama_hwloc_user_t);
            opal_hwloc_topo->userdata  = hwloc_userdata;
        } else {
            hwloc_userdata = (rmaps_lama_hwloc_user_t*)(opal_hwloc_topo->userdata);
        }
    }


    /*
     * Add node information if it is not already there
     */
    mppr_accounting = (rmaps_lama_node_mppr_t*)opal_pointer_array_get_item(hwloc_userdata->node_mppr, node->index);
    if( NULL == mppr_accounting ) {
        /*
         * Add MPPR accounting for this node associated with this object
         */
        rmaps_lama_convert_hwloc_key_to_lama_key(obj->type, obj->attr->cache.depth, &lama_key);

        mppr_accounting = (rmaps_lama_node_mppr_t*)malloc(sizeof(rmaps_lama_node_mppr_t));
        mppr_accounting->max = rmaps_lama_get_mppr_for_key(node, lama_key);
        mppr_accounting->cur =  0;

        opal_pointer_array_set_item(hwloc_userdata->node_mppr, node->index, mppr_accounting);
    }


    /*
     * Decend tree
     */
    for(i = 0; i < (int)obj->arity; ++i ) {
        rmaps_lama_annotate_node_for_mppr(node,
                                          obj->children[i]);
    }

    return ORTE_SUCCESS;
}

static int rmaps_lama_get_mppr_for_key(orte_node_t *node, rmaps_lama_level_type_t lama_key)
{
    int i;

    for( i = 0; i < lama_mppr_num_levels; ++i ) {
        if( lama_key == lama_mppr_levels[i].type ) {
            return lama_mppr_levels[i].max_resources;
        }
    }

    return -1;
}

static int rmaps_lama_convert_lama_key_to_hwloc_key(rmaps_lama_level_type_t lama_key, hwloc_obj_type_t *hwloc_key, int *depth)
{
    *depth = 0;

    switch(lama_key) {
    case LAMA_LEVEL_MACHINE:
        *hwloc_key = HWLOC_OBJ_MACHINE;
        break;
    /* Note: HWLOC does not support boards */
#if 0
    case LAMA_LEVEL_BOARD:
        *hwloc_key = HWLOC_OBJ_MACHINE;
        break;
#endif
    case LAMA_LEVEL_SOCKET:
        *hwloc_key = HWLOC_OBJ_SOCKET;
        break;
    case LAMA_LEVEL_CORE:
        *hwloc_key = HWLOC_OBJ_CORE;
        break;
    case LAMA_LEVEL_PU:
        *hwloc_key = HWLOC_OBJ_PU;
        break;
    case LAMA_LEVEL_CACHE_L1:
        *hwloc_key = HWLOC_OBJ_CACHE;
        *depth = 1;
        break;
    case LAMA_LEVEL_CACHE_L2:
        *hwloc_key = HWLOC_OBJ_CACHE;
        *depth = 2;
        break;
    case LAMA_LEVEL_CACHE_L3:
        *hwloc_key = HWLOC_OBJ_CACHE;
        *depth = 3;
        break;
    case LAMA_LEVEL_NUMA:
        *hwloc_key = HWLOC_OBJ_NODE;
        break;
    default:
        *hwloc_key = HWLOC_OBJ_TYPE_MAX;
        break;
    }

    return ORTE_SUCCESS;
}

static int rmaps_lama_convert_hwloc_key_to_lama_key(hwloc_obj_type_t hwloc_key, int depth, rmaps_lama_level_type_t *lama_key)
{
    switch(hwloc_key) {
    case HWLOC_OBJ_MACHINE:
        *lama_key = LAMA_LEVEL_MACHINE;
        break;
    /* Node: HWLOC does not support boards */
#if 0
    case HWLOC_OBJ_BOARD:
        *lama_key = LAMA_LEVEL_BOARD;
        break;
#endif
    case HWLOC_OBJ_SOCKET:
        *lama_key = LAMA_LEVEL_SOCKET;
        break;
    case HWLOC_OBJ_CORE:
        *lama_key = LAMA_LEVEL_CORE;
        break;
    case HWLOC_OBJ_PU:
        *lama_key = LAMA_LEVEL_PU;
        break;
    case HWLOC_OBJ_CACHE:
        if( 1 == depth ) {
            *lama_key = LAMA_LEVEL_CACHE_L1;
        }
        else if( 2 == depth ) {
            *lama_key = LAMA_LEVEL_CACHE_L2;
        }
        else if( 3 == depth ) {
            *lama_key = LAMA_LEVEL_CACHE_L3;
        }
        else {
            *lama_key = LAMA_LEVEL_UNKNOWN;
        }
        break;
    case HWLOC_OBJ_NODE:
        *lama_key = LAMA_LEVEL_NUMA;
        break;
    default:
        *lama_key = LAMA_LEVEL_UNKNOWN;
        break;
    }

    return ORTE_SUCCESS;
}

static int rmaps_lama_hwloc_compare_topos(hwloc_topology_t *left, hwloc_topology_t *right)
{
    hwloc_obj_t left_root;
    hwloc_obj_t right_root;

    /*
     * Note: I hope that there is a 'better' way of doing this natively with
     * HWLOC, but it is not obvious if they have the ability to compare
     * topologies. So do a depth first comparison of the trees.
     * You may be able to use the below:
     *   OPAL_EQUAL != opal_dss.compare(*last_topo, topo, OPAL_HWLOC_TOPO);
     */

    left_root = hwloc_get_obj_by_depth(*left, 0, 0);
    right_root = hwloc_get_obj_by_depth(*right, 0, 0);

    return rmaps_lama_hwloc_compare_subtrees(left_root, right_root);
}

static int rmaps_lama_hwloc_compare_subtrees(hwloc_obj_t left, hwloc_obj_t right)
{
    int i, ret;

    /*
     * Check Types
     */
    if( 0 != (ret = hwloc_compare_types(left->type, right->type)) ) {
        return ret;
    }

    /*
     * Check 'arity' at this level
     */
    if( left->arity > right->arity ) {
        return -1;
    }
    else if( left->arity < right->arity ) {
        return 1;
    }

    /*
     * Check all subtrees
     */
    for(i = 0; i < (int)left->arity; ++i ) {
        if( 0 != (ret = rmaps_lama_hwloc_compare_subtrees(left->children[i],
                                                          right->children[i])) ) {
            return ret;
        }
    }

    /*
     * Subtree is the same if we get here
     */
    return 0;
}

static int rmaps_lama_merge_trees(opal_tree_t *src_tree, opal_tree_t *max_tree,
                                  opal_tree_item_t *src_parent, opal_tree_item_t *max_parent)
{
    int ret, exit_status = ORTE_SUCCESS;
    rmaps_lama_level_type_t *key_src, *key_max;
    opal_tree_item_t *child_item = NULL, *max_grandparent = NULL;
    opal_tree_item_t *max_child_item = NULL;
    int num_max, num_src;
    int i;
    char *key_src_str = NULL;
    char *key_max_str = NULL;
#if 1
    char *str = NULL;
#endif

    /*
     * Basecase
     */
    if( NULL == src_parent ) {
        return ORTE_SUCCESS;
    }

    key_src = (rmaps_lama_level_type_t*)src_tree->get_key(src_parent);
    key_max = (rmaps_lama_level_type_t*)max_tree->get_key(max_parent);

    key_src_str = lama_type_enum_to_str(*key_src);
    key_max_str = lama_type_enum_to_str(*key_max);

    if( 15 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
        opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                            "mca:rmaps:lama: CHECK: Merge Trees: Keys Src (%2d - %s) vs Max (%2d - %s)",
                            *key_src, key_src_str, *key_max, key_max_str);
    }

    /*
     * Make sure keys at this level match.
     *
     * JJH: Give up if they do not match.
     * JJH: We should pick a victim and prune from the tree
     * JJH: preferably from the 'native' tree.
     */
    if( 0 != max_tree->comp(max_parent, src_tree->get_key(src_parent)) ) {
        /*
         * If the source conflicts due to cache, iterate to children to find a match.
         * JJH: Double check this for different heterogenous systems
         */
        if( LAMA_LEVEL_CACHE_L3 == *key_src ||
            LAMA_LEVEL_CACHE_L2 == *key_src ||
            LAMA_LEVEL_CACHE_L1 == *key_src ||
            LAMA_LEVEL_NUMA     == *key_src ) {
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Warning: Merge Trees: "
                                "Src with Conflicting Memory Hierarchy [Src (%2d - %s) vs Max (%2d - %s)]",
                                *key_src, key_src_str, *key_max, key_max_str);

            /*
             * If we are pruning a cache level, then check to make sure it is
             * not important to the process layout.
             */
            if( !rmaps_lama_ok_to_prune_level(*key_src) ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "orte-rmaps-lama:merge-conflict-bad-prune-src",
                               true,
                               key_src_str,
                               (NULL == rmaps_lama_cmd_map      ? "[Not Provided]" : rmaps_lama_cmd_map),
                               (NULL == rmaps_lama_cmd_bind     ? "[Not Provided]" : rmaps_lama_cmd_bind),
                               (NULL == rmaps_lama_cmd_mppr     ? "[Not Provided]" : rmaps_lama_cmd_mppr),
                               (NULL == rmaps_lama_cmd_ordering ? "[Not Provided]" : rmaps_lama_cmd_ordering));
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            /*
             * If the number of children at this pruned level was larger than
             * the max tree arity at this level, then duplicate the max_tree
             * element the approprate number of times
             */
            max_grandparent = opal_tree_get_parent(max_parent);
            num_max = opal_tree_num_children(max_grandparent);
            num_src = opal_tree_num_children(src_parent);

            for(i = 0; i < (num_src - num_max); ++i ) {
#if 1
                str = rmaps_lama_max_tree_pretty_print_subtree_element_get(max_tree, max_parent, 0);
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "mca:rmaps:lama: Merge: Appending child %s - post prune",
                                    str);
                free(str);
#endif
                /* Duplicate max child subtree */
                opal_tree_copy_subtree(max_tree, max_parent, max_tree, max_grandparent);
            }

            /*
             * Iterate to children, until we find a match
             */
            for(child_item  = opal_tree_get_first_child(src_parent);
                child_item != NULL;
                child_item  = opal_tree_get_next_sibling(child_item) ) {

                if( ORTE_SUCCESS != (ret = rmaps_lama_merge_trees(src_tree,
                                                                  max_tree,
                                                                  child_item,
                                                                  max_parent)) ) {
                    exit_status =  ret;
                    goto cleanup;
                }
            }

            exit_status = ORTE_SUCCESS;
            goto cleanup;
        }
        /*
         * If the max tree conflicts due to cache, then we need to prune the
         * max tree until it matches.
         * JJH: If we are pruning a level of the hierarchy then make sure we
         * JJH: don't need it for the process layout.
         */
        else if( LAMA_LEVEL_CACHE_L3 == *key_max ||
                 LAMA_LEVEL_CACHE_L2 == *key_max ||
                 LAMA_LEVEL_CACHE_L1 == *key_max ||
                 LAMA_LEVEL_NUMA     == *key_max ) {
            opal_output_verbose(10, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: Warning: Merge Trees: "
                                "Max with Conflicting Memory Hierarchy [Src (%2d - %s) vs Max (%2d - %s)]",
                                *key_src, key_src_str, *key_max, key_max_str);

            /*
             * If we are pruning a cache level, then check to make sure it is
             * not important to the process layout.
             */
            if( !rmaps_lama_ok_to_prune_level(*key_max) ) {
                orte_show_help("help-orte-rmaps-lama.txt",
                               "orte-rmaps-lama:merge-conflict-bad-prune-src",
                               true,
                               key_max_str,
                               (NULL == rmaps_lama_cmd_map      ? "[Not Provided]" : rmaps_lama_cmd_map),
                               (NULL == rmaps_lama_cmd_bind     ? "[Not Provided]" : rmaps_lama_cmd_bind),
                               (NULL == rmaps_lama_cmd_mppr     ? "[Not Provided]" : rmaps_lama_cmd_mppr),
                               (NULL == rmaps_lama_cmd_ordering ? "[Not Provided]" : rmaps_lama_cmd_ordering));
                exit_status = ORTE_ERROR;
                goto cleanup;
            }

            max_child_item = opal_tree_get_first_child(max_parent);
            /* Prune parent */
            opal_tree_remove_item(max_tree, max_parent);

            /* Try again with child */
            exit_status = rmaps_lama_merge_trees(src_tree,
                                                 max_tree,
                                                 src_parent,
                                                 max_child_item);
            goto cleanup;
        }

        /*
         * If we cannot resolve it, give up.
         */
        opal_output(0, "mca:rmaps:lama: Error: Merge Trees: "
                    "Different Keys Src (%2d - %s) vs Max (%2d - %s) - Do not know how to resolve - give up!",
                    *key_src, key_src_str, *key_max, key_max_str);

        exit_status = ORTE_ERROR;
        goto cleanup;
    }

    num_max = opal_tree_num_children(max_parent);
    num_src = opal_tree_num_children(src_parent);

    /*
     * If the 'native' tree has more children than the 'max' tree.
     * Add the missing children to the 'max' tree.
     */
    if( num_max < num_src ) {
        i = 0;
        for(child_item  = opal_tree_get_first_child(src_parent);
            child_item != NULL;
            child_item  = opal_tree_get_next_sibling(child_item)) {
            if(i >= num_max ) {
#if 1
                str = rmaps_lama_max_tree_pretty_print_subtree_element_get(src_tree, child_item, 0);
                opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                    "mca:rmaps:lama: Merge: Appending child %s",
                                    str);
                free(str);
#endif
                /* Add child's subtree to max */
                opal_tree_copy_subtree(src_tree, child_item, max_tree, max_parent);
            }
            ++i;
        }
    }

    /*
     * Recursively search all children of 'native' tree.
     *
     * Note: Only need to add the children to the 'left-most' branch of the
     * 'max' tree since that is the only branch that is searched during mapping.
     * But do the whole thing for good measure. 
     */
    for( child_item  = opal_tree_get_first_child(src_parent),
         max_child_item  = opal_tree_get_first_child(max_parent);
         child_item != NULL;
         child_item  = opal_tree_get_next_sibling(child_item),
         max_child_item  = opal_tree_get_next_sibling(max_child_item) ) {

        if( ORTE_SUCCESS != (ret = rmaps_lama_merge_trees(src_tree,
                                                          max_tree,
                                                          child_item,
                                                          max_child_item)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

 cleanup:
    if( NULL != key_src_str ) {
        free(key_src_str);
        key_src_str = NULL;
    }

    if( NULL != key_max_str ) {
        free(key_max_str);
        key_max_str = NULL;
    }

    return exit_status;
}

static int rmaps_lama_prune_max_tree(opal_tree_t *max_tree, opal_tree_item_t *parent_item)
{
    int ret;
    opal_tree_item_t *child_item = NULL, *next_item;
    int i;
    bool found;
    rmaps_lama_level_type_t *key_max;
    char *tmp_str = NULL;

    /*
     * Basecase
     */
    if( NULL == parent_item ) {
        return ORTE_SUCCESS;
    }

    /*
     * Recursively decend tree - Depth first
     * Basecase: No children, loop skipped
     */
    child_item  = opal_tree_get_first_child(parent_item);
    while( child_item != NULL ) {
        /* Do this before the recursive call, since it might remove this
         * child so we need to preserve a pointer to the next sibling.
         */
        next_item  = opal_tree_get_next_sibling(child_item);

        if( ORTE_SUCCESS != (ret = rmaps_lama_prune_max_tree(max_tree,
                                                             child_item)) ) {
            return ret;
        }

        child_item = next_item;
    }

    key_max = (rmaps_lama_level_type_t*)max_tree->get_key(parent_item);

    /*
     * Check keys against the user supplied layout
     */
    found = false;
    for(i = 0; i < lama_mapping_num_layouts; ++i ) {
        if( 0 == max_tree->comp(parent_item, &lama_mapping_layout[i]) ) {
            found = true;
            break;
        }
    }

    if( !found ) {
        if( 15 <= opal_output_get_verbosity(orte_rmaps_base_framework.framework_output) ) {
            tmp_str = lama_type_enum_to_str(*key_max);
            opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                                "mca:rmaps:lama: ----- Before pruning %s",
                                tmp_str);
            free(tmp_str);
            rmaps_lama_max_tree_pretty_print_tree(max_tree);
        }

        opal_tree_remove_item(max_tree, parent_item);

        return ORTE_SUCCESS;
    }

    return ORTE_SUCCESS;
}


hwloc_obj_t * rmaps_lama_find_nth_subtree_match(hwloc_topology_t hwloc_topo,
                                                hwloc_obj_t parent_obj,
                                                int nth,
                                                rmaps_lama_level_type_t lama_key)
{
    hwloc_obj_t *cur_child = NULL;
    hwloc_obj_type_t hwloc_key;
    int depth;
    int num_found;
#if 0
    char str[128];
#endif

    cur_child = (hwloc_obj_t*)malloc(sizeof(hwloc_obj_t) * 1);

    /*
     * Convert LAMA key to HWLOC key
     */
    rmaps_lama_convert_lama_key_to_hwloc_key(lama_key, &hwloc_key, &depth);

    /*
     * Decend tree looking for the n'th matching subtree
     */
    num_found = -1;
    rmaps_lama_find_nth_subtree_match_core(hwloc_topo,
                                           parent_obj,
                                           nth,
                                           &num_found,
                                           hwloc_key,
                                           depth,
                                           cur_child);

    /*
     * Check to see if we found it
     */
#if 0
    hwloc_obj_snprintf(str, sizeof(str), hwloc_topo, *cur_child, "#", 0);
    if( nth == num_found ) {
        printf("--> FOUND   : %-20s \t -- \t %2d of %2d\n", str, nth, num_found);
    }
    else {
        printf("--> MISSING : %-20s \t -- \t %2d of %2d\n", str, nth, num_found);
    }
#endif

    if( nth == num_found ) {
        return cur_child;
    }
    else {
        free(cur_child);
        return NULL;
    }
}

static int rmaps_lama_find_nth_subtree_match_core(hwloc_topology_t hwloc_topo,
                                                  hwloc_obj_t parent_obj,
                                                  int nth,
                                                  int *num_found,
                                                  hwloc_obj_type_t hwloc_key,
                                                  int depth,
                                                  hwloc_obj_t *cur_child)
{
    unsigned i;
    bool found = false;

#if 0
    {
        char str[128];
        hwloc_obj_snprintf(str, sizeof(str), hwloc_topo, parent_obj, "#", 0);
        printf("--> Checking -- %-20s \t -- \t %2d of %2d\n", str, nth, *num_found);
    }
#endif

    /*
     * Check if the keys match
     */
    if( hwloc_key == parent_obj->type ) {
        if( HWLOC_OBJ_CACHE == parent_obj->type &&
            depth == (int)parent_obj->attr->cache.depth ) {
            *num_found += 1;
            found = true;
        } else {
            *num_found += 1;
            found = true;
        }
    }

    /*
     * Basecase:
     * If we have found the correct item, return
     */
    if( nth == *num_found ) {
        *cur_child = parent_obj;
        return ORTE_SUCCESS;
    }

    /*
     * Do no go any deeper in the tree than we have to
     */
    if( !found ) {
        for(i = 0; i < parent_obj->arity; ++i ) {
            rmaps_lama_find_nth_subtree_match_core(hwloc_topo,
                                                   parent_obj->children[i],
                                                   nth,
                                                   num_found,
                                                   hwloc_key,
                                                   depth,
                                                   cur_child);
            if( nth == *num_found ) {
                return ORTE_SUCCESS;
            }
        }
    }

    return ORTE_SUCCESS;
}

hwloc_obj_t * rmaps_lama_find_parent(hwloc_topology_t hwloc_topo,
                                     hwloc_obj_t *child_obj,
                                     rmaps_lama_level_type_t lama_key)
{
    hwloc_obj_t *cur_parent = NULL;
    hwloc_obj_type_t hwloc_key;
    int depth;

    cur_parent = (hwloc_obj_t*)malloc(sizeof(hwloc_obj_t) * 1);

    /*
     * Convert LAMA key to HWLOC key
     */
    rmaps_lama_convert_lama_key_to_hwloc_key(lama_key, &hwloc_key, &depth);

    /*
     * Sanity check
     */
    if( hwloc_key == (*child_obj)->type ) {
        if( HWLOC_OBJ_CACHE == (*child_obj)->type &&
            depth == (int)(*child_obj)->attr->cache.depth ) {
            return child_obj;
        } else {
            return child_obj;
        }
    }

    /*
     * Accend tree to find mathing parent
     */
    *cur_parent = (*child_obj)->parent;
    while(NULL != *cur_parent ) {
        if( hwloc_key == (*cur_parent)->type ) {
            if( HWLOC_OBJ_CACHE == (*cur_parent)->type &&
                depth == (int)(*cur_parent)->attr->cache.depth ) {
                return cur_parent;
            } else {
                return cur_parent;
            }
        }

        *cur_parent = (*cur_parent)->parent;
    }

    free(cur_parent);
    return NULL;
}


/*********************************
 * Max Tree Structure Functions
 *********************************/
opal_tree_t * rmaps_lama_create_empty_max_tree(void)
{
    opal_tree_t *tmp_tree = NULL;

    tmp_tree = OBJ_NEW(opal_tree_t);
    opal_tree_init(tmp_tree,
                   &lama_max_tree_comp,
                   &lama_max_tree_serialize, 
                   &lama_max_tree_deserialize,
                   &lama_max_tree_get_key);

    return tmp_tree;
}

static int lama_max_tree_comp(opal_tree_item_t *item, void *key)
{
    if( ((rmaps_lama_max_tree_item_t *)item)->type == *((rmaps_lama_level_type_t *)key) ) {
        return 0;
    }

    return -1;
}

static int lama_max_tree_serialize(opal_tree_item_t *item, opal_buffer_t *buffer)
{
    opal_dss.pack(buffer, &(((rmaps_lama_max_tree_item_t *)item)->type), 1, OPAL_INT);

    return ORTE_SUCCESS;
}

static int lama_max_tree_deserialize(opal_buffer_t *buffer, opal_tree_item_t **item)
{
    rmaps_lama_max_tree_item_t *element;
    orte_std_cntr_t n = 1;

    element = OBJ_NEW(rmaps_lama_max_tree_item_t);
    if( OPAL_SUCCESS == opal_dss.unpack(buffer, &(element->type), &n, OPAL_INT) ) {
        *item = (opal_tree_item_t*)element;
    } else {
        *item = NULL;
    }

    return ORTE_SUCCESS;
}

static void * lama_max_tree_get_key(opal_tree_item_t *item)
{
    return &(((rmaps_lama_max_tree_item_t *)item)->type);
}


/*********************************
 * Pretty Print Functions
 *********************************/
void rmaps_lama_max_tree_pretty_print_tree(opal_tree_t *tree)
{
    if( NULL == tree ) {
        return;
    }

    if( opal_tree_is_empty(tree) ) {
        return;
    }

    pretty_print_subtree(tree, opal_tree_get_root(tree), 0);

    return;
}

static char * rmaps_lama_max_tree_pretty_print_subtree_element_get(opal_tree_t *tree,
                                                                   opal_tree_item_t *parent,
                                                                   int level)
{
    char *element_str = NULL;
    char *spacer = NULL;
    char *label = NULL;
    rmaps_lama_level_type_t *type = NULL;
    int i;

    if( NULL == parent ) {
        return NULL;
    }

    spacer = (char *)malloc(sizeof(char) * (level+1));
    for(i = 0; i < level; ++i ) {
        spacer[i] = ' ';
    }
    spacer[level] = '\0';

    type = (rmaps_lama_level_type_t *)(tree->get_key(parent));
    label = lama_type_enum_to_str(*type);

    asprintf(&element_str, "%s[%s \t : %3d, %3d",
             spacer, label,
             parent->opal_tree_num_children, parent->opal_tree_num_ancestors);

    free(spacer);
    free(label);

    return element_str;
}

static void pretty_print_subtree(opal_tree_t *tree, opal_tree_item_t *parent, int level)
{
    opal_tree_item_t *child = NULL;

    if( NULL == parent ) {
        return;
    }

    /*
     * Display Self
     */
    pretty_print_subtree_element(tree, parent, level);

    /*
     * Depth-first display children
     * Basecase; If no children - return
     */
    level++;
    for(child  = opal_tree_get_first_child(parent);
        child != NULL;
        child  = opal_tree_get_next_sibling(child) ) {
        pretty_print_subtree(tree, child, level);
    }

    return;

}

static void pretty_print_subtree_element(opal_tree_t *tree, opal_tree_item_t *parent, int level)
{
    char *element_str = NULL;

    if( NULL == parent ) {
        return;
    }

    element_str = rmaps_lama_max_tree_pretty_print_subtree_element_get(tree, parent, level);

    opal_output_verbose(5, orte_rmaps_base_framework.framework_output,
                        "mca:rmaps:lama: Tree Element: %s",
                        element_str);

    free(element_str);

    return;
}
