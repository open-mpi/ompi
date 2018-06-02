/*
 * Copyright © 2016-2017 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <private/netloc.h>
#include <netloc.h>

typedef struct netloc_analysis_data_t {
    int level;
    void *userdata;
} netloc_analysis_data;


static int partition_topology_to_tleaf(netloc_topology_t *topology,
        int partition, int num_cores, netloc_arch_t *arch);
static netloc_arch_tree_t *tree_merge(netloc_arch_tree_t *main,
        netloc_arch_tree_t *sub);
static int netloc_arch_tree_destruct(netloc_arch_tree_t *tree);
static int netloc_arch_node_destruct(netloc_arch_node_t *arch_node);
static netloc_arch_node_t *netloc_arch_node_construct(void);

#define checked_fscanf(f, w, str, failed) \
    if (fscanf(f, " %1023s", w) != 1) { \
        fprintf(stderr, "Cannot read %s\n", str); \
        perror("fscanf"); \
        goto ERROR; \
    }


/* Complete the topology to have a complete balanced tree  */
void netloc_arch_tree_complete(netloc_arch_tree_t *tree, UT_array **down_degrees_by_level,
        int num_hosts, int **parch_idx)
{
    int num_levels = tree->num_levels;
    NETLOC_int *max_degrees = tree->degrees;

    /* Complete the tree by inserting nodes */
    for (int l = 0; l < num_levels-1; l++) { // from the root to the leaves
        int num_degrees = utarray_len(down_degrees_by_level[l]);
        int *degrees = (int *)down_degrees_by_level[l]->d;
        NETLOC_int max_degree = max_degrees[l];

        unsigned int down_level_idx = 0;
        UT_array *down_level_degrees = down_degrees_by_level[l+1];
        NETLOC_int down_level_max_degree = max_degrees[l+1];
        for (int d = 0; d < num_degrees; d++) {
            int degree = degrees[d];
            if (degree > 0) {
                down_level_idx += degree;
                if (degree < max_degree) {
                    int missing_degree = (degree-max_degree)*down_level_max_degree;
                    utarray_insert(down_level_degrees, &missing_degree, down_level_idx);
                    down_level_idx++;
                }
            } else {
                int missing_degree = degree*down_level_max_degree;
                utarray_insert(down_level_degrees, &missing_degree, down_level_idx);
                down_level_idx++;
            }
        }
    }

    /* Indices for the list of hosts, in the complete architecture */
    int num_degrees = utarray_len(down_degrees_by_level[num_levels-1]);
    int *degrees = (int *)down_degrees_by_level[num_levels-1]->d;
    NETLOC_int max_degree = max_degrees[num_levels-1];
    int ghost_idx = 0;
    int idx = 0;
    int *arch_idx = (int *)malloc(sizeof(int[num_hosts]));
    for (int d = 0; d < num_degrees; d++) {
        int degree = degrees[d];
        int diff;

        if (degree > 0) {
            diff = max_degree-degree;
        } else {
            diff = -degree;
        }

        for (int i = 0; i < degree; i++) {
            arch_idx[idx++] = ghost_idx++;
        }
        ghost_idx += diff;
    }
    *parch_idx = arch_idx;
}

NETLOC_int netloc_arch_tree_num_leaves(netloc_arch_tree_t *tree)
{
    NETLOC_int num_leaves = 1;
    for (int l = 0; l < tree->num_levels; l++) {
        num_leaves *= tree->degrees[l];
    }
    return num_leaves;
}

static int get_current_resources(int *pnum_nodes, char ***pnodes, int **pslot_idx,
        int **pslot_list, int **prank_list)
{
    char *filename = getenv("NETLOC_CURRENTSLOTS");
    char word[1024];
    char *end_word;
    int *slot_list = NULL;
    int *rank_list = NULL;
    int *slot_idx = NULL;
    char **nodes = NULL;

    if (!filename) {
        fprintf(stderr, "You need to set NETLOC_CURRENTSLOTS\n");
        return NETLOC_ERROR;
    }

    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("fopen");
        return NETLOC_ERROR;
    }

    checked_fscanf(file, word, "num_nodes", failed);

    int num_nodes;
    num_nodes = strtol(word, &end_word, 10);
    if (*word == '\0' || *end_word != '\0' || num_nodes <= 0) {
        fprintf(stderr, "Oups: incorrect number of nodes (%d) in \"%s\"\n",
                num_nodes, word);
        goto ERROR;
    }

    nodes = (char **)malloc(sizeof(char *[num_nodes]));
    for (int n = 0; n < num_nodes; n++) {
        checked_fscanf(file, word, "node", failed);
        nodes[n] = strdup(word);
    }

    slot_idx = (int *)malloc(sizeof(int[num_nodes+1]));
    slot_idx[0] = 0;
    for (int n = 0; n < num_nodes; n++) {
        checked_fscanf(file, word, "slot index", failed);

        int slot_index = strtol(word, &end_word, 10);
        if (*word == '\0' || *end_word != '\0' || num_nodes <= 0) {
            fprintf(stderr, "Oups: incorrect slot index (%d) in \"%s\"\n",
                    slot_index, word);
            goto ERROR;
        }
        slot_idx[n+1] = slot_idx[n]+slot_index;
    }

    slot_list = (int *)malloc(sizeof(int[slot_idx[num_nodes]]));
    rank_list = (int *)malloc(sizeof(int[slot_idx[num_nodes]]));
    for (int s = 0; s < slot_idx[num_nodes]; s++) {
        checked_fscanf(file, word, "slot number", failed);
        slot_list[s] = strtol(word, &end_word, 10);
        if (*word == '\0' || *end_word != '\0' || num_nodes <= 0) {
            fprintf(stderr, "Oups: incorrect slot number (%d) in \"%s\"\n",
                    slot_list[s], word);
            goto ERROR;
        }

        checked_fscanf(file, word, "rank number", failed);
        rank_list[s] = strtol(word, &end_word, 10);
        if (*word == '\0' || *end_word != '\0' || num_nodes <= 0) {
            fprintf(stderr, "Oups: incorrect rank number (%d) in \"%s\"\n",
                    rank_list[s], word);
            goto ERROR;
        }
    }

    *pnum_nodes = num_nodes;
    *pnodes = nodes;
    *pslot_idx = slot_idx;
    *pslot_list = slot_list;
    *prank_list = rank_list;

    fclose(file);

    return NETLOC_SUCCESS;

ERROR:
    fclose(file);
    free(nodes);
    free(slot_idx);
    free(slot_list);
    free(rank_list);
    return NETLOC_ERROR;
}

int netloc_arch_set_current_resources(netloc_arch_t *arch)
{
    int ret;
    int num_nodes;
    char **nodenames;
    int *slot_idx;
    int *slot_list;
    int *rank_list;

    ret = get_current_resources(&num_nodes, &nodenames, &slot_idx, &slot_list,
            &rank_list);

    if (ret != NETLOC_SUCCESS || num_nodes <= 0)
        assert(0); // XXX

    NETLOC_int *current_nodes = NULL;

    if (!arch->has_slots) {
        current_nodes = (NETLOC_int *) malloc(sizeof(NETLOC_int[num_nodes]));
    }

    netloc_arch_node_t **arch_node_list = (netloc_arch_node_t **)
        malloc(sizeof(netloc_arch_node_t *[num_nodes]));
    netloc_node_t **node_list = (netloc_node_t **)
        malloc(sizeof(netloc_node_t *[num_nodes]));
    for (int n = 0; n < num_nodes; n++) {
        netloc_arch_node_t *arch_node;
        HASH_FIND_STR(arch->nodes_by_name, nodenames[n], arch_node);
        if (!arch_node) {
            ret = NETLOC_ERROR;
            goto ERROR;
        }
        arch_node_list[n] = arch_node;
        node_list[n] = arch_node->node;
    }

    ret = netloc_topology_read_hwloc(arch->topology, num_nodes, node_list);
    if( NETLOC_SUCCESS != ret ) {
        goto ERROR;
    }

    int constant_num_slots = 0;
    for (int n = 0; n < num_nodes; n++) {
        netloc_arch_node_t *node = arch_node_list[n];

        ret = netloc_arch_node_get_hwloc_info(node);
        if (ret != NETLOC_SUCCESS)
            goto ERROR;


        if (!arch->has_slots) {
            current_nodes[n] = node->idx_in_topo;
        }

        int num_slots = slot_idx[n+1]-slot_idx[n];
        node->num_current_slots = num_slots;

        /* Nodes with different number of slots are not handled yet, because we
         * build the scotch architecture without taking account of the
         * available cores inside nodes, and Scotch is not able to weight the
         * nodes */
        if (!arch->has_slots) {
            if (constant_num_slots) {
                if (constant_num_slots != num_slots) {
                    fprintf(stderr, "Oups: the same number of cores by node is needed!\n");
                    assert(constant_num_slots == num_slots);
                }
            } else {
                constant_num_slots = num_slots;
            }
        }

        node->current_slots = (NETLOC_int *)
            malloc(sizeof(NETLOC_int[num_slots]));
        NETLOC_int num_leaves = netloc_arch_tree_num_leaves(node->slot_tree);
        node->slot_ranks = (int *)
            malloc(sizeof(int[num_leaves]));

        for (int s = slot_idx[n]; s < slot_idx[n+1]; s++) {
            int slot = slot_list[s];
            node->current_slots[s-slot_idx[n]] = node->slot_idx[slot];
            node->slot_ranks[node->slot_idx[slot]] = rank_list[s];
        }
    }

    if (!arch->has_slots) {
        arch->num_current_hosts = num_nodes;
        arch->current_hosts = current_nodes;
        arch->arch.global_tree = arch->arch.node_tree;

        /* Build nodes_by_idx */
        NETLOC_int tree_size = netloc_arch_tree_num_leaves(arch->arch.node_tree);
        netloc_arch_node_slot_t *nodes_by_idx = (netloc_arch_node_slot_t *)
            malloc(sizeof(netloc_arch_node_slot_t[tree_size]));
        for (int n = 0; n < num_nodes; n++) {
            netloc_arch_node_t *node = arch_node_list[n];
            nodes_by_idx[node->idx_in_topo].node = node;
            nodes_by_idx[node->idx_in_topo].slot = -1;
        }
        arch->node_slot_by_idx = nodes_by_idx;


    } else {
        int num_hosts = slot_idx[num_nodes];
        NETLOC_int *current_hosts = (NETLOC_int *)
            malloc(sizeof(NETLOC_int[num_hosts]));
        /* Add the slot trees to the node tree */

        /* Check that each slot tree has the same size */
        int slot_tree_size = 0;
        for (int n = 0; n < num_nodes; n++) {
            netloc_arch_node_t *node = arch_node_list[n];
            int current_size = netloc_arch_tree_num_leaves(node->slot_tree);
            if (!slot_tree_size) {
                slot_tree_size = current_size;
            } else {
                if (slot_tree_size != current_size) {
                    assert(0);
                }
            }
        }

        int current_host_idx = 0;
        int node_tree_size = netloc_arch_tree_num_leaves(arch->arch.node_tree);
        int global_tree_size = node_tree_size*slot_tree_size;
        netloc_arch_node_slot_t *nodes_by_idx = (netloc_arch_node_slot_t *)
            malloc(sizeof(netloc_arch_node_slot_t[global_tree_size]));
        for (int n = 0; n < num_nodes; n++) {
            netloc_arch_node_t *node = arch_node_list[n];
            for (int s = slot_idx[n]; s < slot_idx[n+1]; s++) {
                int slot_rank = s-slot_idx[n];
                int topo_idx = node->idx_in_topo*slot_tree_size +
                    node->slot_idx[slot_rank];
                nodes_by_idx[topo_idx].node = node;
                nodes_by_idx[topo_idx].slot = slot_rank;
                current_hosts[current_host_idx++] = topo_idx;
            }
        }
        arch->num_current_hosts = current_host_idx;
        arch->current_hosts = current_hosts;
        arch->node_slot_by_idx = nodes_by_idx;

        netloc_arch_tree_t *new_tree =
            tree_merge(arch->arch.node_tree, arch_node_list[0]->slot_tree);
        netloc_arch_tree_destruct(arch->arch.node_tree);
        arch->arch.global_tree = new_tree;
    }

ERROR:
    for (int n = 0; n < num_nodes; n++) {
        free(nodenames[n]);
    }
    free(nodenames);
    free(slot_idx);
    free(slot_list);
    free(rank_list);
    free(arch_node_list);
    free(node_list);

    if (ret == NETLOC_SUCCESS)
        return ret;

    free(current_nodes);
    return ret;
}

int netloc_arch_set_global_resources(netloc_arch_t *arch)
{
    int ret;
    NETLOC_int *current_nodes = NULL;
    int *slot_idx = NULL;

    int num_nodes =  HASH_COUNT(arch->nodes_by_name);
    if (!arch->has_slots) {
        current_nodes = (NETLOC_int *) malloc(sizeof(NETLOC_int[num_nodes]));
    }

    ret = netloc_topology_read_hwloc(arch->topology, 0, NULL);
    if( NETLOC_SUCCESS != ret ) {
        goto ERROR;
    }

    int constant_num_slots = 0;
    slot_idx = (int *)malloc(sizeof(int[num_nodes+1]));
    slot_idx[0] = 0;
    int current_idx = 0;
    netloc_arch_node_t *node, *node_tmp;
    HASH_ITER(hh, arch->nodes_by_name, node, node_tmp) {
        ret = netloc_arch_node_get_hwloc_info(node);
        if (ret != NETLOC_SUCCESS)
            goto ERROR;

        if (!arch->has_slots) {
            current_nodes[current_idx] = node->idx_in_topo;
        }
        current_idx++;

        int num_slots = node->num_slots;
        node->num_current_slots = num_slots;

        slot_idx[current_idx] = slot_idx[current_idx-1]+num_slots;

        /* Nodes with different number of slots are not handled yet, because we
         * build the scotch architecture without taking account of the
         * available cores inside nodes, and Scotch is not able to weight the
         * nodes */
        if (!arch->has_slots) {
            if (constant_num_slots) {
                if (constant_num_slots != num_slots) {
                    fprintf(stderr, "Oups: the same number of cores by node is needed!\n");
                    assert(constant_num_slots == num_slots);
                }
            } else {
                constant_num_slots = num_slots;
            }
        }
    }

    if (!arch->has_slots) {
        arch->num_current_hosts = num_nodes;
        arch->current_hosts = current_nodes;
        arch->arch.global_tree = arch->arch.node_tree;

        /* Build nodes_by_idx */
        int tree_size = netloc_arch_tree_num_leaves(arch->arch.node_tree);
        netloc_arch_node_slot_t *nodes_by_idx = (netloc_arch_node_slot_t *)
            malloc(sizeof(netloc_arch_node_slot_t[tree_size]));
        netloc_arch_node_t *node, *node_tmp;
        HASH_ITER(hh, arch->nodes_by_name, node, node_tmp) {
            nodes_by_idx[node->idx_in_topo].node = node;
            nodes_by_idx[node->idx_in_topo].slot = -1;
        }
        arch->node_slot_by_idx = nodes_by_idx;


    } else {
        int num_hosts = slot_idx[num_nodes];
        NETLOC_int *current_hosts = (NETLOC_int *)
            malloc(sizeof(NETLOC_int[num_hosts]));
        netloc_arch_node_t *node, *node_tmp;
        /* Add the slot trees to the node tree */

        /* Check that each slot tree has the same size */
        int slot_tree_size = 0;
        HASH_ITER(hh, arch->nodes_by_name, node, node_tmp) {
            int current_size = netloc_arch_tree_num_leaves(node->slot_tree);
            if (!slot_tree_size) {
                slot_tree_size = current_size;
            } else {
                if (slot_tree_size != current_size) {
                    assert(0);
                }
            }
        }

        int current_host_idx = 0;
        int node_tree_size = netloc_arch_tree_num_leaves(arch->arch.node_tree);
        int global_tree_size = node_tree_size*slot_tree_size;
        netloc_arch_node_slot_t *nodes_by_idx = (netloc_arch_node_slot_t *)
            malloc(sizeof(netloc_arch_node_slot_t[global_tree_size]));
        int n = 0;
        HASH_ITER(hh, arch->nodes_by_name, node, node_tmp) {
            for (int s = slot_idx[n]; s < slot_idx[n+1]; s++) {
                int slot_rank = s-slot_idx[n];
                int topo_idx = node->idx_in_topo*slot_tree_size +
                    node->slot_idx[slot_rank];
                nodes_by_idx[topo_idx].node = node;
                nodes_by_idx[topo_idx].slot = slot_rank;
                current_hosts[current_host_idx++] = topo_idx;
            }
            n++;
        }
        arch->num_current_hosts = current_host_idx;
        arch->current_hosts = current_hosts;
        arch->node_slot_by_idx = nodes_by_idx;

        netloc_arch_tree_t *new_tree =
            tree_merge(arch->arch.node_tree, arch->nodes_by_name->slot_tree);
        netloc_arch_tree_destruct(arch->arch.node_tree);
        arch->arch.global_tree = new_tree;
    }

ERROR:
    free(slot_idx);

    if (ret == NETLOC_SUCCESS)
        return ret;

    free(current_nodes);
    return ret;
}

netloc_arch_tree_t *tree_merge(netloc_arch_tree_t *main, netloc_arch_tree_t *sub)
{
    netloc_arch_tree_t *new_tree = (netloc_arch_tree_t *)
        malloc(sizeof(netloc_arch_tree_t));

    int num_levels = main->num_levels+sub->num_levels;
    new_tree->num_levels = num_levels;
    new_tree->degrees = (NETLOC_int *)malloc(sizeof(NETLOC_int[num_levels]));
    new_tree->cost = (NETLOC_int *)malloc(sizeof(NETLOC_int[num_levels]));

    memcpy(new_tree->degrees, main->degrees,
            main->num_levels*sizeof(*new_tree->degrees));
    memcpy(new_tree->degrees+main->num_levels, sub->degrees,
            sub->num_levels*sizeof(*new_tree->degrees));

    int out_coeff = 10;
    for (int l = 0; l < main->num_levels; l++) {
        new_tree->cost[l] = main->cost[l]*sub->cost[0]*out_coeff;
    }
    memcpy(new_tree->cost+main->num_levels, sub->cost,
            sub->num_levels*sizeof(*new_tree->cost));

    return new_tree;
}

static int netloc_arch_tree_destruct(netloc_arch_tree_t *tree)
{
    free(tree->cost);
    free(tree->degrees);
    free(tree);

    return NETLOC_SUCCESS;
}


int partition_topology_to_tleaf(netloc_topology_t *topology,
        int partition, int num_cores, netloc_arch_t *arch)
{
    int ret = 0;
    UT_array *nodes;
    utarray_new(nodes, &ut_ptr_icd);

    netloc_arch_tree_t *tree = (netloc_arch_tree_t *)
        malloc(sizeof(netloc_arch_tree_t));
    arch->arch.node_tree = tree;
    arch->type = NETLOC_ARCH_TREE;

    /* we build nodes from host list in the given partition
     * and we init all the analysis data */
    netloc_node_t *node, *node_tmp;
    netloc_topology_iter_nodes(topology, node, node_tmp) {
        if (!netloc_node_is_in_partition(node, partition))
            continue;
        void *userdata = node->userdata;
        node->userdata = (void *)malloc(sizeof(netloc_analysis_data));
        netloc_analysis_data *analysis_data = (netloc_analysis_data *)node->userdata;
        analysis_data->level = -1;
        analysis_data->userdata = userdata; 

        netloc_edge_t *edge, *edge_tmp;
        netloc_node_iter_edges(node, edge, edge_tmp) {
            void *userdata = edge->userdata;
            edge->userdata = (void *)malloc(sizeof(netloc_analysis_data));
            netloc_analysis_data *analysis_data = (netloc_analysis_data *)edge->userdata;
            analysis_data->level = -1;
            analysis_data->userdata = userdata; 
        }

        if (netloc_node_is_host(node)) {
            utarray_push_back(nodes, &node);
        }
    }

    /* We set the levels in the analysis data */
    /* Upward edges will have the level of the source node and downward edges
     * will have -1 as level */
    int num_levels = 0;
    netloc_node_t *current_node = /* pointer to one host node */
        *(void **)utarray_eltptr(nodes, 0);
    while (utarray_len(nodes)) {
        UT_array *new_nodes;
        utarray_new(new_nodes, &ut_ptr_icd);

        for (unsigned int n = 0; n < utarray_len(nodes); n++) {
            netloc_node_t *node = *(void **)utarray_eltptr(nodes, n);
            netloc_analysis_data *node_data = (netloc_analysis_data *)node->userdata;
            /* There is a problem, this is not a tree */
            if (node_data->level != -1 && node_data->level != num_levels) {
                utarray_free(new_nodes);
                ret = -1;
                goto end;
            }
            else {
                node_data->level = num_levels;
                netloc_edge_t *edge, *edge_tmp;
                netloc_node_iter_edges(node, edge, edge_tmp) {
                    if (!netloc_edge_is_in_partition(edge, partition))
                        continue;
                    netloc_analysis_data *edge_data = (netloc_analysis_data *)edge->userdata;

                    netloc_node_t *dest = edge->dest;
                    netloc_analysis_data *dest_data = (netloc_analysis_data *)dest->userdata;
                    /* If we are going back */
                    if (dest_data->level != -1 && dest_data->level < num_levels) {
                        continue;
                    }
                    else {
                        if (dest_data->level != num_levels) {
                            edge_data->level = num_levels;
                            utarray_push_back(new_nodes, &dest);
                        }
                    }
                }
            }
        }
        num_levels++;
        utarray_free(nodes);
        nodes = new_nodes;
    }

    /* We go though the tree to order the leaves  and find the tree
     * structure */
    UT_array *ordered_name_array = NULL;
    UT_array **down_degrees_by_level = NULL;
    NETLOC_int *max_down_degrees_by_level;

    utarray_new(ordered_name_array, &ut_ptr_icd);

    down_degrees_by_level = (UT_array **)malloc(num_levels*sizeof(UT_array *));
    for (int l = 0; l < num_levels; l++) {
        utarray_new(down_degrees_by_level[l], &ut_int_icd);
    }
    max_down_degrees_by_level = (NETLOC_int *)
        calloc(num_levels-1, sizeof(NETLOC_int));

    UT_array *down_edges = NULL;
    utarray_new(down_edges, &ut_ptr_icd);
    netloc_edge_t *up_edge = current_node->edges;
    utarray_push_back(ordered_name_array, &current_node);
    while (1) {
        if (utarray_len(down_edges)) {
            netloc_edge_t *down_edge = *(void **)utarray_back(down_edges);
            utarray_pop_back(down_edges);
            netloc_node_t *dest_node = down_edge->dest;
            if (netloc_node_is_host(dest_node)) {
                utarray_push_back(ordered_name_array, &dest_node);
            }
            else {
                netloc_edge_t *edge, *edge_tmp;
                int num_edges = 0;
                netloc_node_iter_edges(dest_node, edge, edge_tmp) {
                    if (!netloc_edge_is_in_partition(edge, partition))
                        continue;
                    netloc_analysis_data *edge_data = (netloc_analysis_data *)edge->userdata;
                    int edge_level = edge_data->level;
                    if (edge_level == -1) {
                        utarray_push_back(down_edges, &edge);
                        num_edges++;
                    }
                }
                int level = ((netloc_analysis_data *)dest_node->userdata)->level;
                utarray_push_back(down_degrees_by_level[num_levels-1-level], &num_edges);
                max_down_degrees_by_level[num_levels-1-level] =
                    max_down_degrees_by_level[num_levels-1-level] > num_edges ?
                    max_down_degrees_by_level[num_levels-1-level]: num_edges;
            }
        }
        else {
            netloc_edge_t *new_up_edge = NULL;
            if (!up_edge)
                break;

            netloc_node_t *up_node = up_edge->dest;
            netloc_edge_t *edge, *edge_tmp;
            int num_edges = 0;
            netloc_node_iter_edges(up_node, edge, edge_tmp) {
                if (!netloc_edge_is_in_partition(edge, partition))
                    continue;
                netloc_analysis_data *edge_data = (netloc_analysis_data *)edge->userdata;
                int edge_level = edge_data->level;

                netloc_node_t *dest_node = edge->dest;

                /* If the is the node where we are from */
                if (dest_node == up_edge->node) {
                    num_edges++;
                    continue;
                }

                /* Downward edge */
                if (edge_level == -1) {
                    utarray_push_back(down_edges, &edge);
                    num_edges++;
                }
                /* Upward edge */
                else {
                    new_up_edge = edge;
                }

            }
            int level = ((netloc_analysis_data *)up_node->userdata)->level;
            utarray_push_back(down_degrees_by_level[num_levels-1-level], &num_edges);
            max_down_degrees_by_level[num_levels-1-level] =
                max_down_degrees_by_level[num_levels-1-level] > num_edges ?
                max_down_degrees_by_level[num_levels-1-level]: num_edges;
            up_edge = new_up_edge;
        }
    }

    tree->num_levels = num_levels-1;
    tree->degrees = max_down_degrees_by_level;

    int network_coeff = 2;
    tree->cost = (NETLOC_int *)malloc(sizeof(NETLOC_int[tree->num_levels]));
    tree->cost[tree->num_levels-1] = 1;
    for (int i = tree->num_levels-2; i >= 0 ; i--) {
        tree->cost[i] = tree->cost[i+1]*network_coeff;
    }

    /* Now we have the degree of each node, so we can complete the topology to
     * have a complete balanced tree as requested by the tleaf structure */
    int *arch_idx;
    int num_nodes = utarray_len(ordered_name_array);
    netloc_arch_tree_complete(tree, down_degrees_by_level, num_nodes, &arch_idx);

    netloc_node_t **ordered_nodes = (netloc_node_t **)ordered_name_array->d;
    netloc_arch_node_t *named_nodes = NULL;
    for (int i = 0; i < num_nodes; i++) {
        netloc_arch_node_t *node = netloc_arch_node_construct();
        node->node = ordered_nodes[i];
        node->name = ordered_nodes[i]->hostname;
        node->idx_in_topo = arch_idx[i];
        HASH_ADD_KEYPTR(hh, named_nodes, node->name, strlen(node->name), node);
    }
    free(arch_idx);

    arch->nodes_by_name = named_nodes;

end:
    if (nodes)
        utarray_free(nodes);

    if (ordered_name_array)
        utarray_free(ordered_name_array);

    if (down_degrees_by_level) {
        for (int l = 0; l < num_levels; l++) {
            utarray_free(down_degrees_by_level[l]);
        }
        free(down_degrees_by_level);
    }

    if (down_edges)
        utarray_free(down_edges);

    /* We copy back all userdata */
    netloc_topology_iter_nodes(topology, node, node_tmp) {
        if (!netloc_node_is_in_partition(node, partition))
            continue;
        netloc_analysis_data *analysis_data = (netloc_analysis_data *)node->userdata;
        if (analysis_data->level == -1 && ret != -1) {
            ret = -1;
            printf("The node %s was not browsed\n", node->description);
        }
        free(analysis_data);

        netloc_edge_t *edge, *edge_tmp;
        netloc_node_iter_edges(node, edge, edge_tmp) {
            netloc_analysis_data *analysis_data = (netloc_analysis_data *)edge->userdata;
            node->userdata = analysis_data->userdata;
            free(analysis_data);
        }
    }

    return ret;
}

int netloc_arch_build(netloc_arch_t *arch, int add_slots)
{
    char *partition_name = getenv("NETLOC_PARTITION");
    char *topopath = getenv("NETLOC_TOPOFILE");

    if (!topopath) {
        fprintf(stderr, "Error: you need to set NETLOC_TOPOFILE in your environment.\n");
        return NETLOC_ERROR;
    }
    topopath = strdup(topopath);

    netloc_topology_t *topology = netloc_topology_construct(topopath);
    if (topology == NULL) {
        fprintf(stderr, "Error: netloc_topology_construct failed\n");
        free(topopath);
        return NETLOC_ERROR;
    }

    arch->topology = topology;
    arch->has_slots = add_slots;

    if (!partition_name) {
        fprintf(stderr, "Error: you need to set NETLOC_PARTITION in your environment.\n");
        fprintf(stderr, "\tIt can be: ");
        unsigned int num_partitions = utarray_len(topology->partitions);
        for (unsigned int p = 0; p < num_partitions; p++) {
            char *partition = *(char **)utarray_eltptr(topology->partitions, p);
            fprintf(stderr, "%s%s", partition, p != num_partitions-1 ? ", ": "\n");
        }
        return NETLOC_ERROR;
    }

    int partition =
        netloc_topology_find_partition_idx(topology, partition_name);

    partition_topology_to_tleaf(topology, partition, 1, arch);

    return NETLOC_SUCCESS;
}

netloc_arch_t * netloc_arch_construct(void)
{
    netloc_arch_t *arch = (netloc_arch_t *)calloc(1, sizeof(netloc_arch_t));

    return arch;
}

int netloc_arch_destruct(netloc_arch_t *arch)
{
    netloc_topology_destruct(arch->topology);

    netloc_arch_node_t *node, *node_tmp;
    HASH_ITER(hh, arch->nodes_by_name, node, node_tmp) {
        HASH_DEL(arch->nodes_by_name, node);
        netloc_arch_node_destruct(node);
    }

    free(arch->arch.node_tree->degrees);
    free(arch->arch.node_tree->cost);
    free(arch->arch.node_tree);
    free(arch->current_hosts);
    free(arch->node_slot_by_idx);

    free(arch);

    return NETLOC_SUCCESS;
}

static netloc_arch_node_t *netloc_arch_node_construct(void)
{
    netloc_arch_node_t *arch_node = (netloc_arch_node_t *)
        calloc(1, sizeof(netloc_arch_node_t));
    arch_node->num_slots = -1;

    return arch_node;
}

static int netloc_arch_node_destruct(netloc_arch_node_t *arch_node)
{
    free(arch_node->slot_idx);
    free(arch_node->slot_os_idx);
    if (arch_node->slot_tree)
        netloc_arch_tree_destruct(arch_node->slot_tree);
    free(arch_node->current_slots);
    free(arch_node->slot_ranks);
    free(arch_node);

    return NETLOC_SUCCESS;
}
