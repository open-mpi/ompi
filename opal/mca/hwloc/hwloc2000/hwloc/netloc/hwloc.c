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

#include <sys/types.h>
#include <dirent.h>
#include <libgen.h>

#include <private/netloc.h>
#include <netloc.h>
#include <hwloc.h>

static UT_icd topos_icd = {sizeof(hwloc_topology_t), NULL, NULL, NULL};

int netloc_topology_read_hwloc(netloc_topology_t *topology, int num_nodes,
        netloc_node_t **node_list)
{
    int ret = 0;
    int all = 0;

    char *hwloc_path;

    if (!topology->hwlocpath) {
        printf("No hwloc directory recorded in the topology\n");
        return NETLOC_ERROR;
    }

    if (topology->hwlocpath[0] != '/') {
        char *path_tmp = strdup(topology->topopath);
        asprintf(&hwloc_path, "%s/%s", dirname(path_tmp), topology->hwlocpath);
        free(path_tmp);
    } else {
        hwloc_path = strdup(topology->hwlocpath);
    }

    DIR* dir = opendir(hwloc_path);
    /* Directory does not exist */
    if (!dir) {
        printf("Directory (%s) to hwloc does not exist\n", hwloc_path);
        free(hwloc_path);
        return NETLOC_ERROR;
    }
    else {
        closedir(dir);
    }

    UT_array *hwloc_topo_names = topology->topos;
    UT_array *hwloc_topos;
    utarray_new(hwloc_topos, &topos_icd);

    int num_diffs = 0;

    if (!num_nodes) {
        netloc_node_t *node, *node_tmp;
        num_nodes = HASH_COUNT(topology->nodes);
        node_list = (netloc_node_t **)malloc(sizeof(netloc_node_t *[num_nodes]));
        int n = 0;
        netloc_topology_iter_nodes(topology, node, node_tmp) {
            node_list[n++] = node;
        }
        all = 1;
    }

    for (int n  = 0; n < num_nodes; n++) {
        netloc_node_t *node = node_list[n];
        char *hwloc_file;
        char *refname;

        if (netloc_node_is_switch(node))
            continue;

        /* We try to find a diff file */
        asprintf(&hwloc_file, "%s/%s.diff.xml", hwloc_path, node->hostname);
        hwloc_topology_diff_t diff;
        int err;
        if ((err = hwloc_topology_diff_load_xml(hwloc_file, &diff, &refname)) >= 0) {
            refname[strlen(refname)-4] = '\0';
            hwloc_topology_diff_destroy(diff);
            num_diffs++;
        }
        else {
            free(hwloc_file);
            /* We try to find a regular file */
            asprintf(&hwloc_file, "%s/%s.xml", hwloc_path, node->hostname);
            FILE *fxml;
            if (!(fxml = fopen(hwloc_file, "r"))) {
                printf("Missing hwloc file: %s\n", hwloc_file);
            }
            else
                fclose(fxml);
            asprintf(&refname, "%s", node->hostname);
        }

        /* Add the hwloc topology */
        unsigned int t = 0;
        while (t < utarray_len(hwloc_topo_names) &&
                strcmp(*(char **)utarray_eltptr(hwloc_topo_names, t), refname)) {
            t++;
        }
        /* Topology not found */
        if (t == utarray_len(hwloc_topo_names)) {
            utarray_push_back(hwloc_topo_names, &refname);

            /* Read the hwloc topology */
            hwloc_topology_t topology;
            hwloc_topology_init(&topology);
            hwloc_topology_set_flags(topology, HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM);

            char *hwloc_ref_path;
            asprintf(&hwloc_ref_path, "%s/%s.xml", hwloc_path, refname);
            ret = hwloc_topology_set_xml(topology, hwloc_ref_path);
            free(hwloc_ref_path);
            if (ret == -1) {
                void *null = NULL;
                utarray_push_back(hwloc_topos, &null);
                fprintf(stdout, "Warning: no topology for %s\n", refname);
                hwloc_topology_destroy(topology);
                free(refname); free(hwloc_file);
                continue;
            }

            ret = hwloc_topology_set_all_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_STRUCTURE);
            if (ret == -1) {
                fprintf(stderr, "hwloc_topology_set_all_types_filter failed\n");
                free(refname); free(hwloc_file);
                goto ERROR;
            }

            ret = hwloc_topology_set_io_types_filter(topology, HWLOC_TYPE_FILTER_KEEP_NONE);
            if (ret == -1) {
                fprintf(stderr, "hwloc_topology_set_all_types_filter failed\n");
                free(refname); free(hwloc_file);
                goto ERROR;
            }

            ret = hwloc_topology_load(topology);
            if (ret == -1) {
                fprintf(stderr, "hwloc_topology_load failed\n");
                free(refname); free(hwloc_file);
                goto ERROR;
            }
            utarray_push_back(hwloc_topos, &topology);
        }
        free(refname);
        free(hwloc_file);
        node->hwlocTopo = *(hwloc_topology_t *)utarray_eltptr(hwloc_topos, t);
        node->hwlocTopoIdx = t;
    }

    if (!num_diffs) {
        printf("Warning: no hwloc diff file found!\n");
    }

    topology->topos = hwloc_topo_names;
    topology->hwloc_topos = (hwloc_topology_t *)hwloc_topos->d;

    printf("%d hwloc topologies found:\n", utarray_len(topology->topos));
    for (unsigned int p = 0; p < utarray_len(topology->topos); p++) {
        printf("\t'%s'\n", *(char **)utarray_eltptr(topology->topos, p));
    }

    ret = NETLOC_SUCCESS;

ERROR:
    if (all) {
        free(node_list);
    }
    free(hwloc_path);
    if (ret == NETLOC_SUCCESS)
        free(hwloc_topos);
    else
        utarray_free(hwloc_topos);
    return ret;
}

/* Set the info from hwloc of the node in the correspondig arch */
int netloc_arch_node_get_hwloc_info(netloc_arch_node_t *arch_node)
{
    hwloc_topology_t topology = arch_node->node->hwlocTopo;

    hwloc_obj_t root = hwloc_get_root_obj(topology);

    int depth = hwloc_topology_get_depth(topology);
    hwloc_obj_t first_object = root->first_child;

    UT_array **down_degrees_by_level;
    NETLOC_int *max_down_degrees_by_level;

    down_degrees_by_level = (UT_array **)malloc(depth*sizeof(UT_array *));
    for (int l = 0; l < depth; l++) {
        utarray_new(down_degrees_by_level[l], &ut_int_icd);
    }
    max_down_degrees_by_level = (NETLOC_int *)
        calloc(depth-1, sizeof(NETLOC_int));

    int level = depth-1;
    hwloc_obj_t current_object = first_object;
    while (level >= 1) {
        int degree = 1;
        /* we go through the siblings */
        while (current_object->next_sibling) {
            current_object = current_object->next_sibling;
            degree++;
        }
        /* Add the degree to the list of degrees */
        utarray_push_back(down_degrees_by_level[depth-1-level], &degree);
        max_down_degrees_by_level[depth-1-level] =
            max_down_degrees_by_level[depth-1-level] > degree ?
            max_down_degrees_by_level[depth-1-level] : degree;

        current_object = current_object->next_cousin;

        if (!current_object) {
            level--;
            if (!first_object->first_child)
                break;
            first_object = first_object->first_child;
            current_object = first_object;
        }
    }

    /* List of PUs */
    unsigned int max_os_index = 0;
    UT_array *ordered_host_array;
    int *ordered_hosts;
    utarray_new(ordered_host_array, &ut_int_icd);
    current_object = first_object;
    while (current_object) {
        max_os_index = (max_os_index >= current_object->os_index)?
            max_os_index: current_object->os_index;
        utarray_push_back(ordered_host_array, &current_object->os_index);
        current_object = current_object->next_cousin;
    }
    ordered_hosts = (int *)ordered_host_array->d;;

    /* Weight for the edges in the tree */
    NETLOC_int *cost = (NETLOC_int *)malloc((depth-1)*sizeof(NETLOC_int));
    int level_coeff = 3;
    cost[depth-2] = 1;
    for (int l = depth-3; l >= 0; l--) {
        cost[l] = cost[l+1]*level_coeff;
    }

    netloc_arch_tree_t *tree = (netloc_arch_tree_t *)
        malloc(sizeof(netloc_arch_tree_t));
    tree->num_levels = depth-1;
    tree->degrees = max_down_degrees_by_level;
    tree->cost = cost;

    int *arch_idx;
    int num_cores = utarray_len(ordered_host_array);
    netloc_arch_tree_complete(tree, down_degrees_by_level, num_cores, &arch_idx);

    int *slot_idx = (int *)malloc(sizeof(int[max_os_index+1]));
    for (int i = 0; i < num_cores; i++) {
        slot_idx[ordered_hosts[i]] = arch_idx[i];
    }

    int num_leaves = netloc_arch_tree_num_leaves(tree);
    int *slot_os_idx = (int *)malloc(sizeof(int[num_leaves]));
    for (int i = 0; i < num_cores; i++) {
        slot_os_idx[arch_idx[i]] = ordered_hosts[i];
    }
    free(arch_idx);

    arch_node->slot_tree = tree;
    arch_node->slot_idx = slot_idx;
    arch_node->slot_os_idx = slot_os_idx;
    arch_node->num_slots = max_os_index+1;

    for (int l = 0; l < depth; l++) {
        utarray_free(down_degrees_by_level[l]);
    }
    free(down_degrees_by_level);

    utarray_free(ordered_host_array);

    return NETLOC_SUCCESS;
}
