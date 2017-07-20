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
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <dirent.h>
#include <scotch.h>

#include <netloc.h>
#include <netlocscotch.h>
#include <private/netloc.h>
#include <hwloc.h>

static int arch_tree_to_scotch_arch(netloc_arch_tree_t *tree, SCOTCH_Arch *scotch);
static int comm_matrix_to_scotch_graph(double **matrix, int n, SCOTCH_Graph *graph);
static int netlocscotch_get_mapping_from_graph(SCOTCH_Graph *graph,
        netlocscotch_core_t **pcores);

static int compareint(void const *a, void const *b)
{
   const int *int_a = (const int *)a;
   const int *int_b = (const int *)b;
   return *int_a-*int_b;
}

static int build_subarch(SCOTCH_Arch *scotch, NETLOC_int num_nodes, NETLOC_int *node_list,
        SCOTCH_Arch *subarch)
{
    int ret;

    /* Hack to avoid problem with unsorted node list in the subarch and scotch
     * FIXME TODO */
    qsort(node_list, num_nodes, sizeof(*node_list), compareint);

    ret = SCOTCH_archSub(subarch, scotch, num_nodes, node_list);
    if (ret != 0) {
        fprintf(stderr, "Error: SCOTCH_archSub failed\n");
    }

    return ret;
}

/* Convert a netloc tree to a scotch tleaf architecture */
int arch_tree_to_scotch_arch(netloc_arch_tree_t *tree, SCOTCH_Arch *scotch)
{
    int ret;

    ret = SCOTCH_archTleaf(scotch, tree->num_levels, tree->degrees, tree->cost);
    if (ret != 0) {
        fprintf(stderr, "Error: SCOTCH_archTleaf failed\n");
        return NETLOC_ERROR;
    }

    return NETLOC_SUCCESS;
}

static int build_subgraph(SCOTCH_Graph *graph, int *vertices, int num_vertices,
        SCOTCH_Graph *nodegraph)
{
    int ret;

    SCOTCH_Num base;       /* Base value               */
    SCOTCH_Num vert;       /* Number of vertices       */
    SCOTCH_Num *verttab;   /* Vertex array [vertnbr+1] */
    SCOTCH_Num *vendtab;   /* Vertex array [vertnbr]   */
    SCOTCH_Num *velotab;   /* Vertex load array        */
    SCOTCH_Num *vlbltab;   /* Vertex label array       */
    SCOTCH_Num edge;       /* Number of edges (arcs)   */
    SCOTCH_Num *edgetab;   /* Edge array [edgenbr]     */
    SCOTCH_Num *edlotab;   /* Edge load array          */

    SCOTCH_graphData(graph, &base, &vert, &verttab, &vendtab, &velotab,
            &vlbltab, &edge, &edgetab, &edlotab);

    int *vertex_is_present = (int *)malloc(vert*sizeof(int));
    for (int v = 0; v < vert; v++) {
        vertex_is_present[v] = -1;
    }
    for (int v = 0; v < num_vertices; v++) {
        vertex_is_present[vertices[v]] = v;
    }

    // TODO handle other cases 
    if (vendtab) {
        for (int i = 0; i < vert; i++) {
            assert(vendtab[i] == verttab[i+1]);
        }
    }

    SCOTCH_Num *new_verttab;   /* Vertex array [vertnbr+1] */
    SCOTCH_Num *new_vendtab;   /* Vertex array [vertnbr]   */
    SCOTCH_Num *new_velotab;   /* Vertex load array        */
    SCOTCH_Num *new_vlbltab;   /* Vertex label array       */
    SCOTCH_Num new_edge;       /* Number of edges (arcs)   */
    SCOTCH_Num *new_edgetab;   /* Edge array [edgenbr]     */
    SCOTCH_Num *new_edlotab;   /* Edge load array          */

    new_verttab = (SCOTCH_Num *)malloc((num_vertices+1)*sizeof(SCOTCH_Num));
    new_vendtab = NULL;
    if (velotab)
        new_velotab = (SCOTCH_Num *)malloc(num_vertices*sizeof(SCOTCH_Num));
    else
        new_velotab = NULL;
    if (vlbltab)
        new_vlbltab = (SCOTCH_Num *)malloc(num_vertices*sizeof(SCOTCH_Num));
    else
        new_vlbltab = NULL;

    new_edgetab = (SCOTCH_Num *)malloc(edge*sizeof(SCOTCH_Num));
    new_edlotab = (SCOTCH_Num *)malloc(edge*sizeof(SCOTCH_Num));

    int edge_idx = 0;
    new_verttab[0] = 0;
    for (int v = 0; v < num_vertices; v++) {
        if (velotab)
            new_velotab[v] = velotab[vertices[v]];
        if (vlbltab)
            new_vlbltab[v] = vlbltab[vertices[v]];

        for (int e = verttab[vertices[v]]; e < verttab[vertices[v]+1]; e++) {
            int dest_vertex = edgetab[e];
            int new_dest = vertex_is_present[dest_vertex];
            if (new_dest != -1) {
                new_edgetab[edge_idx] = new_dest;
                new_edlotab[edge_idx] = edlotab[e];
                edge_idx++;
            }
        }
        new_verttab[v+1] = edge_idx;
    }

    new_edge = edge_idx;

    SCOTCH_Num *old_edgetab = new_edgetab;
    new_edgetab = (SCOTCH_Num *)
        realloc(new_edgetab, new_edge*sizeof(SCOTCH_Num));
    if (!new_edgetab) {
        new_edgetab = old_edgetab;
    }

    SCOTCH_Num *old_edlotab = new_edlotab;
    new_edlotab = (SCOTCH_Num *)
        realloc(new_edlotab, new_edge*sizeof(SCOTCH_Num));
    if (!new_edlotab) {
        new_edlotab = old_edlotab;
    }

    ret = SCOTCH_graphBuild (nodegraph, base, num_vertices,
                new_verttab, new_vendtab, new_velotab, new_vlbltab,
                new_edge, new_edgetab, new_edlotab);

    free(vertex_is_present);

    return ret;
}

static int build_current_arch(SCOTCH_Arch *scotch_arch,
        SCOTCH_Arch *scotch_subarch, netloc_arch_t *arch)
{
    int ret;
    /* First we need to get the topology of the whole machine */
    ret = netloc_arch_build(arch, 1);
    if( NETLOC_SUCCESS != ret ) {
        return ret;
    }

    if (scotch_subarch) {
        /* Set the current nodes and slots in the arch */
        ret = netloc_arch_set_current_resources(arch);
    } else {
        ret = netloc_arch_set_global_resources(arch);
    }

    if( NETLOC_SUCCESS != ret ) {
        return ret;
    }

    SCOTCH_archInit(scotch_arch);
    ret = arch_tree_to_scotch_arch(arch->arch.global_tree, scotch_arch);
    if (NETLOC_SUCCESS != ret) {
        return ret;
    }

    if (scotch_subarch) {
        /* Now we can build the sub architecture */
        SCOTCH_archInit(scotch_subarch);
        ret = build_subarch(scotch_arch, arch->num_current_hosts,
                arch->current_hosts, scotch_subarch);
    }

    return ret;
}

int netlocscotch_build_global_arch(SCOTCH_Arch *arch)
{
    int ret;
    netloc_arch_t *netloc_arch = netloc_arch_construct();
    ret = build_current_arch(arch, NULL, netloc_arch);

    netloc_arch_destruct(netloc_arch);
    return ret;
}

int netlocscotch_build_current_arch(SCOTCH_Arch *arch, SCOTCH_Arch *subarch)
{
    int ret;
    netloc_arch_t *netloc_arch = netloc_arch_construct();
    ret = build_current_arch(arch, subarch, netloc_arch);

    if (ret == NETLOC_SUCCESS)
        netloc_arch_destruct(netloc_arch);

    return ret;
}

int netlocscotch_get_mapping_from_graph(SCOTCH_Graph *graph,
        netlocscotch_core_t **pcores)
{
    int ret;

    SCOTCH_Arch scotch_arch;
    SCOTCH_Arch scotch_subarch;
    netlocscotch_core_t *cores = NULL;
    netloc_arch_t *arch = netloc_arch_construct();
    ret = build_current_arch(&scotch_arch, &scotch_subarch, arch);
    if (NETLOC_SUCCESS != ret) {
        netloc_arch_destruct(arch);
        return ret;
    }

    NETLOC_int graph_size;
    SCOTCH_graphSize(graph, &graph_size, NULL);

    int num_hosts = arch->num_current_hosts;

    SCOTCH_Strat strategy;
    SCOTCH_stratInit(&strategy);
    /* We force Scotch to use all the processes
     * barat is 0.01 as in SCOTCH_STRATDEFAULT */
    SCOTCH_stratGraphMapBuild(&strategy, SCOTCH_STRATQUALITY, graph_size, 0.01);

    /* The ranks are the indices of the nodes in the complete graph */
    NETLOC_int *ranks = (NETLOC_int *)malloc(graph_size*sizeof(NETLOC_int));
    ret = SCOTCH_graphMap(graph, &scotch_subarch, &strategy, ranks);

    SCOTCH_stratExit(&strategy);

    SCOTCH_archExit(&scotch_subarch);
    SCOTCH_archExit(&scotch_arch);

    if (ret != 0) {
        fprintf(stderr, "Error: SCOTCH_graphMap failed\n");
        goto ERROR;
    }

    cores = (netlocscotch_core_t *)
        malloc(graph_size*sizeof(netlocscotch_core_t));
    if (!arch->has_slots) {
        /* We have the mapping but only for the nodes, not inside the nodes */

        UT_array *process_by_node[num_hosts];
        for (int n = 0; n < num_hosts; n++) {
            utarray_new(process_by_node[n], &ut_int_icd);
        }

        /* Find the processes mapped to the nodes */
        for (int p = 0; p < graph_size; p++) {
            int rank = ranks[p];
            if (rank >= num_hosts || rank < 0) {
                ret = NETLOC_ERROR;
                goto ERROR;
            }
            utarray_push_back(process_by_node[rank], &p);
        }

        /* Use the intranode topology */
        for (int n = 0; n < num_hosts; n++) {
            int *process_list = (int *)process_by_node[n]->d;
            int num_processes = utarray_len(process_by_node[n]);
            netloc_arch_node_t *node =
                arch->node_slot_by_idx[arch->current_hosts[n]].node;
            NETLOC_int node_ranks[num_processes];

            /* We need to extract the subgraph with only the vertices mapped to the
             * current node */
            SCOTCH_Graph nodegraph; /* graph with only elements for node n */
            build_subgraph(graph, process_list, num_processes, &nodegraph);

            /* Build the scotch arch of the all node */
            SCOTCH_Arch scotch_nodearch;
            ret = arch_tree_to_scotch_arch(node->slot_tree, &scotch_nodearch);
            if (NETLOC_SUCCESS != ret) {
                goto ERROR;
            }

            /* Restrict the scotch arch to the available cores */
            SCOTCH_Arch scotch_nodesubarch;
            ret = build_subarch(&scotch_nodearch, node->num_current_slots,
                    node->current_slots, &scotch_nodesubarch);
            if (NETLOC_SUCCESS != ret) {
                goto ERROR;
            }

            /* Find the mapping to the cores */
            ret = SCOTCH_graphMap(&nodegraph, &scotch_nodesubarch, &strategy, node_ranks);
            if (ret != 0) {
                fprintf(stderr, "Error: SCOTCH_graphMap failed\n");
                goto ERROR;
            }

            /* Report the node ranks in the global rank array */
            for (int p = 0; p < num_processes; p++) {
                int process = process_list[p];
                int arch_idx = node->current_slots[node_ranks[p]];
                cores[process].core = node->slot_os_idx[arch_idx];
                cores[process].nodename = strdup(node->node->hostname);
                cores[process].rank = node->slot_ranks[node_ranks[p]];
            }
        }
        for (int n = 0; n < num_hosts; n++) {
            utarray_free(process_by_node[n]);
        }
    } else {
        for (int p = 0; p < graph_size; p++) {
            int host_idx = arch->current_hosts[ranks[p]];
            netloc_arch_node_t *node = arch->node_slot_by_idx[host_idx].node;
            int slot_rank = arch->node_slot_by_idx[host_idx].slot;
            cores[p].nodename = strdup(node->node->hostname);
            cores[p].core = node->slot_os_idx[node->slot_idx[slot_rank]];
            cores[p].rank = node->slot_ranks[node->slot_idx[slot_rank]];
        }
    }

    *pcores = cores;

ERROR:
    free(ranks);
    netloc_arch_destruct(arch);
    if (ret == NETLOC_SUCCESS)
        return ret;
    free(cores);
    return ret;
}

int netlocscotch_get_mapping_from_comm_matrix(double **comm, int num_vertices,
        netlocscotch_core_t **pcores)
{
    int ret;

    SCOTCH_Graph graph;
    ret = comm_matrix_to_scotch_graph(comm, num_vertices, &graph);
    if (NETLOC_SUCCESS != ret) {
        return ret;
    }

    ret = netlocscotch_get_mapping_from_graph(&graph, pcores);

    /* Free arrays */
    {
        SCOTCH_Num base;       /* Base value               */
        SCOTCH_Num vert;       /* Number of vertices       */
        SCOTCH_Num *verttab;   /* Vertex array [vertnbr+1] */
        SCOTCH_Num *vendtab;   /* Vertex array [vertnbr]   */
        SCOTCH_Num *velotab;   /* Vertex load array        */
        SCOTCH_Num *vlbltab;   /* Vertex label array       */
        SCOTCH_Num edge;       /* Number of edges (arcs)   */
        SCOTCH_Num *edgetab;   /* Edge array [edgenbr]     */
        SCOTCH_Num *edlotab;   /* Edge load array          */

        SCOTCH_graphData(&graph, &base, &vert, &verttab, &vendtab, &velotab,
                &vlbltab, &edge, &edgetab, &edlotab);
        free(edlotab);
        free(edgetab);
        free(verttab);
        SCOTCH_graphExit(&graph);
    }

    return ret;
}

int netlocscotch_get_mapping_from_comm_file(char *filename, int *pnum_processes,
        netlocscotch_core_t **pcores)
{
    int ret;
    int n;
    double **mat;

    ret = netloc_build_comm_mat(filename, &n, &mat);

    if (ret != NETLOC_SUCCESS) {
        return ret;
    }

    *pnum_processes = n;

    ret = netlocscotch_get_mapping_from_comm_matrix(mat, n, pcores);

    free(mat[0]);
    free(mat);

    return ret;
}

static int comm_matrix_to_scotch_graph(double **matrix, int n, SCOTCH_Graph *graph)
{
    int ret;

    SCOTCH_Num base;       /* Base value               */
    SCOTCH_Num vert;       /* Number of vertices       */
    SCOTCH_Num *verttab;   /* Vertex array [vertnbr+1] */
    SCOTCH_Num *vendtab;   /* Vertex array [vertnbr]   */
    SCOTCH_Num *velotab;   /* Vertex load array        */
    SCOTCH_Num *vlbltab;   /* Vertex label array       */
    SCOTCH_Num edge;       /* Number of edges (arcs)   */
    SCOTCH_Num *edgetab;   /* Edge array [edgenbr]     */
    SCOTCH_Num *edlotab;   /* Edge load array          */

    base = 0;
    vert = n;

    verttab = (SCOTCH_Num *)malloc((vert+1)*sizeof(SCOTCH_Num));
    for (int v = 0; v < vert+1; v++) {
        verttab[v] = v*(n-1);
    }

    vendtab = NULL;
    velotab = NULL;
    vlbltab = NULL;

    edge = n*(n-1);

    /* Compute the lowest load to reduce of the values of the load to avoid overflow */
    double min_load = -1;
    for (int v1 = 0; v1 < vert; v1++) {
        for (int v2 = 0; v2 < vert; v2++) {
            double load = matrix[v1][v2];
            if (load >= 0.01 && (load < min_load || min_load < 0)) /* TODO set an epsilon */
                min_load = load;
        }
    }

    edgetab = (SCOTCH_Num *)malloc(n*(n-1)*sizeof(SCOTCH_Num));
    edlotab = (SCOTCH_Num *)malloc(n*(n-1)*sizeof(SCOTCH_Num));
    for (int v1 = 0; v1 < vert; v1++) {
        for (int v2 = 0; v2 < vert; v2++) {
            if (v2 == v1)
                continue;
            int idx = v1*(n-1)+((v2 < v1) ? v2: v2-1);
            edgetab[idx] = v2;
            edlotab[idx] = (int)(matrix[v1][v2]/min_load);
        }
    }

    ret = SCOTCH_graphBuild(graph, base, vert,
            verttab, vendtab, velotab, vlbltab, edge, edgetab, edlotab);

    return ret;
}

