/* -*- C -*-
 *
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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#define bool int
#define false 0
#define true 1


#include <mpi.h>
#include <stdio.h>
#include "../../opal/mca/carto/carto.h"
#include "../../opal/mca/carto/base/base.h"
#include "../../opal/util/output.h"
#include "../../opal/class/opal_graph.h"

int
main(int argc, char* argv[])
{
    int rank, size, distance, distance_array_size, i;
    opal_carto_graph_t *graph;
    opal_carto_base_node_t *slot0, *end_node;
    opal_carto_node_distance_t *node_distance;
    opal_value_array_t *distance_array;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);


    printf("Hello, world, I am %d of %d\n", rank, size);
    if (0 == rank) {
        /**
         * 
         */

        opal_output(0," \n\nget_host_graph Full\n");
        opal_carto_base_get_host_graph(&graph,NULL);
        opal_graph_print(graph);
        slot0 = opal_carto_base_find_node(graph, "slot0");
        if (NULL == slot0) {
            opal_output(0,"couldnt find slot0 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        end_node = opal_carto_base_find_node(graph, "slot3");
        if (NULL == end_node) {
            opal_output(0,"couldnt find mthca1 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        distance = opal_carto_base_spf(graph, slot0, end_node);
        opal_output(0,"\nThe distance between slot0 and slot3 is %d\n",distance);
        distance_array = OBJ_NEW(opal_value_array_t);
        opal_value_array_init(distance_array, sizeof(opal_carto_node_distance_t));
        opal_value_array_reserve(distance_array, 50);
        distance_array_size = opal_carto_base_get_nodes_distance(graph, slot0, NULL, distance_array);
        for (i=0; i < distance_array_size; i++) {
            node_distance = opal_value_array_get_item(distance_array, i);
            opal_output(0,"Node %s distance from slot0 is %d\n",node_distance->node->node_name, node_distance->node_distance);
        }
        OBJ_RELEASE(distance_array);
        opal_carto_base_free_graph(graph);
        /**
         * 
         */

        opal_output(0," \n\nget_host_graph Infiniband\n");
        opal_carto_base_get_host_graph(&graph,"Infiniband");
        opal_graph_print(graph);
        slot0 = opal_carto_base_find_node(graph, "slot0");
        if (NULL == slot0) {
            opal_output(0,"couldnt find slot0 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        end_node = opal_carto_base_find_node(graph, "mthca1");
        if (NULL == end_node) {
            opal_output(0,"couldnt find mthca1 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        distance = opal_carto_base_spf(graph, slot0, end_node);
        opal_output(0,"\nThe distance between slot0 and mthca1 is %d\n",distance);
        distance_array = OBJ_NEW(opal_value_array_t);
        opal_value_array_init(distance_array, sizeof(opal_carto_node_distance_t));
        opal_value_array_reserve(distance_array, 50);
        distance_array_size = opal_carto_base_get_nodes_distance(graph, slot0, "Infiniband", distance_array);
        for (i=0; i < distance_array_size; i++) {
            node_distance = opal_value_array_get_item(distance_array, i);
            opal_output(0,"Node %s distance from slot0 is %d\n",node_distance->node->node_name, node_distance->node_distance);
        }
        OBJ_RELEASE(distance_array);
        opal_carto_base_free_graph(graph);
        /**
         * 
         */

        opal_output(0," \n\nget_host_graph Ethernet\n");
        opal_carto_base_get_host_graph(&graph,"Ethernet");
        opal_graph_print(graph);
        slot0 = opal_carto_base_find_node(graph, "slot0");
        if (NULL == slot0) {
            opal_output(0,"couldnt find slot0 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        end_node = opal_carto_base_find_node(graph, "eth1");
        if (NULL == end_node) {
            opal_output(0,"couldnt find mthca1 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        distance = opal_carto_base_spf(graph, slot0, end_node);
        opal_output(0,"\nThe distance between slot0 and eth1 is %d\n",distance);
        distance_array = OBJ_NEW(opal_value_array_t);
        opal_value_array_init(distance_array, sizeof(opal_carto_node_distance_t));
        opal_value_array_reserve(distance_array, 50);
        distance_array_size = opal_carto_base_get_nodes_distance(graph, slot0, "Ethernet", distance_array);
        for (i=0; i < distance_array_size; i++) {
            node_distance = opal_value_array_get_item(distance_array, i);
            opal_output(0,"Node %s distance from slot0 is %d\n",node_distance->node->node_name, node_distance->node_distance);
        }
        OBJ_RELEASE(distance_array);
        opal_carto_base_free_graph(graph);
        /**
         * 
         */

        opal_output(0," \n\nget_host_graph Memory\n");
        opal_carto_base_get_host_graph(&graph,"Memory");
        opal_graph_print(graph);
        slot0 = opal_carto_base_find_node(graph, "slot0");
        if (NULL == slot0) {
            opal_output(0,"couldnt find slot0 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        end_node = opal_carto_base_find_node(graph, "mem3");
        if (NULL == end_node) {
            opal_output(0,"couldnt find mthca1 in the graph exiting\n");
            opal_carto_base_free_graph(graph);
            return -1;
        }
        distance = opal_carto_base_spf(graph, slot0, end_node);
        opal_output(0,"\nThe distance between slot0 and mem3 is %d\n",distance);
        distance_array = OBJ_NEW(opal_value_array_t);
        opal_value_array_init(distance_array, sizeof(opal_carto_node_distance_t));
        opal_value_array_reserve(distance_array, 50);
        distance_array_size = opal_carto_base_get_nodes_distance(graph, slot0, "Memory", distance_array);
        for (i=0; i < distance_array_size; i++) {
            node_distance = opal_value_array_get_item(distance_array, i);
            opal_output(0,"Node %s distance from slot0 is %d\n",node_distance->node->node_name, node_distance->node_distance);
        }
        OBJ_RELEASE(distance_array);
       opal_carto_base_free_graph(graph);

    }
    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();

    return 0;

}

