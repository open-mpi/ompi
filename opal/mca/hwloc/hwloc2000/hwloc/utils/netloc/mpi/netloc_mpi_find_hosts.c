/*
 * Copyright © 2017 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#define _GNU_SOURCE	   /* See feature_test_macros(7) */
#include <stdio.h>
#include <libgen.h>
#include <mpi.h>
#include <hwloc.h>
#include <private/netloc.h>

typedef struct {
    UT_hash_handle hh; /* Makes this structure hashable */
    char *name; /* Hash key */
    UT_array *slots;
    UT_array *ranks;
} node_t;

int main(int argc, char **argv)
{
    int rank;
    int num_ranks;
    MPI_Status status;
    hwloc_topology_t topology;
    hwloc_cpuset_t set;
    int pu_rank = -1;
    char name[1024];
    int resultlen;
    int master; /* To be responsible for a node */
    int one = 1;
    int zero = 0;

    MPI_Init(&argc,&argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &num_ranks);

    if (argc != 2) {
        if (rank == 0)
            fprintf(stderr, "Usage: %s <output file>\n", argv[0]);
        MPI_Abort(MPI_COMM_WORLD, 1);
    }

    hwloc_topology_init(&topology);
    hwloc_topology_load(topology);
    set = hwloc_bitmap_alloc();
    hwloc_get_cpubind(topology, set, 0);
    pu_rank = hwloc_bitmap_first(set);

    MPI_Get_processor_name(name, &resultlen);
    resultlen++;

    if (rank == 0) {
        FILE *output;
        output = fopen(argv[1], "w");
        if (!output) {
            perror("fopen");
            MPI_Abort(MPI_COMM_WORLD, 2);
        }

        node_t *nodes = NULL;
        node_t *node;

        master = 1;

        /* Rank 0 info */
        /* Find node */
        HASH_FIND_PTR(nodes, &name, node);
        /* If node does not exist yet, create it */
        if (!node) {
            node = (node_t *)malloc(sizeof(node_t));
            node->name = name;
            utarray_new(node->slots, &ut_int_icd);
            utarray_new(node->ranks, &ut_int_icd);
            HASH_ADD_KEYPTR(hh, nodes, node->name, strlen(node->name), node);
        }
        /* Add the slot to the list of slots */
        utarray_push_back(node->slots, &pu_rank);
        utarray_push_back(node->ranks, &rank);

        /* Info about other ranks */
        for (int p = 1; p < num_ranks; p++) {
            /* Receive node name size, and slot index */
            char *nodename;
            int buffer[2];
            MPI_Recv (buffer, 2, MPI_INT, p, 0, MPI_COMM_WORLD, &status);
            int size = buffer[0];
            int slot = buffer[1];

            /* Receive node name */
            nodename = (char *)malloc(sizeof(char[size]));
            MPI_Recv(nodename, size, MPI_CHAR, p, 0, MPI_COMM_WORLD, &status);

            /* Find node */
            HASH_FIND_STR(nodes, nodename, node);
            /* If node does not exist yet, create it */
            if (!node) {
                node = (node_t *)malloc(sizeof(node_t));
                node->name = nodename;
                utarray_new(node->slots, &ut_int_icd);
                utarray_new(node->ranks, &ut_int_icd);
                HASH_ADD_KEYPTR(hh, nodes, node->name, strlen(node->name), node);

                /* p will be the master for node */
                MPI_Send(&one, 1, MPI_INT, p, 0, MPI_COMM_WORLD);

            } else {
                /* p won't be a master */
                MPI_Send(&zero, 1, MPI_INT, p, 0, MPI_COMM_WORLD);
            }
            /* Add the slot to the list of slots */
            utarray_push_back(node->slots, &slot);
            utarray_push_back(node->ranks, &p);
        }

        /* Write the list of nodes and slots by node */

        /* Number of nodes */
        int num_nodes = HASH_COUNT(nodes);
        fprintf(output, "%d", num_nodes);

        /* Names of nodes */
        node_t *node_tmp;
        HASH_ITER(hh, nodes, node, node_tmp) {
            fprintf(output, " %s", node->name);
        }

        /* Number of slots by node */
        HASH_ITER(hh, nodes, node, node_tmp) {
            int num_slots = utarray_len(node->slots);
            fprintf(output, " %d", num_slots);
        }

        /* List of slots */
        HASH_ITER(hh, nodes, node, node_tmp) {
            int num_slots = utarray_len(node->slots);
            int *slots = (int *)node->slots->d;
            int *ranks = (int *)node->ranks->d;
            for (int s = 0; s < num_slots; s++) {
                fprintf(output, " %d", slots[s]);
                fprintf(output, " %d", ranks[s]);
            }
        }
        fclose(output);
    } else {
        int buffer[2];
        buffer[0] = resultlen;
        buffer[1] = pu_rank;
        /* Send node name size, and slot index */
        MPI_Send(buffer, 2, MPI_INT, 0, 0, MPI_COMM_WORLD);
        /* Send node name */
        MPI_Send(name, resultlen, MPI_CHAR, 0, 0, MPI_COMM_WORLD);

        /* Receive if is a master or not */
        MPI_Recv (&master, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
    }

    /* In charge of writing hwloc file */
    if (master) {
        /* Get the Netloc topology to find hwloc path */
        char *topopath = getenv("NETLOC_TOPOFILE");
        if (!topopath) {
            fprintf(stderr, "Error: you need to set NETLOC_TOPOFILE in your environment.\n");
        } else {
            topopath = strdup(topopath);
            netloc_topology_t *netloc_topology = netloc_topology_construct(topopath);
            if (netloc_topology == NULL) {
                fprintf(stderr, "Error: netloc_topology_construct failed\n");
                free(topopath);
                return NETLOC_ERROR;
            }

            /* Find hwloc dir path */
            char *hwloc_path;
            if (netloc_topology->hwlocpath[0] != '/') {
                char *path_tmp = strdup(netloc_topology->topopath);
                asprintf(&hwloc_path, "%s/%s", dirname(path_tmp), netloc_topology->hwlocpath);
                free(path_tmp);
            } else {
                hwloc_path = strdup(netloc_topology->hwlocpath);
            }

            /* Check if already have an hwloc file */
            /* We try to find a diff file */
            char *hwloc_file;
            asprintf(&hwloc_file, "%s/%s.diff.xml", hwloc_path, name);
            FILE *fxml;
            if ((fxml = fopen(hwloc_file, "r"))) {
                fclose(fxml);
                free(hwloc_file);
                hwloc_file = NULL;
            } else {
                free(hwloc_file);
                /* We try to find a regular file */
                asprintf(&hwloc_file, "%s/%s.xml", hwloc_path, name);
                if ((fxml = fopen(hwloc_file, "r"))) {
                    fclose(fxml);
                    free(hwloc_file);
                    hwloc_file = NULL;
                }
            }

            /* if there is no hwloc file, let's write one */
            if (hwloc_file) {
                if (hwloc_topology_export_xml(topology, hwloc_file, 0) == -1) {
                    fprintf(stderr, "Error: netloc_topology_construct failed\n");
                    free(topopath);
                    return NETLOC_ERROR;
                }
                free(hwloc_path);
                free(hwloc_file);
            }
        }
    }

    MPI_Finalize();
}
