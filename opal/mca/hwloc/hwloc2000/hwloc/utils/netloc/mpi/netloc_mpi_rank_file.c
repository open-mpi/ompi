/*
 * Copyright © 2016 Inria.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 * See COPYING in top-level directory.
 *
 * $HEADER$
 */

#define _GNU_SOURCE         /* See feature_test_macros(7) */

#include <netloc.h>
#include <netlocscotch.h>
#include <private/netloc.h>
#include <hwloc.h>

int main(int argc, char **argv)
{
    int ret;
    netlocscotch_core_t *cores;
    int num_processes;

    char *comm_filename;
    char *rank_filename;

    assert(argc == 3);

    comm_filename = argv[1];
    rank_filename = argv[2];

    ret = netlocscotch_get_mapping_from_comm_file(comm_filename, &num_processes, &cores);

    if (ret != NETLOC_SUCCESS) {
        fprintf(stderr, "Error: netlocscotch_get_mapping_from_comm_file failed\n");
        return NETLOC_ERROR;
    }

    FILE *rank_file = fopen(rank_filename, "w");
    if (!rank_file) {
        perror("fopen");
        ret = NETLOC_ERROR;

    } else {
        for (int p = 0; p < num_processes; p++) {
            fprintf(rank_file, "rank %d=%s slot=%d\n",
                    p, cores[p].nodename, cores[p].core);
        }
        fclose(rank_file);
    }

    for (int p = 0; p < num_processes; p++) {
        free(cores[p].nodename);
    }
    free(cores);

    return ret;
}

