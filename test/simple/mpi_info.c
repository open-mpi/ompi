/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>
#include "mpi.h"

int main(int argc, char* argv[])
{
    char value[MPI_MAX_INFO_VAL];
    int flag;
    char *keys[] = {
        "command",
        "argv",
        "maxprocs",
        "soft",
        "host",
        "arch",
        "wdir",
        "thread_level",
        "ompi_num_apps",
        "ompi_first_rank",
        "ompi_np",
        "ompi_positioned_file_dir"
    };
    int i, nk;

    MPI_Init(&argc, &argv);

    nk = sizeof(keys) / sizeof(char*);

    for (i=0; i < nk; i++) {
        MPI_Info_get(MPI_INFO_ENV, keys[i], MPI_MAX_INFO_VAL,
                     value, &flag);
        fprintf(stderr, "%s: %s\n", keys[i], (flag) ? value : "Not found");
    }

    MPI_Finalize();
    return 0;
}
