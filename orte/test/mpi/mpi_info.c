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
    char value[MPI_MAX_INFO_KEY];
    int flag;
    char *keys[] = {
        "command",
        "argv",
        "maxprocs",
        "soft",
        "host",
        "arch",
        "wdir",
        "num_app_ctx",
        "first_rank",
        "np"
    };
    int i, nk;

    MPI_Init(&argc, &argv);

    nk = sizeof(keys) / sizeof(char*);

    for (i=0; i < nk; i++) {
        MPI_Info_get(MPI_INFO_GET_ENV, keys[i], MPI_MAX_INFO_KEY,
                     value, &flag);
        fprintf(stderr, "%s: %s\n", keys[i], value);
    }

    MPI_Finalize();
    return 0;
}
