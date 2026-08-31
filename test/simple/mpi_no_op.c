/* -*- C -*-
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * The most basic of MPI applications
 */

#include "mpi.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    MPI_Init(&argc, &argv);

    MPI_Finalize();
    return 0;
}
