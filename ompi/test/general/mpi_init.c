/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * Unit test for a full singleton MPI_Init / MPI_Finalize cycle (no
 * launcher), which works under "make check" because Open MPI supports
 * singleton startup.  This exercises the basic runtime/instance bring-up
 * and teardown plus trivial MPI_COMM_WORLD / MPI_COMM_SELF queries.  It
 * also establishes that "full init" tests are viable for covering the
 * runtime-dependent ompi/ directories that partial init cannot reach.
 */

#include "ompi_config.h"

#include "support.h"

#include "mpi.h"

int main(int argc, char *argv[])
{
    test_init("MPI singleton init");

    int rc = MPI_Init(&argc, &argv);
    test_verify("MPI_Init returns MPI_SUCCESS", MPI_SUCCESS == rc);

    int rank = -1, size = -1;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    test_verify("singleton MPI_COMM_WORLD rank is 0", 0 == rank);
    test_verify("singleton MPI_COMM_WORLD size is 1", 1 == size);

    int self_size = -1;
    MPI_Comm_size(MPI_COMM_SELF, &self_size);
    test_verify("MPI_COMM_SELF size is 1", 1 == self_size);

    rc = MPI_Finalize();
    test_verify("MPI_Finalize returns MPI_SUCCESS", MPI_SUCCESS == rc);

    return test_finalize();
}
