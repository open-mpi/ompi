/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * Copyright (c) 2026      Yongqiang Tian.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "support.h"

#include "mpi.h"
#include "opal/util/os_path.h"
#include "opal/util/proc.h"

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

    char *empty_output = NULL;
    test_verify("singleton process session directory is available",
                NULL != opal_process_info.proc_session_dir);
    if (NULL != opal_process_info.proc_session_dir) {
        empty_output = opal_os_path(false, opal_process_info.proc_session_dir,
                                    "output-empty-regression", NULL);
    }
    test_verify("singleton session output path is available", NULL != empty_output);

    FILE *stream = NULL;
    if (NULL != empty_output) {
        stream = fopen(empty_output, "w");
    }
    test_verify("empty singleton output file can be created", NULL != stream);
    if (NULL != stream) {
        fclose(stream);
    }

    rc = MPI_Finalize();
    test_verify("MPI_Finalize returns MPI_SUCCESS", MPI_SUCCESS == rc);

    if (NULL != empty_output) {
        errno = 0;
        test_verify("MPI_Finalize removes empty singleton output",
                    -1 == access(empty_output, F_OK) && ENOENT == errno);
        free(empty_output);
    }

    return test_finalize();
}
