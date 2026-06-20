/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * With all in-tree producers disabled, the event interface must be inert but
 * correct: zero events/sources, and the standard error contracts hold.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>

static int failures = 0;
static void expect(const char *what, int cond)
{
    printf("%s: %s\n", cond ? "PASS" : "FAIL", what);
    if (!cond) {
        ++failures;
    }
}

int main(void)
{
    int provided, num = -1, idx = -1, nevents = -1;

    /* Disable the master producer switch before MPI_T initialization. */
    setenv("OMPI_MCA_mca_base_event_register_producers", "0", 1);

    if (MPI_SUCCESS != MPI_T_init_thread(MPI_THREAD_SINGLE, &provided)) {
        printf("FAIL: MPI_T_init_thread\nRESULT: FAIL\n");
        return 1;
    }

    (void) MPI_T_event_get_num(&num);
    expect("event count is 0 with producers disabled", 0 == num);
    (void) MPI_T_source_get_num(&num);
    expect("source count is 0 with producers disabled", 0 == num);

    expect("event_get_info(0) -> INVALID_INDEX",
           MPI_T_ERR_INVALID_INDEX
               == MPI_T_event_get_info(0, NULL, NULL, NULL, NULL, NULL, &num, NULL, NULL, NULL,
                                       NULL, NULL));
    expect("event_get_index -> INVALID_NAME",
           MPI_T_ERR_INVALID_NAME == MPI_T_event_get_index("anything", &idx));

    /* A valid category still reports zero events. */
    if (MPI_SUCCESS == MPI_T_category_get_num_events(0, &nevents)) {
        expect("category_get_num_events(valid) == 0", 0 == nevents);
    }

    MPI_T_finalize();

    printf("RESULT: %s (%d failures)\n", 0 == failures ? "PASS" : "FAIL", failures);
    return 0 == failures ? 0 : 1;
}
