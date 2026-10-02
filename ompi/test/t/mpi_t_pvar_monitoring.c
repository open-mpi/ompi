/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Singleton regression test for bringing up the monitoring component.
 *
 * Enabling monitoring interposes a PML wrapper whose add_procs() builds
 * the table that maps each peer to its rank in MPI_COMM_WORLD.  The PML
 * is given its process list from ompi_mpi_instance_init_common(), which
 * runs before MPI_COMM_WORLD exists, so that table has to be built from
 * the runtime's view of the job rather than from the communicator.
 * Reading it out of MPI_COMM_WORLD instead dereferenced a group that was
 * still NULL and killed the process during MPI_Init().
 *
 * This asks only for initialization to survive and for the monitoring
 * performance variables to become usable; it makes no traffic, so it
 * needs no launcher and runs as a 'make check' singleton.  The
 * multi-process behaviour of the counters themselves is covered by
 * ompi/test/monitoring/.
 */

#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NAME_LEN 256

static int failures = 0;

static void expect(const char *what, int cond)
{
    if (cond) {
        printf("PASS: %s\n", what);
    } else {
        ++failures;
        printf("FAIL: %s\n", what);
    }
}

int main(int argc, char **argv)
{
    int num_pvar = 0, found = 0, usable = 0;
    int provided = 0;

    /* Turn monitoring on the only way a singleton can: the MCA reads the
       environment during initialization.  setenv() before MPI_Init() is
       what makes this reachable without a launcher. */
    setenv("OMPI_MCA_pml_monitoring_enable", "1", 1);

    MPI_Init(&argc, &argv);
    MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);

    /* Getting here at all is the point: the crash was in MPI_Init(). */
    expect("MPI_Init() completes with monitoring enabled", 1);

    MPI_T_pvar_get_num(&num_pvar);

    for (int i = 0; i < num_pvar; ++i) {
        char name[NAME_LEN];
        int name_len = NAME_LEN, desc_len = 0, verbosity, var_class, bind;
        int readonly, continuous, atomic;
        MPI_Datatype datatype;
        MPI_T_enum enumtype;
        int rc = MPI_T_pvar_get_info(i, name, &name_len, &verbosity, &var_class,
                                     &datatype, &enumtype, NULL, &desc_len,
                                     &bind, &readonly, &continuous, &atomic);

        if (MPI_SUCCESS != rc) {
            continue;
        }
        if (NULL == strstr(name, "monitoring")) {
            continue;
        }

        ++found;

        /* A variable the component registered must also be allocatable;
           one left behind by a component that never came up would report
           MPI_T_ERR_INVALID_INDEX here. */
        MPI_T_pvar_session session;
        MPI_T_pvar_handle handle;
        int count = 0;
        void *obj = NULL;
        MPI_Comm world = MPI_COMM_WORLD;

        if (MPI_T_BIND_MPI_COMM == bind) {
            obj = &world;
        }

        if (MPI_SUCCESS != MPI_T_pvar_session_create(&session)) {
            continue;
        }
        if (MPI_SUCCESS
            == MPI_T_pvar_handle_alloc(session, i, obj, &handle, &count)) {
            ++usable;
            MPI_T_pvar_handle_free(session, &handle);
        } else {
            printf("       %s could not be allocated\n", name);
        }
        MPI_T_pvar_session_free(&session);
    }

    printf("monitoring performance variables: %d visible, %d allocatable\n",
           found, usable);

    if (0 == found) {
        /* Monitoring is a common component and may not be part of this
           build at all; there is nothing to regress against. */
        printf("RESULT: SKIP (monitoring is not available in this build)\n");
        MPI_T_finalize();
        MPI_Finalize();
        return 77;
    }

    expect("every monitoring performance variable can be allocated",
           found == usable);

    MPI_T_finalize();
    MPI_Finalize();

    printf("RESULT: %s\n", 0 == failures ? "PASS" : "FAIL");
    return 0 == failures ? 0 : 1;
}
