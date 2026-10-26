/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Copyright (c) 2026       Hewlett Packard Enterprise Development LP. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mpi.h"

#ifndef HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
#    define HWPC_CXI_MPI_T_PVARS_ENABLED_TEST 0
#endif

#if HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
static int check_pvar_read(int index)
{
    MPI_T_pvar_session session;
    MPI_T_pvar_handle handle;
    int count = 0;
    int result = EXIT_FAILURE;

    if (MPI_SUCCESS != MPI_T_pvar_session_create(&session)) {
        return EXIT_FAILURE;
    }
    if (MPI_SUCCESS != MPI_T_pvar_handle_alloc(session, index, NULL, &handle, &count)) {
        MPI_T_pvar_session_free(&session);
        return EXIT_FAILURE;
    }
    if (count > 0) {
        unsigned long *values = calloc((size_t) count, sizeof(*values));
        if (NULL != values) {
            if (MPI_SUCCESS == MPI_T_pvar_read(session, handle, values)
                && MPI_SUCCESS == MPI_T_pvar_read(session, handle, values)) {
                result = EXIT_SUCCESS;
            }
            free(values);
        }
    }
    if (MPI_SUCCESS != MPI_T_pvar_handle_free(session, &handle)) {
        result = EXIT_FAILURE;
    }
    if (MPI_SUCCESS != MPI_T_pvar_session_free(&session)) {
        result = EXIT_FAILURE;
    }
    return result;
}
#endif

int main(int argc, char **argv)
{
    int provided;
    int num_pvars;
    int num_hwpc_cxi_pvars = 0;
    int hwpc_cxi_pvar_index = -1;
    int rc;

#if HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
    /* Pvars are only registered for counters that exist, which needs libcxi support and a Cassini NIC. */
    if (!OMPI_HOOK_HWPC_CXI_HAVE_LIBCXI || 0 != access("/sys/class/cxi/cxi0", F_OK)) {
        printf("SKIP: no Cassini (CXI) device available\n");
        return 77;
    }
    if (0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_mpi_t_enable", "true", 1)) {
        return EXIT_FAILURE;
    }
#endif

    if (0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_report", "2", 1)
        || 0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_summary_filter_zeros", "false", 1)) {
        return EXIT_FAILURE;
    }

    rc = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != rc) {
        return EXIT_FAILURE;
    }

    rc = MPI_T_init_thread(MPI_THREAD_SINGLE, &provided);
    if (MPI_SUCCESS != rc) {
        MPI_Finalize();
        return EXIT_FAILURE;
    }

    rc = MPI_T_pvar_get_num(&num_pvars);
    if (MPI_SUCCESS != rc) {
        MPI_T_finalize();
        MPI_Finalize();
        return EXIT_FAILURE;
    }

    /* Print out the number of pvars */
    printf("Number of pvars: %d\n", num_pvars);

    int name_buffer_size = 256;
    int description_buffer_size = 1024;
    char name[name_buffer_size];
    char description[description_buffer_size];

    for (int pvar_index = 0; pvar_index < num_pvars; ++pvar_index) {
        int verbosity;
        int var_class;
        int bind;
        int readonly;
        int continuous;
        int atomic;
        MPI_Datatype datatype;
        MPI_T_enum enumtype;

        int name_length = name_buffer_size;
        int description_length = description_buffer_size;

        rc = MPI_T_pvar_get_info(pvar_index, name, &name_length, &verbosity, &var_class,
                                 &datatype, &enumtype, description, &description_length, &bind,
                                 &readonly, &continuous, &atomic);
        if (MPI_T_ERR_INVALID_INDEX == rc || MPI_T_ERR_INVALID == rc) {
            continue;
        }
        if (MPI_SUCCESS != rc) {
            MPI_T_finalize();
            MPI_Finalize();
            return EXIT_FAILURE;
        }

        printf("PVAR Index: %d Name: %s\n", pvar_index, name);
        printf("PVAR Index: %d Description: %s\n", pvar_index, description);

        if (0 == strncmp(name, "hook_hwpc_cxi_", strlen("hook_hwpc_cxi_"))) {
#if HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
            if (MPI_T_PVAR_CLASS_COUNTER != var_class || MPI_UNSIGNED_LONG != datatype
                || MPI_T_BIND_NO_OBJECT != bind || !readonly || !continuous
                || 0 == description_length || EXIT_SUCCESS != check_pvar_read(pvar_index)) {
                fprintf(stderr, "Invalid HWPC_CXI MPI_T metadata or unreadable handle: %s\n", name);
                MPI_T_finalize();
                MPI_Finalize();
                return EXIT_FAILURE;
            }
#endif
            if (0 > hwpc_cxi_pvar_index) {
                hwpc_cxi_pvar_index = pvar_index;
            }
            ++num_hwpc_cxi_pvars;
        }
    }

#if HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
    if (0 == num_hwpc_cxi_pvars) {
        fprintf(stderr, "No HWPC_CXI MPI_T pvars were registered when registration was enabled\n");
        MPI_T_finalize();
        MPI_Finalize();
        return EXIT_FAILURE;
    }

    rc = MPI_Finalize();
    if (MPI_SUCCESS != rc) {
        MPI_T_finalize();
        return EXIT_FAILURE;
    }

    int verbosity;
    int var_class;
    int bind;
    int readonly;
    int continuous;
    int atomic;
    MPI_Datatype datatype;
    MPI_T_enum enumtype;

    name_buffer_size = sizeof(name);
    description_buffer_size = sizeof(description);
    rc = MPI_T_pvar_get_info(hwpc_cxi_pvar_index, name, &name_buffer_size, &verbosity, &var_class,
                             &datatype, &enumtype, description, &description_buffer_size, &bind,
                             &readonly, &continuous, &atomic);
    if (MPI_T_ERR_INVALID_INDEX != rc && MPI_T_ERR_INVALID != rc) {
        fprintf(stderr, "HWPC_CXI MPI_T pvar remained valid after MPI_Finalize\n");
        MPI_T_finalize();
        return EXIT_FAILURE;
    }
#else
    if (0 != num_hwpc_cxi_pvars) {
        fprintf(stderr, "Found %d HWPC_CXI MPI_T pvars when registration was disabled by default\n",
                num_hwpc_cxi_pvars);
        MPI_T_finalize();
        MPI_Finalize();
        return EXIT_FAILURE;
    }
#endif

    rc = MPI_T_finalize();
    if (MPI_SUCCESS != rc) {
#if !HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
        MPI_Finalize();
#endif
        return EXIT_FAILURE;
    }

#if !HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
    rc = MPI_Finalize();
    if (MPI_SUCCESS != rc) {
        return EXIT_FAILURE;
    }
#endif

    return EXIT_SUCCESS;
}
