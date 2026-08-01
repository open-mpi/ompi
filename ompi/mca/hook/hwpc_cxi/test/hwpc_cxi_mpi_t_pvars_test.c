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

#include "mpi.h"
#include "hook_hwpc_cxi.h"

#ifndef HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
#    define HWPC_CXI_MPI_T_PVARS_ENABLED_TEST 0
#endif

int main(int argc, char **argv)
{
    int provided;
    int num_pvars;
    int num_hwpc_cxi_pvars = 0;
    int rc;

#if HWPC_CXI_MPI_T_PVARS_ENABLED_TEST
    if (0 != setenv("OMPI_MCA_ompi_hook_hwpc_cxi_counter_mpi_t_enable", "true", 1)) {
        return EXIT_FAILURE;
    }
#endif

    mca_hook_hwpc_cxi_counter_report = 2;
    mca_hook_hwpc_cxi_counter_summary_filter_zeros = false;

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
        MPI_Finalize();
        return EXIT_FAILURE;
    }

    rc = MPI_Finalize();
    if (MPI_SUCCESS != rc) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
