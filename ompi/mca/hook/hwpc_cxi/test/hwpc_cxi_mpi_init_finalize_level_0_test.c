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

#include <stdlib.h>

#include "mpi.h"
#include "hook_hwpc_cxi.h"

int main(int argc, char **argv)
{
    mca_hook_hwpc_cxi_counter_report = 0;

    int rc = MPI_Init(&argc, &argv);
    if (MPI_SUCCESS != rc) {
        return EXIT_FAILURE;
    }

    rc = MPI_Finalize();
    if (MPI_SUCCESS != rc) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
