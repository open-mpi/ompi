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

#include "hook_hwpc_cxi.h"

/*
 * These helpers are implemented in hook_hwpc_cxi_counters.c.
 * They are intentionally not part of the public hook header API.
 */
extern void ompi_hwpc_cxi_init(void);
extern void ompi_hwpc_cxi_fini(void);

int main(void)
{
    /*
     * Force QUIET mode so init/fini return immediately and can be
     * safely exercised during make check without requiring MPI runtime
     * initialization or Cassini hardware.
     */
    mca_hook_hwpc_cxi_counter_report = 0;

    ompi_hwpc_cxi_init();
    ompi_hwpc_cxi_fini();

    return EXIT_SUCCESS;
}
