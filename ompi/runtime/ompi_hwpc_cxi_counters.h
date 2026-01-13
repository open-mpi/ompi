/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * SPDX-FileCopyrightText:  Copyright Hewlett Packard Enterprise Development LP
 * SPDX-License-Identifier:  MIT
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_HWPC_CXI_COUNTERS_H
#define OMPI_HWPC_CXI_COUNTERS_H

#include "ompi_config.h"

#include "mpi.h"
#include <stdio.h>

/* OMPI HWPC CXI Public API Utility Functions */
void ompi_hwpc_cxi_init(void);
void ompi_hwpc_cxi_fini(void);

#if HWPC_CXI_ENABLE == 1 // HWPCs for HPE's Cassini (CXI) devices are enabled 

#define HWPC_CXI_INIT()  \
    ompi_hwpc_cxi_init()

#define HWPC_CXI_FINI()  \
    ompi_hwpc_cxi_fini()

#else /* HWPCs for HPE's Cassini (CXI) devices are not enabled */
#define HWPC_CXI_INIT()  \
    ((void)0)

#define HWPC_CXI_FINI()  \
    ((void)0)
#endif

#endif /* HWPC_CXI_COUNTERS_H */
