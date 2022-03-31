/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2006 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016-2021 IBM Corporation. All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef OMPI_MCA_OSC_BASE_H
#define OMPI_MCA_OSC_BASE_H

#include "ompi_config.h"
#include "ompi/info/info.h"
#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "opal/mca/base/base.h"

/*
 * Global functions for MCA overall collective open and close
 */
BEGIN_C_DECLS

/*
 * function definitions
 */
int ompi_osc_base_find_available(bool enable_progress_threads,
                                 bool enable_mpi_threads);

void ompi_osc_base_set_memory_alignment(struct opal_info_t *info,
                                        size_t *memory_alignment);

int ompi_osc_base_select(ompi_win_t *win,
                         void **base,
                         size_t size,
                         int disp_unit,
                         ompi_communicator_t *comm,
                         int flavor,
                         int *model);

OMPI_DECLSPEC extern mca_base_framework_t ompi_osc_base_framework;


/* Helper to check whether osc can support atomic operation on the size the operands
 * Currently used with rdma and ucx.
 */
static inline __opal_attribute_always_inline__ bool ompi_osc_base_is_atomic_size_supported(uint64_t remote_addr,
                                                                                           size_t size)
{
    return ((sizeof(uint32_t) == size && !(remote_addr & 0x3)) ||
            (sizeof(uint64_t) == size && !(remote_addr & 0x7)));
}

END_C_DECLS

#endif
