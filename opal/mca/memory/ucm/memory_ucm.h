/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#if !defined(OPAL_MEMORY_UCM_H)
#    define OPAL_MEMORY_UCM_H

#    include "opal_config.h"

#    include "opal/mca/memory/memory.h"

typedef struct opal_memory_ucm_component_t {
    opal_memory_base_component_2_0_0_t super;
} opal_memory_ucm_component_t;

extern opal_memory_ucm_component_t mca_memory_ucm_component;

#endif /* !defined(OPAL_MEMORY_UCM_H) */
