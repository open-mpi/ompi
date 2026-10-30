/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#if !defined(OPAL_MEMORY_KDREG2_H)
#    define OPAL_MEMORY_KDREG2_H

#    include "opal_config.h"

#    include "opal/mca/memory/memory.h"

typedef struct opal_memory_kdreg2_component_t {
    opal_memory_base_component_2_0_0_t super;
} opal_memory_kdreg2_component_t;

OPAL_DECLSPEC extern opal_memory_kdreg2_component_t mca_memory_kdreg2_component;

#endif /* !defined(OPAL_MEMORY_KDREG2_H) */
