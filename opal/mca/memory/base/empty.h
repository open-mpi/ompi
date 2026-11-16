/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2026      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 */

#ifndef OPAL_MCA_MEMCPY_BASE_MEMORY_BASE_DEFAULT_H
#define OPAL_MCA_MEMCPY_BASE_MEMORY_BASE_DEFAULT_H

#include "opal_config.h"

BEGIN_C_DECLS

/**
 * Default (empty) implementation of the memoryc_register function.
 *
 * See opal/mca/memory/memory.h for a description of the parameters.
 */
OPAL_DECLSPEC int opal_memory_base_component_register_empty(void *start, size_t len,
                                                            uint64_t cookie);

/**
 * Default (empty) implementation of the memoryc_deregister function
 *
 * See opal/mca/memory/memory.h for a description of the parameters.
 */
OPAL_DECLSPEC int opal_memory_base_component_deregister_empty(void *start, size_t len,
                                                              uint64_t cookie);

/**
 * Default (empty) implementation of the memoryc_set_alignment function
 *
 * See opal/mca/memory/memory.h for a description of the parameters.
 */
OPAL_DECLSPEC void opal_memory_base_component_set_alignment_empty(int use_memalign,
                                                                  size_t memalign_threshold);

END_C_DECLS

#endif
