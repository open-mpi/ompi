/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_MEMORY_PATCHER_H)
#define OPAL_MEMORY_PATCHER_H

#include "opal_config.h"

#include "opal/mca/memory/memory.h"
#include "opal/mca/patcher/patcher.h"

typedef struct opal_memory_patcher_component_t {
  opal_memory_base_component_2_0_0_t super;
} opal_memory_patcher_component_t;

extern opal_memory_patcher_component_t mca_memory_patcher_component;

#endif /* !defined(OPAL_MEMORY_PATCHER_H) */
