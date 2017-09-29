/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_POOL_TRIVIAL_H)
#define OPAL_POOL_TRIVIAL_H

#include "opal_config.h"

#include "opal/mca/pool/pool.h"

typedef struct opal_pool_trivial_component_t {
  opal_pool_base_component_2_0_0_t super;
} opal_pool_trivial_component_t;

extern opal_pool_trivial_component_t mca_pool_trivial_component;

#endif /* !defined(OPAL_POOL_TRIVIAL_H) */
