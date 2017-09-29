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


#ifndef OPAL_MCA_POOL_POOL_H
#define OPAL_MCA_POOL_POOL_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

BEGIN_C_DECLS

typedef int (*opal_pool_base_component_query_fn_t)(int *priority);

/**
 * Convert a handle to a string
 *
 * Arguments:
 *   handle = The string handler
 * Returns:
 *   the string associated to the handler, or NULL.
 */
typedef char* (*opal_pool_base_component_get_fn_t)(void *handle);

/**
 * Convert a string to a handle
 *
 * Arguments:
 *   str = The string
 * Returns:
 *   a handler that is associated to the string
 */
typedef void* (*opal_pool_base_component_put_fn_t)(char *str);

/**
 * Free a handle
 *
 * Arguments:
 *   handle = The string handler
 */
typedef void (*opal_pool_base_component_free_fn_t)(void *handle);

/**
 * Structure for memory components.
 */
typedef struct opal_pool_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t data;

    opal_pool_base_component_query_fn_t query;
    opal_pool_base_component_get_fn_t get;
    opal_pool_base_component_put_fn_t put;
    opal_pool_base_component_free_fn_t free;

} opal_pool_base_component_2_0_0_t;

OPAL_DECLSPEC extern opal_pool_base_component_2_0_0_t *opal_pool;

END_C_DECLS

/*
 * Macro for use in components that are of type pool
 */
#define OPAL_POOL_BASE_VERSION_2_0_0 \
    OPAL_MCA_BASE_VERSION_2_1_0("pool", 2, 0, 0)

#endif /* OPAL_MCA_POOL_POOL_H */
