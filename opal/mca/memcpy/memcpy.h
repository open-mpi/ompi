/*
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/**
 * @file
 *
 */

#ifndef OPAL_MCA_MEMCPY_MEMCPY_H
#define OPAL_MCA_MEMCPY_MEMCPY_H

#include "opal_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

/**
 * Structure for memcpy v1.0.0 components.
 * Chained to MCA v1.0.0
 */
struct opal_memcpy_base_component_1_0_0_t {
    /** MCA base component */
    mca_base_component_t memcpyc_version;
    /** MCA base data */
    mca_base_component_data_1_0_0_t memcpyc_data;
};

/**
 * Convenience typedef
 */
typedef struct opal_memcpy_base_component_1_0_0_t opal_memcpy_base_component_1_0_0_t;

/*
 * Macro for use in components that are of type memcpy v1.0.0
 */
#define OPAL_MEMCPY_BASE_VERSION_1_0_0 \
    /* memcpy v1.0 is chained to MCA v1.0 */ \
    MCA_BASE_VERSION_1_0_0, \
    /* memcpy v1.0 */ \
    "memcpy", 1, 0, 0

#endif /* OPAL_MCA_MEMCPY_MEMCPY_H */
