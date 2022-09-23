/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2022 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_ACCELERATOR_BASE_NULL_COMPONENT_H
#define MCA_ACCELERATOR_BASE_NULL_COMPONENT_H

#include "opal/mca/accelerator/accelerator.h"

BEGIN_C_DECLS

typedef struct {
    opal_accelerator_base_component_t super;
} opal_accelerator_null_component_t;

OPAL_DECLSPEC extern opal_accelerator_null_component_t mca_accelerator_null_component;

OPAL_DECLSPEC extern opal_accelerator_base_module_t opal_accelerator_null_module;

END_C_DECLS

#endif /* MCA_ACCELERATOR_BASE_NULL_COMPONENT_H */
