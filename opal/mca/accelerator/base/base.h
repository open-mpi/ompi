/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_ACCELERATOR_BASE_H
#define MCA_ACCELERATOR_BASE_H

#include "opal_config.h"

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/mca/mca.h"


BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_accelerator_base_framework;

/**
 * Select an accelerator module
 */
OPAL_DECLSPEC int opal_accelerator_base_select(void);

OPAL_DECLSPEC extern opal_accelerator_base_component_t opal_accelerator_base_selected_component;

END_C_DECLS

#endif
