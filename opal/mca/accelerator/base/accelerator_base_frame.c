/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/mca/accelerator/base/base.h"
#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"

/*
 * The following file was created by configure.  It contains extern
 * components and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */
#include "opal/mca/accelerator/base/static-components.h"


opal_accelerator_base_module_t opal_accelerator = {0};
opal_accelerator_base_component_t opal_accelerator_base_selected_component = {{0}};

static int opal_accelerator_base_frame_register(mca_base_register_flag_t flags)
{
    return OPAL_SUCCESS;
}

static int opal_accelerator_base_frame_close(void)
{
    return mca_base_framework_components_close(&opal_accelerator_base_framework, NULL);
}

static int opal_accelerator_base_frame_open(mca_base_open_flag_t flags)
{
    return mca_base_framework_components_open(&opal_accelerator_base_framework, flags);
}

OBJ_CLASS_INSTANCE(
    opal_accelerator_stream_t,
    opal_object_t,
    NULL,
    NULL);

OBJ_CLASS_INSTANCE(
    opal_accelerator_event_t,
    opal_object_t,
    NULL,
    NULL);

MCA_BASE_FRAMEWORK_DECLARE(opal, accelerator, "OPAL Accelerator Framework",
                           opal_accelerator_base_frame_register, opal_accelerator_base_frame_open,
                           opal_accelerator_base_frame_close, mca_accelerator_base_static_components,
                           0);
