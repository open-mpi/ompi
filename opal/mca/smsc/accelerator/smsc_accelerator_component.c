/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2024      Advanced Micro Devices, Inc. All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "opal_config.h"

#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/accelerator/smsc_accelerator_internal.h"
#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/accelerator/base/base.h"

static int mca_smsc_accelerator_component_register(void);
static int mca_smsc_accelerator_component_open(void);
static int mca_smsc_accelerator_component_close(void);
static int mca_smsc_accelerator_component_query(void);
static mca_smsc_module_t *mca_smsc_accelerator_component_enable(void);

#define MCA_SMSC_ACCELERATOR_DEFAULT_PRIORITY 0
static const int mca_smsc_accelerator_default_priority = MCA_SMSC_ACCELERATOR_DEFAULT_PRIORITY;

mca_smsc_accelerator_component_t mca_smsc_accelerator_component = {
    .super = {
        .smsc_version = {
            MCA_SMSC_DEFAULT_VERSION("accelerator"),
            .mca_open_component = mca_smsc_accelerator_component_open,
            .mca_close_component = mca_smsc_accelerator_component_close,
            .mca_register_component_params = mca_smsc_accelerator_component_register,
        },
        .priority = MCA_SMSC_ACCELERATOR_DEFAULT_PRIORITY,
        .query = mca_smsc_accelerator_component_query,
        .enable = mca_smsc_accelerator_component_enable,
    },
};

static int mca_smsc_accelerator_component_register(void)
{
    mca_smsc_base_register_default_params(&mca_smsc_accelerator_component.super,
                                          mca_smsc_accelerator_default_priority);
    return OPAL_SUCCESS;
}

static int mca_smsc_accelerator_component_open(void)
{
    return OPAL_SUCCESS;
}

static int mca_smsc_accelerator_component_close(void)
{
    if (mca_smsc_accelerator_module.rcache) {
        (void) mca_rcache_base_module_destroy(mca_smsc_accelerator_module.rcache);
        mca_smsc_accelerator_module.rcache = NULL;
    }

    return OPAL_SUCCESS;
}

static int mca_smsc_accelerator_component_query(void)
{
    if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name,
                    "null")) {
        opal_output_verbose(10, opal_smsc_base_framework.framework_output,
                          "smsc:accelerator:component_query: accelerator component is null: disqualifying myself");
        return OPAL_ERROR;
    }

    if (!opal_accelerator.is_ipc_enabled()) {
        opal_output_verbose(10, opal_smsc_base_framework.framework_output,
                          "smsc:accelerator:component_query: accelerator component does not have support for IPC operations: disqualifying myself");
        return OPAL_ERROR;
    }

    if (1 > mca_smsc_accelerator_component.super.priority) {
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

static mca_smsc_module_t *mca_smsc_accelerator_component_enable(void)
{
    if (0 > mca_smsc_accelerator_component.super.priority) {
        return NULL;
    }

    mca_rcache_base_resources_t rcache_resources;
    mca_smsc_accelerator_module.rcache = mca_rcache_base_module_create("gpusm", NULL, &rcache_resources);
    if (NULL == mca_smsc_accelerator_module.rcache) {
        return NULL;
    }

    /* Not set. Will initialize later */
    mca_smsc_accelerator_module.device_id = MCA_ACCELERATOR_NO_DEVICE_ID;

    mca_smsc_accelerator_module.prev_smsc = mca_smsc;
    if ((NULL != mca_smsc_accelerator_module.prev_smsc) &&
        (mca_smsc_accelerator_module.prev_smsc->features & MCA_SMSC_FEATURE_CAN_MAP)) {
            mca_smsc_accelerator_module.super.features |= MCA_SMSC_FEATURE_CAN_MAP;
    }

    return &mca_smsc_accelerator_module.super;
}
