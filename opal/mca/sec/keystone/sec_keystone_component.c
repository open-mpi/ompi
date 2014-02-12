/*
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/base.h"

#include "opal/mca/sec/sec.h"
#include "opal/mca/sec/base/base.h"
#include "sec_keystone.h"

static int sec_keystone_component_open(void);
static int sec_keystone_component_query(mca_base_module_t **module, int *priority);
static int sec_keystone_component_close(void);
static int sec_keystone_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
mca_sec_keystone_component_t mca_sec_keystone_component = {
    {
        {
            OPAL_SEC_BASE_VERSION_1_0_0,

            /* Component name and version */
            "keystone",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */
            sec_keystone_component_open,
            sec_keystone_component_close,
            sec_keystone_component_query,
            sec_keystone_component_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int sec_keystone_component_open(void)
{
    return OPAL_SUCCESS;
}

static int sec_keystone_component_query(mca_base_module_t **module, int *priority)
{
    if (NULL != mca_sec_keystone_component.url) {
        /* we are the default, so set ourselves low in the priority */
        *priority = 0;
        *module = (mca_base_module_t*)&opal_sec_keystone_module;
        return OPAL_SUCCESS;
    }

    /* otherwise, we cannot be selected */
    *module = NULL;
    return OPAL_ERROR;
}


static int sec_keystone_component_close(void)
{
    return OPAL_SUCCESS;
}

static int sec_keystone_component_register(void);
{
    mca_base_component_t *c = &mca_sec_keystone_file_component.super.base_version;
    char *value;

    mca_sec_keystone_component.url = NULL;
    value = NULL;
    tmp = mca_base_component_var_register(c, "address",
                                          "Address of the Keystone server (hostname or IP)",
                                          MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY, &value);
    if (NULL != value) {
        /* we can operate */
        asprintf(&mca_sec_keystone_component.url, "http://%s/ws/v1/", value);
    }
}
