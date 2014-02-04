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
#include "sec_basic.h"

static int sec_basic_component_open(void);
static int sec_basic_component_query(mca_base_module_t **module, int *priority);
static int sec_basic_component_close(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_sec_base_component_t mca_sec_basic_component = {
    {
        OPAL_SEC_BASE_VERSION_1_0_0,

        /* Component name and version */
        "basic",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        sec_basic_component_open,
        sec_basic_component_close,
        sec_basic_component_query,
        NULL
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};

static int sec_basic_component_open(void)
{
    return OPAL_SUCCESS;
}

static int sec_basic_component_query(mca_base_module_t **module, int *priority)
{
    /* we are the default, so set ourselves low in the priority */
    *priority = 0;
    *module = (mca_base_module_t*)&opal_sec_basic_module;
    return OPAL_SUCCESS;
}


static int sec_basic_component_close(void)
{
    return OPAL_SUCCESS;
}
