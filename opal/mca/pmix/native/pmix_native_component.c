/*
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "opal_config.h"

#include "opal/constants.h"
#include "opal/util/proc.h"
#include "opal/mca/pmix/pmix.h"
#include "pmix_native.h"

/*
 * Public string showing the pmix native component version number
 */
const char *opal_pmix_native_component_version_string =
    "OPAL native pmix MCA component version " OPAL_VERSION;

/*
 * Local function
 */
static int pmix_native_open(void);
static int pmix_native_close(void);
static int pmix_native_component_query(mca_base_module_t **module, int *priority);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

opal_pmix_native_component_t mca_pmix_native_component = {
    {

        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pmix v1.1.0 component (which also
               implies a specific MCA version) */
        
            OPAL_PMIX_BASE_VERSION_2_0_0,

            /* Component name and version */

            "native",
            OPAL_MAJOR_VERSION,
            OPAL_MINOR_VERSION,
            OPAL_RELEASE_VERSION,

            /* Component open and close functions */

            pmix_native_open,
            pmix_native_close,
            pmix_native_component_query,
            NULL
        },
        /* Next the MCA v1.0.0 component meta data */
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int pmix_native_open(void)
{
    /* construct the component fields */
    mca_pmix_native_component.uri = NULL;
    mca_pmix_native_component.id = opal_name_invalid;
    mca_pmix_native_component.cache_local = NULL;
    mca_pmix_native_component.cache_remote = NULL;
    mca_pmix_native_component.cache_global = NULL;
    mca_pmix_native_component.sd = -1;
    mca_pmix_native_component.state = PMIX_USOCK_UNCONNECTED;
    mca_pmix_native_component.tag = 0;
    OBJ_CONSTRUCT(&mca_pmix_native_component.send_queue, opal_list_t);
    OBJ_CONSTRUCT(&mca_pmix_native_component.posted_recvs, opal_list_t);
    mca_pmix_native_component.send_msg = NULL;
    mca_pmix_native_component.recv_msg = NULL;
    mca_pmix_native_component.send_ev_active = false;
    mca_pmix_native_component.recv_ev_active = false;
    mca_pmix_native_component.timer_ev_active = false;

    return OPAL_SUCCESS;
}

static int pmix_native_close(void)
{
    if (NULL != mca_pmix_native_component.uri) {
        free(mca_pmix_native_component.uri);
    }
    OPAL_LIST_DESTRUCT(&mca_pmix_native_component.send_queue);
    OPAL_LIST_DESTRUCT(&mca_pmix_native_component.posted_recvs);
    return OPAL_SUCCESS;
}


static int pmix_native_component_query(mca_base_module_t **module, int *priority)
{
    char *t, *id;

    /* see if a PMIx server is present */
    if (NULL == (t = getenv("PMIX_SERVER_URI")) ||
        NULL == (id = getenv("PMIX_ID"))) {
        /* we still have to be considered because this might
         * be a singleton, and even a singleton requires some
         * degree of support. So set us at a very low priority
         * so the other components can be selected it they
         * are in a better position to run */
        *priority = 1;
        mca_pmix_native_component.uri = NULL;
    } else {
        /* if PMIx is present, then we need to use it */
        opal_convert_string_to_process_name(&mca_pmix_native_component.id, id);
        mca_pmix_native_component.uri = strdup(t);
        opal_proc_set_name(&mca_pmix_native_component.id);
        *priority = 100;
    }
    *module = (mca_base_module_t *)&opal_pmix_native_module;
    return OPAL_SUCCESS;
}
