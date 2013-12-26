/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/base.h"

#include "opal/mca/common/pmi/common_pmi.h"

#include "opal/mca/db/db.h"
#include "opal/mca/db/base/base.h"
#include "db_pmi.h"

static int db_pmi_component_open(void);
static int db_pmi_component_query(opal_db_base_module_t **module,
                                  int *store_priority,
                                  int *fetch_priority,
                                  bool restrict_local);
static int db_pmi_component_close(void);
static int db_pmi_component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_db_base_component_t mca_db_pmi_component = {
    {
        OPAL_DB_BASE_VERSION_1_0_0,

        /* Component name and version */
        "pmi",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        db_pmi_component_open,
        db_pmi_component_close,
        NULL,
        db_pmi_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    db_pmi_component_query
};

/* if we are to be used, someone external will set
 * our store priority to a high level
 */
static int my_store_priority = 0;
/* fetch from us if not found elsewhere */
static int my_fetch_priority = 1;


static int db_pmi_component_open(void)
{
    return OPAL_SUCCESS;
}

static int db_pmi_component_query(opal_db_base_module_t **module,
                                  int *store_priority,
                                  int *fetch_priority,
                                  bool restrict_local)
{
    if (!restrict_local) {
        /* only use PMI if available - the ESS pmi module
         * will force our selection if we are direct-launched,
         * and the orted will turn us "off" if indirectly launched
         */
        if (mca_common_pmi_init()) {
            *store_priority = my_store_priority;
            *fetch_priority = my_fetch_priority;
            *module = &opal_db_pmi_module;
            return OPAL_SUCCESS;
        }
    }

    *store_priority = 0;
    *fetch_priority = 0;
    *module = NULL;
    return OPAL_ERROR;
}


static int db_pmi_component_close(void)
{
    mca_common_pmi_finalize();
    return OPAL_SUCCESS;
}

static int db_pmi_component_register(void)
{
    mca_base_component_t *c = &mca_db_pmi_component.base_version;

    my_store_priority = 0;
    (void) mca_base_component_var_register(c, "store_priority",
                                           "Priority dictating order in which store commands will given to database components",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_store_priority);

    my_fetch_priority = 1;
    (void) mca_base_component_var_register(c, "fetch_priority",
                                           "Priority dictating order in which fetch commands will given to database components",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &my_fetch_priority);

    return OPAL_SUCCESS;
}

