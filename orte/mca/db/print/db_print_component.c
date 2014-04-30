/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/db/db.h"
#include "orte/mca/db/base/base.h"
#include "db_print.h"

static int component_register(void);
static bool component_avail(void);
static orte_db_base_module_t *component_create(opal_list_t *props);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_db_base_component_t mca_db_print_component = {
    {
        ORTE_DB_BASE_VERSION_2_0_0,

        /* Component name and version */
        "print",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL,
        NULL,
        component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    5,
    component_avail,
    component_create,
    NULL
};

static char *filename;

static int component_register(void)
{
    filename = NULL;
    (void) mca_base_component_var_register (&mca_db_print_component.base_version,
                                            "file", "Print to the indicated file (- => stdout, + => stderr)",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &filename);

    return ORTE_SUCCESS;
}

static bool component_avail(void)
{
    /* always available */
    return true;
}

static orte_db_base_module_t *component_create(opal_list_t *props)
{
    mca_db_print_module_t *mod;
    opal_value_t *kv;
    bool found;
    char *file;

    /* if the props include a filename, then use it */
    found = false;
    if (NULL != props) {
        OPAL_LIST_FOREACH(kv, props, opal_value_t) {
            if (0 == strcmp(kv->key, "printfile")) {
                file = kv->data.string;
            }
        }
    }
    if (!found) {
        /* otherwise, fall back to the default */
        if (NULL != filename) {
            file = filename;
        } else {
            /* nothing for us to do */
            return NULL;
        }
    }
    mod = (mca_db_print_module_t*)malloc(sizeof(mca_db_print_module_t));
    if (NULL == mod) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    /* copy the APIs across */
    memcpy(mod, &mca_db_print_module.api, sizeof(orte_db_base_module_t));

    /* set the globals */
    mod->file = strdup(file);
    mod->fp = NULL;

    /* let the module init */
    if (ORTE_SUCCESS != mod->api.init((struct orte_db_base_module_t*)mod)) {
        free(mod);
        return NULL;
    }

    return (orte_db_base_module_t*)mod;
}

