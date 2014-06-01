/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc. All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/common/pmi/common_pmi.h"

#include "opal/mca/base/base.h"

#include "opal/mca/dstore/dstore.h"
#include "opal/mca/dstore/base/base.h"
#include "opal/runtime/opal_params.h"
#include "dstore_pmi.h"

static int dstore_pmi_component_register(void);
static bool component_avail(void);
static opal_dstore_base_module_t *component_create(void);
static void component_finalize(void);
static int setup_pmi(void);

/* local storage */
static char *pmi_kvs_name = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_dstore_base_component_t mca_dstore_pmi_component = {
    {
        OPAL_DSTORE_BASE_VERSION_2_0_0,

        /* Component name and version */
        "pmi",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL,
        NULL,
        dstore_pmi_component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    80,
    component_avail,
    component_create,
    component_finalize
};

static int dstore_pmi_component_register(void)
{
    mca_base_component_t *c = &mca_dstore_pmi_component.base_version;

    mca_dstore_pmi_component.priority = 80;
    (void) mca_base_component_var_register(c, "priority",
                                           "Priority dictating order in which components will be considered",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &mca_dstore_pmi_component.priority);
    return OPAL_SUCCESS;
}

static bool component_avail(void)
{
    /* only use PMI if available - the ESS pmi module
     * will force our selection if we are direct-launched,
     * and the orted will turn us "off" if indirectly launched
     */
    int rc = mca_common_pmi_init(opal_pmi_version);
    if ( OPAL_SUCCESS == rc && OPAL_SUCCESS == setup_pmi()) {
        return true;
    }
    /* if not, then we are not available */
    return false;
}


static void component_finalize(void)
{
    mca_common_pmi_finalize();
    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
    }
}

static opal_dstore_base_module_t *component_create(void)
{
    mca_dstore_pmi_module_t *mod;

    mod = (mca_dstore_pmi_module_t*)malloc(sizeof(mca_dstore_pmi_module_t));
    if (NULL == mod) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    /* copy the APIs across */
    memcpy(mod, &opal_dstore_pmi_module.api, sizeof(opal_dstore_base_module_t));
    /* copy the global values */
    mod->pmi_kvs_name = strdup(pmi_kvs_name);
    mod->pmi_vallen_max = pmi_vallen_max;
    mod->pmi_keylen_max = pmi_keylen_max;
    /* init the other values */
    mod->pmi_packed_data = NULL;
    mod->pmi_pack_key = 0;
    mod->pmi_packed_data_off = 0;
    OBJ_CONSTRUCT(&mod->hash_data, opal_hash_table_t);
    opal_hash_table_init(&mod->hash_data, 256);

    return (opal_dstore_base_module_t*)mod;
}

static int setup_pmi(void)
{
    int max_length, rc;

    pmi_vallen_max = mca_common_pmi_vallen();
    max_length = mca_common_pmi_kvslen();
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    rc = mca_common_pmi_kvsname(pmi_kvs_name, max_length);
    if( OPAL_SUCCESS != rc ){
        OPAL_OUTPUT_VERBOSE((1, opal_dstore_base_framework.framework_output,
                             "dstore:pmi:pmi_setup failed %s with error %s",
                             "mca_common_pmi_jobname",
                             opal_errmgr_base_pmi_error(rc)));
        return rc;
    }

    pmi_keylen_max = mca_common_pmi_keylen();

    return OPAL_SUCCESS;
}

