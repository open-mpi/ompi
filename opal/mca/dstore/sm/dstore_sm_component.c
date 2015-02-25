/* Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/base/base.h"
#include "opal/util/argv.h"

#include "opal/mca/dstore/dstore.h"
#include "opal/mca/dstore/base/base.h"
#include "dstore_sm.h"
#include "opal/mca/shmem/base/base.h"

static int opal_dstore_sm_enable = 0;
static int dstore_sm_query(mca_base_module_t **module, int *priority);
static opal_dstore_base_module_t *component_create(opal_list_t *attrs);
static int component_update(int hdl, opal_list_t *attributes);
static int add_trk(opal_dstore_base_module_t *imod,
        uint32_t jid, char* seg_info);
static int component_register(void);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
opal_dstore_base_component_t mca_dstore_sm_component = {
    {
        OPAL_DSTORE_BASE_VERSION_2_0_0,

        /* Component name and version */
        "sm",
        OPAL_MAJOR_VERSION,
        OPAL_MINOR_VERSION,
        OPAL_RELEASE_VERSION,

        /* Component open and close functions */
        NULL,
        NULL,
        dstore_sm_query,
        component_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    component_create,
    component_update,
    NULL
};

static int dstore_sm_query(mca_base_module_t **module, int *priority)
{
    *priority = 0;
    *module = NULL;
    return OPAL_SUCCESS;
}

static int component_register(void)
{
    opal_dstore_sm_enable = 0;
    (void)mca_base_component_var_register(&mca_dstore_sm_component.base_version, "enable",
            "Enable/disable dstore sm component (default: disabled)",
            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
            OPAL_INFO_LVL_9, MCA_BASE_VAR_SCOPE_READONLY,
            &opal_dstore_sm_enable);
    return OPAL_SUCCESS;
}

static opal_dstore_base_module_t *component_create(opal_list_t *attrs)
{
    mca_dstore_sm_module_t *mod;

    if (0 == opal_dstore_sm_enable) {
        return NULL;
    }
    mod = (mca_dstore_sm_module_t*)malloc(sizeof(mca_dstore_sm_module_t));
    if (NULL == mod) {
        OPAL_ERROR_LOG(OPAL_ERR_OUT_OF_RESOURCE);
        return NULL;
    }
    /* copy the APIs across */
    memcpy(mod, &opal_dstore_sm_module.api, sizeof(opal_dstore_base_module_t));
    /* let the module init itself */
    if (OPAL_SUCCESS != mod->api.init((struct opal_dstore_base_module_t*)mod)) {
        /* release the module and return the error */
        free(mod);
        return NULL;
    }
    return (opal_dstore_base_module_t*)mod;
}

static int component_update(int hdl, opal_list_t *attributes)
{
    opal_dstore_handle_t *handle;
    opal_dstore_base_module_t *mod;
    int rc;

    if (hdl < 0) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    if (NULL == (handle = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, hdl))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    if (NULL == attributes) {
        return OPAL_SUCCESS;
    }

    mod = handle->module;
    opal_dstore_attr_t *attr = (opal_dstore_attr_t *)opal_list_get_last(attributes);
    rc = add_trk(mod, attr->jobid, attr->connection_info);
    return rc;
}

static int add_trk(opal_dstore_base_module_t *imod,
        uint32_t jid, char* seg_info)
{
    int i;
    char** tokens;
    int num_tokens;
    opal_sm_tracker_t *trk;
    bool found_trk = false;
    num_tokens = 0;
    mca_dstore_sm_module_t *mod;

    mod = (mca_dstore_sm_module_t*)imod;
    if (NULL == seg_info) {
        return OPAL_ERROR;
    }
    OPAL_LIST_FOREACH(trk, &mod->tracklist, opal_sm_tracker_t) {
        if (trk->jobid == jid) {
            found_trk = true;
            break;
        }
    }
    if (!found_trk) {
        trk = OBJ_NEW(opal_sm_tracker_t);
        tokens = opal_argv_split(seg_info, ':');
        for (i = 0; NULL != tokens[i]; i++) {
            num_tokens++;
        }
        memset(&trk->seg_ds, 0, sizeof(opal_shmem_ds_t));
        trk->seg_ds.seg_cpid = atoi(tokens[0]);
        trk->seg_ds.seg_id = atoi(tokens[1]);
        trk->seg_ds.seg_size = strtoul(tokens[2], NULL, 10);
        trk->seg_ds.seg_base_addr = (unsigned char*)strtoul(tokens[3], NULL, 16);
        if (5 == num_tokens && NULL != tokens[4]) {
            strncpy(trk->seg_ds.seg_name, tokens[4], strlen(tokens[4])+1);
        }
        opal_argv_free(tokens);

        trk->jobid = jid;
        trk->addr = opal_shmem_segment_attach (&trk->seg_ds);
        if (NULL == trk->addr) {
            if (trk->seg_ds.seg_cpid == getpid()) {
                opal_shmem_unlink (&trk->seg_ds);
            }
            OBJ_RELEASE(trk);
            return OPAL_ERROR;
        }
        if (trk->seg_ds.seg_cpid == getpid()) {
            memset(trk->addr, 0, trk->seg_ds.seg_size);
        }
        opal_list_append(&mod->tracklist, &trk->super);
    }
    return OPAL_SUCCESS;
}

