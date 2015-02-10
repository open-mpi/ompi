/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2013-2015 Intel Inc. All rights reserved
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"
#include "opal/constants.h"
#include "opal_stdint.h"

#include "opal/mca/mca.h"
#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/dstore/base/base.h"


int opal_dstore_base_open(const char *name, char* desired_components, opal_list_t *attrs)
{
    opal_dstore_handle_t *hdl;
    int index;
    opal_dstore_base_module_t *mod;
    int i;
    mca_base_component_list_item_t* cli;
    char** tokens;

    if (NULL != desired_components) {
        tokens = opal_argv_split(desired_components, ',');
        for (i = 0; NULL != tokens[i]; i++) {
            OPAL_LIST_FOREACH(cli, &opal_dstore_base.available_components, mca_base_component_list_item_t) {
                if (0 == strncmp(tokens[i], cli->cli_component->mca_component_name, strlen(tokens[i]))) {
                    if (NULL != ((opal_dstore_base_component_t*)cli->cli_component)->create_handle && NULL != (mod = ((opal_dstore_base_component_t*)cli->cli_component)->create_handle(attrs))) {
                        /* have our module, so create a new dstore_handle */
                        hdl = OBJ_NEW(opal_dstore_handle_t);
                        if (NULL != name) {
                            hdl->name = strdup(name);
                        }
                        hdl->module = mod;
                        hdl->storage_component = (opal_dstore_base_component_t*)cli->cli_component;
                        if (0 > (index = opal_pointer_array_add(&opal_dstore_base.handles, hdl))) {
                            OPAL_ERROR_LOG(index);
                            OBJ_RELEASE(hdl);
                        }
                        opal_argv_free(tokens);
                        opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                                            "Created handle for %s dstore to component %s",
                                            (NULL == hdl->name) ? "NULL" : hdl->name,
                                            cli->cli_component->mca_component_name);
                        return index;
                    }
                }
            }
        }
        opal_argv_free(tokens);
    } else {
        OPAL_LIST_FOREACH(cli, &opal_dstore_base.available_components, mca_base_component_list_item_t) {
            if (NULL != ((opal_dstore_base_component_t*)cli->cli_component)->create_handle && NULL != (mod = ((opal_dstore_base_component_t*)cli->cli_component)->create_handle(attrs))) {
                /* have our module, so create a new dstore_handle */
                hdl = OBJ_NEW(opal_dstore_handle_t);
                if (NULL != name) {
                    hdl->name = strdup(name);
                }
                hdl->module = mod;
                hdl->storage_component = (opal_dstore_base_component_t*)cli->cli_component;
                if (0 > (index = opal_pointer_array_add(&opal_dstore_base.handles, hdl))) {
                    OPAL_ERROR_LOG(index);
                    OBJ_RELEASE(hdl);
                }
                opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                                    "Created handle for %s dstore to component %s",
                                    (NULL == hdl->name) ? "NULL" : hdl->name,
                                    cli->cli_component->mca_component_name);
                return index;
            }
        }
    }

    /* if we get here, then we were unable to create a module
     * for this scope
     */
    return OPAL_ERROR;
}

int opal_dstore_base_update(int dstorehandle, opal_list_t *attrs)
{
    int rc;
    opal_dstore_handle_t *hdl;

    if (dstorehandle < 0) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    if (NULL == hdl->storage_component->update_handle) {
        return OPAL_SUCCESS;
    }

    if (OPAL_SUCCESS != (rc = hdl->storage_component->update_handle(dstorehandle, attrs))) {
        OPAL_ERROR_LOG(rc);
    }

    return rc;
}

int opal_dstore_base_close(int dstorehandle)
{
    opal_dstore_handle_t *hdl;
    int i;

    /* if the handle is -1, then close all handles */
    if (dstorehandle < 0) {
        for (i=0; i < opal_dstore_base.handles.size; i++) {
            if (NULL != (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, i))) {
                OBJ_RELEASE(hdl);
                opal_pointer_array_set_item(&opal_dstore_base.handles, i, NULL);
            }
        }
        return OPAL_SUCCESS;
    }

    /* get the datastore handle */
    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        return OPAL_ERR_NOT_FOUND;
    }
    opal_pointer_array_set_item(&opal_dstore_base.handles, dstorehandle, NULL);
    /* release the handle - this will also finalize and free the module */
    OBJ_RELEASE(hdl);

    return OPAL_SUCCESS;
}


int opal_dstore_base_store(int dstorehandle,
                           const opal_process_name_t *id,
                           opal_value_t *kv)
{
    opal_dstore_handle_t *hdl;

    if (dstorehandle < 0) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "storing data in %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);

    return hdl->module->store((struct opal_dstore_base_module_t*)hdl->module, id, kv);
}

int opal_dstore_base_fetch(int dstorehandle,
                           const opal_process_name_t *id,
                           const char *key,
                           opal_list_t *kvs)
{
    opal_dstore_handle_t *hdl;
    int rc;

    if (dstorehandle < 0) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "fetching data from %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);

    if (OPAL_SUCCESS == (rc = hdl->module->fetch((struct opal_dstore_base_module_t*)hdl->module, id, key, kvs))) {
        /* found the data, so we can just return it */
        return rc;
    }

    /* if the storage module didn't find it, then let the backfill module try
     * to retrieve it if we have one */
    if (NULL != opal_dstore_base.backfill_module) {
        rc = opal_dstore_base.backfill_module->fetch((struct opal_dstore_base_module_t*)opal_dstore_base.backfill_module, id, key, kvs);
    }
    return rc;
}

int opal_dstore_base_remove_data(int dstorehandle,
                                 const opal_process_name_t *id,
                                 const char *key)
{
    opal_dstore_handle_t *hdl;

    if (dstorehandle < 0) {
        return OPAL_ERR_NOT_INITIALIZED;
    }

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "removing data from %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);

    return hdl->module->remove((struct opal_dstore_base_module_t*)hdl->module, id, key);
}

int opal_dstore_base_get_handle(int dstorehandle, void **dhdl)
{
    opal_dstore_handle_t *hdl;

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    *dhdl = (void*)hdl;
    return OPAL_SUCCESS;
}

/**
 * Find data for a given key in a given proc_data_t
 * container.
 */
opal_value_t* opal_dstore_base_lookup_keyval(opal_dstore_proc_data_t *proc_data,
                                             const char *key)
{
    opal_value_t *kv;

    OPAL_LIST_FOREACH(kv, &proc_data->data, opal_value_t) {
        if (0 == strcmp(key, kv->key)) {
            return kv;
        }
    }
    return NULL;
}


/**
 * Find proc_data_t container associated with given
 * opal_process_name_t.
 */
opal_dstore_proc_data_t* opal_dstore_base_lookup_proc(opal_proc_table_t *ptable,
                                                      opal_process_name_t id, bool create)
{
    opal_dstore_proc_data_t *proc_data = NULL;

    opal_proc_table_get_value(ptable, id, (void**)&proc_data);
    if (NULL == proc_data && create) {
        proc_data = OBJ_NEW(opal_dstore_proc_data_t);
        if (NULL == proc_data) {
            opal_output(0, "dstore:hash:lookup_opal_proc: unable to allocate proc_data_t\n");
            return NULL;
        }
        opal_proc_table_set_value(ptable, id, proc_data);
    }
    
    return proc_data;
}

