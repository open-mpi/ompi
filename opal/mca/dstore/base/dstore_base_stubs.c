/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2013-2014 Intel Inc. All rights reserved
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
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/dstore/base/base.h"


int opal_dstore_base_open(const char *name)
{
    opal_dstore_handle_t *hdl;
    int index;
    opal_dstore_base_module_t *mod;

    /* create the module  */
    if (NULL != (mod = opal_dstore_base.active->create_handle())) {
        /* have our module, so create a new dstore_handle */
        hdl = OBJ_NEW(opal_dstore_handle_t);
        if (NULL != name) {
            hdl->name = strdup(name);
        }
        hdl->module = mod;
        if (0 > (index = opal_pointer_array_add(&opal_dstore_base.handles, hdl))) {
            OPAL_ERROR_LOG(index);
            OBJ_RELEASE(hdl);
        }
        return index;
    }

    /* if we get here, then we were unable to create a module
     * for this scope
     */
    return OPAL_ERROR;
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
                           const opal_identifier_t *id,
                           opal_value_t *kv)
{
    opal_dstore_handle_t *hdl;

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "storing data in %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);

    return hdl->module->store((struct opal_dstore_base_module_t*)hdl->module, id, kv);
}

void opal_dstore_base_commit(int dstorehandle,
                             const opal_identifier_t *id)
{
    opal_dstore_handle_t *hdl;

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return;
    }

    if (NULL != hdl->module->commit) {
        opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                            "committing data in %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);
        hdl->module->commit((struct opal_dstore_base_module_t*)hdl->module, id);
    }
}


int opal_dstore_base_fetch(int dstorehandle,
                           const opal_identifier_t *id,
                           const char *key,
                           opal_list_t *kvs)
{
    opal_dstore_handle_t *hdl;

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "fetching data from %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);

    return hdl->module->fetch((struct opal_dstore_base_module_t*)hdl->module, id, key, kvs);
}

int opal_dstore_base_remove_data(int dstorehandle,
                                 const opal_identifier_t *id,
                                 const char *key)
{
    opal_dstore_handle_t *hdl;

    if (NULL == (hdl = (opal_dstore_handle_t*)opal_pointer_array_get_item(&opal_dstore_base.handles, dstorehandle))) {
        OPAL_ERROR_LOG(OPAL_ERR_NOT_FOUND);
        return OPAL_ERR_NOT_FOUND;
    }

    opal_output_verbose(1, opal_dstore_base_framework.framework_output,
                        "removing data from %s dstore", (NULL == hdl->name) ? "NULL" : hdl->name);

    return hdl->module->remove((struct opal_dstore_base_module_t*)hdl->module, id, key);
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
 * opal_identifier_t.
 */
opal_dstore_proc_data_t* opal_dstore_base_lookup_proc(opal_hash_table_t *jtable, opal_identifier_t id)
{
    opal_dstore_proc_data_t *proc_data = NULL;

    opal_hash_table_get_value_uint64(jtable, id, (void**)&proc_data);
    if (NULL == proc_data) {
        /* The proc clearly exists, so create a data structure for it */
        proc_data = OBJ_NEW(opal_dstore_proc_data_t);
        if (NULL == proc_data) {
            opal_output(0, "dstore:hash:lookup_opal_proc: unable to allocate proc_data_t\n");
            return NULL;
        }
        opal_hash_table_set_value_uint64(jtable, id, proc_data);
    }
    
    return proc_data;
}

