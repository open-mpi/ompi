/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * Copyright (c) 2013      Intel Inc. All rights reserved
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

#include "opal/mca/db/base/base.h"


void opal_db_base_set_id(const opal_identifier_t *proc)
{
    /* to protect alignment, copy the data across */
    memcpy(&opal_db_base.my_id, proc, sizeof(opal_identifier_t));
    opal_db_base.id_set = true;
}

int opal_db_base_store(const opal_identifier_t *proc,
                       opal_scope_t scope,
                       const char *key, const void *object,
                       opal_data_type_t type)
{
    opal_db_active_module_t *mod;
    int rc;

    if (!opal_db_base.id_set) {
        return OPAL_ERR_FATAL;
    }

    /* cycle thru the active modules until one agrees to perform the op */
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->store) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->store(proc, scope, key, object, type))) {
            return OPAL_SUCCESS;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    /* we did not perform any operation, that's an error */
    OPAL_ERROR_LOG(OPAL_ERROR);
    return OPAL_ERROR;
}

int opal_db_base_store_pointer(const opal_identifier_t *proc,
                               opal_value_t *kv)
{
    opal_db_active_module_t *mod;
    int rc;

    if (!opal_db_base.id_set) {
        return OPAL_ERR_FATAL;
    }

    /* cycle thru the active modules until one agrees to perform the op */
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->store_pointer) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->store_pointer(proc, kv))) {
            return OPAL_SUCCESS;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    /* we did not perform any operation, that's an error */
    OPAL_ERROR_LOG(OPAL_ERROR);
    return OPAL_ERROR;
}

void opal_db_base_commit(const opal_identifier_t *proc)
{
    opal_db_active_module_t *mod;

    /* cycle thru the active modules giving each a chance to perform the op */
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->commit) {
            continue;
        }
        mod->module->commit(proc);
    }
}


int opal_db_base_fetch(const opal_identifier_t *proc,
                       const char *key, void **data,
                       opal_data_type_t type)
{
    opal_db_active_module_t *mod;
    int rc, i;

    if (!opal_db_base.id_set) {
        return OPAL_ERR_FATAL;
    }

    /* cycle thru the active modules until one agrees to perform the op.
     * we cycle thru the list of modules twice - this allows us to check
     * a local store first, then attempt to obtain the data from an
     * external store that puts it in the local store
     */
    for(i = 0; i < 2; i++) {
        OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
            if (NULL == mod->module->fetch) {
                continue;
            }
            if (OPAL_SUCCESS == (rc = mod->module->fetch(proc, key, data, type))) {
                return OPAL_SUCCESS;
            }
            /* modules return "next option" if they didn't perform
             * the operation - anything else is a true error.
             */
            if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
                return rc;
            }
        }
    }

    /* if we get here without performing the operation, that's an error */
    return OPAL_ERR_DATA_VALUE_NOT_FOUND;
}

int opal_db_base_fetch_pointer(const opal_identifier_t *proc,
                               const char *key,
                               void **data, opal_data_type_t type)
{
    opal_db_active_module_t *mod;
    int rc, i;

    if (!opal_db_base.id_set) {
        return OPAL_ERR_FATAL;
    }

    /* cycle thru the active modules until one agrees to perform the op.
     * we cycle thru the list of modules twice - this allows us to check
     * a local store first, then attempt to obtain the data from an
     * external store that puts it in the local store
     */
    for(i = 0; i < 2; i++) {
        OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
            if (NULL == mod->module->fetch_pointer) {
                continue;
            }
            if (OPAL_SUCCESS == (rc = mod->module->fetch_pointer(proc, key, data, type))) {
                return OPAL_SUCCESS;
            }
            /* modules return "next option" if they didn't perform
             * the operation - anything else is a true error.
             */
            if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
                return rc;
            }
        }
    }

    /* if we get here without performing the operation, that's an error */
    return OPAL_ERR_DATA_VALUE_NOT_FOUND;
}

int opal_db_base_fetch_multiple(const opal_identifier_t *proc,
                                opal_scope_t scope,
                                const char *key,
                                opal_list_t *kvs)
{
    opal_db_active_module_t *mod;
    int rc;

    if (!opal_db_base.id_set) {
        return OPAL_ERR_FATAL;
    }

    /* cycle thru the active modules until one agrees to perform the op */
    OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
        if (NULL == mod->module->fetch_multiple) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->fetch_multiple(proc, scope, key, kvs))) {
            return OPAL_SUCCESS;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    return OPAL_ERR_DATA_VALUE_NOT_FOUND;
}

int opal_db_base_remove_data(const opal_identifier_t *proc,
                             const char *key)
{
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->remove) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->remove(proc, key))) {
            return OPAL_SUCCESS;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
            OPAL_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    OPAL_ERROR_LOG(OPAL_ERROR);
    return OPAL_ERROR;
}


int opal_db_base_add_log(const char *table,
                         const opal_value_t *kvs, int nkvs)
{
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the active modules until one agrees to perform the op */
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->add_log) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->add_log(table, kvs, nkvs))) {
            return OPAL_SUCCESS;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (OPAL_ERR_TAKE_NEXT_OPTION != rc) {
            /* don't error log it here */
            return rc;
        }
    }

    /* if we get here without performing the operation, let the caller know */
    return OPAL_ERROR;
}
