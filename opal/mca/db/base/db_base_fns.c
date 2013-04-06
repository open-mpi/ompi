/*
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "opal_config.h"
#include "opal/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "opal/mca/db/base/base.h"


int opal_db_base_store(opal_identifier_t proc,
                       opal_db_locality_t locality,
                       const char *key, const void *object,
                       opal_data_type_t type)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    opal_output_verbose(1, opal_db_base_framework.framework_output,
                        "db:hash:base:store storing data for proc %" PRIu64 " at locality %d",
                        proc, (int)locality);

    /* cycle thru the active modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->store) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->store(proc, locality, key, object, type))) {
            did_op = true;
            break;
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
    if (!did_op) {
        OPAL_ERROR_LOG(OPAL_ERROR);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int opal_db_base_store_pointer(opal_identifier_t proc,
                               opal_db_locality_t locality,
                               opal_value_t *kv)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->store_pointer) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->store_pointer(proc, locality, kv))) {
            did_op = true;
            break;
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
    if (!did_op) {
        OPAL_ERROR_LOG(OPAL_ERROR);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}

int opal_db_base_fetch(opal_identifier_t proc,
                       const char *key, void **data,
                       opal_data_type_t type)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
        if (NULL == mod->module->fetch) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->fetch(proc, key, data, type))) {
            did_op = true;
            break;
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
    if (!did_op) {
        OPAL_ERROR_LOG(OPAL_ERR_DATA_VALUE_NOT_FOUND);
        return OPAL_ERR_DATA_VALUE_NOT_FOUND;
    }
    return OPAL_SUCCESS;
}

int opal_db_base_fetch_pointer(opal_identifier_t proc,
                               const char *key,
                               void **data, opal_data_type_t type)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
        if (NULL == mod->module->fetch_pointer) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->fetch_pointer(proc, key, data, type))) {
            did_op = true;
            break;
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
    if (!did_op) {
        OPAL_ERROR_LOG(OPAL_ERR_DATA_VALUE_NOT_FOUND);
        return OPAL_ERR_DATA_VALUE_NOT_FOUND;
    }
    return OPAL_SUCCESS;
}

int opal_db_base_fetch_multiple(opal_identifier_t proc,
                                const char *key,
                                opal_list_t *kvs)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.fetch_order, opal_db_active_module_t) {
        if (NULL == mod->module->fetch_multiple) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->fetch_multiple(proc, key, kvs))) {
            did_op = true;
            break;
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
    if (!did_op) {
        OPAL_ERROR_LOG(OPAL_ERR_DATA_VALUE_NOT_FOUND);
        return OPAL_ERR_DATA_VALUE_NOT_FOUND;
    }
    return OPAL_SUCCESS;
}

int opal_db_base_remove_data(opal_identifier_t proc,
                             const char *key)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->remove) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->remove(proc, key))) {
            did_op = true;
            break;
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
    if (!did_op) {
        OPAL_ERROR_LOG(OPAL_ERROR);
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}


int opal_db_base_add_log(const char *table,
                         const opal_value_t *kvs, int nkvs)
{
    bool did_op;
    opal_db_active_module_t *mod;
    int rc;

    /* cycle thru the active modules until one agrees to perform the op */
    did_op = false;
    OPAL_LIST_FOREACH(mod, &opal_db_base.store_order, opal_db_active_module_t) {
        if (NULL == mod->module->add_log) {
            continue;
        }
        if (OPAL_SUCCESS == (rc = mod->module->add_log(table, kvs, nkvs))) {
            did_op = true;
            break;
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
    if (!did_op) {
        /* don't error log it here */
        return OPAL_ERROR;
    }
    return OPAL_SUCCESS;
}
