/*
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/db/base/base.h"


int orte_db_base_store(const orte_process_name_t *proc,
                       const char *key, const void *object,
                       opal_data_type_t type)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->store) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->store(proc, key, object, type))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    if (!did_op) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int orte_db_base_store_pointer(const orte_process_name_t *proc,
                               opal_value_t *kv)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->store_pointer) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->store_pointer(proc, kv))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    if (!did_op) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int orte_db_base_fetch(const orte_process_name_t *proc,
                       const char *key, void **data,
                       opal_data_type_t type)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->fetch) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->fetch(proc, key, data, type))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    if (!did_op) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int orte_db_base_fetch_pointer(const orte_process_name_t *proc,
                               const char *key,
                               void **data, opal_data_type_t type)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->fetch_pointer) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->fetch_pointer(proc, key, data, type))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    if (!did_op) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int orte_db_base_fetch_multiple(const orte_process_name_t *proc,
                                const char *key,
                                opal_list_t *kvs)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->fetch_multiple) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->fetch_multiple(proc, key, kvs))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    if (!did_op) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}

int orte_db_base_remove_data(const orte_process_name_t *proc,
                             const char *key)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the actiove modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->remove) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->remove(proc, key))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* if we get here without performing the operation, that's an error */
    if (!did_op) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}


int orte_db_base_add_log(const char *table,
                         const opal_value_t *kvs, int nkvs)
{
    bool did_op;
    opal_list_item_t *item;
    orte_db_active_module_t *mod;
    int rc;

    /* cycle thru the active modules until one agrees to perform the op */
    did_op = false;
    for (item = opal_list_get_first(&orte_db_base.active_modules);
         item != opal_list_get_end(&orte_db_base.active_modules);
         item = opal_list_get_next(item)) {
        mod = (orte_db_active_module_t*)item;
        if (NULL == mod->module->add_log) {
            continue;
        }
        if (ORTE_SUCCESS == (rc = mod->module->add_log(table, kvs, nkvs))) {
            did_op = true;
            break;
        }
        /* modules return "next option" if they didn't perform
         * the operation - anything else is a true error.
         */
        if (ORTE_ERR_TAKE_NEXT_OPTION != rc) {
            /* don't error log it here */
            return rc;
        }
    }

    /* if we get here without performing the operation, let the caller know */
    if (!did_op) {
        /* don't error log it here */
        return ORTE_ERROR;
    }
    return ORTE_SUCCESS;
}
