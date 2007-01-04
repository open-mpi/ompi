/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * Attribute functions for the RMGR subsystem
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/class/opal_list.h"

#include "orte/dss/dss.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

/*
 * FIND ATTRIBUTE
 */
orte_attribute_t* orte_rmgr_base_find_attribute(opal_list_t* attr_list, char* key)
{
    opal_list_item_t *item;
    orte_attribute_t *kval;

    if (NULL == attr_list) {
        /* if the list is NULL, then by definition we couldn't find it! */
        return NULL;
    }
    
    for (item = opal_list_get_first(attr_list);
         item != opal_list_get_end(attr_list);
         item = opal_list_get_next(item)) {
        kval = (orte_attribute_t*)item;
        if (strcmp(key, kval->key) == 0) {
            /** found it */
            return kval; /** return a pointer to this attribute */
        }
    }

    /** didn't find it */
    return NULL;
}


/*
 * ADD ATTRIBUTE
 */
int orte_rmgr_base_add_attribute(opal_list_t* attr_list, char* key,
                                 orte_data_type_t type, void *data,
                                 bool overwrite)
{
    int rc;
    orte_gpr_keyval_t *kval;
    orte_attribute_t *attr;
    
    /* protect against NULL case */
    if (NULL == attr_list) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* see if this attribute is already present */
    if (NULL != (attr = orte_rmgr_base_find_attribute(attr_list, key))) {
            /** found it - do we want to replace this value? */
        if (overwrite) {
            /* yes - remove the existing value, we will add
             * the new value down below
             */
            opal_list_remove_item(attr_list, (opal_list_item_t*)attr);
            OBJ_RELEASE(attr);
            goto ADD_ITEM;
        }
        /* don't overwrite, so just return - it is okay for us NOT to update if
         * overwrite is set to "no"
         */
        return ORTE_SUCCESS;
    }
    
ADD_ITEM:
    /** didn't find it or replacing the old one - add the attribute */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&kval, key, type, data))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    opal_list_append(attr_list, &kval->super);
    
    return ORTE_SUCCESS;
}


/*
 * MERGE ATTRIBUTES
 */
int orte_rmgr_base_merge_attributes(opal_list_t* target, opal_list_t* source, bool override)
{
    int rc;
    opal_list_item_t *item;
    orte_attribute_t *attr;
    
    /* protect against NULL cases */
    if (NULL == target || NULL == source) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* Since the add_attribute function takes care of the override issue, we just
     * need to cycle through the source list and "add" everything to the target
     */
    for (item = opal_list_get_first(source);
         item != opal_list_get_end(source);
         item = opal_list_get_next(item)) {
        attr = (orte_attribute_t*)item;
        if (ORTE_SUCCESS != (rc = orte_rmgr_base_add_attribute(target, attr->key,
                                                               attr->value->type,
                                                               attr->value->data,
                                                               override))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
        
    return ORTE_SUCCESS;
}


/*
 * DELETE ATTRIBUTE
 */
int orte_rmgr_base_delete_attribute(opal_list_t* attr_list, char* key)
{
    opal_list_item_t *item;
    orte_attribute_t *kval;
    
    /* protect against the NULL case */
    if (NULL == attr_list) {
        return ORTE_SUCCESS;
    }
    
    for (item = opal_list_get_first(attr_list);
         item != opal_list_get_end(attr_list);
         item = opal_list_get_next(item)) {
        kval = (orte_attribute_t*)item;
        if (strcmp(key, kval->key) == 0) {
            /** found it - remove it from list */
            opal_list_remove_item(attr_list, item);
            OBJ_RELEASE(item);
            return ORTE_SUCCESS;
        }
    }
    
    /** didn't find it - don't error log this as it might not be an error */
    return ORTE_ERR_NOT_FOUND;
}


