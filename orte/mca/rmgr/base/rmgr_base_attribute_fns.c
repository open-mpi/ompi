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
                                 orte_data_type_t type, void *data)
{
    int rc;
    orte_attribute_t *kval;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&kval, key, type, data))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    opal_list_append(attr_list, &kval->super);
    
    return ORTE_SUCCESS;
}

/*
 * UPDATE ATTRIBUTE
 */
int orte_rmgr_base_update_attribute(opal_list_t* attr_list, char* key,
                                    orte_data_type_t type, void *data)
{
    opal_list_item_t *item;
    orte_attribute_t *kval;
    int rc;
    
    for (item = opal_list_get_first(attr_list);
         item != opal_list_get_end(attr_list);
         item = opal_list_get_next(item)) {
        kval = (orte_attribute_t*)item;
        if (strcmp(key, kval->key) == 0) {
            /** found it - replace the value by releasing
             * this item and replacing it with a new one
             */
            opal_list_remove_item(attr_list, item);
            OBJ_RELEASE(item);
            goto ADD_ITEM;
        }
    }

ADD_ITEM:
    /** didn't find it or replacing the old one - add the attribute */
    if (ORTE_SUCCESS != (rc = orte_rmgr_base_add_attribute(attr_list, key, type, data))) {
        ORTE_ERROR_LOG(rc);
    }
    return rc;
}


/*
 * DELETE ATTRIBUTE
 */
int orte_rmgr_base_delete_attribute(opal_list_t* attr_list, char* key)
{
    opal_list_item_t *item;
    orte_attribute_t *kval;
    
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


