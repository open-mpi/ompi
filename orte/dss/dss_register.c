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

#include "orte_config.h"
#include "orte/orte_types.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"

#include "orte/dss/dss.h"
#include "orte/dss/dss_internal.h"


int orte_dss_register(orte_dss_pack_fn_t pack_fn,
                      orte_dss_unpack_fn_t unpack_fn,
                      orte_dss_copy_fn_t copy_fn,
                      orte_dss_compare_fn_t compare_fn,
                      orte_dss_size_fn_t size_fn,
                      orte_dss_print_fn_t print_fn,
                      orte_dss_release_fn_t release_fn,
                      bool structured,
                      const char *name, orte_data_type_t *type)
{
    int ret;
    orte_dss_type_info_t *info, **ptr;
    orte_std_cntr_t i;
    orte_data_type_t j;

    /* Check for bozo cases */

    if (NULL == pack_fn || NULL == unpack_fn || NULL == copy_fn || NULL == compare_fn ||
        NULL == size_fn || NULL == print_fn || NULL == name || NULL == type) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* check if this entry already exists - if so, error - we do NOT allow multiple type registrations */
    ptr = (orte_dss_type_info_t**)(orte_dss_types->addr);
    for (i=0, j=0; j < orte_dss_num_reg_types &&
         i < orte_dss_types->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            if (0 == strcmp(ptr[i]->odti_name, name)) {
                ORTE_ERROR_LOG(ORTE_ERR_DATA_TYPE_REDEF);
                return ORTE_ERR_DATA_TYPE_REDEF;
            }
        }
    }

    /* if type is given (i.e., *type > 0), then just use it.
     * otherwise, go and get a new type id from the name
     * service
     */
    if (0 >= *type) {
        if (ORTE_SUCCESS != (ret = orte_ns.define_data_type(name, type))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* Add a new entry to the table */
    info = (orte_dss_type_info_t*) OBJ_NEW(orte_dss_type_info_t);
    if (NULL == info) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    info->odti_type = *type;
    info->odti_name = strdup(name);
    info->odti_pack_fn = pack_fn;
    info->odti_unpack_fn = unpack_fn;
    info->odti_copy_fn = copy_fn;
    info->odti_compare_fn = compare_fn;
    info->odti_size_fn = size_fn;
    info->odti_print_fn = print_fn;
    info->odti_release_fn = release_fn;
    info->odti_structured = structured;
    if (ORTE_SUCCESS != (ret = orte_pointer_array_set_item(orte_dss_types, *type, info))) {
        ORTE_ERROR_LOG(ret);
    }

    /* All done */

    return ret;
}
