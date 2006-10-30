/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/util/argv.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

/*
 * APP CONTEXT
 */
int orte_rmgr_base_copy_app_context(orte_app_context_t **dest, orte_app_context_t *src, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t i;

    /* create the new object */
    *dest = OBJ_NEW(orte_app_context_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy data into it */
    (*dest)->idx = src->idx;
    if (NULL != src->app) {
        (*dest)->app = strdup(src->app);
    }
    (*dest)->num_procs = src->num_procs;
    (*dest)->argv = opal_argv_copy(src->argv);
    (*dest)->env = opal_argv_copy(src->env);
    if (NULL != src->cwd) {
        (*dest)->cwd = strdup(src->cwd);
    }
    (*dest)->user_specified_cwd = src->user_specified_cwd;
    (*dest)->num_map = src->num_map;

    if (0 < src->num_map) {
        (*dest)->map_data = (orte_app_context_map_t**)malloc(src->num_map * sizeof(orte_app_context_map_t*));
        if (NULL == (*dest)->map_data) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for (i=0; i < src->num_map; i++) {
            if (ORTE_SUCCESS != (rc = orte_rmgr_base_copy_app_context_map(&((*dest)->map_data[i]), src->map_data[i],
                                            ORTE_APP_CONTEXT_MAP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    if (NULL != src->prefix_dir) {
        (*dest)->prefix_dir = strdup(src->prefix_dir);
    }

    return ORTE_SUCCESS;
}

/*
 * APP CONTEXT MAP
 */
int orte_rmgr_base_copy_app_context_map(orte_app_context_map_t **dest, orte_app_context_map_t *src, orte_data_type_t type)
{
    /* create the new object */
    *dest = OBJ_NEW(orte_app_context_map_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* copy data into it */
    (*dest)->map_type = src->map_type;
    if (NULL != src->map_data) {
        (*dest)->map_data = strdup(src->map_data);
    }

    return ORTE_SUCCESS;
}

/*
 * ATTRIBUTE
 */
int orte_rmgr_base_copy_attribute(orte_attribute_t **dest, orte_attribute_t *src, orte_data_type_t type)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)dest, src, ORTE_GPR_KEYVAL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

/*
 * ATTRIBUTE LIST
 */
int orte_rmgr_base_copy_attr_list(opal_list_t **dest, opal_list_t *src, orte_data_type_t type)
{
    int rc;
    opal_list_item_t *item;
    orte_attribute_t *attr;
    
    /* create the new object */
    *dest = OBJ_NEW(opal_list_t);
    if (NULL == *dest) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* copy data into it */
    for (item = opal_list_get_first(src);
         item != opal_list_get_end(src);
         item = opal_list_get_next(item)) {
        if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&attr, item, ORTE_ATTRIBUTE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_list_append(*dest, &attr->super);
    }
    
    return ORTE_SUCCESS;
}

