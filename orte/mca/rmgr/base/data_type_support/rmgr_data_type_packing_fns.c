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
int orte_rmgr_base_pack_app_context(orte_buffer_t *buffer, void *src,
                                 orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    int8_t have_prefix, user_specified;
    orte_std_cntr_t i, count;
    orte_app_context_t **app_context;

    /* array of pointers to orte_app_context objects - need to pack the objects a set of fields at a time */
    app_context = (orte_app_context_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the application index (for multiapp jobs) */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(app_context[i]->idx)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the application name */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(app_context[i]->app)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of processes */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(app_context[i]->num_procs)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of entries in the argv array */
        count = opal_argv_count(app_context[i]->argv);
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)(&count), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the argv entries */
        if (0 < count) {
            if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                            (void*)(app_context[i]->argv), count, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* pack the number of entries in the enviro array */
        count = opal_argv_count(app_context[i]->env);
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)(&count), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the enviro entries */
        if (0 < count) {
            if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                (void*)(app_context[i]->env), count, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* pack the cwd */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&(app_context[i]->cwd)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the user specified cwd flag */
        if (app_context[i]->user_specified_cwd) {
            user_specified = 1;
        } else {
            user_specified = 0;
        }
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                        (void*)(&user_specified), 1, ORTE_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* Pack the map data */
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                (void*)(&(app_context[i]->num_map)), 1, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        if (app_context[i]->num_map > 0) {
            if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                     (void*)(app_context[i]->map_data), app_context[i]->num_map, ORTE_APP_CONTEXT_MAP))) {
                 ORTE_ERROR_LOG(rc);
                 return rc;
           }
        }

        /* pack the prefix dir if we have one */
        if (NULL != app_context[i]->prefix_dir) {
            have_prefix = 1;
        } else {
            have_prefix = 0;
        }

        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                                                       (void*)(&have_prefix), 1, ORTE_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        if (have_prefix) {
            if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                                                           (void*)(&(app_context[i]->prefix_dir)), 1, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    return ORTE_SUCCESS;
}

/*
 * APP CONTEXT MAP
 */
int orte_rmgr_base_pack_app_context_map(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_app_context_map_t **app_context_map;
    orte_std_cntr_t i;

    app_context_map = (orte_app_context_map_t**) src;
    for (i=0; i < num_vals; i++) {
        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                      (void*)(&(app_context_map[i]->map_type)), 1, ORTE_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
         }

        if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer,
                      (void*)(&(app_context_map[i]->map_data)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
         }
    }

    return ORTE_SUCCESS;
}


/*
 * ATTRIBUTE
 */
int orte_rmgr_base_pack_attribute(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, &src, num_vals, ORTE_GPR_KEYVAL))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/*
 * ATTRIBUTE LIST
 */
int orte_rmgr_base_pack_attr_list(orte_buffer_t *buffer, void *src,
                                  orte_std_cntr_t num_vals, orte_data_type_t type)
{
    int rc;
    opal_list_t *attrs = (opal_list_t*)src;
    opal_list_item_t *item;
    orte_std_cntr_t num_attr;
    
    /* if the list is NULL, we have zero attributes */
    if (NULL == src) {
        num_attr = 0;
    } else {
        /* get the number of attributes */
        num_attr = (orte_std_cntr_t)opal_list_get_size(attrs);        
    }
    if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)&num_attr, 1, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* if there were attributes, then pack those too */
    if (0 < num_attr) {
        for (item = opal_list_get_first(attrs);
             item != opal_list_get_end(attrs);
             item = opal_list_get_next(item)) {
            if (ORTE_SUCCESS != (rc = orte_dss_pack_buffer(buffer, (void*)item, 1, ORTE_ATTRIBUTE))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    return ORTE_SUCCESS;
}

