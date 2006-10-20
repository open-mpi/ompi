/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/dss/dss_internal.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

/*
 * APP_CONTEXT
 */
int orte_rmgr_base_unpack_app_context(orte_buffer_t *buffer, void *dest,
                                 orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_app_context_t **app_context;
    orte_std_cntr_t i, max_n=1, count;
    int8_t have_prefix, user_specified;

    /* unpack into array of app_context objects */
    app_context = (orte_app_context_t**) dest;
    for (i=0; i < *num_vals; i++) {

        /* create the app_context object */
        app_context[i] = OBJ_NEW(orte_app_context_t);
        if (NULL == app_context[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* get the app index number */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(app_context[i]->idx),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the application name */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(app_context[i]->app),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of processes */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(app_context[i]->num_procs),
                    &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of argv strings that were packed */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &count, &max_n, ORTE_STD_CNTR))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

        /* if there are argv strings, allocate the required space for the char * pointers */
        if (0 < count) {
            app_context[i]->argv = (char **)malloc((count+1) * sizeof(char*));
            if (NULL == app_context[i]->argv) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->argv[count] = NULL;

            /* and unpack them */
            max_n = count;
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, app_context[i]->argv, &max_n, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* get the number of env strings */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &count, &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are env strings, allocate the required space for the char * pointers */
        if (0 < count) {
            app_context[i]->env = (char **)malloc((count+1) * sizeof(char*));
            if (NULL == app_context[i]->env) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->env[count] = NULL;

            /* and unpack them */
            max_n = count;
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, app_context[i]->env, &max_n, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* unpack the cwd */
        max_n = 1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &app_context[i]->cwd,
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the user-specified cwd flag */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &user_specified,
                    &max_n, ORTE_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (user_specified) {
            app_context[i]->user_specified_cwd = true;
        } else {
            app_context[i]->user_specified_cwd = false;
        }

        /* unpack the map data */
        max_n=1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(app_context[i]->num_map),
                   &max_n, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        if (app_context[i]->num_map > 0) {
            app_context[i]->map_data = (orte_app_context_map_t**)malloc(sizeof(orte_app_context_map_t*) * app_context[i]->num_map);
            if (NULL == app_context[i]->map_data) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, app_context[i]->map_data,
                        &(app_context[i]->num_map), ORTE_APP_CONTEXT_MAP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

        /* unpack the prefix dir if there is one */
        max_n=1;
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &have_prefix,
                   &max_n, ORTE_INT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        if (have_prefix) {
            if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &app_context[i]->prefix_dir,
                                                             &max_n, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        } else {
            app_context[i]->prefix_dir = NULL;
        }
    }

    return ORTE_SUCCESS;
}

/*
 * APP_CONTEXT_MAP
 */
int orte_rmgr_base_unpack_app_context_map(orte_buffer_t *buffer, void *dest,
                                  orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_app_context_map_t **app_context_map;
    orte_std_cntr_t i, max_n=1;

    /* unpack into array of app_context_map objects */
    app_context_map = (orte_app_context_map_t**) dest;
    for (i=0; i < *num_vals; i++) {

        /* create the app_context object */
        app_context_map[i] = OBJ_NEW(orte_app_context_map_t);
        if (NULL == app_context_map[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }

        /* map type */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(app_context_map[i]->map_type),
                   &max_n, ORTE_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* map data */
        if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &(app_context_map[i]->map_data),
                   &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}


/*
 * ATTRIBUTE
 */
int orte_rmgr_base_unpack_attribute(orte_buffer_t *buffer, void *dest,
                                    orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, dest, num_vals, ORTE_GPR_KEYVAL))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return rc;
}


/*
 * ATTRIBUTE LIST
 */
int orte_rmgr_base_unpack_attr_list(orte_buffer_t *buffer, void *dest,
                                    orte_std_cntr_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_std_cntr_t count, num_attr, i;
    orte_attribute_t *attr;
    
    count = 1;
    if(ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &num_attr, &count, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* if there are any...unpack them */
    for (i=0; i < num_attr; i++) {
        attr = OBJ_NEW(orte_attribute_t);
        if (NULL == attr) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        count = 1;
        if(ORTE_SUCCESS != (rc = orte_dss_unpack_buffer(buffer, &attr, &count, ORTE_ATTRIBUTE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        opal_list_append((opal_list_t*)dest, &attr->super);
    }
    
    return ORTE_SUCCESS;
}

