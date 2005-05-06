/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "mca/errmgr/errmgr.h"
#include "dps/dps_internal.h"

#include "mca/rmgr/base/base.h"

/*
 * APP_CONTEXT
 */
int orte_rmgr_base_unpack_app_context(orte_buffer_t *buffer, void *dest,
                                 size_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_app_context_t **app_context;
    size_t i, max_n=1, temp;
    
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
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context[i]->idx),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the application name */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context[i]->app),
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of processes */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context[i]->num_procs),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* get the number of argv strings */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context[i]->argc),
                    &max_n, ORTE_INT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are argv strings, allocate the required space for the char * pointers */
        if (0 < app_context[i]->argc) {
            app_context[i]->argv = (char **)malloc((app_context[i]->argc+1) * sizeof(char*));
            if (NULL == app_context[i]->argv) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->argv[app_context[i]->argc] = NULL;

            /* and unpack them */
            temp = (size_t)app_context[i]->argc;
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, app_context[i]->argv,
                        (size_t*)(&temp), ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (INT_MAX < temp) {
                ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
                return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
            }
            app_context[i]->argc = (int)temp;
        }
        
        /* get the number of env strings */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context[i]->num_env),
                    &max_n, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are env strings, allocate the required space for the char * pointers */
        if (0 < app_context[i]->num_env) {
            app_context[i]->env = (char **)malloc((app_context[i]->num_env+1) * sizeof(char*));
            if (NULL == app_context[i]->env) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            app_context[i]->env[app_context[i]->num_env] = NULL;
    
            /* and unpack them */
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, app_context[i]->env,
                        &(app_context[i]->num_env), ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* unpack the cwd */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &app_context[i]->cwd,
                    &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* unpack the map data */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context[i]->num_map),
                   &max_n, DPS_TYPE_SIZE_T))) {
            return rc;
        }

        if (app_context[i]->num_map > 0) {
            app_context[i]->map_data = malloc(sizeof(orte_app_context_map_t*) * app_context[i]->num_map);
            if (NULL == app_context[i]->map_data) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, app_context[i]->map_data,
                        &(app_context[i]->num_map), ORTE_APP_CONTEXT_MAP))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

    }

    return ORTE_SUCCESS;
}

/*
 * APP_CONTEXT_MAP
 */
int orte_rmgr_base_unpack_app_context_map(orte_buffer_t *buffer, void *dest,
                                  size_t *num_vals, orte_data_type_t type)
{
    int rc;
    orte_app_context_map_t **app_context_map;
    size_t i, max_n=1;

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
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context_map[i]->map_type),
                   &max_n, ORTE_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* map data */
        if (ORTE_SUCCESS != (rc = orte_dps_unpack_buffer(buffer, &(app_context_map[i]->map_data),
                   &max_n, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}

