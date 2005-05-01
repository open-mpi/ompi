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

#include <sys/types.h>
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "mca/errmgr/errmgr.h"
#include "dps/dps_internal.h"

#include "mca/rmgr/base/base.h"

/*
 * APP CONTEXT
 */
int orte_rmgr_base_pack_app_context(orte_buffer_t *buffer, void *src,
                                 size_t num_vals, orte_data_type_t type)
{
    int rc;
    size_t i;
    orte_app_context_t **app_context;

    /* array of pointers to orte_app_context objects - need to pack the objects a set of fields at a time */
    app_context = (orte_app_context_t**) src;

    for (i=0; i < num_vals; i++) {
        /* pack the application index (for multiapp jobs) */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(app_context[i]->idx)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the application name */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(app_context[i]->app)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of processes */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(app_context[i]->num_procs)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* pack the number of entries in the argv array */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(app_context[i]->argc)), 1, ORTE_INT))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the argv entries */
        if (0 < app_context[i]->argc) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)(app_context[i]->argv), (size_t)app_context[i]->argc, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* pack the number of entries in the enviro array */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(app_context[i]->num_env)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /* if there are entries, pack the enviro entries */
        if (0 < app_context[i]->num_env) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                            (void*)(app_context[i]->env), app_context[i]->num_env, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        
        /* pack the cwd */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                        (void*)(&(app_context[i]->cwd)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
                        
        /* Pack the map data */
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                (void*)(&(app_context[i]->num_map)), 1, DPS_TYPE_SIZE_T))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        if (app_context[i]->num_map > 0) {
            if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                     (void*)(app_context[i]->map_data), app_context[i]->num_map, ORTE_APP_CONTEXT_MAP))) {
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
                                  size_t num_vals, orte_data_type_t type)
{
    int rc;
    orte_app_context_map_t **app_context_map;
    size_t i;

    app_context_map = (orte_app_context_map_t**) src;
    for (i=0; i < num_vals; i++) {
        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                      (void*)(&(app_context_map[i]->map_type)), 1, ORTE_UINT8))) {
            ORTE_ERROR_LOG(rc);
            return rc;
         }

        if (ORTE_SUCCESS != (rc = orte_dps_pack_buffer(buffer,
                      (void*)(&(app_context_map[i]->map_data)), 1, ORTE_STRING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
         }
    }
    
    return ORTE_SUCCESS;
}
