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
#include "include/constants.h"

#include "opal/util/argv.h"
#include "opal/util/output.h"

#include "mca/errmgr/errmgr.h"

#include "mca/gpr/base/base.h"


int orte_gpr_base_xfer_payload(orte_gpr_value_union_t *dest,
                               orte_gpr_value_union_t *src,
                               orte_data_type_t type)
{
    size_t i;

    switch(type) {

        case ORTE_BOOL:
            dest->tf_flag = src->tf_flag;
            break;
            
        case ORTE_SIZE:
            dest->size = src->size;
            break;
            
        case ORTE_PID:
            dest->pid = src->pid;
            break;
            
        case ORTE_STRING:
            dest->strptr = strdup(src->strptr);
            if (NULL == dest->strptr) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            break;
            
        case ORTE_UINT8:
            dest->ui8 = src->ui8;
            break;
            
        case ORTE_UINT16:
            dest->ui16 = src->ui16;
            break;
            
        case ORTE_UINT32:
            dest->ui32 = src->ui32;
            break;
            
#ifdef HAVE_INT64_T
        case ORTE_UINT64:
            dest->ui64 = src->ui64;
            break;
#endif

        case ORTE_INT8:
            dest->i8 = src->i8;
            break;
        
        case ORTE_INT16:
            dest->i16 = src->i16;
            break;
        
        case ORTE_INT32:
            dest->i32 = src->i32;
            break;
        
#ifdef HAVE_INT64_T
        case ORTE_INT64:
            dest->i64 = src->i64;
            break;
#endif

        case ORTE_NAME:
            dest->proc = src->proc;;
            break;
            
        case ORTE_JOBID:
            dest->jobid = src->jobid;
            break;
            
        case ORTE_CELLID:
            dest->cellid = src->cellid;
            break;
            
        case ORTE_VPID:
            dest->vpid = src->vpid;
            break;
            
        case ORTE_NODE_STATE:
            dest->node_state = src->node_state;
            break;
            
        case ORTE_PROC_STATE:
            dest->proc_state = src->proc_state;
            break;
            
        case ORTE_EXIT_CODE:
            dest->exit_code = src->exit_code;
            break;
            
        case ORTE_BYTE_OBJECT:
            (dest->byteobject).size = (src->byteobject).size;
            (dest->byteobject).bytes = (uint8_t*)malloc((dest->byteobject).size);
            if (NULL == (dest->byteobject).bytes) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            memcpy((dest->byteobject).bytes, (src->byteobject).bytes, (dest->byteobject).size);
            break;

        case ORTE_APP_CONTEXT:
            if(NULL == src->app_context) {
                dest->app_context = NULL;
                break;
            }
            dest->app_context = OBJ_NEW(orte_app_context_t);
            dest->app_context->idx = src->app_context->idx;
            if(NULL != src->app_context->app) {
                dest->app_context->app = strdup(src->app_context->app);
            } else {
                dest->app_context->app = NULL;
            }
            dest->app_context->num_procs = src->app_context->num_procs;
            dest->app_context->argc = src->app_context->argc;
            dest->app_context->argv = opal_argv_copy(src->app_context->argv);
            dest->app_context->num_env = src->app_context->num_env;
            dest->app_context->env = opal_argv_copy(src->app_context->env);
            if(NULL != src->app_context->cwd) {
                dest->app_context->cwd = strdup(src->app_context->cwd);
            } else {
                dest->app_context->cwd = NULL;
            }
            dest->app_context->num_map = src->app_context->num_map;
            if (NULL != src->app_context->map_data) {
                dest->app_context->map_data = (orte_app_context_map_t **) malloc(sizeof(orte_app_context_map_t *) * src->app_context->num_map);
                for (i = 0; i < src->app_context->num_map; ++i) {
                    dest->app_context->map_data[i] = 
                        OBJ_NEW(orte_app_context_map_t);
                    dest->app_context->map_data[i]->map_type =
                        src->app_context->map_data[i]->map_type;
                    dest->app_context->map_data[i]->map_data =
                        strdup(src->app_context->map_data[i]->map_data);
                }
            } else {
                dest->app_context->map_data = NULL;
            }
            break;

        case ORTE_NULL:
            break;
            
        default:
            return ORTE_ERR_BAD_PARAM;
            break;
    }
    return ORTE_SUCCESS;
}
