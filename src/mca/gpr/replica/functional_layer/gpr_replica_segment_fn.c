/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "util/output.h"
#include "util/argv.h"
#include "mca/errmgr/errmgr.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "gpr_replica_fn.h"


int orte_gpr_replica_find_containers(int *num_found, orte_gpr_replica_segment_t *seg,
                                     orte_gpr_replica_addr_mode_t addr_mode,
                                     orte_gpr_replica_itag_t *taglist, int num_tags)
{
    orte_gpr_replica_container_t **cptr;
    int i;
    
    /* ensure the search array is clear */
    orte_pointer_array_clear(orte_gpr_replica_globals.srch_cptr);
    *num_found = 0;

    cptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
    for (i=0; i < (seg->containers)->size; i++) {
        if (NULL != cptr[i] && orte_gpr_replica_check_itag_list(addr_mode,
                                             num_tags, taglist,
                                             cptr[i]->num_itags, cptr[i]->itags)) {
            if (0 > orte_pointer_array_add(orte_gpr_replica_globals.srch_cptr, cptr[i])) {
                ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                orte_pointer_array_clear(orte_gpr_replica_globals.srch_cptr);
                return ORTE_ERR_OUT_OF_RESOURCE;
            }
            (*num_found)++;
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_create_container(orte_gpr_replica_container_t **cptr,
                                      orte_gpr_replica_segment_t *seg,
                                      int num_itags,
                                      orte_gpr_replica_itag_t *itags)
{
    int rc;
    
    *cptr = OBJ_NEW(orte_gpr_replica_container_t);
    if (NULL == *cptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (ORTE_SUCCESS !=
          (rc = orte_gpr_replica_copy_itag_list(&((*cptr)->itags), itags, num_itags))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(*cptr);
        return rc;
    }
    
    (*cptr)->num_itags = num_itags;
    
    if (0 > ((*cptr)->index = orte_pointer_array_add(seg->containers, (void*)(*cptr)))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_release_container(orte_gpr_replica_segment_t *seg,
                                       orte_gpr_replica_container_t *cptr)
{
    orte_gpr_replica_itagval_t **iptr;
    int i, rc;
    
    /* delete all the itagvals in the container */
    iptr = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
    for (i=0; i < (cptr->itagvals)->size; i++) {
        if (NULL != iptr[i]) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_delete_itagval(seg, cptr, iptr[i]))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }

    /* remove container from segment and release it */
    orte_pointer_array_set_item(seg->containers, cptr->index, NULL);
    OBJ_RELEASE(cptr);
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_add_keyval(orte_gpr_replica_itagval_t **ivalptr,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_container_t *cptr,
                                orte_gpr_keyval_t *kptr)
{
    orte_gpr_replica_itagval_t *iptr;
    int rc;
    
    iptr = OBJ_NEW(orte_gpr_replica_itagval_t);
    if (NULL == iptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&(iptr->itag),
                                            seg, kptr->key))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(iptr);
        return rc;
    }
    
    iptr->type = kptr->type;
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_xfer_payload(&(iptr->value),
                                               &(kptr->value), kptr->type))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(iptr);
        return rc;
    }
    
    if (0 > (iptr->index = orte_pointer_array_add(cptr->itagvals, (void*)iptr))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(iptr);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (0 > (rc = orte_value_array_append_item(&(cptr->itaglist), (void*)(&(iptr->itag))))) {
        ORTE_ERROR_LOG(rc);
        orte_pointer_array_set_item(cptr->itagvals, iptr->index, NULL);
        OBJ_RELEASE(iptr);
        return rc;
    }
    
    *ivalptr = iptr;
    return ORTE_SUCCESS;
}


int orte_gpr_replica_delete_itagval(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_replica_itagval_t *iptr)
{
    /* see if anyone cares that this value is deleted */
/*    trig = (orte_gpr_replica_triggers_t**)((orte_gpr_replica.triggers)->addr);

    for (i=0; i < (orte_gpr_replica.triggers)->size; i++) {
        if (NULL != trig[i] &&
            ORTE_GPR_REPLICA_ENTRY_DELETED & trig[i]->action) {
            sptr = (orte_gpr_replica_subscribed_data_t**)((trig[i]->subscribed_data)->addr);
            for (k=0; k < (trig[i]->subscribed_data)->size; k++) {
                if (NULL != sptr[k]) {
                    if (ORTE_SUCCESS != (rc = orte_gpr_replica_register_callback(trig[i]))) {
                        ORTE_ERROR_LOG(rc);
                        return rc;
                    }
                }
    
*/    

    /* remove the entry from the container's itagval array */
    orte_pointer_array_set_item(cptr->itagvals, iptr->index, NULL);
    
    /* release the data storage */
    OBJ_RELEASE(iptr);
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_update_keyval(orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_keyval_t *kptr)
{
    int i, rc;
    orte_pointer_array_t *ptr;
    orte_gpr_replica_itagval_t *iptr;

    ptr = orte_gpr_replica_globals.srch_ival;
    
    /* for each item in the search array, delete it */
    for (i=0; i < ptr->size; i++) {
        if (NULL != ptr->addr[i]) {
            iptr = (orte_gpr_replica_itagval_t*)ptr->addr[i];
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_delete_itagval(seg, cptr, iptr))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    /* now add new item in their place */
   if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr, kptr))) {
       ORTE_ERROR_LOG(rc);
       return rc;
   }
   
   /* update any storage locations that were pointing to these items */
   if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_storage_locations(iptr))) {
       ORTE_ERROR_LOG(rc);
       return rc;
   }
   
   return ORTE_SUCCESS;
}


int orte_gpr_replica_search_container(int *cnt, orte_gpr_replica_addr_mode_t addr_mode,
                                      orte_gpr_replica_itag_t *itags, int num_itags,
                                      orte_gpr_replica_container_t *cptr)
{
    orte_gpr_replica_itagval_t **ptr;
    int i;
    
    /* ensure the search array is clear */
    orte_pointer_array_clear(orte_gpr_replica_globals.srch_ival);
    *cnt = 0;
    
    /* check list of itags in container to see if there is a match according
     * to addr_mode spec
     */
    if (orte_gpr_replica_check_itag_list(addr_mode, num_itags, itags,
            (int)orte_value_array_get_size(&(cptr->itaglist)),
            ORTE_VALUE_ARRAY_GET_BASE(&(cptr->itaglist), orte_gpr_replica_itag_t))) {
        /* there is! so now collect those values into the search array */
        ptr = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
        for (i=0; i < (cptr->itagvals)->size; i++) {
            if (NULL != ptr[i] && orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_OR,
                                                 num_itags, itags,
                                                 1, &(ptr[i]->itag))) {
        
                if (0 > orte_pointer_array_add(orte_gpr_replica_globals.srch_ival, ptr[i])) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    orte_pointer_array_clear(orte_gpr_replica_globals.srch_ival);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                (*cnt)++;
            }
        }
    }
    
    return ORTE_SUCCESS;
}


int orte_gpr_replica_get_value(void *value, orte_gpr_replica_itagval_t *ival)
{
    orte_gpr_value_union_t *src;
    
    src = &(ival->value);
    
    switch(ival->type) {

        case ORTE_UINT8:
            *((uint8_t*)value) = src->ui8;
            break;
            
        case ORTE_UINT16:
            *((uint16_t*)value) = src->ui16;
            break;
            
        case ORTE_UINT32:
            *((uint32_t*)value) = src->ui32;
            break;
            
#ifdef HAVE_I64
        case ORTE_UINT64:
            *((uint64_t*)value) = src->ui64;
            break;
#endif

        case ORTE_INT8:
            *((int8_t*)value) = src->i8;
            break;
        
        case ORTE_INT16:
            *((int16_t*)value) = src->i16;
            break;
        
        case ORTE_INT32:
            *((int32_t*)value) = src->i32;
            break;
        
#ifdef HAVE_I64
        case ORTE_INT64:
            *((int64_t*)value) = src->i64;
            break;
#endif

        case ORTE_JOBID:
            *((orte_jobid_t*)value) = src->jobid;
            break;
            
        case ORTE_CELLID:
            *((orte_cellid_t*)value) = src->cellid;
            break;
            
        case ORTE_VPID:
            *((orte_vpid_t*)value) = src->vpid;
            break;
            
        case ORTE_NODE_STATE:
            *((orte_node_state_t*)value) = src->node_state;
            break;
            
        case ORTE_PROC_STATE:
            *((orte_proc_state_t*)value) = src->proc_state;
            break;
            
        case ORTE_EXIT_CODE:
            *((orte_exit_code_t*)value) = src->exit_code;
            break;
            
        case ORTE_NULL:
            *((uint8_t*)value) = (uint8_t)NULL;
            break;
            
        default:
            return ORTE_ERR_BAD_PARAM;
            break;
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_xfer_payload(orte_gpr_value_union_t *dest,
                                  orte_gpr_value_union_t *src,
                                  orte_data_type_t type)
{
    int i;

    switch(type) {

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
            
#ifdef HAVE_I64
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
        
#ifdef HAVE_I64
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
            dest->app_context->argv = ompi_argv_copy(src->app_context->argv);
            dest->app_context->num_env = src->app_context->num_env;
            dest->app_context->env = ompi_argv_copy(src->app_context->env);
            if(NULL != src->app_context->cwd) {
                dest->app_context->cwd = strdup(src->app_context->cwd);
            } else {
                dest->app_context->cwd = NULL;
            }
            dest->app_context->num_map = src->app_context->num_map;
            if (NULL != src->app_context->map_data) {
                dest->app_context->map_data = malloc(sizeof(orte_app_context_map_t *) * src->app_context->num_map);
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


int orte_gpr_replica_release_segment(orte_gpr_replica_segment_t **seg)
{
    int rc;
    
    if (0 > (rc = orte_pointer_array_set_item(orte_gpr_replica.segments, (*seg)->itag, NULL))) {
        return rc;
    }
    OBJ_RELEASE(*seg);
    
    return ORTE_SUCCESS;
}

int orte_gpr_replica_purge_itag(orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t itag)
{
     /*
     * Begin by looping through the segment's containers and check
     * their descriptions first - if removing this name leaves that
     * list empty, then remove the container.
     * If the container isn't to be removed, then loop through all
     * the container's keyvalue pairs and check the "key" - if
     * it matches, then remove that pair. If all pairs are removed,
     * then remove the container
     * */

    return ORTE_SUCCESS;
}
