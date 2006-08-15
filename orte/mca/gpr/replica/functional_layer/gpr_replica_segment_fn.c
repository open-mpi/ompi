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
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "opal/class/opal_object.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"


int orte_gpr_replica_find_containers(orte_gpr_replica_segment_t *seg,
                                     orte_gpr_replica_addr_mode_t addr_mode,
                                     orte_gpr_replica_itag_t *taglist, orte_std_cntr_t num_tags)
{
    orte_gpr_replica_container_t **cptr;
    orte_std_cntr_t i, j, index;

    OPAL_TRACE(3);

    /* ensure the search array is clear */
    orte_pointer_array_clear(orte_gpr_replica_globals.srch_cptr);
    orte_gpr_replica_globals.num_srch_cptr = 0;

    cptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
    for (i=0, j=0; j < seg->num_containers &&
                   i < (seg->containers)->size; i++) {
        if (NULL != cptr[i]) {
            j++;
            if (orte_gpr_replica_check_itag_list(addr_mode,
                                             num_tags, taglist,
                                             cptr[i]->num_itags, cptr[i]->itags)) {
                if (0 > orte_pointer_array_add(&index, orte_gpr_replica_globals.srch_cptr, cptr[i])) {
                    ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                    orte_pointer_array_clear(orte_gpr_replica_globals.srch_cptr);
                    return ORTE_ERR_OUT_OF_RESOURCE;
                }
                (orte_gpr_replica_globals.num_srch_cptr)++;
            }
        }
    }
    return ORTE_SUCCESS;
}


int orte_gpr_replica_create_container(orte_gpr_replica_container_t **cptr,
                                      orte_gpr_replica_segment_t *seg,
                                      orte_std_cntr_t num_itags,
                                      orte_gpr_replica_itag_t *itags)
{
    int rc;
    orte_std_cntr_t index;

    OPAL_TRACE(3);

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

    if (0 > orte_pointer_array_add(&index, seg->containers, (void*)(*cptr))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (seg->num_containers)++;

    (*cptr)->index = index;
    return ORTE_SUCCESS;
}


int orte_gpr_replica_release_container(orte_gpr_replica_segment_t *seg,
                                       orte_gpr_replica_container_t *cptr)
{
    orte_gpr_replica_itagval_t **iptr;
    orte_std_cntr_t i;
    int rc;

    OPAL_TRACE(3);

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
    i = cptr->index;
    OBJ_RELEASE(cptr);
    orte_pointer_array_set_item(seg->containers, i, NULL);
    (seg->num_containers)--;

    /* if the segment is now empty of containers, release it too */
    if (0 == seg->num_containers) {
        if (ORTE_SUCCESS != (rc = orte_gpr_replica_release_segment(&seg))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return ORTE_SUCCESS;
}


int orte_gpr_replica_add_keyval(orte_gpr_replica_itagval_t **ivalptr,
                                orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_container_t *cptr,
                                orte_gpr_keyval_t *kptr)
{
    orte_gpr_replica_itagval_t *iptr;
    int rc;

    OPAL_TRACE(3);

    /* protect against dumb errors - caller must at least provide us with a key */
    if (NULL == kptr || NULL == kptr->key) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    iptr = OBJ_NEW(orte_gpr_replica_itagval_t);
    if (NULL == iptr) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    iptr->value = OBJ_NEW(orte_data_value_t);
    if (NULL == iptr->value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(iptr);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&(iptr->itag),
                                            seg, kptr->key))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(iptr);
        return rc;
    }

    /* it is perfectly acceptable to give us a keyval that doesn't have a value. For
     * example, we may want to predefine a location when we setup a trigger, then actually
     * put a value in it later.
    */
    if (NULL != kptr->value) {
        iptr->value->type = kptr->value->type;
        if (NULL != kptr->value->data) {
            if (ORTE_SUCCESS != (rc = orte_dss.copy(&((iptr->value)->data), kptr->value->data, kptr->value->type))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(iptr);
                return rc;
            }
        }
    }

    if (0 > orte_pointer_array_add(&(iptr->index), cptr->itagvals, (void*)iptr)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(iptr);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (cptr->num_itagvals)++;

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
    orte_std_cntr_t i;
    int rc;

    OPAL_TRACE(3);

    /* record that we are going to do this
     * NOTE: it is important that we make the record BEFORE doing the release.
     * The record_action function will do a RETAIN on the object so it
     * doesn't actually get released until we check subscriptions to see
     * if someone wanted to be notified if/when this object was released
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_record_action(seg, cptr, iptr,
                            ORTE_GPR_REPLICA_ENTRY_DELETED))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* remove the itag value from the container's list */
    for (i=0; i < orte_value_array_get_size(&(cptr->itaglist)); i++) {
        if (iptr->itag == ORTE_VALUE_ARRAY_GET_ITEM(&(cptr->itaglist), orte_gpr_replica_itag_t, i)) {
            orte_value_array_remove_item(&(cptr->itaglist), i);
            goto MOVEON;
        }
    }
    ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    return ORTE_ERR_NOT_FOUND;

MOVEON:
    /* release the data storage */
    i = iptr->index;
    OBJ_RELEASE(iptr);

    /* remove the entry from the container's itagval array */
    orte_pointer_array_set_item(cptr->itagvals, i, NULL);
    (cptr->num_itagvals)--;

    /* NOTE: If the container is now empty, *don't* remove it here
     * This is cause improper recursion if called from orte_gpr_replica_release_container
     */

    return ORTE_SUCCESS;
}


int orte_gpr_replica_update_keyval(orte_gpr_replica_itagval_t **iptr2,
                                   orte_gpr_replica_segment_t *seg,
                                   orte_gpr_replica_container_t *cptr,
                                   orte_gpr_keyval_t *kptr)
{
    orte_std_cntr_t i, j, k;
    int rc;
    orte_pointer_array_t *ptr;
    orte_gpr_replica_itagval_t *iptr;

    OPAL_TRACE(3);

    ptr = orte_gpr_replica_globals.srch_ival;

    /* record the error value */
    *iptr2 = NULL;

    /* for each item in the search array, delete it */
    for (i=0; i < ptr->size; i++) {
        if (NULL != ptr->addr[i]) {
            iptr = (orte_gpr_replica_itagval_t*)ptr->addr[i];
            /* release the data storage */
            j = iptr->index;
            /* DON'T RECORD THE ACTION - THIS WILL PREVENT US FROM SENDING
             * BOTH THE OLD AND THE NEW DATA BACK ON A SUBSCRIPTION
             * REQUEST
             */
            /* remove the itag value from the container's list */
            for (k=0; k < orte_value_array_get_size(&(cptr->itaglist)); k++) {
                if (iptr->itag == ORTE_VALUE_ARRAY_GET_ITEM(&(cptr->itaglist), orte_gpr_replica_itag_t, k)) {
                    orte_value_array_remove_item(&(cptr->itaglist), k);
                    goto MOVEON;
                }
            }
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;

MOVEON:
            OBJ_RELEASE(iptr);
            /* remove the entry from the container's itagval array */
            orte_pointer_array_set_item(cptr->itagvals, j, NULL);
            (cptr->num_itagvals)--;
        }
    }

    /* now add new item in their place */
   if (ORTE_SUCCESS != (rc = orte_gpr_replica_add_keyval(&iptr, seg, cptr, kptr))) {
       ORTE_ERROR_LOG(rc);
       return rc;
   }

    /* record that we did this */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_record_action(seg, cptr, iptr,
                                    ORTE_GPR_REPLICA_ENTRY_CHANGED |
                                    ORTE_GPR_REPLICA_ENTRY_CHG_TO))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }


   /* update any storage locations that were pointing to these items */
   if (ORTE_SUCCESS != (rc = orte_gpr_replica_update_storage_locations(iptr))) {
       ORTE_ERROR_LOG(rc);
       return rc;
   }

   /* return the location of the new iptr */
   *iptr2 = iptr;

   return ORTE_SUCCESS;
}


int orte_gpr_replica_search_container(orte_gpr_replica_addr_mode_t addr_mode,
                                      orte_gpr_replica_itag_t *itags, orte_std_cntr_t num_itags,
                                      orte_gpr_replica_container_t *cptr)
{
    orte_gpr_replica_itagval_t **ptr;
    orte_std_cntr_t i, j, index;

    OPAL_TRACE(3);

    /* ensure the search array is clear */
    orte_pointer_array_clear(orte_gpr_replica_globals.srch_ival);
    orte_gpr_replica_globals.num_srch_ival = 0;

    /* check list of itags in container to see if there is a match according
     * to addr_mode spec
     */
    if (orte_gpr_replica_check_itag_list(addr_mode, num_itags, itags,
            orte_value_array_get_size(&(cptr->itaglist)),
            ORTE_VALUE_ARRAY_GET_BASE(&(cptr->itaglist), orte_gpr_replica_itag_t))) {
        /* there is! so now collect those values into the search array */
        ptr = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
        for (i=0, j=0; j < cptr->num_itagvals &&
                       i < (cptr->itagvals)->size; i++) {
            if (NULL != ptr[i]) {
                j++;
                if (orte_gpr_replica_check_itag_list(ORTE_GPR_REPLICA_OR,
                                                 num_itags, itags,
                                                 1, &(ptr[i]->itag))) {

                    if (0 > orte_pointer_array_add(&index, orte_gpr_replica_globals.srch_ival, ptr[i])) {
                        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
                        orte_pointer_array_clear(orte_gpr_replica_globals.srch_ival);
                        return ORTE_ERR_OUT_OF_RESOURCE;
                    }
                    (orte_gpr_replica_globals.num_srch_ival)++;
                }
            }
        }
    }

    return ORTE_SUCCESS;
}


bool orte_gpr_replica_value_in_container(orte_gpr_replica_container_t *cptr,
                                      orte_gpr_replica_itagval_t *iptr)
{
    orte_gpr_replica_itagval_t **ptr;
    orte_std_cntr_t i, j;

    ptr = (orte_gpr_replica_itagval_t**)((cptr->itagvals)->addr);
    for (i=0, j=0; j < cptr->num_itagvals &&
                   i < (cptr->itagvals)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            if ((ptr[i]->itag == iptr->itag) && (ptr[i]->value->type == iptr->value->type)) {
                if (ORTE_EQUAL == orte_dss.compare(ptr[i]->value->data, iptr->value->data, iptr->value->type)) {
                    return true;
                }
            }
        }
    }

    return false;
}

int orte_gpr_replica_release_segment(orte_gpr_replica_segment_t **seg)
{
    int rc;
    orte_std_cntr_t i;

    OPAL_TRACE(3);

    i = (*seg)->itag;
    OBJ_RELEASE(*seg);

    if (0 > (rc = orte_pointer_array_set_item(orte_gpr_replica.segments, i, NULL))) {
        return rc;
    }
    (orte_gpr_replica.num_segs)--;

    return ORTE_SUCCESS;
}

int orte_gpr_replica_purge_itag(orte_gpr_replica_segment_t *seg,
                                orte_gpr_replica_itag_t itag)
{
    OPAL_TRACE(3);

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
