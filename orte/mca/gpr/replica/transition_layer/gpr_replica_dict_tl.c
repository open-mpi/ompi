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
/** @file:
 *
 * The Open MPI general purpose registry - support functions.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "orte/class/orte_pointer_array.h"
#include "opal/class/opal_hash_table.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/gpr_replica.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

int
orte_gpr_replica_create_itag(orte_gpr_replica_itag_t *itag,
                             orte_gpr_replica_segment_t *seg, char *name)
{
    orte_gpr_replica_dict_entry_t *new_dict;
    size_t len;

    OPAL_TRACE(3);

    /* default to illegal value */
    *itag = ORTE_GPR_REPLICA_ITAG_MAX;
    
    /* if name or seg is NULL, error */
    if (NULL == name || NULL == seg) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    len = strlen(name);
    
    /* check seg's dictionary to ensure uniqueness */
    if(opal_list_get_size(&seg->dict_entries)) {
        opal_list_item_t* item;
        for(item = opal_list_get_first(&seg->dict_entries);
            item != opal_list_get_end(&seg->dict_entries);
            item = opal_list_get_next(item)) {
            orte_gpr_replica_dict_entry_t* value = (orte_gpr_replica_dict_entry_t*)item;
            if ((len == value->len && 0 == strncmp(value->entry, name, len))) {
                /* already present */
                *itag = value->itag;
                return ORTE_SUCCESS;
            }
        }
    }

    /* okay, name is unique - create dictionary entry */
    
    /* first check to see if one is available */
    if (ORTE_GPR_REPLICA_ITAG_MAX-1 < opal_list_get_size(&seg->dict_entries)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    new_dict = OBJ_NEW(orte_gpr_replica_dict_entry_t);
    if (NULL == new_dict) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    new_dict->itag = seg->dict_next_itag++;
    new_dict->entry = strdup(name);
    new_dict->len = strlen(name);

    if (OMPI_SUCCESS != opal_hash_table_set_value_uint32(&seg->dict_hash, new_dict->itag, new_dict)) {
        *itag = ORTE_GPR_REPLICA_ITAG_MAX;
        OBJ_RELEASE(new_dict);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    opal_list_append(&seg->dict_entries, &new_dict->super);
    *itag = new_dict->itag;
    return ORTE_SUCCESS;
}


int orte_gpr_replica_delete_itag(orte_gpr_replica_segment_t *seg, char *name)
{
    opal_list_item_t* item;
    orte_gpr_replica_dict_entry_t *value = NULL;
    size_t len;
    int rc;

    OPAL_TRACE(3);

    /* check for errors */
    if (NULL == name || NULL == seg) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* find dictionary element to delete */ 
    len = strlen(name);
    for(item = opal_list_get_first(&seg->dict_entries);
        item != opal_list_get_end(&seg->dict_entries);
        item = opal_list_get_next(item)) {
        value = (orte_gpr_replica_dict_entry_t*)item;
        if ((len == value->len && 0 == strncmp(value->entry, name, len))) {
            break;
        }
    }
    if(NULL == value) {
        return ORTE_SUCCESS;
    }

    /* found name in dictionary */
    /* need to search this segment's registry to find all instances
     * that name & delete them
     */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_purge_itag(seg, value->itag))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* free the dictionary element data */
    opal_hash_table_remove_value_uint32(&seg->dict_hash, value->itag);
    opal_list_remove_item(&seg->dict_entries, &value->super);
    OBJ_RELEASE(value);
    
    return ORTE_SUCCESS;
}


int
orte_gpr_replica_dict_lookup(orte_gpr_replica_itag_t *itag,
                             orte_gpr_replica_segment_t *seg, char *name)
{
    opal_list_item_t* item;
    size_t len;
    
    OPAL_TRACE(3);

    /* initialize to illegal value */
    *itag = ORTE_GPR_REPLICA_ITAG_MAX;
    
    /* protect against error */
    if (NULL == seg) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    if (NULL == name) { /* just want segment token-itag pair */
        *itag = seg->itag;
        return ORTE_SUCCESS;
	}

    len = strlen(name);
    
    /* want specified token-itag pair in that segment's dictionary */
    for(item = opal_list_get_first(&seg->dict_entries);
        item != opal_list_get_next(&seg->dict_entries);
        item = opal_list_get_next(item)) {
        orte_gpr_replica_dict_entry_t* value = (orte_gpr_replica_dict_entry_t*)item;
    	if (len == value->len && 0 == strncmp(value->entry, name, len)) {
            *itag = value->itag;
            return ORTE_SUCCESS;
        }
	}

    return ORTE_ERR_NOT_FOUND; /* couldn't find the specified entry */
}


int orte_gpr_replica_dict_reverse_lookup(char **name,
        orte_gpr_replica_segment_t *seg, orte_gpr_replica_itag_t itag)
{
    orte_gpr_replica_dict_entry_t *value;
    orte_gpr_replica_segment_t **segptr;

    OPAL_TRACE(3);

    /* initialize to nothing */
    *name = NULL;

    /* protect against error (shouldn't happen) */
    if (ORTE_GPR_REPLICA_ITAG_MAX == itag) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    if (NULL == seg) {
	   /* return the segment name
        * note that itag is the index of the segment in that array
        */
        segptr = (orte_gpr_replica_segment_t**)(orte_gpr_replica.segments->addr);
        if (NULL == segptr[itag]) { /* this segment is no longer alive */
            return ORTE_ERR_NOT_FOUND;
        }
	   *name = strdup(segptr[itag]->name);
	   return ORTE_SUCCESS;
    }

    /* seg is provided - find the matching token for this itag
     * note again that itag is the index into this segment's
     * dictionary array
     */

    if(ORTE_SUCCESS == opal_hash_table_get_value_uint32(&seg->dict_hash, itag, (void**)&value)) {
        *name = strdup(value->entry);
        return ORTE_SUCCESS;
    }
    /* get here if entry not found */
    return ORTE_ERR_NOT_FOUND;
}

int
orte_gpr_replica_get_itag_list(orte_gpr_replica_itag_t **itaglist,
                    orte_gpr_replica_segment_t *seg, char **names,
                    size_t *num_names)
{
    char **namptr;
    int rc;
    size_t i;

    OPAL_TRACE(3);

    *itaglist = NULL;

    /* check for errors */
    if (NULL == seg) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    
    /* check for wild-card case */
    if (NULL == names) {
	   return ORTE_SUCCESS;
    }

    if (0 >= (*num_names)) { /* NULL-terminated list - count them */
        *num_names = 0;
        namptr = names;
        while (NULL != *namptr) {
	       *num_names = (*num_names) + 1;
	       namptr++;
        }
    }

    *itaglist = (orte_gpr_replica_itag_t*)malloc((*num_names)*sizeof(orte_gpr_replica_itag_t));
    if (NULL == *itaglist) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    namptr = names;

    for (i=0; i < (*num_names); i++) {  /* traverse array of names - ignore any NULL's */
        if (NULL != names[i]) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&((*itaglist)[i]), seg, names[i]))) {
                ORTE_ERROR_LOG(rc);
                free(*itaglist);
                *itaglist = NULL;
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

