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

#include "class/orte_pointer_array.h"
#include "util/output.h"

#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"

#include "gpr_replica_tl.h"

int
orte_gpr_replica_create_itag(orte_gpr_replica_itag_t *itag,
                             orte_gpr_replica_segment_t *seg, char *name)
{
    orte_gpr_replica_dict_t **ptr, *new;
    int i;
    size_t len, len2;

    /* default to illegal value */
    *itag = ORTE_GPR_REPLICA_ITAG_MAX;
    
    /* if name or seg is NULL, error */
    if (NULL == name || NULL == seg) {
        return ORTE_ERR_BAD_PARAM;
    }

    len = strlen(name);
    
    /* check seg's dictionary to ensure uniqueness */
    ptr = (orte_gpr_replica_dict_t**)(seg->dict)->addr;
    for (i=0; i < (seg->dict)->size; i++) {
        if (NULL != ptr[i]) {
            len2 = strlen(ptr[i]->entry);
            if ((len == len2 && 0 == strncmp(ptr[i]->entry, name, len))) {
                /* already present */
                *itag = ptr[i]->itag;
                return ORTE_SUCCESS;
            }
        }
    }

    /* okay, name is unique - create dictionary entry */
    new = (orte_gpr_replica_dict_t*)malloc(sizeof(orte_gpr_replica_dict_t));
    if (NULL == new) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    new->entry = strdup(name);
    if (0 > (i = orte_pointer_array_add(seg->dict, (void*)new))) {
        *itag = ORTE_GPR_REPLICA_ITAG_MAX;
        free(new);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    *itag = (orte_gpr_replica_itag_t)i;
    new->itag = *itag;
    return ORTE_SUCCESS;
}


int orte_gpr_replica_delete_itag(orte_gpr_replica_segment_t *seg, char *name)
{
    orte_gpr_replica_itag_t itag;
    int rc;

    /* check for errors */
    if (NULL == name || NULL == seg) {
        return ORTE_ERR_BAD_PARAM;
    }

    /* find dictionary element to delete */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_lookup(&itag, seg, name))) {
        return rc;
    }

    /* found name in dictionary */
    /* need to search this segment's registry to find all instances
     * that name & delete them
     */
     if (ORTE_SUCCESS != (rc = orte_gpr_replica_purge_itag(seg, itag))) {
        return rc;
     }

     /* remove itag from segment dictionary */
     return orte_pointer_array_set_item(seg->dict, itag, NULL);
}


int
orte_gpr_replica_dict_lookup(orte_gpr_replica_itag_t *itag,
                             orte_gpr_replica_segment_t *seg, char *name)
{
    orte_gpr_replica_dict_t **ptr;
    int i;
    size_t len, len2;
    
    /* initialize to illegal value */
    *itag = ORTE_GPR_REPLICA_ITAG_MAX;
    
    /* protect against error */
    if (NULL == seg) {
        return ORTE_ERR_BAD_PARAM;
    }
    
    if (NULL == name) { /* just want segment token-itag pair */
        *itag = seg->itag;
        return ORTE_SUCCESS;
	}

    len = strlen(name);
    
    /* want specified token-itag pair in that segment's dictionary */
    ptr = (orte_gpr_replica_dict_t**)((seg->dict)->addr);
    for (i=0; i < (seg->dict)->size; i++) {
        if (NULL != ptr[i]) {
            len2 = strlen(ptr[i]->entry);
    	       if (len == len2 && 0 == strncmp(ptr[i]->entry, name, len)) {
                *itag = ptr[i]->itag;
                return ORTE_SUCCESS;
            }
        }
	}

    return ORTE_ERR_NOT_FOUND; /* couldn't find the specified entry */
}


int orte_gpr_replica_dict_reverse_lookup(char **name,
        orte_gpr_replica_segment_t *seg, orte_gpr_replica_itag_t itag)
{
    orte_gpr_replica_dict_t **ptr;
    orte_gpr_replica_segment_t **segptr;


    /* initialize to nothing */
    *name = NULL;

    /* protect against error (shouldn't happen) */
    if ((ORTE_GPR_REPLICA_ITAG_MAX == itag)||(0 > itag)) {
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
    ptr = (orte_gpr_replica_dict_t**)((seg->dict)->addr);
    if (NULL == ptr[itag]) {  /* dict element no longer valid */
        return ORTE_ERR_NOT_FOUND;
    }
    *name = strdup(ptr[itag]->entry);
    return ORTE_SUCCESS;

}

int
orte_gpr_replica_get_itag_list(orte_gpr_replica_itag_t **itaglist,
                    orte_gpr_replica_segment_t *seg, char **names,
                    int *num_names)
{
    char **namptr;
    int rc, i;

    *itaglist = NULL;

    /* check for errors */
    if (NULL == seg) {
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
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    namptr = names;

    for (i=0; i < (*num_names); i++) {  /* traverse array of names - ignore any NULL's */
        if (NULL != names[i]) {
            if (ORTE_SUCCESS != (rc = orte_gpr_replica_create_itag(&((*itaglist)[i]), seg, names[i]))) {
                free(*itaglist);
                *itaglist = NULL;
                return rc;
            }
        }
    }
    return ORTE_SUCCESS;
}

