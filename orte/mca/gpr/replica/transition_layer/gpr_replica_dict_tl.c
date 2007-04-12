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

#include "orte/class/orte_pointer_array.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"

#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

int
orte_gpr_replica_create_itag(orte_gpr_replica_itag_t *itag,
                             orte_gpr_replica_segment_t *seg, char *name)
{
    char **ptr, *new_dict;
    orte_gpr_replica_itag_t j;
    orte_std_cntr_t i, len, len2, index;

    OPAL_TRACE(3);

    /* default to illegal value */
    *itag = ORTE_GPR_REPLICA_ITAG_MAX;
    
    /* if name or seg is NULL, error */
    if (NULL == name || NULL == seg) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    len = (orte_std_cntr_t)strlen(name);
    
    /* check seg's dictionary to ensure uniqueness */
    ptr = (char**)(seg->dict)->addr;
    for (i=0, j=0; j < seg->num_dict_entries &&
                   i < (seg->dict)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            len2 = (orte_std_cntr_t)strlen(ptr[i]);
            if ((len == len2 && 0 == strncmp(ptr[i], name, len))) {
                /* already present */
                if (i < ORTE_GPR_REPLICA_ITAG_MAX) {
                    *itag = (orte_gpr_replica_itag_t)i;
                    return ORTE_SUCCESS;
                }
                /* otherwise, the itag violates the max value */
                return ORTE_ERR_BAD_PARAM;
            }
        }
    }

    /* okay, name is unique - create dictionary entry */
    new_dict = strdup(name);
    if (0 > orte_pointer_array_add(&index, seg->dict, (void*)new_dict)) {
        free(new_dict);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if ((orte_gpr_replica_itag_t)index < ORTE_GPR_REPLICA_ITAG_MAX) {
        *itag = (orte_gpr_replica_itag_t)index;
        (seg->num_dict_entries)++;
        return ORTE_SUCCESS;
    }
    
    /* otherwise, the itag violates the max value */
    free(new_dict);
    ptr[index] = NULL;
    
    return ORTE_ERR_OUT_OF_RESOURCE;
}


int orte_gpr_replica_delete_itag(orte_gpr_replica_segment_t *seg, char *name)
{
    char **ptr;
    orte_gpr_replica_itag_t itag;
    int rc;

    OPAL_TRACE(3);

    /* check for errors */
    if (NULL == name || NULL == seg) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* find dictionary element to delete */
    if (ORTE_SUCCESS != (rc = orte_gpr_replica_dict_lookup(&itag, seg, name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* found name in dictionary */
    /* need to search this segment's registry to find all instances
     * that name & delete them
     */
     if (ORTE_SUCCESS != (rc = orte_gpr_replica_purge_itag(seg, itag))) {
        ORTE_ERROR_LOG(rc);
        return rc;
     }

     /* free the dictionary element data */
     ptr = (char**)((seg->dict)->addr);
     if (NULL == ptr[itag]) {  /* dict element no longer valid */
         return ORTE_ERR_NOT_FOUND;
     }
     free(ptr[itag]);
     
     /* remove itag from segment dictionary */
    orte_pointer_array_set_item(seg->dict, (orte_std_cntr_t)itag, NULL);
    
    /* decrease the dict counter */
    (seg->num_dict_entries)--;
    
    return ORTE_SUCCESS;
}


int
orte_gpr_replica_dict_lookup(orte_gpr_replica_itag_t *itag,
                             orte_gpr_replica_segment_t *seg, char *name)
{
    char **ptr;
    orte_std_cntr_t i;
    orte_gpr_replica_itag_t j;
    orte_std_cntr_t len, len2;
    
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

    len = (orte_std_cntr_t)strlen(name);
    
    /* want specified token-itag pair in that segment's dictionary */
    ptr = (char**)((seg->dict)->addr);
    for (i=0, j=0; j < seg->num_dict_entries &&
                   i < (seg->dict)->size; i++) {
        if (NULL != ptr[i]) {
            j++;
            len2 = (orte_std_cntr_t)strlen(ptr[i]);
	        if (len == len2 && 0 == strncmp(ptr[i], name, len)) {
               if (i < ORTE_GPR_REPLICA_ITAG_MAX) {
                   *itag = (orte_gpr_replica_itag_t)i;
                   return ORTE_SUCCESS;
               }
               /* otherwise, the itag violates the max value */
               return ORTE_ERR_BAD_PARAM;
            }
        }
	}

    return ORTE_ERR_NOT_FOUND; /* couldn't find the specified entry */
}


int orte_gpr_replica_dict_reverse_lookup(char **name,
        orte_gpr_replica_segment_t *seg, orte_gpr_replica_itag_t itag)
{
    char **ptr;
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
    ptr = (char**)((seg->dict)->addr);
    if (NULL == ptr[itag]) { /* this entry is no longer valid! */
        return ORTE_ERR_NOT_FOUND;
    }
    *name = strdup(ptr[itag]);
    
    return ORTE_SUCCESS;
}

int
orte_gpr_replica_get_itag_list(orte_gpr_replica_itag_t **itaglist,
                    orte_gpr_replica_segment_t *seg, char **names,
                    orte_std_cntr_t *num_names)
{
    char **namptr;
    int rc;
    orte_std_cntr_t i;

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

