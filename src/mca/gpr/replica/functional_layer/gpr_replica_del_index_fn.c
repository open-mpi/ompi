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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include "include/orte_constants.h"

#include "util/output.h"
#include "util/proc_info.h"

#include "gpr_replica_fn.h"


int orte_gpr_replica_delete_entries_fn(orte_gpr_addr_mode_t addr_mode,
				     orte_gpr_replica_segment_t *seg,
				     orte_gpr_replica_itag_t *token_itags, int num_tokens,
                      orte_gpr_replica_itag_t *key_itags, int num_keys)
{
#if 0
    orte_gpr_replica_container_t **ptr;
    orte_gpr_replica_itagval_t  **valptr;
    int i, j;

    if (orte_gpr_replica_globals.debug) {
	   ompi_output(0, "[%d,%d,%d] replica_delete_object entered: segment %s",
		    ORTE_NAME_ARGS(*(orte_process_info.my_name)), seg->name);
    }

    /* if num_tokens == 0 and num_keys == 0, remove segment */
    if (0 == num_tokens && 0 == num_keys) {
        return orte_gpr_replica_release_segment(seg);
    }
    
    /* if num_tokens == 0 and num_keys > 0, check every container */
    if (0 == num_tokens) {
        ptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
        for (i=0; i < (seg->containers)->size; i++) {
            if (NULL != ptr[i]) {
                valptr = (orte_gpr_replica_itagval_t**)((ptr->itagvals)->addr);
                for (j=0; j < (ptr[i]->itagvals)->size; j++) {
                     if (NULL != valptr[j]) {
                         if (orte_gpr_replica_check_itag(valptr[j]->itag, key_itags, num_keys)) {
                             OBJ_RELEASE(valptr[j]);
                             if (ORTE_SUCCESS != (rc = orte_pointer_array_set_item(ptr[i]->itagvals, j, NULL))) {
                                 return rc;
                             }
                         }
                     }
                }
            }
        }
        return ORTE_SUCCESS;
    }
    

    /* if num_tokens > 0, find the specified container and then check it */
    ptr = (orte_gpr_replica_container_t**)((seg->containers)->addr);
    for (i=0; i < (seg->containers)->size; i++) {
        if (NULL != ptr[i]) {
                if (orte_gpr_replica_check_itag_list(addr_mode, num_tokens, token_itags,
                            ptr[i]->num_itags, ptr[i]->itags)) {
                    /* right container - check for matching entries and delete them.
                     * if num_keys == 0, delete entire container
                     */
                    if (0 == num_keys) {
                        /* delete container */
                    } else {
                        /* delete entries */
                    }
                }
            } 
    count = 0;
    for (reg = (orte_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 reg != (orte_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 ) {

	next = (orte_gpr_replica_core_t*)ompi_list_get_next(reg);

	/* for each registry entry, check the key list */
	if (orte_gpr_replica_check_key_list(addr_mode, num_keys, keys,
				       reg->num_keys, reg->keys)) { /* found the key(s) on the list */
	    count++;
	    ompi_list_remove_item(&seg->registry_entries, &reg->item);
	}
	reg = next;
    }


    /* update trigger counters */
    for (trig = (orte_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	 trig != (orte_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
	 trig = (orte_gpr_replica_trigger_list_t*)ompi_list_get_next(trig)) {
	if (orte_gpr_replica_check_key_list(trig->addr_mode, trig->num_keys, trig->keys,
				       num_keys, keys)) {
	    trig->count = trig->count - count;
	}
    }
#endif

    return ORTE_ERR_NOT_IMPLEMENTED;
}

int orte_gpr_replica_delete_entries_nb_fn(
                    orte_gpr_addr_mode_t addr_mode,
                    orte_gpr_replica_segment_t *seg,
                    orte_gpr_replica_itag_t *token_itags, int num_tokens,
                    orte_gpr_replica_itag_t *key_tags, int num_keys)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}                           


int orte_gpr_replica_index_fn(orte_gpr_replica_segment_t *seg,
                            size_t *cnt, char **index)
{
#if 0
    ompi_list_t *answer;
    orte_gpr_replica_keytable_t *ptr;
    ompi_registry_index_value_t *ans;

    if (orte_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: index entered segment: %s",
		    ORTE_NAME_ARGS(*ompi_rte_get_self()), seg->name);
    }

    answer = OBJ_NEW(ompi_list_t);

    if (NULL == seg) { /* looking for index of global registry */
	for (ptr = (orte_gpr_replica_keytable_t*)ompi_list_get_first(&orte_gpr_replica_head.segment_dict);
	     ptr != (orte_gpr_replica_keytable_t*)ompi_list_get_end(&orte_gpr_replica_head.segment_dict);
	     ptr = (orte_gpr_replica_keytable_t*)ompi_list_get_next(ptr)) {
	    ans = OBJ_NEW(ompi_registry_index_value_t);
	    ans->token = strdup(ptr->token);
	    ompi_list_append(answer, &ans->item);
	}
    } else {  /* want index of specific segment */
	for (ptr = (orte_gpr_replica_keytable_t*)ompi_list_get_first(&seg->keytable);
	     ptr != (orte_gpr_replica_keytable_t*)ompi_list_get_end(&seg->keytable);
	     ptr = (orte_gpr_replica_keytable_t*)ompi_list_get_next(ptr)) {
	    ans = OBJ_NEW(ompi_registry_index_value_t);
	    ans->token = strdup(ptr->token);
	    ompi_list_append(answer, &ans->item);
	}

    }
    return answer;
#endif
    return ORTE_ERR_NOT_IMPLEMENTED;
}


int orte_gpr_replica_index_nb_fn(orte_gpr_replica_segment_t *seg,
                        orte_gpr_notify_cb_fn_t cbfunc, void *user_tag)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

