/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"


int mca_gpr_replica_delete_segment(char *segment)
{
    mca_gpr_replica_segment_t *seg;

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_ERROR;
    }

    if (mca_gpr_replica_compound_cmd_mode) {
	return mca_gpr_base_pack_delete_segment(mca_gpr_replica_compound_cmd,
						mca_gpr_replica_silent_mode, segment);
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* locate the segment */
    seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);
    if (NULL == seg) {
	return OMPI_ERROR;
    }

    mca_gpr_replica_delete_segment_nl(seg);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return OMPI_SUCCESS;
}


void mca_gpr_replica_delete_segment_nl(mca_gpr_replica_segment_t *seg)
{

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: delete_segment entered",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    /* empty the segment storage */
    mca_gpr_replica_empty_segment(seg);

    /* remove segment name from global registry dictionary */
    mca_gpr_replica_delete_key(seg, NULL);

    return;
}

int mca_gpr_replica_delete_object(ompi_registry_mode_t addr_mode,
			      char *segment, char **tokens)
{
    int rc;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t *keys;
    int num_keys;

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_ERROR;
    }

    if (mca_gpr_replica_compound_cmd_mode) {
	return mca_gpr_base_pack_delete_object(mca_gpr_replica_compound_cmd,
					       mca_gpr_replica_silent_mode,
					       addr_mode, segment, tokens);
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

     /* locate the segment */
    seg = mca_gpr_replica_find_seg(false, segment, ompi_name_server.get_jobid(ompi_rte_get_self()));
    if (NULL == seg) {
	return OMPI_ERROR;
    }

    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    rc = mca_gpr_replica_delete_object_nl(addr_mode, seg, keys, num_keys);

    mca_gpr_replica_check_subscriptions(seg, MCA_GPR_REPLICA_OBJECT_DELETED);

    mca_gpr_replica_check_synchros(seg);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    mca_gpr_replica_process_callbacks();

    return rc;
}


int mca_gpr_replica_delete_object_nl(ompi_registry_mode_t addr_mode,
				     mca_gpr_replica_segment_t *seg,
				     mca_gpr_replica_key_t *keys,
				     int num_keys)
{
    mca_gpr_replica_core_t *reg, *next;
    int count;
    mca_gpr_replica_trigger_list_t *trig;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] replica_delete_object entered: segment %s",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), seg->name);
    }

    /* traverse the segment's registry, looking for matching tokens per the specified mode */
    count = 0;
    for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 ) {

	next = (mca_gpr_replica_core_t*)ompi_list_get_next(reg);

	/* for each registry entry, check the key list */
	if (mca_gpr_replica_check_key_list(addr_mode, num_keys, keys,
				       reg->num_keys, reg->keys)) { /* found the key(s) on the list */
	    count++;
	    ompi_list_remove_item(&seg->registry_entries, &reg->item);
	}
	reg = next;
    }


    /* update trigger counters */
    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
	 trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig)) {
	if (mca_gpr_replica_check_key_list(trig->addr_mode, trig->num_keys, trig->keys,
				       num_keys, keys)) {
	    trig->count = trig->count - count;
	}
    }

    return OMPI_SUCCESS;
}

ompi_list_t* mca_gpr_replica_index(char *segment)
{
    ompi_list_t* list;
    mca_gpr_replica_segment_t *seg;

    if (mca_gpr_replica_compound_cmd_mode) {
	mca_gpr_base_pack_index(mca_gpr_replica_compound_cmd, segment);
	return NULL;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    if (NULL == segment) {  /* want global level index */
	seg = NULL;
    } else {
	/* locate the segment */
	seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);
	if (NULL == seg) {
	    return NULL;
	}
    }

    list = mca_gpr_replica_index_nl(seg);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return list;
}

ompi_list_t* mca_gpr_replica_index_nl(mca_gpr_replica_segment_t *seg)
{
    ompi_list_t *answer;
    mca_gpr_replica_keytable_t *ptr;
    ompi_registry_index_value_t *ans;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: index entered segment: %s",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), seg->name);
    }

    answer = OBJ_NEW(ompi_list_t);

    if (NULL == seg) { /* looking for index of global registry */
	for (ptr = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	     ptr != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	     ptr = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr)) {
	    ans = OBJ_NEW(ompi_registry_index_value_t);
	    ans->token = strdup(ptr->token);
	    ompi_list_append(answer, &ans->item);
	}
    } else {  /* want index of specific segment */
	for (ptr = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&seg->keytable);
	     ptr != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&seg->keytable);
	     ptr = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr)) {
	    ans = OBJ_NEW(ompi_registry_index_value_t);
	    ans->token = strdup(ptr->token);
	    ompi_list_append(answer, &ans->item);
	}

    }
    return answer;
}
