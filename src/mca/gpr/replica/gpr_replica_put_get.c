/*
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

int mca_gpr_replica_put(ompi_registry_mode_t addr_mode, char *segment,
		    char **tokens, ompi_registry_object_t object,
		    ompi_registry_object_size_t size)
{
    int rc;
    int8_t action_taken;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t *keys;
    int num_keys;

    /* protect ourselves against errors */
    if (NULL == segment || NULL == object || 0 == size ||
	NULL == tokens || NULL == *tokens) {
	if (mca_gpr_replica_debug) {
	    ompi_output(0, "[%d,%d,%d] gpr replica: error in input - put rejected",
                        OMPI_NAME_ARGS(*ompi_rte_get_self()));
	}
	return OMPI_ERROR;
    }

    if (mca_gpr_replica_compound_cmd_mode) {
	return mca_gpr_base_pack_put(mca_gpr_replica_compound_cmd,
				     mca_gpr_replica_silent_mode,
				     addr_mode, segment,
				     tokens, object, size);
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* find the segment */
    seg = mca_gpr_replica_find_seg(true, segment,
				   ompi_name_server.get_jobid(ompi_rte_get_self()));
    if (NULL == seg) { /* couldn't find segment or create it */
	return OMPI_ERROR;
    }

    /* convert tokens to array of keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    rc = mca_gpr_replica_put_nl(addr_mode, seg, keys, num_keys,
				object, size, &action_taken);

    mca_gpr_replica_check_subscriptions(seg, action_taken);

    mca_gpr_replica_check_synchros(seg);

    /* release list of keys */
    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    mca_gpr_replica_process_callbacks();

    return rc;
}

int mca_gpr_replica_put_nl(ompi_registry_mode_t addr_mode,
			   mca_gpr_replica_segment_t *seg,
			   mca_gpr_replica_key_t *keys,
			   int num_keys, ompi_registry_object_t object,
			   ompi_registry_object_size_t size,
			   int8_t *action_taken)
{
    mca_gpr_replica_core_t *entry_ptr;
    ompi_registry_mode_t put_mode;
    mca_gpr_replica_trigger_list_t *trig;
    int return_code;


    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: put entered on segment %s",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), seg->name);
    }

    /* ignore addressing mode - all tokens are used
     * only overwrite permission mode flag has any affect
     */
    put_mode = addr_mode & OMPI_REGISTRY_OVERWRITE;

    /* see if specified entry already exists */
    for (entry_ptr = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 entry_ptr != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 entry_ptr = (mca_gpr_replica_core_t*)ompi_list_get_next(entry_ptr)) {
	if (mca_gpr_replica_check_key_list(put_mode, num_keys, keys,
				       entry_ptr->num_keys, entry_ptr->keys)) {
	    /* found existing entry - overwrite if mode set, else error */
	    if (put_mode) {  /* overwrite enabled */
		free(entry_ptr->object);
		entry_ptr->object = NULL;
		entry_ptr->object_size = size;
		entry_ptr->object = (ompi_registry_object_t)malloc(size);
		memcpy(entry_ptr->object, object, size);
		return_code = OMPI_SUCCESS;
		*action_taken = MCA_GPR_REPLICA_OBJECT_UPDATED;
		goto CLEANUP;
	    } else {
		return_code = OMPI_ERROR;
		goto CLEANUP;
	    }
	}
    }

    /* no existing entry - create new one */
    entry_ptr = OBJ_NEW(mca_gpr_replica_core_t);
    entry_ptr->keys = (mca_gpr_replica_key_t*)malloc(num_keys*sizeof(mca_gpr_replica_key_t));
    memcpy(entry_ptr->keys, keys, num_keys*sizeof(mca_gpr_replica_key_t));
    entry_ptr->num_keys = num_keys;
    entry_ptr->object_size = size;
    entry_ptr->object = (ompi_registry_object_t*)malloc(size);
    memcpy(entry_ptr->object, object, size);
    ompi_list_append(&seg->registry_entries, &entry_ptr->item);

    *action_taken = MCA_GPR_REPLICA_OBJECT_ADDED;
    return_code = OMPI_SUCCESS;

    /* update trigger list */
    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
         trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig)) {
	if (mca_gpr_replica_check_key_list(trig->addr_mode, trig->num_keys, trig->keys,
				       num_keys, keys)) {
	    trig->count++;
	}
    }

 CLEANUP:
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-put: complete", OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    return return_code;
}


ompi_list_t* mca_gpr_replica_get(ompi_registry_mode_t addr_mode,
			     char *segment, char **tokens)
{
    ompi_list_t* list;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t *keys;
    int num_keys;

    /* protect against errors */
    if (NULL == segment) {
	return list;
    }

    if (mca_gpr_replica_compound_cmd_mode) {
	mca_gpr_base_pack_get(mca_gpr_replica_compound_cmd, addr_mode, segment, tokens);
	return NULL;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* find the specified segment */
    seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);
    if (NULL == seg) {  /* segment not found */
	return list;
    }

    /* convert tokens to array of keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    list = mca_gpr_replica_get_nl(addr_mode, seg, keys, num_keys);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return list;
}

ompi_list_t* mca_gpr_replica_get_nl(ompi_registry_mode_t addr_mode,
				    mca_gpr_replica_segment_t *seg,
				    mca_gpr_replica_key_t *keys,
				    int num_keys)
{
    ompi_list_t *answer=NULL;
    ompi_registry_value_t *ans=NULL;
    mca_gpr_replica_core_t *reg=NULL;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: get entered", OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    answer = OBJ_NEW(ompi_list_t);

    /* traverse the segment's registry, looking for matching tokens per the specified mode */
    for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {

	/* for each registry entry, check the key list */
	if (mca_gpr_replica_check_key_list(addr_mode, num_keys, keys,
				       reg->num_keys, reg->keys)) { /* found the key(s) on the list */
	    ans = OBJ_NEW(ompi_registry_value_t);
	    ans->object_size = reg->object_size;
	    ans->object = (ompi_registry_object_t*)malloc(ans->object_size);
	    memcpy(ans->object, reg->object, ans->object_size);
	    ompi_list_append(answer, &ans->item);
	}
    }
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-get: finished search", OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    return answer;
}
