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

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "include/constants.h"

#include "threads/mutex.h"

#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"

#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"


int gpr_replica_delete_segment(char *segment)
{
    int rc;
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    rc = gpr_replica_delete_segment_nl(segment);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return rc;
}


int gpr_replica_delete_segment_nl(char *segment)
{
    mca_gpr_replica_segment_t *seg;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: delete_segment entered", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

    seg = gpr_replica_find_seg(true, segment);

    if (NULL == seg) {  /* couldn't locate segment */
	return OMPI_ERROR;
    }

    OBJ_RELEASE(seg);

    if (OMPI_SUCCESS != gpr_replica_delete_key(segment, NULL)) { /* couldn't remove dictionary entry */
	return OMPI_ERROR;
    }
    return OMPI_SUCCESS;
}

int gpr_replica_put(ompi_registry_mode_t addr_mode, char *segment,
		    char **tokens, ompi_registry_object_t object,
		    ompi_registry_object_size_t size)
{
    int rc;
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    rc = gpr_replica_put_nl(addr_mode, segment, tokens, object, size);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return rc;
}

int gpr_replica_put_nl(ompi_registry_mode_t addr_mode, char *segment,
		    char **tokens, ompi_registry_object_t object,
		    ompi_registry_object_size_t size)
{
    ompi_list_t *keylist;
    mca_gpr_replica_keytable_t *keyptr;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_core_t *entry_ptr;
    ompi_registry_mode_t put_mode;
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;
    int return_code, num_tokens;
    mca_gpr_replica_key_t *keys, *key2;
    bool still_valid;


    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: put entered on segment %s 1st token %s",
		    ompi_process_info.name->cellid, ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid, segment, *tokens);
    }

    /* protect ourselves against errors */
    if (NULL == segment || NULL == object || 0 == size || NULL == tokens || NULL == *tokens) {
	if (mca_gpr_replica_debug) {
	    ompi_output(0, "[%d,%d,%d] gpr replica: error in input - put rejected",
			ompi_process_info.name->cellid, ompi_process_info.name->jobid,
			ompi_process_info.name->vpid);
	}
	return OMPI_ERROR;
    }

    /* ignore addressing mode - all tokens are used
     * only overwrite permission mode flag has any affect
     */
    put_mode = addr_mode & OMPI_REGISTRY_OVERWRITE;

    /* find the segment */
    seg = gpr_replica_find_seg(true, segment);
    if (NULL == seg) { /* couldn't find segment or create it */
	return OMPI_ERROR;
    }

    /* convert tokens to list of keys */
    keylist = gpr_replica_get_key_list(segment, tokens);
    if (0 >= (num_tokens = ompi_list_get_size(keylist))) {
	return OMPI_ERROR;
    }

    keys = (mca_gpr_replica_key_t*)malloc(num_tokens*sizeof(mca_gpr_replica_key_t));
    key2 = keys;

    /* traverse the list to find undefined tokens - get new keys for them */
    for (keyptr = (mca_gpr_replica_keytable_t*)ompi_list_get_first(keylist);
	 keyptr != (mca_gpr_replica_keytable_t*)ompi_list_get_end(keylist);
	 keyptr = (mca_gpr_replica_keytable_t*)ompi_list_get_next(keyptr)) {
	if (MCA_GPR_REPLICA_KEY_MAX == keyptr->key) { /* need to get new key */
	    keyptr->key = gpr_replica_define_key(segment, keyptr->token);
	}
	*key2 = keyptr->key;
	key2++;
    }

    /* see if specified entry already exists */
    for (entry_ptr = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 entry_ptr != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 entry_ptr = (mca_gpr_replica_core_t*)ompi_list_get_next(entry_ptr)) {
	if (gpr_replica_check_key_list(put_mode, num_tokens, keys,
				       entry_ptr->num_keys, entry_ptr->keys)) {
	    /* found existing entry - overwrite if mode set, else error */
	    if (put_mode) {  /* overwrite enabled */
		free(entry_ptr->object);
		entry_ptr->object = NULL;
		entry_ptr->object_size = size;
		entry_ptr->object = (ompi_registry_object_t)malloc(size);
		memcpy(entry_ptr->object, object, size);
		return_code = OMPI_SUCCESS;
		goto CLEANUP;
	    } else {
		return_code = OMPI_ERROR;
		goto CLEANUP;
	    }
	}
    }

    /* no existing entry - create new one */
    entry_ptr = OBJ_NEW(mca_gpr_replica_core_t);
    entry_ptr->keys = (mca_gpr_replica_key_t*)malloc(num_tokens*sizeof(mca_gpr_replica_key_t));
    memcpy(entry_ptr->keys, keys, num_tokens*sizeof(mca_gpr_replica_key_t));
    entry_ptr->num_keys = num_tokens;
    entry_ptr->object_size = size;
    entry_ptr->object = (ompi_registry_object_t*)malloc(size);
    memcpy(entry_ptr->object, object, size);
    ompi_list_append(&seg->registry_entries, &entry_ptr->item);

    return_code = OMPI_SUCCESS;

    /* update trigger list and check for trigger conditions */
    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	     trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
         ) {
    mca_gpr_replica_trigger_list_t* next = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);
	if (gpr_replica_check_key_list(trig->addr_mode, trig->num_keys, trig->keys,
				       num_tokens, keys)) {
	    trig->count++;
	}
	still_valid = true;
	if (((OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING & trig->synch_mode)
	     && (trig->count >= trig->trigger)
	     && (MCA_GPR_REPLICA_TRIGGER_BELOW_LEVEL == trig->above_below)) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_LEVEL & trig->synch_mode && trig->count == trig->trigger) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL & trig->synch_mode && trig->count >= trig->trigger)) {
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, trig->tokens);
	    notify_msg->trig_action = OMPI_REGISTRY_NOTIFY_NONE;
	    notify_msg->trig_synchro = trig->synch_mode;
	    still_valid = gpr_replica_process_triggers(segment, trig, notify_msg);
	} else if ((OMPI_REGISTRY_NOTIFY_ALL & trig->action) ||
		   (OMPI_REGISTRY_NOTIFY_ADD_ENTRY & trig->action) ||
		   (OMPI_REGISTRY_NOTIFY_MODIFICATION & trig->action && OMPI_REGISTRY_OVERWRITE & put_mode)) {
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, trig->tokens);
	    notify_msg->trig_action = trig->action;
	    notify_msg->trig_synchro = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
	    still_valid = gpr_replica_process_triggers(segment, trig, notify_msg);
	}
	if (still_valid) {
	    if (trig->count > trig->trigger) {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_ABOVE_LEVEL;
	    } else if (trig->count == trig->trigger) {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_AT_LEVEL;
	    }
	}
    trig = next;
    }

 CLEANUP:
    /* release list of keys */
    if (NULL != keylist) {
	OBJ_RELEASE(keylist);
    }
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-put: complete", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

    return return_code;
}

int gpr_replica_delete_object(ompi_registry_mode_t addr_mode,
			      char *segment, char **tokens)
{
    int rc;
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    rc = gpr_replica_delete_object_nl(addr_mode, segment, tokens);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return rc;
}


int gpr_replica_delete_object_nl(ompi_registry_mode_t addr_mode,
			      char *segment, char **tokens)
{
    mca_gpr_replica_core_t *reg, *prev;
    mca_gpr_replica_keytable_t *keyptr;
    ompi_list_t *keylist;
    mca_gpr_replica_key_t *keys, *key2;
    mca_gpr_replica_segment_t *seg;
    int num_tokens, return_code;
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;
    bool still_valid;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: delete_object entered: segment 1st token",
		    ompi_process_info.name->cellid, ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid, segment, *tokens);
    }

    keys = NULL;
    return_code = OMPI_ERROR;

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_ERROR;
    }


    /* find the specified segment */
    seg = gpr_replica_find_seg(false, segment);
    if (NULL == seg) {  /* segment not found */
	return OMPI_ERROR;
    }

    /* convert tokens to list of keys */
    keylist = gpr_replica_get_key_list(segment, tokens);
    if (0 == (num_tokens = ompi_list_get_size(keylist))) {  /* no tokens provided - wildcard case */
	keys = NULL;

    } else {  /* tokens provided */
	keys = (mca_gpr_replica_key_t*)malloc(num_tokens*sizeof(mca_gpr_replica_key_t));
	key2 = keys;

	/* traverse the list to find undefined tokens - error if found */
	for (keyptr = (mca_gpr_replica_keytable_t*)ompi_list_get_first(keylist);
	     keyptr != (mca_gpr_replica_keytable_t*)ompi_list_get_end(keylist);
	     keyptr = (mca_gpr_replica_keytable_t*)ompi_list_get_next(keyptr)) {
	    if (MCA_GPR_REPLICA_KEY_MAX == keyptr->key) { /* unknown token */
		return_code = OMPI_ERROR;
		goto CLEANUP;
	    }
	    *key2 = keyptr->key;
	    key2++;
	}
    }

    /* traverse the segment's registry, looking for matching tokens per the specified mode */
    for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {

	/* for each registry entry, check the key list */
	if (gpr_replica_check_key_list(addr_mode, num_tokens, keys,
				       reg->num_keys, reg->keys)) { /* found the key(s) on the list */
	    prev = (mca_gpr_replica_core_t*)ompi_list_get_prev(reg);
	    ompi_list_remove_item(&seg->registry_entries, &reg->item);
	    reg = prev;
	}
    }

    return_code = OMPI_SUCCESS;

    /* update synchro list and check for trigger conditions */
    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	     trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
	     ) {
    mca_gpr_replica_trigger_list_t* next = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);
	if (gpr_replica_check_key_list(trig->addr_mode, trig->num_keys, trig->keys,
				       num_tokens, keys)) {
	    trig->count--;
	}
	still_valid = true;
	if (((OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING & trig->synch_mode)
	     && (trig->count <= trig->trigger)
	     && (MCA_GPR_REPLICA_TRIGGER_ABOVE_LEVEL == trig->above_below)) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_LEVEL & trig->synch_mode && trig->count == trig->trigger)) {
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, trig->tokens);
	    notify_msg->trig_action = OMPI_REGISTRY_NOTIFY_NONE;
	    notify_msg->trig_synchro = trig->synch_mode;
	    still_valid = gpr_replica_process_triggers(segment, trig, notify_msg);
	} else if ((OMPI_REGISTRY_NOTIFY_ALL & trig->action) ||
		   (OMPI_REGISTRY_NOTIFY_DELETE_ENTRY & trig->action)) {
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, trig->tokens);
	    notify_msg->trig_action = trig->action;
	    notify_msg->trig_synchro = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
	    still_valid = gpr_replica_process_triggers(segment, trig, notify_msg);
	}
	if (still_valid) {
	    if (trig->count < trig->trigger) {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_BELOW_LEVEL;
	    } else if (trig->count == trig->trigger) {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_AT_LEVEL;
	    }
	}
    trig = next;
    }


 CLEANUP:
    if (NULL != keylist) {
	OBJ_RELEASE(keylist);
    }

    if (NULL != keys) {
	free(keys);
	keys = NULL;
    }

    return return_code;
}

ompi_list_t* gpr_replica_index(char *segment)
{
    ompi_list_t* list;
	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    list = gpr_replica_index_nl(segment);
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return list;
}

ompi_list_t* gpr_replica_index_nl(char *segment)
{
    ompi_list_t *answer;
    mca_gpr_replica_keytable_t *ptr;
    mca_gpr_replica_segment_t *seg;
    ompi_registry_index_value_t *ans;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: index entered segment: %s", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid, segment);
    }

    answer = OBJ_NEW(ompi_list_t);

    if (NULL == segment) { /* looking for index of global registry */
	for (ptr = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	     ptr != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	     ptr = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr)) {
	    ans = OBJ_NEW(ompi_registry_index_value_t);
	    ans->token = strdup(ptr->token);
	    ompi_list_append(answer, &ans->item);
	}
    } else {  /* want index of specific segment */
	/* find the specified segment */
	seg = gpr_replica_find_seg(false, segment);
	if (NULL == seg) {  /* segment not found */
	    return answer;
	}
	/* got segment - now index that dictionary */
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

int gpr_replica_subscribe(ompi_registry_mode_t addr_mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens,
			  ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    int rc;
    mca_gpr_notify_id_t local_idtag;

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* enter request on notify tracking system */
    local_idtag = gpr_replica_enter_notify_request(NULL, 0, cb_func, user_tag);

    /* process subscription */
    rc = gpr_replica_subscribe_nl(addr_mode,action,segment,tokens,local_idtag);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return rc;
}


int gpr_replica_subscribe_nl(ompi_registry_mode_t addr_mode,
			     ompi_registry_notify_action_t action,
			     char *segment, char **tokens, mca_gpr_notify_id_t id_tag)
{
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: subscribe entered: segment %s 1st token %s",
		    ompi_process_info.name->cellid, ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid, segment, *tokens);
    }

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_ERROR;
    }


   /* construct the trigger - add to notify tracking system if success, otherwise dump */
    if (NULL != (trig = gpr_replica_construct_trigger(OMPI_REGISTRY_SYNCHRO_MODE_NONE, action,
						      addr_mode, segment, tokens,
						      0, id_tag))) {

	if (OMPI_REGISTRY_NOTIFY_PRE_EXISTING & action) {  /* want list of everything there */
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, trig->tokens);
	    notify_msg->trig_action = action;
	    notify_msg->trig_synchro = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
	    gpr_replica_process_triggers(segment, trig, notify_msg);
	}
	return OMPI_SUCCESS;
    } else {
	return OMPI_ERROR;
    }
}


int gpr_replica_unsubscribe(ompi_registry_mode_t addr_mode,
			    ompi_registry_notify_action_t action,
			    char *segment, char **tokens)
{
    int rc;
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    rc = gpr_replica_unsubscribe_nl(addr_mode,action,segment,tokens);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    if (MCA_GPR_NOTIFY_ID_MAX == rc) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


mca_gpr_notify_id_t gpr_replica_unsubscribe_nl(ompi_registry_mode_t addr_mode,
					       ompi_registry_notify_action_t action,
					       char *segment, char **tokens)
{
    mca_gpr_notify_id_t id_tag, req_idtag;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: unsubscribe entered: segment %s 1st token %s",
		    ompi_process_info.name->cellid, ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid, segment, *tokens);
    }

    /* protect against errors */
    if (NULL == segment) {
	return MCA_GPR_NOTIFY_ID_MAX;
    }


    /* find trigger on replica - return id_tag */
    id_tag = gpr_replica_remove_trigger(OMPI_REGISTRY_SYNCHRO_MODE_NONE, action,
					addr_mode, segment, tokens, 0);

    if (MCA_GPR_NOTIFY_ID_MAX != id_tag) {  /* removed trigger successfully */

	req_idtag = gpr_replica_remove_notify_request(id_tag);
	return req_idtag;
    } else {
	return MCA_GPR_NOTIFY_ID_MAX;
    }

}

int gpr_replica_synchro(ompi_registry_synchro_mode_t synchro_mode,
			ompi_registry_mode_t addr_mode,
			char *segment, char **tokens, int trigger,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    int rc;
    mca_gpr_notify_id_t local_idtag;

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* enter request on notify tracking system */
    local_idtag = gpr_replica_enter_notify_request(NULL, 0, cb_func, user_tag);

    /* process synchro request */
    rc = gpr_replica_synchro_nl(synchro_mode, addr_mode,
				segment, tokens, trigger, local_idtag);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return rc;
}

int gpr_replica_synchro_nl(ompi_registry_synchro_mode_t synchro_mode,
			   ompi_registry_mode_t addr_mode,
			   char *segment, char **tokens, int trigger,
			   mca_gpr_notify_id_t id_tag)
{
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: synchro entered on segment %s trigger %d",
		    ompi_process_info.name->cellid, ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid, segment, trigger);
    }

    /* protect against errors */
    if (NULL == segment || 0 > trigger) {
	return OMPI_ERROR;
    }

    /* construct the trigger */
    if (NULL != (trig = gpr_replica_construct_trigger(synchro_mode, OMPI_REGISTRY_NOTIFY_NONE,
						      addr_mode, segment, tokens,
						      trigger, id_tag))) {

	/* if synchro condition already met, construct and send message */
	if ((OMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL & synchro_mode && trig->count >= trigger) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_LEVEL & synchro_mode && trig->count == trigger) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_LT_EQUAL & synchro_mode && trig->count <= trigger)) {
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, trig->tokens);
	    notify_msg->trig_action = OMPI_REGISTRY_NOTIFY_NONE;
	    notify_msg->trig_synchro = trig->synch_mode;
	    gpr_replica_process_triggers(segment, trig, notify_msg);
	}
	return OMPI_SUCCESS;
    } else {
	return OMPI_ERROR;
    }
}

int gpr_replica_cancel_synchro(ompi_registry_synchro_mode_t synchro_mode,
			       ompi_registry_mode_t addr_mode,
			       char *segment, char **tokens, int trigger)
{
    int rc;
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    rc = gpr_replica_cancel_synchro_nl(synchro_mode,addr_mode,segment,tokens,trigger);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    if (MCA_GPR_NOTIFY_ID_MAX == rc) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

mca_gpr_notify_id_t gpr_replica_cancel_synchro_nl(ompi_registry_synchro_mode_t synchro_mode,
						  ompi_registry_mode_t addr_mode,
						  char *segment, char **tokens, int trigger)
{
    mca_gpr_notify_id_t id_tag, req_idtag;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: cancel_synchro entered: segment %s 1st token %s",
		    ompi_process_info.name->cellid, ompi_process_info.name->jobid,
		    ompi_process_info.name->vpid, segment, *tokens);
    }

    /* protect against errors */
    if (NULL == segment || 0 > trigger) {
	return OMPI_ERROR;
    }

    /* find trigger on replica - return local id_tag */
    id_tag = gpr_replica_remove_trigger(synchro_mode, OMPI_REGISTRY_NOTIFY_NONE,
					addr_mode, segment, tokens, trigger);

    if (MCA_GPR_NOTIFY_ID_MAX != id_tag) {  /* removed trigger successfully */
	/* remove notify request - return requestor id_tag */
	req_idtag = gpr_replica_remove_notify_request(id_tag);
	return req_idtag;
    } else {
	return MCA_GPR_NOTIFY_ID_MAX;
    }
}


int gpr_replica_rte_register(char *contact_info, size_t num_procs,
			     ompi_registry_notify_cb_fn_t start_cb_func, void *start_user_tag,
			     ompi_registry_notify_cb_fn_t end_cb_func, void *end_user_tag)
{
    int ret;
    mca_gpr_notify_id_t local_idtag1, local_idtag2;
    ompi_buffer_t buffer;

    /* create the buffer to store the local information */
    ompi_buffer_init(&buffer, 0);
    ompi_pack_string(buffer, contact_info);
    ompi_pack(buffer, &ompi_process_info.pid, 1, OMPI_INT32);
    ompi_pack_string(buffer, ompi_system_info.nodename);

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    local_idtag1 = gpr_replica_enter_notify_request(NULL, 0, start_cb_func, start_user_tag);

    local_idtag2 = gpr_replica_enter_notify_request(NULL, 0, end_cb_func, end_user_tag);

    ret = gpr_replica_rte_register_nl(contact_info, buffer, num_procs,
				      local_idtag1, local_idtag2);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return ret;
}

int gpr_replica_rte_register_nl(char *contact_info, ompi_buffer_t buffer, size_t num_procs,
				mca_gpr_notify_id_t start_idtag,
				mca_gpr_notify_id_t end_idtag)
{
    char *segment;
    char *keys[2];
    void *addr;
    int rc, size;
    ompi_process_name_t proc={0,0,0};


    /* extract process name from contact info */
    mca_oob_parse_contact_info(contact_info, &proc, NULL);

    /* setup keys and segment for this job */
    asprintf(&segment, "ompi-job-%s", ns_base_get_jobid_string(&proc));
    keys[0] = ns_base_get_proc_name_string(&proc);
    keys[1] = NULL;

    if (mca_gpr_replica_debug) {
        ompi_output(0, "gpr_replica_register: entered for proc %s", keys[0]);
    }

    /* peek the buffer and resulting size */
    ompi_buffer_get(buffer, &addr, &size);

    /* place on registry, no overwrite - error if already there */
    rc = gpr_replica_put_nl(OMPI_REGISTRY_XAND,
			    segment, keys, addr, size);

    if (OMPI_SUCCESS != rc) {
	if (mca_gpr_replica_debug) {
	    ompi_output(0, "gpr_replica_register: duplicate registration attempt from %s", keys[0]);
	}
	return rc;
    }

    /* register a synchro on the segment so we get notified when everyone registers */
    rc = gpr_replica_synchro_nl(
	 OMPI_REGISTRY_SYNCHRO_MODE_LEVEL|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
	 OMPI_REGISTRY_OR,
	 segment,
	 NULL,
	 num_procs,
	 start_idtag);

    /* register a synchro on the segment so we get notified when everyone is gone */
    rc = gpr_replica_synchro_nl(
        OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT,
        OMPI_REGISTRY_OR,
        segment,
        NULL,
        0,
	end_idtag);

    return rc;
}

int gpr_replica_rte_unregister(char *proc_name_string)
{
    int ret;

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    ret = gpr_replica_rte_unregister_nl(proc_name_string);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    return ret;
}


int gpr_replica_rte_unregister_nl(char *proc_name_string)
{
    char *segment;
    char *keys[2];
    int rc;
    ompi_process_name_t *proc;

    /* convert string to process name */
    proc = ns_base_convert_string_to_process_name(proc_name_string);

    /* setup keys and segment for this job */
    asprintf(&segment, "ompi-job-%s", ns_base_get_jobid_string(proc));
    keys[0] = strdup(proc_name_string);
    keys[1] = NULL;
        
    rc = gpr_replica_delete_object_nl(OMPI_REGISTRY_XAND, segment, keys);
    free(keys[0]);
    free(proc);
    return rc;

}


ompi_list_t* gpr_replica_get(ompi_registry_mode_t addr_mode,
			     char *segment, char **tokens)
{
    ompi_list_t* list;
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    list = gpr_replica_get_nl(addr_mode, segment, tokens);
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    return list;
}

ompi_list_t* gpr_replica_get_nl(ompi_registry_mode_t addr_mode,
			     char *segment, char **tokens)
{
    mca_gpr_replica_segment_t *seg=NULL;
    ompi_list_t *answer=NULL;
    ompi_registry_value_t *ans=NULL;
    mca_gpr_replica_key_t *keys=NULL, *key2=NULL;
    ompi_list_t *keylist=NULL;
    mca_gpr_replica_keytable_t *keyptr=NULL;
    mca_gpr_replica_core_t *reg=NULL;
    int num_tokens=0;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: get entered", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

    answer = OBJ_NEW(ompi_list_t);

    /* protect against errors */
    if (NULL == segment) {
	return answer;
    }

    /* find the specified segment */
    seg = gpr_replica_find_seg(false, segment);
    if (NULL == seg) {  /* segment not found */
	return answer;
    }
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-get: segment found", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

    if (NULL == tokens) { /* wildcard case - return everything */
	keylist = NULL;
	keys = NULL;
    } else {

	/* convert tokens to list of keys */
	keylist = gpr_replica_get_key_list(segment, tokens);
	if (0 == (num_tokens = ompi_list_get_size(keylist))) {
	    return answer;
	}

	keys = (mca_gpr_replica_key_t*)malloc(num_tokens*sizeof(mca_gpr_replica_key_t));
	key2 = keys;

	/* traverse the list to find undefined tokens - error if found */
	for (keyptr = (mca_gpr_replica_keytable_t*)ompi_list_get_first(keylist);
	     keyptr != (mca_gpr_replica_keytable_t*)ompi_list_get_end(keylist);
	     keyptr = (mca_gpr_replica_keytable_t*)ompi_list_get_next(keyptr)) {
	    if (MCA_GPR_REPLICA_KEY_MAX == keyptr->key) { /* unknown token */
		goto CLEANUP;
	    }
	    *key2 = keyptr->key;
	    key2++;
	}
    }
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-get: got keylist", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

    /* traverse the segment's registry, looking for matching tokens per the specified mode */
    for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	 reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	 reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {

	/* for each registry entry, check the key list */
	if (gpr_replica_check_key_list(addr_mode, num_tokens, keys,
				       reg->num_keys, reg->keys)) { /* found the key(s) on the list */
	    ans = OBJ_NEW(ompi_registry_value_t);
	    ans->object_size = reg->object_size;
	    ans->object = (ompi_registry_object_t*)malloc(ans->object_size);
	    memcpy(ans->object, reg->object, ans->object_size);
	    ompi_list_append(answer, &ans->item);
	}
    }
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-get: finished search", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }

 CLEANUP:
    /* release list of keys */
    if(NULL != keylist)
        OBJ_RELEASE(keylist);

    if (NULL != keys) {
	free(keys);
	keys = NULL;
    }

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-get: leaving", ompi_process_info.name->cellid,
		    ompi_process_info.name->jobid, ompi_process_info.name->vpid);
    }
    return answer;
}

