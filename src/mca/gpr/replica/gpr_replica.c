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
#include "util/output.h"
#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"


int gpr_replica_delete_segment(char *segment)
{
    mca_gpr_replica_segment_t *seg;

    seg = gpr_replica_find_seg(true, segment);

    if (NULL == seg) {  /* couldn't locate segment */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != gpr_replica_empty_segment(seg)) {  /* couldn't empty the segment */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != gpr_replica_delete_key(segment, NULL)) { /* couldn't remove dictionary entry */
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

int gpr_replica_put(ompi_registry_mode_t addr_mode, char *segment,
		    char **tokens, ompi_registry_object_t object,
		    ompi_registry_object_size_t size)
{
    ompi_list_t *keylist;
    mca_gpr_replica_keytable_t *keyptr;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_core_t *entry_ptr;
    ompi_registry_mode_t put_mode;
    mca_gpr_replica_synchro_list_t *synchro;
    ompi_registry_notify_message_t *notify_msg;
    int return_code, num_tokens;
    mca_gpr_replica_key_t *keys, *key2;


    /* protect ourselves against errors */
    if (NULL == object || 0 == size || NULL == tokens || NULL == *tokens) {
	return OMPI_ERROR;
    }
    put_mode = addr_mode & OMPI_REGISTRY_OVERWRITE;  /* only overwrite permission mode flag allowed */

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

    /* update synchro list and check for trigger conditions */
    for (synchro = (mca_gpr_replica_synchro_list_t*)ompi_list_get_first(&seg->synchros);
	 synchro != (mca_gpr_replica_synchro_list_t*)ompi_list_get_end(&seg->synchros);
	 synchro = (mca_gpr_replica_synchro_list_t*)ompi_list_get_next(synchro)) {
	if (gpr_replica_check_key_list(synchro->addr_mode, synchro->num_keys, synchro->keys,
				       num_tokens, keys)) {
	    synchro->count++;
	}
	if ((OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING & synchro->synch_mode ||
	     OMPI_REGISTRY_SYNCHRO_MODE_LEVEL & synchro->synch_mode) &&
	    synchro->count == synchro->trigger) {
	    notify_msg = gpr_replica_construct_notify_message(addr_mode, segment, tokens);
	    gpr_replica_process_triggers(notify_msg, synchro->id_tag);
	}
    }


 CLEANUP:
    /* release list of keys */
    while (NULL != (keyptr = (mca_gpr_replica_keytable_t*)ompi_list_remove_last(keylist))) {
	OBJ_RELEASE(keyptr);
    }
    OBJ_RELEASE(keylist);

    return return_code;
}

int gpr_replica_delete_object(ompi_registry_mode_t addr_mode,
			      char *segment, char **tokens)
{
    mca_gpr_replica_core_t *reg, *prev;
    mca_gpr_replica_keytable_t *keyptr;
    ompi_list_t *keylist;
    mca_gpr_replica_key_t *keys, *key2;
    mca_gpr_replica_segment_t *seg;
    int num_tokens;

    keys = NULL;

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

    return OMPI_SUCCESS;

 CLEANUP:
    if (NULL != keys) {
	free(keys);
    }
    return OMPI_ERROR;
}

ompi_list_t* gpr_replica_index(char *segment)
{
    ompi_list_t *answer;
    mca_gpr_replica_keytable_t *ptr;
    mca_gpr_replica_segment_t *seg;
    ompi_registry_index_value_t *ans;

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

int gpr_replica_subscribe(ompi_registry_mode_t mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens,
			  ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int gpr_replica_unsubscribe(ompi_registry_mode_t mode,
			    ompi_registry_notify_action_t action,
			    char *segment, char **tokens,
			    ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int gpr_replica_synchro(ompi_registry_synchro_mode_t synchro_mode,
			ompi_registry_mode_t addr_mode,
			char *segment, char **tokens, int trigger,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    mca_gpr_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;

    /* protect against errors */
    if (NULL == segment || 0 > trigger) {
	return OMPI_ERROR;
    }

    /* enter request on notify tracking system */
    trackptr = OBJ_NEW(mca_gpr_notify_request_tracker_t);
    trackptr->requestor = NULL;
    trackptr->req_tag = 0;
    trackptr->callback = cb_func;
    trackptr->user_tag = user_tag;
    if (ompi_list_is_empty(&mca_gpr_replica_free_notify_id_tags)) {
	trackptr->id_tag = mca_gpr_replica_last_notify_id_tag;
	mca_gpr_replica_last_notify_id_tag++;
    } else {
	ptr_free_id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_replica_free_notify_id_tags);
	trackptr->id_tag = ptr_free_id->id_tag;
    }
    trackptr->synchro = synchro_mode;

    /* construct the synchro - add to notify tracking system if success, otherwise dump */
    if (OMPI_SUCCESS == gpr_replica_construct_synchro(synchro_mode, addr_mode, segment, tokens,
						      trigger, trackptr->id_tag)) {
	ompi_list_append(&mca_gpr_replica_notify_request_tracker, &trackptr->item);
	return OMPI_SUCCESS;
    } else {
	OBJ_RELEASE(trackptr);
	return OMPI_ERROR;
    }
}

ompi_list_t* gpr_replica_get(ompi_registry_mode_t addr_mode,
			     char *segment, char **tokens)
{
    mca_gpr_replica_segment_t *seg;
    ompi_list_t *answer;
    ompi_registry_value_t *ans;
    mca_gpr_replica_key_t *keys, *key2;
    ompi_list_t *keylist;
    mca_gpr_replica_keytable_t *keyptr;
    mca_gpr_replica_core_t *reg;
    int num_tokens;

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
    if (NULL == tokens) { /* wildcard case - return everything */
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

 CLEANUP:
    /* release list of keys */
    while (NULL != (keyptr = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(keylist))) {
	OBJ_RELEASE(keyptr);
    }
    OBJ_RELEASE(keylist);
    if (NULL != keys) {
	free(keys);
    }

    return answer;
}

