/*
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

#include "ompi_config.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <libgen.h>

#include "include/constants.h"
#include "util/output.h"
#include "util/printf.h"
#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/gpr/base/base.h"
#include "gpr_replica.h"
#include "gpr_replica_internals.h"

/*
 *
 */

mca_gpr_replica_segment_t *gpr_replica_define_segment(char *segment)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t key;

     key = gpr_replica_define_key(segment, NULL);
     if (MCA_GPR_REPLICA_KEY_MAX == key) {  /* got some kind of error code */
 	return NULL;
     }

     /* need to add the segment to the registry */
     seg = OBJ_NEW(mca_gpr_replica_segment_t);
     seg->segment = key;
     ompi_list_append(&mca_gpr_replica_head.registry, &seg->item);

     return seg;
}


mca_gpr_replica_segment_t *gpr_replica_find_seg(bool create, char *segment)
{
    mca_gpr_replica_keytable_t *ptr_seg;
    mca_gpr_replica_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	 ptr_seg != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	 ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    /* search mca_gpr_replica_head to find segment */
	    for (seg=(mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
		 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
		 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {
		if(seg->segment == ptr_seg->key) {
		    return(seg);
		}
	    }
	}
    }
    if (create) {
    /* didn't find the dictionary entry - create it */
    return gpr_replica_define_segment(segment);
    }
    return NULL;  /* don't create it - just return NULL */
}

mca_gpr_replica_keytable_t *gpr_replica_find_dict_entry(char *segment, char *token)
{
    mca_gpr_replica_keytable_t *ptr_seg;
    mca_gpr_replica_keytable_t *ptr_key;
    mca_gpr_replica_segment_t *seg;

    /* search the registry segments to find which one is being referenced */
    for (ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	 ptr_seg != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	 ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr_seg)) {
	if (0 == strcmp(segment, ptr_seg->token)) {
	    if (NULL == token) { /* just want segment token-key pair */
		return(ptr_seg);
	    }
	    /* search registry to find segment */
	    for (seg=(mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
		 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
		 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {
		if(seg->segment == ptr_seg->key) {
		    /* got segment - now find specified token-key pair in that dictionary */
		    for (ptr_key = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&seg->keytable);
			 ptr_key != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&seg->keytable);
			 ptr_key = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr_key)) {
			if (0 == strcmp(token, ptr_key->token)) {
			    return(ptr_key);
			}
		    }
		    return(NULL); /* couldn't find the specified entry */
		}
	    }
	    return(NULL); /* couldn't find segment, even though we found entry in registry dict */
	}
    }
    return(NULL); /* couldn't find segment token-key pair */
}


mca_gpr_replica_key_t gpr_replica_get_key(char *segment, char *token)
{
    mca_gpr_replica_keytable_t *ptr_key;

    /* find registry segment */
    ptr_key = gpr_replica_find_dict_entry(segment, NULL);
    if (NULL != ptr_key) {
	if (NULL == token) { /* only want segment key */
	    return(ptr_key->key);
	}
	/* if token specified, find the dictionary entry that matches token */
	ptr_key = gpr_replica_find_dict_entry(segment, token);
	if (NULL != ptr_key) {
	    return(ptr_key->key);
	}
	return MCA_GPR_REPLICA_KEY_MAX; /* couldn't find dictionary entry */
    }
    return MCA_GPR_REPLICA_KEY_MAX; /* couldn't find segment */
}


ompi_list_t *gpr_replica_get_key_list(char *segment, char **tokens)
{
    ompi_list_t *keys;
    char **token;
    mca_gpr_replica_keytable_t *keyptr;

    token = tokens;
    keys = OBJ_NEW(ompi_list_t);

    /* protect against errors */
    if (NULL == segment || NULL == tokens) {
	return keys;
    }

    while (NULL != *token) {  /* traverse array of tokens until NULL */
	keyptr = OBJ_NEW(mca_gpr_replica_keytable_t);
	keyptr->token = strdup(*token);
	keyptr->key = gpr_replica_get_key(segment, *token);
	ompi_list_append(keys, &keyptr->item);
	token++;
    }
    return keys;
}

mca_gpr_replica_key_t gpr_replica_define_key(char *segment, char *token)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_keytable_t *ptr_seg, *ptr_key, *new;

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_ERROR;
    }

    /* if token is NULL, then this is defining a segment name. Check dictionary to ensure uniqueness */
    if (NULL == token) {
	for (ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&mca_gpr_replica_head.segment_dict);
	     ptr_seg != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&mca_gpr_replica_head.segment_dict);
	     ptr_seg = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr_seg)) {
	    if (0 == strcmp(segment, ptr_seg->token)) {
		return ptr_seg->key;
	    }
	}

	/* okay, name is not previously taken. Define a key value for it and return */
	new = OBJ_NEW(mca_gpr_replica_keytable_t);
	new->token = strdup(segment);
	if (0 == ompi_list_get_size(&mca_gpr_replica_head.freekeys)) { /* no keys waiting for reuse */
	    if (MCA_GPR_REPLICA_KEY_MAX-2 > mca_gpr_replica_head.lastkey) {  /* have a key left */
	    mca_gpr_replica_head.lastkey++;
	    new->key = mca_gpr_replica_head.lastkey;
	    } else {  /* out of keys */
		return MCA_GPR_REPLICA_KEY_MAX;
	    }
	} else {
	    ptr_key = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&mca_gpr_replica_head.freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&mca_gpr_replica_head.segment_dict, &new->item);
	return new->key;
    }

    /* okay, token is specified */
    /* search the registry segments to find which one is being referenced */
    seg = gpr_replica_find_seg(true, segment);
    if (NULL != seg) {
	/* using that segment, check dictionary to ensure uniqueness */
	for (ptr_key = (mca_gpr_replica_keytable_t*)ompi_list_get_first(&seg->keytable);
	     ptr_key != (mca_gpr_replica_keytable_t*)ompi_list_get_end(&seg->keytable);
	     ptr_key = (mca_gpr_replica_keytable_t*)ompi_list_get_next(ptr_key)) {
	    if (0 == strcmp(token, ptr_key->token)) {
		return ptr_key->key; /* already taken, report value */
	    }
	}
	/* okay, token is unique - create dictionary entry */
	new = OBJ_NEW(mca_gpr_replica_keytable_t);
	new->token = strdup(token);
	if (0 == ompi_list_get_size(&seg->freekeys)) { /* no keys waiting for reuse */
	    seg->lastkey++;
	    new->key = seg->lastkey;
	} else {
	    ptr_key = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&seg->freekeys);
	    new->key = ptr_key->key;
	}
	ompi_list_append(&seg->keytable, &new->item);
	return new->key;
    }
    /* couldn't find segment */
    return MCA_GPR_REPLICA_KEY_MAX;
}

int gpr_replica_delete_key(char *segment, char *token)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_core_t *reg;
    mca_gpr_replica_keytable_t *ptr_seg, *ptr_key, *new;
    mca_gpr_replica_key_t *key;
    int i;

    /* protect ourselves against errors */
    if (NULL == segment) {
	return(OMPI_ERROR);
    }

    /* find the segment */
    seg = gpr_replica_find_seg(false, segment);
    if (NULL != seg) {

	/* if specified token is NULL, then this is deleting a segment name.*/
	if (NULL == token) {
	    if (OMPI_SUCCESS != gpr_replica_empty_segment(seg)) { /* couldn't empty segment */
		return OMPI_ERROR;
	    }
	    /* now remove the dictionary entry from the global registry dictionary*/
	    ptr_seg = gpr_replica_find_dict_entry(segment, NULL);
	    if (NULL == ptr_seg) { /* failed to find dictionary entry */
		return OMPI_ERROR;
	    }
	    /* add key to global registry's freekey list */
	    new = OBJ_NEW(mca_gpr_replica_keytable_t);
	    new->token = NULL;
	    new->key = ptr_seg->key;
	    ompi_list_append(&mca_gpr_replica_head.freekeys, &new->item);

	    /* remove the dictionary entry */
	    ompi_list_remove_item(&mca_gpr_replica_head.segment_dict, &ptr_seg->item);
	    return(OMPI_SUCCESS);

	} else {  /* token not null, so need to find dictionary element to delete */
	    ptr_key = gpr_replica_find_dict_entry(segment, token);
	    if (NULL != ptr_key) {
		/* found key in dictionary */
		/* need to search this segment's registry to find all instances of key & "delete" them */
		for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
		     reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
		     reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {

		    /* check the key list */
		    for (i=0, key=reg->keys; i < reg->num_keys; i++, key++) {
			if (ptr_key->key == *key) {  /* found match */
			    *key = MCA_GPR_REPLICA_KEY_MAX;
			}
		    }

		    /* add key to this segment's freekey list */
		    new = OBJ_NEW(mca_gpr_replica_keytable_t);
		    new->token = NULL;
		    new->key = ptr_key->key;
		    ompi_list_append(&seg->freekeys, &new->item);

		    /* now remove the dictionary entry from the segment's dictionary */
		    ompi_list_remove_item(&seg->keytable, &ptr_key->item);
		    return(OMPI_SUCCESS);
		}
	    }
	    return(OMPI_ERROR); /* if we get here, then we couldn't find token in dictionary */
	}
    }
    return(OMPI_ERROR); /* if we get here, then we couldn't find segment */
}

int gpr_replica_empty_segment(mca_gpr_replica_segment_t *seg)
{
    mca_gpr_replica_core_t *ptr;
    mca_gpr_replica_keytable_t *keytab;
    mca_gpr_replica_keylist_t *keylst;

    /* need to free memory from each entry - remove_last returns pointer to the entry */

    /* empty the segment's registry */
    while (!ompi_list_is_empty(&seg->registry_entries)) {
	ptr = (mca_gpr_replica_core_t*)ompi_list_remove_first(&seg->registry_entries);
	OBJ_RELEASE(ptr);
    }

    /* empty the segment's dictionary */
    while (!ompi_list_is_empty(&seg->keytable)) {
	keytab = (mca_gpr_replica_keytable_t*)ompi_list_remove_first(&seg->keytable);
	OBJ_RELEASE(keytab);
    }

    /* empty the list of free keys */
    while (!ompi_list_is_empty(&seg->freekeys)) {
	keylst = (mca_gpr_replica_keylist_t*)ompi_list_remove_first(&seg->freekeys);
	OBJ_RELEASE(keylst);
    }
    /* now remove segment from global registry */
    ompi_list_remove_item(&mca_gpr_replica_head.registry, &seg->item);
    OBJ_RELEASE(seg);

    return OMPI_SUCCESS;
}

/*
 * A mode of "NONE" or "OVERWRITE" defaults to "XAND" behavior
 */
bool gpr_replica_check_key_list(ompi_registry_mode_t addr_mode,
				mca_gpr_replica_key_t num_keys_search, mca_gpr_replica_key_t *keys,
				mca_gpr_replica_key_t num_keys_entry, mca_gpr_replica_key_t *entry_keys)
{
    mca_gpr_replica_key_t *key1, *key2;
    int num_found;
    bool exclusive, no_match;
    int i, j;

    /* check for trivial case */
    if (NULL == keys) {  /* wildcard case - automatically true */
	return true;
    }

    if (OMPI_REGISTRY_NONE == addr_mode ||
	OMPI_REGISTRY_OVERWRITE == addr_mode) { /* set default behavior for search */
	addr_mode = OMPI_REGISTRY_XAND;
    }

    /* take care of trivial cases that don't require search */
    if ((OMPI_REGISTRY_XAND & addr_mode) &&
	(num_keys_search != num_keys_entry)) { /* can't possibly turn out "true" */
	return false;
    }

    if ((OMPI_REGISTRY_AND & addr_mode) &&
	(num_keys_search > num_keys_entry)) {  /* can't find enough matches */
	return false;
    }

    /* okay, have to search for remaining possibilities */
    num_found = 0;
    exclusive = true;
    for (i=0, key1=entry_keys; i < num_keys_entry; i++, key1++) {
	no_match = true;
	for (j=0, key2=keys; j < num_keys_search; j++, key2++) {
	    if (*key1 == *key2) { /* found a match */
		num_found++;
		no_match = false;
		if (OMPI_REGISTRY_OR & addr_mode) { /* only need one match */
		    return true;
		}
	    }
	}
	if (no_match) {
	    exclusive = false;
	}
    }

    if (OMPI_REGISTRY_XAND & addr_mode) {  /* deal with XAND case */
	if (num_found == num_keys_entry) { /* found all, and nothing more */
	    return true;
	} else {  /* found either too many or not enough */
	    return false;
	}
    }

    if (OMPI_REGISTRY_XOR & addr_mode) {  /* deal with XOR case */
	if (num_found > 0 && exclusive) {  /* found at least one and nothing not on list */
	    return true;
	} else {
	    return false;
	}
    }

    if (OMPI_REGISTRY_AND & addr_mode) {  /* deal with AND case */
	if (num_found == num_keys_search) {  /* found all the required keys */
	    return true;
	} else {
	    return false;
	}
    }

    /* should be impossible situation, but just to be safe... */
    return false;
}

int gpr_replica_construct_synchro(ompi_registry_synchro_mode_t synchro_mode,
				  ompi_registry_mode_t addr_mode,
				  char *segment, char **tokens, int trigger,
				  mca_gpr_notify_id_t id_tag)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_synchro_list_t *synch;
    char **tokptr;
    mca_gpr_replica_key_t *keyptr;
    int i, num_tokens;

    seg = gpr_replica_find_seg(true, segment);
    if (NULL == seg) { /* couldn't find segment */
	return OMPI_ERROR;
    }

    synch = OBJ_NEW(mca_gpr_replica_synchro_list_t);

    synch->synch_mode = synchro_mode;
    synch->addr_mode = addr_mode;
    synch->trigger = trigger;
    synch->count = 0;
    synch->id_tag = id_tag;

    synch->num_keys = 0;
    synch->keys = NULL;

    if (NULL != tokens) {  /* tokens provided */

	/* count number of tokens */
	tokptr = tokens;
	num_tokens = 0;
	while (NULL != tokptr && NULL != *tokptr) {
	    num_tokens++;
	    tokptr++;
	}
	synch->keys = (mca_gpr_replica_key_t*)malloc(num_tokens*sizeof(mca_gpr_replica_key_t));
	keyptr = synch->keys;
	/* store key values of tokens, defining them if needed */
	for (i=0, tokptr=tokens; NULL != tokptr && NULL != *tokptr; i++, tokptr++) {
	    *keyptr = gpr_replica_get_key(segment, *tokptr);
	    if (MCA_GPR_REPLICA_KEY_MAX == *keyptr) {
		*keyptr = gpr_replica_define_key(segment, *tokptr);
	    }
	    keyptr++;
	}
	synch->num_keys = num_tokens;
    }

    ompi_list_append(&seg->synchros, &synch->item);

    return OMPI_SUCCESS;

}


ompi_registry_notify_message_t *gpr_replica_construct_notify_message(ompi_registry_mode_t addr_mode, char *segment, char **tokens)
{
    ompi_list_t *reg_entries;
    ompi_registry_value_t *reg, *obj;
    ompi_registry_notify_message_t *msg;
    char **tokptr, **tokptr2;
    int num_tokens, i;

    /* protect against errors */
    if (NULL == segment) {
	return NULL;
    }

    reg_entries = gpr_replica_get(addr_mode, segment, tokens);

    /* compute number of tokens */
    tokptr = tokens;
    num_tokens = 0;
    while (NULL != *tokptr) {
	num_tokens++;
	tokptr++;
    }

    msg = OBJ_NEW(ompi_registry_notify_message_t);
    msg->num_tokens = num_tokens;
    msg->tokens = (char**)malloc(num_tokens*(sizeof(char*)));
    tokptr = tokens;
    tokptr2 = msg->tokens;
    for (i=0, tokptr=tokens, tokptr2=msg->tokens;
	 i < num_tokens;
	 i++, tokptr++, tokptr2++) {
	*tokptr2 = strdup(*tokptr);
    }

    while (NULL != (reg = (ompi_registry_value_t*)ompi_list_remove_first(reg_entries))) {
	obj = OBJ_NEW(ompi_registry_value_t);
	obj->object = (ompi_registry_object_t)malloc(reg->object_size);
	memcpy(obj->object, reg->object, reg->object_size);
	obj->object_size = reg->object_size;
	ompi_list_append(&msg->data, &obj->item);
	OBJ_RELEASE(reg);
    }

    OBJ_RELEASE(reg_entries);

    return msg;
}

void gpr_replica_process_triggers(ompi_registry_notify_message_t *message,
				  mca_gpr_notify_id_t id_tag)
{
    mca_gpr_notify_request_tracker_t *trackptr, *tmpptr;
    ompi_registry_object_t *data;
    char **tokptr;
    int i;
    bool found;

    /* protect against errors */
    if (NULL == message) {
	return;
    }

    /* find corresponding notify request */
    found = false;
    for (trackptr = (mca_gpr_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
	     trackptr != (mca_gpr_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker);
	     trackptr = (mca_gpr_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
	if (trackptr->id_tag == id_tag) {
	    found = true;
        break;
	}
    }

    if (!found) {  /* didn't find request */
	ompi_output(0, "Notification error - request not found");
	return;
    }

    /* process request */
    if (NULL == trackptr->requestor) {  /* local request - callback fn with their tag */
	trackptr->callback(message, trackptr->user_tag);
	/* dismantle message and free memory */
	while (NULL != (data = (ompi_registry_object_t*)ompi_list_remove_first(&message->data))) {
	    OBJ_RELEASE(data);
	}
	for (i=0, tokptr=message->tokens; i < message->num_tokens; i++, tokptr++) {
	    free(*tokptr);
	}
	free(message->tokens);
	free(message);

    } else {  /* remote request - send message back */
	gpr_replica_remote_notify(trackptr->requestor, trackptr->req_tag, message);
    }

    /* if one-shot, remove request from tracking system */
    if (OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT & trackptr->synchro) {
	    ompi_list_remove_item(&mca_gpr_replica_notify_request_tracker, &trackptr->item);
	    OBJ_RELEASE(trackptr);
    }
}

ompi_list_t *gpr_replica_test_internals(int level)
{
    ompi_list_t *test_results, *keylist;
    ompi_registry_internal_test_results_t *result;
    char name[30], name2[30];
    char *name3[30];
    int i, j, k;
    mca_gpr_replica_key_t segkey, key;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_keytable_t *dict_entry;
    bool success;

    test_results = OBJ_NEW(ompi_list_t);

    ompi_output(0, "testing define segment");
    /* create several test segments */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-create-segment");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	if (NULL == gpr_replica_define_segment(name)) {
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing get key for segment ");
    /* check ability to get key for a segment */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-seg-key");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	key = gpr_replica_get_key(name, NULL);
	if (MCA_GPR_REPLICA_KEY_MAX == key) { /* got an error */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing define key");
    /* check that define key protects uniqueness */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-define-key-uniqueness");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	segkey = gpr_replica_get_key(name, NULL);
	key = gpr_replica_define_key(name, NULL);
	if (segkey != key) { /* got an error */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing find segment");
    /* check the ability to find a segment */
    i = 2;
    sprintf(name, "test-def-seg%d", i);
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-find-seg");
    seg = gpr_replica_find_seg(false, name);
    if (NULL == seg) {
	asprintf(&result->message, "test failed with NULL returned: %s", name);
    } else {  /* locate key and check it */
	segkey = gpr_replica_get_key(name, NULL);
	if (segkey == seg->segment) {
	    result->message = strdup("success");
	} else {
	    asprintf(&result->message, "test failed: key %d seg %d", segkey, seg->segment);
	}
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing define key within segment");
    /* check ability to define key within a segment */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-define-key-segment");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	for (j=0; j<10 && success; j++) {
 	    sprintf(name2, "test-key%d", j);
	    k = gpr_replica_define_key(name, name2);
	    if (0 > k) { /* got an error */
		success = false;
	    }
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);


    ompi_output(0, "testing get key within segment");
    /* check ability to retrieve key within a segment */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-key-segment");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	for (j=0; j<10 && success; j++) {
 	    sprintf(name2, "test-key%d", j);
	    key = gpr_replica_get_key(name, name2);
	    if (MCA_GPR_REPLICA_KEY_MAX == key) { /* got an error */
		success = false;
	    }
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);


    ompi_output(0, "testing get dict entry - global");
    /* check ability to get dictionary entries */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-dict-entry");
    /* first check ability to get segment values */
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	dict_entry = gpr_replica_find_dict_entry(name, NULL);
	if (NULL == dict_entry) { /* got an error */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    ompi_output(0, "testing get dict entry - segment");
    if (success) { /* segment values checked out - move on to within a segment */
	result = OBJ_NEW(ompi_registry_internal_test_results_t);
	result->test = strdup("test-get-dict-entry-segment");
	for (i=0; i<5; i++) {
	    sprintf(name, "test-def-seg%d", i);
	    for (j=0; j<10; j++) {
		sprintf(name2, "test-key%d", j);
		dict_entry = gpr_replica_find_dict_entry(name, NULL);
		if (NULL == dict_entry) { /* got an error */
		    success = false;
		}
	    }
	}
	if (success) {
	    result->message = strdup("success");
	} else {
	    result->message = strdup("failed");
	}
	ompi_list_append(test_results, &result->item);
    }


    ompi_output(0, "testing get key list");
    /* check ability to get key list */
    success = true;
    result = OBJ_NEW(ompi_registry_internal_test_results_t);
    result->test = strdup("test-get-keylist");
    for (i=0; i<5 && success; i++) {
	sprintf(name, "test-def-seg%d", i);
	for (j=0; j<10 && success; j++) {
 	    asprintf(&name3[j], "test-key%d", j);
	}
	name3[j] = NULL;
	keylist = gpr_replica_get_key_list(name, name3);
	if (0 >= ompi_list_get_size(keylist)) { /* error condition */
	    success = false;
	}
    }
    if (success) {
	result->message = strdup("success");
    } else {
	result->message = strdup("failed");
    }
    ompi_list_append(test_results, &result->item);

    /* check ability to empty segment */

    return test_results;
}
