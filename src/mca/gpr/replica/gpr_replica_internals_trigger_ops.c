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

#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"

mca_gpr_replica_trigger_list_t*
mca_gpr_replica_construct_trigger(ompi_registry_synchro_mode_t synchro_mode,
				  ompi_registry_notify_action_t action,
				  ompi_registry_mode_t addr_mode,
				  mca_gpr_replica_segment_t *seg,
				  mca_gpr_replica_key_t *keys,
				  int num_keys,
				  int trigger,
				  ompi_registry_notify_id_t id_tag)
{
    mca_gpr_replica_core_t *reg;
    mca_gpr_replica_trigger_list_t *trig;
    mca_gpr_replica_key_t *key2, *keyptr;
    int i;


    trig = OBJ_NEW(mca_gpr_replica_trigger_list_t);

    trig->synch_mode = synchro_mode;
    trig->action = action;
    trig->addr_mode = addr_mode;
    trig->trigger = trigger;
    trig->count = 0;
    trig->local_idtag = id_tag;

    trig->num_keys = num_keys;
    if (0 < num_keys) {
	trig->keys = (mca_gpr_replica_key_t*)malloc(num_keys*sizeof(mca_gpr_replica_key_t));
	keyptr = trig->keys;
	key2 = keys;
	for (i=0; i < num_keys; i++) {
	    *keyptr = *key2;
	    keyptr++; key2++;
	}
    } else {
	trig->keys = NULL;
    }

    if (OMPI_REGISTRY_SYNCHRO_MODE_NONE != synchro_mode) { /* this is a synchro, so initialize the count */
	/* traverse segment entries and initialize trigger count */
	for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
	     reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
	     reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {
	    if (mca_gpr_replica_check_key_list(addr_mode, trig->num_keys, trig->keys,
					       reg->num_keys, reg->keys)) {
		trig->count++;
	    }
	}

	/* initialize edge trigger state */
	if (OMPI_REGISTRY_SYNCHRO_MODE_NONE != trig->synch_mode) { /* looking at synchro event */
	    if (trig->count > trig->trigger) {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_ABOVE_LEVEL;
	    } else if (trig->count < trig->trigger) {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_BELOW_LEVEL;
	    } else {
		trig->above_below = MCA_GPR_REPLICA_TRIGGER_AT_LEVEL;
	    }
	}
    }


    ompi_list_append(&seg->triggers, &trig->item);


    return trig;

}

ompi_registry_notify_id_t
mca_gpr_replica_remove_trigger(ompi_registry_notify_id_t idtag)
{
    /* 
     * need to register callback to remove entry on remote notify_id_tracker
     * if remote_idtag != 0
     */

    mca_gpr_replica_notify_request_tracker_t *trackptr;
    ompi_registry_notify_id_t remote_idtag;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_trigger_list_t *trig;

    /* find request on notify tracking system */
    for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
	 trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker);
	 trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {

	if (trackptr->local_idtag == idtag) {
	    /* find the trigger on the segment and remove it */
	    seg = trackptr->segptr;
	    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
		 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
		 trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig)) {
		if (trig->local_idtag == idtag) {
		    ompi_list_remove_item(&seg->triggers, &trig->item);
		    OBJ_RELEASE(trig);
		    /* save the remote_idtag so it can be returned */
		    remote_idtag = trackptr->remote_idtag;
		    /* remove the request from the notify tracking system */
		    ompi_list_remove_item(&mca_gpr_replica_notify_request_tracker, &trackptr->item);
		    OBJ_RELEASE(trackptr);
		    return remote_idtag;
		}
	    }
	}
    }
    return OMPI_REGISTRY_NOTIFY_ID_MAX; /* couldn't find the trigger */
}


ompi_registry_notify_message_t
*mca_gpr_replica_construct_notify_message(mca_gpr_replica_segment_t *seg,
					  mca_gpr_replica_trigger_list_t *trig)
{
    ompi_list_t *reg_entries;
    ompi_registry_value_t *reg, *obj;
    ompi_registry_notify_message_t *msg;
    mca_gpr_replica_key_t *keyptr;
    char **tokptr;
    int i;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "trigger fired on segment %s", seg->name);
    }

    reg_entries = OBJ_NEW(ompi_list_t);
    mca_gpr_replica_get_nl(reg_entries, trig->addr_mode, seg, trig->keys, trig->num_keys);

    msg = OBJ_NEW(ompi_registry_notify_message_t);
    msg->segment = strdup(seg->name);
    msg->owning_job = seg->owning_job;
    msg->num_tokens = trig->num_keys;
    if(0 < trig->num_keys) {
        msg->tokens = (char**)malloc(trig->num_keys*(sizeof(char*)));
	keyptr = trig->keys;
	tokptr = msg->tokens;
	for (i=0; i < (int)msg->num_tokens; i++, keyptr++, tokptr++) {
	    *tokptr = mca_gpr_replica_get_token(seg, *keyptr);
	}
    } else {
        msg->tokens = NULL;
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
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-construct_notify: msg built",
                    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    return msg;
}

bool mca_gpr_replica_process_triggers(mca_gpr_replica_segment_t *seg,
				      mca_gpr_replica_trigger_list_t *trig,
				      ompi_registry_notify_message_t *message)
{
    mca_gpr_replica_notify_request_tracker_t *trackptr;
    bool found;
    mca_gpr_replica_callbacks_t *cb;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: process_trig entered",
                    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    /* find corresponding notify request */
    found = false;
    for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
	 trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker);
	 trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
	if (trackptr->local_idtag == trig->local_idtag) {
	    found = true;
	    break;
	}
    }

    if (!found) {  /* didn't find request */
	ompi_output(0, "Notification error - request not found");
	/* OMPI_THREAD_UNLOCK(&mca_gpr_replica_internals_mutex); */
	return true;
    }

    /* process request */
    cb = OBJ_NEW(mca_gpr_replica_callbacks_t);
    if (NULL == trackptr->requestor) {  /* local request - queue callback fn with their tag */
	cb->requestor = NULL;
	cb->cb_func = trackptr->callback;
	cb->user_tag = trackptr->user_tag;
	cb->message = message;
	cb->remote_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;

    } else {  /* remote request - queue remote callback */
	cb->requestor = ompi_name_server.copy_process_name(trackptr->requestor);
	cb->cb_func = NULL;
	cb->user_tag = NULL;
	cb->message = message;
	cb->remote_idtag = trackptr->remote_idtag;
    }
    ompi_list_append(&mca_gpr_replica_callbacks, &cb->item);

    /* if one-shot, remove request from tracking system */
    if ((OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT & trig->synch_mode) ||
	(OMPI_REGISTRY_NOTIFY_ONE_SHOT & trig->action)) {
	ompi_list_remove_item(&mca_gpr_replica_notify_request_tracker, &trackptr->item);
	OBJ_RELEASE(trackptr);

	/* ....and from the corresponding registry segment */
	ompi_list_remove_item(&seg->triggers, &trig->item);
	OBJ_RELEASE(trig);
    }
    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica-process_trig: complete",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    return false;


}


int mca_gpr_replica_purge_subscriptions(ompi_process_name_t *proc)
{
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_notify_request_tracker_t *trackptr, *next;
    mca_gpr_replica_trigger_list_t *trig, *next_trig;

    if (NULL == proc) {  /* protect against errors */
	return OMPI_ERROR;
    }

    /* locate any notification events that have proc as the recipient
     */
    for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
	 trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker);) {
	next = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr);
	if ((NULL != trackptr->requestor &&
	     0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, proc, trackptr->requestor)) ||
	    (NULL == trackptr->requestor &&
	     0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, proc, ompi_rte_get_self()))) {

	    /* ...find the associated subscription... */
	    if (NULL != trackptr->segptr) {
		seg = trackptr->segptr;
		for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
		     trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
		     ) {
		    next_trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);
		    if (trackptr->local_idtag == trig->local_idtag) { /* found it */
			/* ...delete it... */
			ompi_list_remove_item(&seg->triggers, &trig->item);
		    }
		    trig = next_trig;
		}
	    }
	    /* ...and delete me too! */
	    ompi_list_remove_item(&mca_gpr_replica_notify_request_tracker, &trackptr->item);
	    OBJ_RELEASE(trackptr);
	}
	trackptr = next;
    }

    return OMPI_SUCCESS;
}


ompi_registry_notify_id_t
mca_gpr_replica_enter_notify_request(mca_gpr_replica_segment_t *seg,
				     ompi_registry_notify_action_t action,
				     ompi_process_name_t *requestor,
				     ompi_registry_notify_id_t idtag,
				     ompi_registry_notify_cb_fn_t cb_func,
				     void *user_tag)
 {
    mca_gpr_replica_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;

    trackptr = OBJ_NEW(mca_gpr_replica_notify_request_tracker_t);
    trackptr->segptr = seg;
    trackptr->action = action;
    trackptr->requestor = ompi_name_server.copy_process_name(requestor);
    trackptr->local_idtag = idtag;
    trackptr->remote_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
    trackptr->callback = cb_func;
    trackptr->user_tag = user_tag;
    if (ompi_list_is_empty(&mca_gpr_replica_free_notify_id_tags)) {
	trackptr->local_idtag = mca_gpr_replica_last_notify_id_tag;
	mca_gpr_replica_last_notify_id_tag++;
    } else {
	ptr_free_id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_replica_free_notify_id_tags);
	trackptr->local_idtag = ptr_free_id->id_tag;
    }
    ompi_list_append(&mca_gpr_replica_notify_request_tracker, &trackptr->item);

    return trackptr->local_idtag;
}


ompi_registry_notify_id_t mca_gpr_replica_remove_notify_request(ompi_registry_notify_id_t idtag)
{
    mca_gpr_replica_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;
    ompi_registry_notify_id_t remote_idtag;

    /* find request on replica notify tracking system */
    for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
	 trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker) &&
	     trackptr->local_idtag != idtag;
	 trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr));

    if (trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker)) {
	/* save the remote idtag */
	remote_idtag = trackptr->remote_idtag;

	/* ...and remove the request */
	ompi_list_remove_item(&mca_gpr_replica_notify_request_tracker, &trackptr->item);
	/* put local id tag on free list */
	ptr_free_id = OBJ_NEW(mca_gpr_idtag_list_t);
	ptr_free_id->id_tag = trackptr->local_idtag;
	ompi_list_append(&mca_gpr_replica_free_notify_id_tags, &ptr_free_id->item);
	/* release tracker item */
	OBJ_RELEASE(trackptr);

	return remote_idtag;
    }
    /* error condition if reach here */
    return OMPI_REGISTRY_NOTIFY_ID_MAX;
}

int mca_gpr_replica_check_synchros(mca_gpr_replica_segment_t *seg)
{
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;
    mca_gpr_replica_trigger_list_t* next;
    bool still_valid=false;

    /* search the segment and re-compute the trigger levels */

    /* check for trigger conditions */
    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
         ) {
	next = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);
	still_valid = true;
	if (((OMPI_REGISTRY_SYNCHRO_MODE_ASCENDING & trig->synch_mode)
	     && (trig->count >= trig->trigger)
	     && (MCA_GPR_REPLICA_TRIGGER_BELOW_LEVEL == trig->above_below)) ||
	    ((OMPI_REGISTRY_SYNCHRO_MODE_DESCENDING & trig->synch_mode)
	     && (trig->count <= trig->trigger)
	     && (MCA_GPR_REPLICA_TRIGGER_ABOVE_LEVEL == trig->above_below)) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_LEVEL & trig->synch_mode && trig->count == trig->trigger) ||
	    (OMPI_REGISTRY_SYNCHRO_MODE_GT_EQUAL & trig->synch_mode && trig->count >= trig->trigger)) {

	    notify_msg = mca_gpr_replica_construct_notify_message(seg, trig);
	    notify_msg->trig_action = OMPI_REGISTRY_NOTIFY_NONE;
	    notify_msg->trig_synchro = trig->synch_mode;
	    still_valid = mca_gpr_replica_process_triggers(seg, trig, notify_msg);

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
    return OMPI_SUCCESS;
}

void mca_gpr_replica_check_subscriptions(mca_gpr_replica_segment_t *seg, int8_t action_taken)
{
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;
    mca_gpr_replica_trigger_list_t* next;
    bool still_valid=false;

    if (!seg->triggers_active) {  /* triggers are not active */
	return;
    }

    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
	 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
         ) {
	next = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);
	if ((OMPI_REGISTRY_NOTIFY_ALL & trig->action) ||
	    ((OMPI_REGISTRY_NOTIFY_ADD_ENTRY & trig->action) && (MCA_GPR_REPLICA_OBJECT_ADDED == action_taken)) ||
	    ((OMPI_REGISTRY_NOTIFY_MODIFICATION & trig->action) && (MCA_GPR_REPLICA_OBJECT_UPDATED == action_taken)) ||
	    ((OMPI_REGISTRY_NOTIFY_DELETE_ENTRY & trig->action) && (MCA_GPR_REPLICA_OBJECT_DELETED == action_taken)) ||
	    ((OMPI_REGISTRY_NOTIFY_ADD_SUBSCRIBER & trig->action) && (MCA_GPR_REPLICA_SUBSCRIBER_ADDED == action_taken))) {
	    notify_msg = mca_gpr_replica_construct_notify_message(seg, trig);
	    notify_msg->trig_action = trig->action;
	    notify_msg->trig_synchro = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
	    still_valid = mca_gpr_replica_process_triggers(seg, trig, notify_msg);
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
}
