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

ompi_registry_notify_id_t
mca_gpr_replica_subscribe(ompi_registry_mode_t addr_mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens,
			  ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    int rc;
    ompi_registry_notify_id_t local_idtag;
    mca_gpr_replica_segment_t *seg;
    mca_gpr_replica_key_t *keys;
    int num_keys;

    /* protect against errors */
    if (NULL == segment) {
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }

    seg = mca_gpr_replica_find_seg(true, segment, ompi_name_server.get_jobid(ompi_rte_get_self()));
    if (NULL == seg) { /* segment couldn't be found or created */
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }

    if (mca_gpr_replica_compound_cmd_mode) {

	mca_gpr_base_pack_subscribe(mca_gpr_replica_compound_cmd,
				    addr_mode, action,
				    segment, tokens);

	OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

	/* enter request on notify tracking system */
	local_idtag = mca_gpr_replica_enter_notify_request(seg, action, NULL, 0, cb_func, user_tag);

	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

	ompi_pack(mca_gpr_replica_compound_cmd, &local_idtag, 1, OMPI_INT32);

	return local_idtag;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* enter request on notify tracking system */
    local_idtag = mca_gpr_replica_enter_notify_request(seg, action, NULL, 0, cb_func, user_tag);

    /* convert tokens to keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    /* register subscription */
    rc = mca_gpr_replica_subscribe_nl(addr_mode, action, seg,
				      keys, num_keys, local_idtag);

    /* check subscriptions */
    mca_gpr_replica_check_subscriptions(seg, MCA_GPR_REPLICA_SUBSCRIBER_ADDED);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    mca_gpr_replica_process_callbacks();

    return local_idtag;
}


int mca_gpr_replica_subscribe_nl(ompi_registry_mode_t addr_mode,
				 ompi_registry_notify_action_t action,
				 mca_gpr_replica_segment_t *seg,
				 mca_gpr_replica_key_t *keys,
				 int num_keys,
				 ompi_registry_notify_id_t id_tag)
{
    mca_gpr_replica_trigger_list_t *trig;
    ompi_registry_notify_message_t *notify_msg;
    ;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: subscribe entered: segment %s",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), seg->name);
    }

    /* construct the trigger */
    if (NULL != (trig = mca_gpr_replica_construct_trigger(OMPI_REGISTRY_SYNCHRO_MODE_NONE, action,
							  addr_mode, seg, keys, num_keys,
							  0, id_tag))) {

	if ((OMPI_REGISTRY_NOTIFY_PRE_EXISTING & action) && seg->triggers_active) {  /* want list of everything there */
	    notify_msg = mca_gpr_replica_construct_notify_message(seg, trig);
	    notify_msg->trig_action = action;
	    notify_msg->trig_synchro = OMPI_REGISTRY_SYNCHRO_MODE_NONE;
	    mca_gpr_replica_process_triggers(seg, trig, notify_msg);
	}
	return OMPI_SUCCESS;
    } else {
	return OMPI_ERROR;
    }
}


int mca_gpr_replica_unsubscribe(ompi_registry_notify_id_t sub_number)
{
    uint rc;

    if (mca_gpr_replica_compound_cmd_mode) {
	return mca_gpr_base_pack_unsubscribe(mca_gpr_replica_compound_cmd,
					     mca_gpr_replica_silent_mode, sub_number);
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    rc = mca_gpr_replica_unsubscribe_nl(sub_number);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    if (OMPI_REGISTRY_NOTIFY_ID_MAX == rc) {
	return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


ompi_registry_notify_id_t
mca_gpr_replica_unsubscribe_nl(ompi_registry_notify_id_t sub_number)
{

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: unsubscribe entered for sub number %d",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), sub_number);
    }

    /* find trigger on replica and remove it - return requestor's id_tag */
    return mca_gpr_replica_remove_trigger(sub_number);

}
