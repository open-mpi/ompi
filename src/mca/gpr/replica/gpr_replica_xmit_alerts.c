/* -*- C -*-
 *
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
 * The Open MPI General Purpose Registry - Replica component
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "gpr_replica.h"
#include "gpr_replica_internals.h"


void mca_gpr_replica_process_callbacks(void)
{
    mca_gpr_replica_callbacks_t *cb;

    /* aggregate messages for identical recipient - local messages just get called */

    /* send messages to de-aggregator - that function unpacks them and issues callbacks */
    if (mca_gpr_replica_debug) {
	ompi_output(0, "gpr replica: process_callbacks entered");
    }


    while (NULL != (cb = (mca_gpr_replica_callbacks_t*)ompi_list_remove_first(&mca_gpr_replica_callbacks))) {
	if (NULL == cb->requestor) {  /* local callback */
	    if (mca_gpr_replica_debug) {
		ompi_output(0, "process_callbacks: local");
	    }
	    cb->cb_func(cb->message, cb->user_tag);
	} else {  /* remote request - send message back */
	    if (mca_gpr_replica_debug) {
		ompi_output(0, "process_callbacks: remote to [%d,%d,%d]", cb->requestor->cellid,
			    cb->requestor->jobid, cb->requestor->vpid);
	    }
	    mca_gpr_replica_remote_notify(cb->requestor, cb->remote_idtag, cb->message);
	}
	OBJ_RELEASE(cb);
    }
}


ompi_buffer_t mca_gpr_replica_get_startup_msg(mca_ns_base_jobid_t jobid,
					      ompi_list_t *recipients)
{
    ompi_buffer_t msg;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] entered get_startup_msg",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    msg = mca_gpr_replica_construct_startup_msg_nl(jobid, recipients);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    return msg;
}


ompi_buffer_t
mca_gpr_replica_construct_startup_msg_nl(mca_ns_base_jobid_t jobid,
						               ompi_list_t *recipients)
{
    mca_gpr_replica_segment_t *seg=NULL, *proc_stat_seg;
    mca_gpr_replica_key_t *keys;
    int num_keys;
    mca_gpr_replica_core_t *reg=NULL;
    mca_gpr_replica_trigger_list_t *trig, *next_trig;
    mca_gpr_replica_notify_request_tracker_t *trackptr;
    ompi_name_server_namelist_t *peer, *ptr;
    ompi_rte_process_status_t *proc_status;
    ompi_process_name_t *name;
    char *segment, *tokens[2];
    int32_t size;
    ompi_buffer_t msg;
    ompi_list_t *returned_list;
    ompi_registry_value_t *value;
    bool found, include_data, done;
    size_t bufsize;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] entered construct_startup_msg for job %d",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()), (int)jobid);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0)) {
	return NULL;
    }

    /* setup tokens and segments for this job */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT,
	     ompi_name_server.convert_jobid_to_string(jobid));

    /* find the specified segment */
    proc_stat_seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);

    /* traverse the registry's segments */
    for (seg = (mca_gpr_replica_segment_t*)ompi_list_get_first(&mca_gpr_replica_head.registry);
	 seg != (mca_gpr_replica_segment_t*)ompi_list_get_end(&mca_gpr_replica_head.registry);
	 seg = (mca_gpr_replica_segment_t*)ompi_list_get_next(seg)) {

	if (mca_gpr_replica_debug) {
	    ompi_output(0, "[%d,%d,%d] construct_ss_msg: checking segment %s owned by %d",
			OMPI_NAME_ARGS(*ompi_rte_get_self()), seg->name, (int)seg->owning_job);
	}

	if ((jobid == seg->owning_job) ||    /* this segment is part of the specified jobid */
        (MCA_NS_BASE_JOBID_MAX == seg->owning_job)) {    /* wildcard segment - belongs to all */

	    ompi_pack_string(msg, seg->name);  /* pack the segment name */
	    include_data = false;

	    /* construct the list of recipients and find out if data is desired */
	    for (trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_first(&seg->triggers);
		 trig != (mca_gpr_replica_trigger_list_t*)ompi_list_get_end(&seg->triggers);
		 ) {
		next_trig = (mca_gpr_replica_trigger_list_t*)ompi_list_get_next(trig);

		if (OMPI_REGISTRY_NOTIFY_ON_STARTUP & trig->action) {

		    /* see if data is requested - only one trig has to ask for it */
		    if (OMPI_REGISTRY_NOTIFY_INCLUDE_STARTUP_DATA & trig->action) {
				include_data = true;
		    }

		    /***** if notify_one_shot is set, need to remove subscription from system */

		    /* find subscription on notify tracker */
		    done = false;
		    for (trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_replica_notify_request_tracker);
			 trackptr != (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_replica_notify_request_tracker)
			     && !done;
			 trackptr = (mca_gpr_replica_notify_request_tracker_t*)ompi_list_get_next(trackptr)) {
			if (trackptr->local_idtag == trig->local_idtag) {
			    done = true;
			    if (NULL != trackptr->requestor) {
				name = trackptr->requestor;
			    } else {  /* local requestor */
				name = ompi_rte_get_self();
			    }
			    /* see if process already on list of recipients */
			    found = false;
			    for (ptr = (ompi_name_server_namelist_t*)ompi_list_get_first(recipients);
				 ptr != (ompi_name_server_namelist_t*)ompi_list_get_end(recipients) && !found;
				 ptr = (ompi_name_server_namelist_t*)ompi_list_get_next(ptr)) {
				if (0 == ompi_name_server.compare(OMPI_NS_CMP_ALL, name, ptr->name)) {
				    found = true;
				}
			    }

			    if (!found) {
				/* check job status segment to verify recipient still alive */
				tokens[0] = ompi_name_server.get_proc_name_string(name);
				tokens[1] = NULL;

				/* convert tokens to array of keys */
				keys = mca_gpr_replica_get_key_list(proc_stat_seg, tokens, &num_keys);

				returned_list = OBJ_NEW(ompi_list_t);
				mca_gpr_replica_get_nl(returned_list,
						       OMPI_REGISTRY_XAND,
						       proc_stat_seg, keys, num_keys);

				free(tokens[0]);
				free(keys);

				if (NULL != (value = (ompi_registry_value_t*)ompi_list_remove_first(returned_list))) {
				    proc_status = ompi_rte_unpack_process_status(value);
				    if ((OMPI_PROC_KILLED != proc_status->status_key) &&
					(OMPI_PROC_STOPPED != proc_status->status_key)) {
					/* add process to list of recipients */
					peer = OBJ_NEW(ompi_name_server_namelist_t);
					peer->name = ompi_name_server.copy_process_name(name);
					ompi_list_append(recipients, &peer->item);
				    }
				}
			    }
			}
		    }
		}
		trig = next_trig;
	    }

	    if (include_data) {  /* add in the data from all the registry entries on this segment */

		size = (int32_t)ompi_list_get_size(&seg->registry_entries); /* and number of data objects */
		ompi_pack(msg, &size, 1, OMPI_INT32);


		for (reg = (mca_gpr_replica_core_t*)ompi_list_get_first(&seg->registry_entries);
		     reg != (mca_gpr_replica_core_t*)ompi_list_get_end(&seg->registry_entries);
		     reg = (mca_gpr_replica_core_t*)ompi_list_get_next(reg)) {

		    /* add info to msg payload */
		    size = (int32_t)reg->object_size;
		    ompi_pack(msg, &size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE);
		    ompi_pack(msg, reg->object, reg->object_size, OMPI_BYTE);
		}
	    } else {
		size = 0;
		ompi_pack(msg, &size, 1, OMPI_INT32);
	    }
	}

	if (mca_gpr_replica_debug) {
	    ompi_buffer_size(msg, &bufsize);
	    ompi_output(0, "[%d,%d,%d] built startup_msg of length %d with %d recipients",
			OMPI_NAME_ARGS(*ompi_rte_get_self()), bufsize, (int)ompi_list_get_size(recipients));
	    for (peer = (ompi_name_server_namelist_t*)ompi_list_get_first(recipients);
		 peer != (ompi_name_server_namelist_t*)ompi_list_get_end(recipients);
		 peer = (ompi_name_server_namelist_t*)ompi_list_get_next(peer)) {
		ompi_output(0, "\trecipient: %s", ompi_name_server.get_proc_name_string(peer->name));
	    }
	}
    }

    return msg;
}


void mca_gpr_replica_remote_notify(ompi_process_name_t *recipient, int recipient_tag,
				   ompi_registry_notify_message_t *message)
{
    ompi_buffer_t msg;
    mca_gpr_cmd_flag_t command;
    int32_t num_items;
    uint i;
    ompi_registry_value_t *regval;
    char **tokptr;
    int recv_tag;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "sending trigger message");
    }

    command = MCA_GPR_NOTIFY_CMD;
    recv_tag = MCA_OOB_TAG_GPR_NOTIFY;

    if (OMPI_SUCCESS != ompi_buffer_init(&msg, 0)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_pack(msg, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return;
    }

    if (0 > ompi_pack_string(msg, message->segment)) {
	return;
    }

    i = (int32_t)message->owning_job;
    if (OMPI_SUCCESS != ompi_pack(msg, &i, 1, OMPI_INT32)) {
	return;
    }

    i = (int32_t)recipient_tag;
    if (OMPI_SUCCESS != ompi_pack(msg, &i, 1, OMPI_INT32)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_pack(msg, &message->trig_action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_pack(msg, &message->trig_synchro, 1, MCA_GPR_OOB_PACK_SYNCHRO_MODE)) {
	return;
    }

    
    num_items = (int32_t)ompi_list_get_size(&message->data);
    if (OMPI_SUCCESS != ompi_pack(msg, &num_items, 1, OMPI_INT32)) {
	return;
    }

    if (0 < num_items) { /* don't send anything else back if the list is empty */
	while (NULL != (regval = (ompi_registry_value_t*)ompi_list_remove_first(&message->data))) {
	    if (OMPI_SUCCESS != ompi_pack(msg, &regval->object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
		return;
	    }
	    if (OMPI_SUCCESS != ompi_pack(msg, regval->object, regval->object_size, OMPI_BYTE)) {
		return;
	    }
	    OBJ_RELEASE(regval);
	}
    }
    if (OMPI_SUCCESS != ompi_pack(msg, &message->num_tokens, 1, OMPI_INT32)) {
	return;
    }

    for (i=0, tokptr=message->tokens; i < (uint)message->num_tokens; i++, tokptr++) {
	if (OMPI_SUCCESS != ompi_pack_string(msg, *tokptr)) {
	    return;
	}
    }

    if (0 > mca_oob_send_packed(recipient, msg, recv_tag, 0)) {
	return;
    }

    ompi_buffer_free(msg);
    OBJ_RELEASE(message);
}
