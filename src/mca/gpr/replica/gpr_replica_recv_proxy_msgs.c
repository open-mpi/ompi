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

/*
 * define the local functions for processing commands
 */
static int32_t mca_gpr_replica_recv_delete_segment_cmd(ompi_buffer_t buffer);
static int32_t mca_gpr_replica_recv_put_cmd(ompi_buffer_t buffer);
static ompi_list_t* mca_gpr_replica_recv_get_cmd(ompi_buffer_t buffer);
static int32_t mca_gpr_replica_recv_delete_object_cmd(ompi_buffer_t buffer);
static ompi_list_t* mca_gpr_replica_recv_index_cmd(ompi_buffer_t buffer);
static ompi_registry_notify_id_t mca_gpr_replica_recv_subscribe_cmd(ompi_process_name_t* sender, ompi_buffer_t buffer);
static int32_t mca_gpr_replica_recv_unsubscribe_cmd(ompi_buffer_t buffer);
static ompi_registry_notify_id_t mca_gpr_replica_recv_synchro_cmd(ompi_process_name_t* sender, ompi_buffer_t buffer);
static int32_t mca_gpr_replica_recv_cancel_synchro_cmd(ompi_buffer_t buffer);
static void mca_gpr_replica_recv_dump_cmd(ompi_buffer_t answer);
static void mca_gpr_replica_recv_get_startup_msg_cmd(ompi_buffer_t buffer, ompi_buffer_t answer);
static void mca_gpr_replica_recv_triggers_active_cmd(ompi_buffer_t buffer);
static void mca_gpr_replica_recv_triggers_inactive_cmd(ompi_buffer_t buffer);
static void mca_gpr_replica_recv_cleanup_job_cmd(ompi_buffer_t buffer);
static void mca_gpr_replica_recv_cleanup_proc_cmd(ompi_buffer_t buffer);
static void mca_gpr_replica_recv_notify_on_cmd(ompi_buffer_t buffer);
static void mca_gpr_replica_recv_notify_off_cmd(ompi_buffer_t buffer);
static int32_t  mca_gpr_replica_recv_assign_ownership_cmd(ompi_buffer_t buffer);

static bool mca_gpr_replica_recv_silent_mode(ompi_buffer_t buffer);

 
/* 
 * handle message from proxies
 */

void mca_gpr_replica_recv(int status, ompi_process_name_t* sender,
			  ompi_buffer_t buffer, int tag,
			  void* cbdata)
{
    ompi_buffer_t answer;
    size_t buf_size=0;
    bool return_requested=true;
    bool compound_cmd_detected=false;

    if (mca_gpr_replica_debug) {
	ompi_output(0, "[%d,%d,%d] gpr replica: received message from [%d,%d,%d]",
			    OMPI_NAME_ARGS(*ompi_rte_get_self()), OMPI_NAME_ARGS(*sender));
    }

    return_requested = true;
    compound_cmd_detected = false;

    if (NULL != (answer = mca_gpr_replica_process_command_buffer(buffer, sender,
								 &return_requested,
								 &compound_cmd_detected))) {

	ompi_buffer_size(answer, &buf_size);

	if ((compound_cmd_detected && return_requested) ||
	    (!compound_cmd_detected && 0 < buf_size)) { /* must be some data or status codes to return */
		if (mca_gpr_replica_debug) {
			ompi_output(0, "[%d,%d,%d] gpr replica: sending response of length %d to [%d,%d,%d]",
						OMPI_NAME_ARGS(*ompi_rte_get_self()), (int)buf_size, OMPI_NAME_ARGS(*sender));
		}
	    if (0 > mca_oob_send_packed(sender, answer, tag, 0)) {
		/* RHC -- not sure what to do if the return send fails */
	    }
	}

	ompi_buffer_free(answer);
	if (mca_gpr_replica_debug) {
	    ompi_output(0, "gpr replica: msg processing complete - processing callbacks");
	}

	mca_gpr_replica_process_callbacks();
    }

    /* reissue the non-blocking receive */
    mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_GPR, 0, mca_gpr_replica_recv, NULL);
}


ompi_buffer_t mca_gpr_replica_process_command_buffer(ompi_buffer_t buffer,
						     ompi_process_name_t *sender,
						     bool *return_requested,
						     bool *compound_cmd_detected)
{
    ompi_buffer_t answer, error_answer;
    ompi_registry_value_t *regval=NULL;
    ompi_list_t *returned_list=NULL;
    ompi_registry_internal_test_results_t *testval=NULL;
    ompi_registry_index_value_t *indexval=NULL;
    ompi_registry_notify_id_t return_tag=OMPI_REGISTRY_NOTIFY_ID_MAX;
    int32_t test_level=0;
    mca_gpr_cmd_flag_t command;
    int32_t response=0;
    int8_t tmp_bool=0;


    if (OMPI_SUCCESS != ompi_buffer_init(&answer, 0)) {
	/* RHC -- not sure what to do if this fails */
	return NULL;
    }

    while (OMPI_SUCCESS == ompi_unpack(buffer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {

	switch(command) {

	case MCA_GPR_COMPOUND_CMD:  /*****     COMPOUND COMMAND     ******/
	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tcompound cmd");
	    }

	    if (OMPI_ERROR == ompi_unpack(buffer, &tmp_bool, 1, MCA_GPR_OOB_PACK_BOOL)) {
		goto RETURN_ERROR;
	    }

	    *return_requested = (bool)tmp_bool;
	    *compound_cmd_detected = true;
	    break;
       
	
	case MCA_GPR_DELETE_SEGMENT_CMD:  /******    DELETE SEGMENT    *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tdelete segment cmd");
	    }

	    tmp_bool = mca_gpr_replica_recv_silent_mode(buffer);

	    if (OMPI_ERROR == (response = mca_gpr_replica_recv_delete_segment_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (!tmp_bool) {
		if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		    goto RETURN_ERROR;
		}
		if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		    goto RETURN_ERROR;
		}
	    }
	    break;

	    
	case MCA_GPR_PUT_CMD:  /*****    PUT    *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tput cmd");
	    }

	    tmp_bool = mca_gpr_replica_recv_silent_mode(buffer);

	    if (OMPI_ERROR == (response = mca_gpr_replica_recv_put_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (!tmp_bool) {
		if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		    goto RETURN_ERROR;
		}

		if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		    goto RETURN_ERROR;
		}
	    }
	    break;

	    
	case MCA_GPR_GET_CMD:  /*****    GET    *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tget cmd");
	    }

	    if (NULL == (returned_list = mca_gpr_replica_recv_get_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		goto RETURN_ERROR;
	    }

	    response = (int32_t)ompi_list_get_size(returned_list);
	    if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		goto RETURN_ERROR;
	    }

	    if (0 < response) { /* don't send anything else back if the list is empty */
		for (regval = (ompi_registry_value_t*)ompi_list_get_first(returned_list);
		     regval != (ompi_registry_value_t*)ompi_list_get_end(returned_list);
		     regval = (ompi_registry_value_t*)ompi_list_get_next(regval)) {  /* traverse the list */
		    if (OMPI_SUCCESS != ompi_pack(answer, &regval->object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
			goto RETURN_ERROR;
		    }
		    if (OMPI_SUCCESS != ompi_pack(answer, regval->object, regval->object_size, OMPI_BYTE)) {
			goto RETURN_ERROR;
		    }
		}
	    }
	    break;

	    
	case MCA_GPR_DELETE_OBJECT_CMD:  /*****     DELETE OBJECT     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tdelete object cmd");
	    }

	    tmp_bool = mca_gpr_replica_recv_silent_mode(buffer);

	    if (OMPI_ERROR == (response = mca_gpr_replica_recv_delete_object_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (!tmp_bool) {

		if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		    goto RETURN_ERROR;
		}

		if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		    goto RETURN_ERROR;
		}
	    }
	    break;

	    
	case MCA_GPR_INDEX_CMD:  /*****     INDEX     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tindex cmd");
	    }

	    if (NULL == (returned_list = mca_gpr_replica_recv_index_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		goto RETURN_ERROR;
	    }

	    response = (int32_t)ompi_list_get_size(returned_list);
	    if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		goto RETURN_ERROR;
	    }

	    if (0 < response) { /* don't send anything else back if the list is empty */
		for (indexval = (ompi_registry_index_value_t*)ompi_list_get_first(returned_list);
		     indexval != (ompi_registry_index_value_t*)ompi_list_get_end(returned_list);
		     indexval = (ompi_registry_index_value_t*)ompi_list_get_next(indexval)) {  /* traverse the list */
		    if (OMPI_SUCCESS != ompi_pack_string(answer, indexval->token)) {
			goto RETURN_ERROR;
		    }
		}
	    }
	    break;

	    
	case MCA_GPR_SUBSCRIBE_CMD:  /*****     SUBSCRIBE     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tsubscribe cmd");
	    }

	    if (OMPI_REGISTRY_NOTIFY_ID_MAX == (return_tag = mca_gpr_replica_recv_subscribe_cmd(sender, buffer))) {
		goto RETURN_ERROR;
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		goto RETURN_ERROR;
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &return_tag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
		goto RETURN_ERROR;
	    }
	    break;

	   
	case MCA_GPR_UNSUBSCRIBE_CMD:  /*****     UNSUBSCRIBE     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tunsubscribe cmd");
	    }

	    tmp_bool = mca_gpr_replica_recv_silent_mode(buffer);

	    if (OMPI_ERROR == (response = mca_gpr_replica_recv_unsubscribe_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (!tmp_bool) {

		if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		    goto RETURN_ERROR;
		}

		if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		    goto RETURN_ERROR;
		}
	    }
	    break;


	    
	case MCA_GPR_SYNCHRO_CMD:  /*****     SYNCHRO     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tsynchro cmd");
	    }

	    if (OMPI_REGISTRY_NOTIFY_ID_MAX == (return_tag = mca_gpr_replica_recv_synchro_cmd(sender, buffer))) {
		goto RETURN_ERROR;
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		goto RETURN_ERROR;
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &return_tag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
		goto RETURN_ERROR;
	    }
	    break;


	    
	case MCA_GPR_CANCEL_SYNCHRO_CMD:  /*****     CANCEL SYNCHRO    *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tcancel synchro cmd");
	    }

	    tmp_bool = mca_gpr_replica_recv_silent_mode(buffer);

	    if (OMPI_ERROR == (response = mca_gpr_replica_recv_cancel_synchro_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (!tmp_bool) {
		if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		    goto RETURN_ERROR;
		}

		if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		    goto RETURN_ERROR;
		}
	    }
	    break;


	    
	case MCA_GPR_DUMP_CMD:  /*****     DUMP     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tdump cmd");
	    }

	    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		goto RETURN_ERROR;
	    }

	    mca_gpr_replica_recv_dump_cmd(answer);
	    break;


	    
	case MCA_GPR_GET_STARTUP_MSG_CMD:  /*****     GET STARTUP MSG    *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tget startup msg cmd");
	    }

	    mca_gpr_replica_recv_get_startup_msg_cmd(buffer, answer);
	    break;


	    
	case MCA_GPR_NOTIFY_ON_CMD:  /*****     NOTIFY ON     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tnotify on cmd");
	    }

	    mca_gpr_replica_recv_notify_on_cmd(buffer);
	    break;


	    
	case MCA_GPR_NOTIFY_OFF_CMD:  /*****     NOTIFY OFF     ******/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tnotify off cmd");
	    }

	    mca_gpr_replica_recv_notify_off_cmd(buffer);
	    break;


	    
	case MCA_GPR_TRIGGERS_ACTIVE_CMD:  /*****     TRIGGERS ACTIVE     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\ttriggers active cmd");
	    }

	    mca_gpr_replica_recv_triggers_active_cmd(buffer);
	    break;


	    
	case MCA_GPR_TRIGGERS_INACTIVE_CMD:  /*****     TRIGGERS INACTIVE     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\ttriggers inactive cmd");
	    }

	    mca_gpr_replica_recv_triggers_inactive_cmd(buffer);
	    break;


	    
	case MCA_GPR_CLEANUP_JOB_CMD:  /*****     CLEANUP JOB    *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tcleanup job cmd");
	    }

	    mca_gpr_replica_recv_cleanup_job_cmd(buffer);
	    break;


	    
	case MCA_GPR_CLEANUP_PROC_CMD:  /*****     CLEANUP PROCESS     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tcleanup proc cmd");
	    }

	    mca_gpr_replica_recv_cleanup_proc_cmd(buffer);
	    break;


	    
	case MCA_GPR_ASSIGN_OWNERSHIP_CMD:  /*****     ASSIGN OWNERSHIP     *****/

	    if (mca_gpr_replica_debug) {
		ompi_output(0, "\tassign ownership command");
	    }

	    tmp_bool = mca_gpr_replica_recv_silent_mode(buffer);

	    if (OMPI_ERROR == (response = mca_gpr_replica_recv_assign_ownership_cmd(buffer))) {
		goto RETURN_ERROR;
	    }

	    if (!tmp_bool) {
		if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		    goto RETURN_ERROR;
		}

		if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		    goto RETURN_ERROR;
		}
	    }
	    break;


	    
	case MCA_GPR_TEST_INTERNALS_CMD:  /*****     TEST INTERNALS     *****/


	    if ((OMPI_SUCCESS != ompi_unpack(buffer, &test_level, 1, OMPI_INT32)) ||
		(0 > test_level)) {
		goto RETURN_ERROR;
	    }

	    returned_list = mca_gpr_replica_test_internals(test_level);

	    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
		goto RETURN_ERROR;
	    }

	    response = (int32_t)ompi_list_get_size(returned_list);
	    if (OMPI_SUCCESS != ompi_pack(answer, &response, 1, OMPI_INT32)) {
		goto RETURN_ERROR;
	    }

	    if (0 < response) { /* don't send anything else back if the list is empty */
		for (testval = (ompi_registry_internal_test_results_t*)ompi_list_get_first(returned_list);
		     testval != (ompi_registry_internal_test_results_t*)ompi_list_get_end(returned_list);
		     testval = (ompi_registry_internal_test_results_t*)ompi_list_get_next(testval)) {  /* traverse the list */
		    if (OMPI_SUCCESS != ompi_pack_string(answer, testval->test)) {
			goto RETURN_ERROR;
		    }
		    if (OMPI_SUCCESS != ompi_pack_string(answer, testval->message)) {
			goto RETURN_ERROR;
		    }
		}
	    }
	    break;
 
	    
	default:  /****    UNRECOGNIZED COMMAND   ****/
	RETURN_ERROR:
	    if (mca_gpr_replica_debug) {
		ompi_output(0, "unrecognized command");
	    }
	    ompi_buffer_init(&error_answer, 8);
	    command = MCA_GPR_ERROR;
	    ompi_pack(error_answer, (void*)&command, 1, MCA_GPR_OOB_PACK_CMD);
	    ompi_buffer_free(answer);
	    return error_answer;

	}  /* end switch command */

    }

    return answer;
}

static int32_t mca_gpr_replica_recv_delete_segment_cmd(ompi_buffer_t buffer)
{
    char *segment=NULL;
    mca_gpr_replica_segment_t *seg=NULL;

    if (0 > ompi_unpack_string(buffer, &segment)) {
	return OMPI_ERROR;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);

    mca_gpr_replica_delete_segment_nl(seg);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    return OMPI_SUCCESS;
}

static int32_t mca_gpr_replica_recv_put_cmd(ompi_buffer_t buffer)
{
    ompi_registry_mode_t mode;
    char *segment=NULL, **tokens=NULL, **tokptr=NULL;
    int32_t num_tokens=0, response=(int32_t)OMPI_ERROR;
    ompi_registry_object_size_t object_size=0;
    ompi_registry_object_t *object=NULL;
    mca_gpr_replica_segment_t *seg=NULL;
    mca_gpr_replica_key_t *keys=NULL;
    mca_ns_base_jobid_t jobid=0;
    int num_keys=0;
    int8_t action_taken=0;
    int i=0;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto RETURN_ERROR;
    }

    if (0 > ompi_unpack_string(buffer, &segment)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (0 >= num_tokens) {  /** no tokens provided - error for PUT */
	goto RETURN_ERROR;
    }

    tokens = (char**)malloc((num_tokens+1)*sizeof(char*));

    tokptr = tokens;
    for (i=0; i<num_tokens; i++) {
	if (0 > ompi_unpack_string(buffer, tokptr)) {
	    goto RETURN_ERROR;
	}
	tokptr++;
    }
    *tokptr = NULL;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	goto RETURN_ERROR;
    }

    if (0 >= object_size) {  /* error condition - nothing to store */
	goto RETURN_ERROR;
    }

    object = (ompi_registry_object_t)malloc(object_size);
    if (OMPI_SUCCESS != ompi_unpack(buffer, object, object_size, OMPI_BYTE)) {
	goto RETURN_ERROR;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* find the segment */
    seg = mca_gpr_replica_find_seg(true, segment, jobid);
    if (NULL == seg) { /* couldn't find segment or create it */
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	goto RETURN_ERROR;
    }

    /* convert tokens to array of keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    response = (int32_t)mca_gpr_replica_put_nl(mode, seg, keys, num_keys,
					       object, object_size, &action_taken);

    mca_gpr_replica_check_subscriptions(seg, action_taken);

    mca_gpr_replica_check_synchros(seg);

    /* release list of keys */
    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

 RETURN_ERROR:
    if (NULL != segment) {
	free(segment);
    }
    if (NULL != object) {
	free(object);
    }
    if (NULL != tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    free(*tokptr);
	    tokptr++;
	}
	free(tokens);
    }

    return response;
}

static ompi_list_t* mca_gpr_replica_recv_get_cmd(ompi_buffer_t buffer)
{
    ompi_registry_mode_t mode;
    mca_gpr_replica_segment_t *seg=NULL;
    mca_gpr_replica_key_t *keys=NULL;
    int num_keys=0;
    char *segment=NULL, **tokens=NULL, **tokptr=NULL;
    int32_t num_tokens=0;
    ompi_list_t *returned_list=NULL;
    int i=0;

    returned_list = OBJ_NEW(ompi_list_t);

    if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	return returned_list;
    }

    if (0 > ompi_unpack_string(buffer, &segment)) {
	return returned_list;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (0 >= num_tokens) {  /* no tokens provided - wildcard case */
	tokens = NULL;
    } else {  /* tokens provided */
	tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
	}
	*tokptr = NULL;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* find the specified segment */
    seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);
    if (NULL == seg) {  /* segment not found */
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	return returned_list;
    }

    /* convert tokens to array of keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);


    mca_gpr_replica_get_nl(returned_list, mode, seg, keys, num_keys);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

 RETURN_ERROR:
    if (NULL != segment) {
	free(segment);
    }
    if (NULL != tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    free(*tokptr);
	    tokptr++;
	}
	free(tokens);
    }

    return returned_list;
}

static int32_t mca_gpr_replica_recv_delete_object_cmd(ompi_buffer_t buffer)
{
    ompi_registry_mode_t mode;
    mca_gpr_replica_key_t *keys=NULL;
    mca_gpr_replica_segment_t *seg=NULL;
    char *segment=NULL, **tokens=NULL, **tokptr=NULL;
    int32_t num_tokens=0, response=(int32_t)OMPI_ERROR;
    int i=0, num_keys=0;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto RETURN_ERROR;
    }

    if (0 > ompi_unpack_string(buffer, &segment)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (0 >= num_tokens) {  /* no tokens provided - wildcard case */
	tokens = NULL;
    } else {  /* tokens provided */
	tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
	}
	*tokptr = NULL;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

     /* locate the segment */
    seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);
    if (NULL == seg) {
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	return OMPI_ERROR;
    }

    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    response = (int32_t)mca_gpr_replica_delete_object_nl(mode, seg, keys, num_keys);

    mca_gpr_replica_check_subscriptions(seg, MCA_GPR_REPLICA_OBJECT_DELETED);

    mca_gpr_replica_check_synchros(seg);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

 RETURN_ERROR:
    if (NULL != segment) {
	free(segment);
    }
    if (NULL != tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    free(*tokptr);
	    tokptr++;
	}
	free(tokens);
    }

    if (NULL != keys) {
	free(keys);
    }

    return response;
}

static ompi_list_t* mca_gpr_replica_recv_index_cmd(ompi_buffer_t buffer)
{
    ompi_registry_mode_t mode;
    mca_gpr_replica_segment_t *seg=NULL;
    char *segment=NULL;
    ompi_list_t *returned_list=NULL;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto RETURN_ERROR;
    }

    if (0 == mode) {  /* only want dict of segments */
	segment = NULL;
    } else {
	if (0 > ompi_unpack_string(buffer, &segment)) {
	    goto RETURN_ERROR;
	}
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    if (NULL == segment) {  /* want global level index */
	seg = NULL;
    } else {
	/* locate the segment */
	seg = mca_gpr_replica_find_seg(false, segment, MCA_NS_BASE_JOBID_MAX);
	if (NULL == seg) {
	    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	    return NULL;
	}
    }

    returned_list = mca_gpr_replica_index_nl(seg);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

 RETURN_ERROR:
    if (NULL != segment) {
	free(segment);
    }
    return returned_list;
}

static ompi_registry_notify_id_t mca_gpr_replica_recv_subscribe_cmd(ompi_process_name_t* sender,
								    ompi_buffer_t buffer)
{
    ompi_registry_mode_t mode;
    char *segment=NULL, **tokens=NULL, **tokptr=NULL;
    ompi_registry_notify_action_t action;
    ompi_registry_notify_id_t local_idtag1=0, id_tag=0, return_tag=0;
    mca_gpr_replica_segment_t *seg=NULL;
    mca_gpr_replica_key_t *keys=NULL;
    int num_keys=0;
    int32_t num_tokens=0, response=(int32_t)OMPI_ERROR;
    int i=0;
    mca_ns_base_jobid_t owning_jobid;

    return_tag = OMPI_REGISTRY_NOTIFY_ID_MAX;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &action, 1, MCA_GPR_OOB_PACK_ACTION)) {
	goto RETURN_ERROR;
    }

    if (0 > ompi_unpack_string(buffer, &segment)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (0 < num_tokens) {  /* tokens provided */ 
	tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
	}
	*tokptr = NULL;
    } else {  /* no tokens provided - wildcard case */
	tokens = NULL;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &id_tag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	goto RETURN_ERROR;
    }

    if (NULL != sender) {
        owning_jobid = ompi_name_server.get_jobid(sender);
    } else {
        owning_jobid = ompi_name_server.get_jobid(ompi_rte_get_self());
    }

    /*******   LOCK    *****/
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    seg = mca_gpr_replica_find_seg(true, segment, owning_jobid);
    if (NULL == seg) { /* segment couldn't be found */
        	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
        	goto RETURN_ERROR;
    }

    /* convert tokens to keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);

    if (NULL != sender) {  /* remote sender */

		if (mca_gpr_replica_debug) {
			ompi_output(0, "[%d,%d,%d] subscribe requested for remote sender [%d,%d,%d] on segment %s for idtag %d",
						OMPI_NAME_ARGS(*ompi_rte_get_self()), OMPI_NAME_ARGS(*sender), segment, id_tag);
		}
		
		/* enter request on local notify tracking system */
		local_idtag1 = mca_gpr_replica_enter_notify_request(seg, action, sender, id_tag, NULL, NULL);
	
		response = (int32_t)mca_gpr_replica_subscribe_nl(mode, action, seg, keys, num_keys,
								 local_idtag1, owning_jobid);
		if (OMPI_SUCCESS == response) {
		    return_tag = local_idtag1;
		}

    } else {  /* local sender - id_tag is for local notify tracking system*/
		response = (int32_t)mca_gpr_replica_subscribe_nl(mode, action, seg,
								 keys, num_keys, id_tag, owning_jobid);
		if (OMPI_SUCCESS == response) {
		    return_tag = id_tag;
		}
    }

    mca_gpr_replica_check_subscriptions(seg, MCA_GPR_REPLICA_SUBSCRIBER_ADDED);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    /******     UNLOCK     ******/

 RETURN_ERROR:
    if (NULL != segment) {
	free(segment);
    }
    if (NULL != tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    free(*tokptr);
	    tokptr++;
	}
	free(tokens);
    }

    return return_tag;
}

static int32_t mca_gpr_replica_recv_unsubscribe_cmd(ompi_buffer_t buffer)
{
    ompi_registry_notify_id_t sub_number=0;
    int32_t response=(int32_t)OMPI_ERROR;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &sub_number, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	goto RETURN_ERROR;
    }

    /*******   LOCK    *****/
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    response = (int32_t)mca_gpr_replica_unsubscribe_nl(sub_number);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    /******     UNLOCK     ******/

 RETURN_ERROR:
    return response;
}

static ompi_registry_notify_id_t mca_gpr_replica_recv_synchro_cmd(ompi_process_name_t* sender,
								  ompi_buffer_t buffer)
{
    ompi_registry_mode_t mode;
    char *segment=NULL, **tokens=NULL, **tokptr=NULL;
    ompi_registry_notify_id_t local_idtag1=0, id_tag=0, return_tag=0;
    mca_gpr_replica_segment_t *seg=NULL;
    mca_gpr_replica_key_t *keys=NULL;
    int num_keys=0;
    int32_t num_tokens=0, response=(int32_t)OMPI_ERROR, synchro_mode=0, trigger=0;
    int i=0;
    mca_ns_base_jobid_t owning_jobid;

    return_tag = OMPI_REGISTRY_NOTIFY_ID_MAX;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &synchro_mode, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (OMPI_REGISTRY_SYNCHRO_MODE_NONE == synchro_mode) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto RETURN_ERROR;
    }

    if (0 > ompi_unpack_string(buffer, &segment)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &num_tokens, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (0 < num_tokens) {  /* tokens provided */ 
	tokens = (char**)malloc((num_tokens+1)*sizeof(char*));
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    if (0 > ompi_unpack_string(buffer, tokptr)) {
		goto RETURN_ERROR;
	    }
	    tokptr++;
	}
	*tokptr = NULL;
    } else {  /* no tokens provided - wildcard case, just count entries on segment */
	tokens = NULL;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &trigger, 1, OMPI_INT32)) {
	goto RETURN_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(buffer, &id_tag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	goto RETURN_ERROR;
    }

    if (NULL != sender) {
        owning_jobid = ompi_name_server.get_jobid(sender);
    } else {
        owning_jobid = ompi_name_server.get_jobid(ompi_rte_get_self());
    }

    /*******   LOCK    *****/
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    seg = mca_gpr_replica_find_seg(true, segment, owning_jobid);
    if (NULL == seg) { /* segment couldn't be found */
       OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
          goto RETURN_ERROR;
    }
    
    /* convert tokens to keys */
    keys = mca_gpr_replica_get_key_list(seg, tokens, &num_keys);
    
    if (NULL != sender) {  /* remote sender */

        	/* enter request on local notify tracking system */
        	local_idtag1 = mca_gpr_replica_enter_notify_request(seg,
        							    OMPI_REGISTRY_NOTIFY_NONE, sender,
        							    id_tag, NULL, NULL);
        
        	response = (int32_t)mca_gpr_replica_synchro_nl(synchro_mode,
        						       mode, seg, keys, num_keys,
        						       trigger, local_idtag1, owning_jobid);
        
        	if (OMPI_SUCCESS == response) {
        	    return_tag = local_idtag1;
        	}
        
    } else {  /* local sender - id_tag already on local notify tracking system */

        	response = (int32_t)mca_gpr_replica_synchro_nl(synchro_mode,
        						       mode, seg, keys, num_keys,
        						       trigger, id_tag, owning_jobid);
        	if (OMPI_SUCCESS == response) {
        	    return_tag = id_tag;
        	}
    }

    mca_gpr_replica_check_synchros(seg);

    if (NULL != keys) {
	free(keys);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    /******     UNLOCK     ******/

 RETURN_ERROR:
    if (NULL != segment) {
	free(segment);
    }
    if (NULL != tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {
	    free(*tokptr);
	    tokptr++;
	}
	free(tokens);
    }

    return return_tag;
}

static int32_t mca_gpr_replica_recv_cancel_synchro_cmd(ompi_buffer_t buffer)
{
    ompi_registry_notify_id_t synch_number=0;
    int32_t response=(int32_t)OMPI_ERROR;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &synch_number, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	goto RETURN_ERROR;
    }

    /*******   LOCK    *****/
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    response = (int32_t)mca_gpr_replica_cancel_synchro_nl(synch_number);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
    /******     UNLOCK     ******/

 RETURN_ERROR:
    return response;
}

static void mca_gpr_replica_recv_dump_cmd(ompi_buffer_t answer)
{
    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_dump_nl(answer);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


static void mca_gpr_replica_recv_get_startup_msg_cmd(ompi_buffer_t buffer, ompi_buffer_t answer)
{
    mca_gpr_cmd_flag_t command=MCA_GPR_GET_STARTUP_MSG_CMD;
    mca_ns_base_jobid_t jobid=0;
    ompi_list_t *recipients=NULL;
    ompi_buffer_t msg;
    ompi_name_server_namelist_t *recip=NULL;
    int32_t num_objects=0, num_recipients=0, i=0;
    char *segment=NULL;
    ompi_registry_object_t *data_object;
    ompi_registry_object_size_t data_obj_size;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &jobid, 1, OMPI_JOBID)) {
		return;
    }

    recipients = OBJ_NEW(ompi_list_t);

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    msg = mca_gpr_replica_construct_startup_msg_nl(jobid, recipients);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    if (OMPI_SUCCESS != ompi_pack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
        return;
    }
    
    num_recipients = (int32_t)ompi_list_get_size(recipients);
    if (OMPI_SUCCESS != ompi_pack(answer, &num_recipients, 1, OMPI_INT32)) {
	return;
    }

    for (i=0; i<num_recipients; i++) {
	recip = (ompi_name_server_namelist_t*)ompi_list_remove_first(recipients);
	ompi_pack(answer, recip->name, 1, OMPI_NAME);
	OBJ_RELEASE(recip);
    }

    while (0 < ompi_unpack_string(msg, &segment)) {
        ompi_output(0, "replica_recv_proxy: transferring startup data for segment %s", segment);
        ompi_pack_string(answer, segment);
        ompi_unpack(msg, &num_objects, 1, OMPI_INT32);  /* unpack #data objects */
        ompi_pack(answer, &num_objects, 1, OMPI_INT32);

        if (0 < num_objects) {
            for (i=0; i < num_objects; i++) {
                ompi_unpack(msg, &data_obj_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE);
                data_object = (ompi_registry_object_t)malloc(data_obj_size);
                ompi_unpack(msg, data_object, data_obj_size, OMPI_BYTE);
                ompi_pack(answer, &data_obj_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE);
                ompi_pack(answer, data_object, data_obj_size, OMPI_BYTE);
                free(data_object);
            }
        }
        free(segment);
    }
    ompi_buffer_free(msg);
}


static void mca_gpr_replica_recv_triggers_active_cmd(ompi_buffer_t cmd)
{
    mca_ns_base_jobid_t jobid=0;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_triggers_active_nl(jobid);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}

static void mca_gpr_replica_recv_triggers_inactive_cmd(ompi_buffer_t cmd)
{
    mca_ns_base_jobid_t jobid=0;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_triggers_inactive_nl(jobid);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}

static void mca_gpr_replica_recv_cleanup_job_cmd(ompi_buffer_t cmd)
{
    mca_ns_base_jobid_t jobid=0;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_cleanup_job_nl(jobid);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


static void mca_gpr_replica_recv_cleanup_proc_cmd(ompi_buffer_t cmd)
{
    ompi_process_name_t proc;
    bool purge=false;
    int8_t tmp=0;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &tmp, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return;
    }
    purge = (bool)tmp;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &proc, 1, MCA_GPR_OOB_PACK_NAME)) {
	return;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_cleanup_proc_nl(purge, &proc);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


static void mca_gpr_replica_recv_notify_on_cmd(ompi_buffer_t cmd)
{
    ompi_process_name_t proc;
    ompi_registry_notify_id_t sub_number;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &proc, 1, MCA_GPR_OOB_PACK_NAME)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_unpack(cmd, &sub_number, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_notify_on_nl(&proc, sub_number);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}

static void mca_gpr_replica_recv_notify_off_cmd(ompi_buffer_t cmd)
{
    ompi_process_name_t proc;
    ompi_registry_notify_id_t sub_number;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &proc, 1, MCA_GPR_OOB_PACK_NAME)) {
	return;
    }

    if (OMPI_SUCCESS != ompi_unpack(cmd, &sub_number, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	return;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);
    mca_gpr_replica_notify_off_nl(&proc, sub_number);
    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
}


static int32_t mca_gpr_replica_recv_assign_ownership_cmd(ompi_buffer_t cmd)
{
    mca_ns_base_jobid_t jobid=0;
    mca_gpr_replica_segment_t *seg=NULL;
    char *segment=NULL;
    int32_t rc=0;

    if (OMPI_SUCCESS != ompi_unpack(cmd, &jobid, 1, MCA_GPR_OOB_PACK_JOBID)) {
	return OMPI_ERROR;
    }

    if (0 > ompi_unpack_string(cmd, &segment)) {
	return OMPI_ERROR;
    }

    OMPI_THREAD_LOCK(&mca_gpr_replica_mutex);

    /* find the segment */
    seg = mca_gpr_replica_find_seg(true, segment, jobid);
    if (NULL == seg) {  /* segment couldn't be found or created */
	OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);
	return OMPI_ERROR;
    }

    rc = (int32_t)mca_gpr_replica_assign_ownership_nl(seg, jobid);

    OMPI_THREAD_UNLOCK(&mca_gpr_replica_mutex);

    return rc;
}



static bool mca_gpr_replica_recv_silent_mode(ompi_buffer_t buffer)
{
    int8_t tmp=0;

    if (OMPI_SUCCESS != ompi_unpack(buffer, &tmp, 1, MCA_GPR_OOB_PACK_BOOL)) {
	return false;
    }

    return (bool)tmp;
}
