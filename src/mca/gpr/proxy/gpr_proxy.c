/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"

#include <string.h>
#include "mca/mca.h"
#include "mca/gpr/base/base.h"
#include "gpr_proxy.h"

/**
 * globals
 */

/*
 * Implemented registry functions
 */


int gpr_proxy_delete_segment(char *segment)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    int recv_tag;
    int32_t response;

    command = MCA_GPR_DELETE_SEGMENT_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	return OMPI_ERROR;
    }


    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	return OMPI_ERROR;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_DELETE_SEGMENT_CMD != command)) {
	ompi_buffer_free(answer);
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(answer, &response, 1, OMPI_INT32)) {
	ompi_buffer_free(answer);
	return OMPI_ERROR;
    } else {
	ompi_buffer_free(answer);
	return (int)response;
    }
    return OMPI_ERROR;
}


int gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		  char **tokens, ompi_registry_object_t object,
		  ompi_registry_object_size_t size)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int recv_tag, i;
    int32_t num_tokens, object_size;
    int16_t response;

    command = MCA_GPR_PUT_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	return OMPI_ERROR;
    }

    /* compute number of tokens */
    tokptr = tokens;
    num_tokens = 0;
    while (NULL != *tokptr) {
	num_tokens++;
	tokptr++;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	return OMPI_ERROR;
    }

    tokptr = tokens;
    for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
	    return OMPI_ERROR;
	}
	tokptr++;
    }

    object_size = (int32_t)size;

    if (OMPI_SUCCESS != ompi_pack(cmd, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, object, object_size, OMPI_BYTE)) {
	return OMPI_ERROR;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	return OMPI_ERROR;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	return OMPI_ERROR;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_PUT_CMD != command)) {
	ompi_buffer_free(answer);
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_unpack(answer, &response, 1, OMPI_INT32)) {
	ompi_buffer_free(answer);
	return OMPI_ERROR;
    } else {
	ompi_buffer_free(answer);
	return (int)response;
    }
    return OMPI_ERROR;
}


int gpr_proxy_delete_object(ompi_registry_mode_t mode,
			    char *segment, char **tokens)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int recv_tag, i;
    int32_t num_tokens, response;

    /* need to protect against errors */
    if (NULL == segment || NULL == tokens || NULL == *tokens) {
	return OMPI_ERROR;
    }

    command = MCA_GPR_DELETE_OBJECT_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	goto CLEANUP;
    }

    /* compute number of tokens */
    tokptr = tokens;
    num_tokens = 0;
    while (NULL != *tokptr) {
	num_tokens++;
	tokptr++;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    tokptr = tokens;
    for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
	    goto CLEANUP;
	}
	tokptr++;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_DELETE_OBJECT_CMD != command)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_unpack(answer, &response, 1, OMPI_INT32)) {
	ompi_buffer_free(answer);
	return OMPI_ERROR;
    } else {
	ompi_buffer_free(answer);
	return (int)response;
    }

 CLEANUP:
    ompi_buffer_free(cmd);
    return OMPI_ERROR;

}


ompi_list_t* gpr_proxy_index(char *segment)
{
    ompi_list_t *return_list;
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    char *string1;
    int recv_tag, i;
    int32_t num_responses;
    ompi_registry_mode_t mode;
    ompi_registry_index_value_t *newptr;

    return_list = OBJ_NEW(ompi_list_t);

    command = MCA_GPR_INDEX_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return return_list;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto CLEANUP;
    }

    if (NULL == segment) {  /* no segment specified - want universe dict */
	mode = 0;
	if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto CLEANUP;
	}
    } else {
	mode = 1;
	if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	    goto CLEANUP;
	}
	if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	    goto CLEANUP;
	}
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_INDEX_CMD != command)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &num_responses, 1, OMPI_INT32)) ||
	(0 >= num_responses)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    for (i=0; i<num_responses; i++) {
	if (0 > ompi_unpack_string(answer, &string1)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	}
	newptr = OBJ_NEW(ompi_registry_index_value_t);
	newptr->token = strdup(string1);
	ompi_list_append(return_list, &newptr->item);
    }


 CLEANUP:
    ompi_buffer_free(cmd);
    return return_list;
}


int gpr_proxy_subscribe(ompi_registry_mode_t mode,
			ompi_registry_notify_action_t action,
			char *segment, char **tokens,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int gpr_proxy_unsubscribe(ompi_registry_mode_t mode,
			  ompi_registry_notify_action_t action,
			  char *segment, char **tokens,
			  ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}

int gpr_proxy_synchro(ompi_registry_synchro_mode_t synchro_mode,
		      ompi_registry_mode_t mode,
		      char *segment, char **tokens, int trigger,
		      ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int recv_tag, i;
    int32_t num_tokens, response;
    mca_gpr_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;

    trackptr = NULL;

    /* need to protect against errors */
    if (NULL == segment) {
	return OMPI_ERROR;
    }

    command = MCA_GPR_SYNCHRO_CMD;

    if (OMPI_REGISTRY_SYNCHRO_MODE_NONE == synchro_mode) {  /* not allowed */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto CLEANUP;
    }

    response = (int32_t)synchro_mode;
    if (OMPI_SUCCESS != ompi_pack(cmd, &response, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	goto CLEANUP;
    }

    num_tokens = 0;
    if (NULL != tokens) {
	/* compute number of tokens */
	tokptr = tokens;
	while (NULL != *tokptr) {
	    num_tokens++;
	    tokptr++;
	}
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    if (0 < num_tokens) {
	tokptr = tokens;
	for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	    if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
		goto CLEANUP;
	    }
	    tokptr++;
	}
    }

    response = (int32_t)trigger;
    if (OMPI_SUCCESS != ompi_pack(cmd, &response, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    /* store callback function and user_tag in local list for lookup */
    /* generate id_tag to send to replica to identify lookup entry */
    trackptr = OBJ_NEW(mca_gpr_notify_request_tracker_t);
    trackptr->requestor = NULL;
    trackptr->req_tag = 0;
    trackptr->callback = cb_func;
    trackptr->user_tag = user_tag;
    if (ompi_list_is_empty(&mca_gpr_proxy_free_notify_id_tags)) {
	trackptr->id_tag = mca_gpr_proxy_last_notify_id_tag;
	mca_gpr_proxy_last_notify_id_tag++;
    } else {
	ptr_free_id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_proxy_free_notify_id_tags);
	trackptr->id_tag = ptr_free_id->id_tag;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &trackptr->id_tag, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_SYNCHRO_CMD != command)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &response, 1, OMPI_INT32)) ||
	(OMPI_SUCCESS != response)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    ompi_list_append(&mca_gpr_proxy_notify_request_tracker, &trackptr->item);
    ompi_buffer_free(answer);
    ompi_buffer_free(cmd);
    return OMPI_SUCCESS;

 CLEANUP:
    if (NULL != trackptr) {
	ptr_free_id = OBJ_NEW(mca_gpr_idtag_list_t);
	ptr_free_id->id_tag = trackptr->id_tag;
	ompi_list_append(&mca_gpr_proxy_free_notify_id_tags, &ptr_free_id->item);
    }
    ompi_buffer_free(cmd);
    return OMPI_ERROR;

}

ompi_list_t* gpr_proxy_get(ompi_registry_mode_t mode, char *segment, char **tokens)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    char **tokptr;
    int recv_tag, i;
    int32_t num_tokens, object_size, num_responses;
    ompi_registry_value_t *newptr;
    ompi_registry_object_t *object;
    ompi_list_t *returned_list;

    returned_list = OBJ_NEW(ompi_list_t);

    /* need to protect against errors */
    if (NULL == segment || NULL == tokens || NULL == *tokens) {
	return returned_list;
    }

    command = MCA_GPR_GET_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return returned_list;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &mode, 1, MCA_GPR_OOB_PACK_MODE)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack_string(cmd, segment)) {
	goto CLEANUP;
    }

    /* compute number of tokens */
    tokptr = tokens;
    num_tokens = 0;
    while (NULL != *tokptr) {
	num_tokens++;
	tokptr++;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &num_tokens, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    tokptr = tokens;
    for (i=0; i<num_tokens; i++) {  /* pack the tokens */
	if (OMPI_SUCCESS != ompi_pack_string(cmd, *tokptr)) {
	    goto CLEANUP;
	}
	tokptr++;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }


    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_GET_CMD != command)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &num_responses, 1, OMPI_INT32)) ||
	(0 >= num_responses)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    for (i=0; i<num_responses; i++) {
	if (OMPI_SUCCESS != ompi_unpack(answer, &object_size, 1, MCA_GPR_OOB_PACK_OBJECT_SIZE)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	}
	object = (ompi_registry_object_t)malloc(object_size);
	if (OMPI_SUCCESS != ompi_unpack(answer, object, object_size, OMPI_BYTE)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	}
	newptr = OBJ_NEW(ompi_registry_value_t);
	newptr->object_size = object_size;
	newptr->object = (ompi_registry_object_t)malloc(object_size);
	memcpy(newptr->object, object, object_size);
	free(object);
	ompi_list_append(returned_list, &newptr->item);
    }

 CLEANUP:
    ompi_buffer_free(cmd);
    return returned_list;
}

ompi_list_t* gpr_proxy_test_internals(int level)
{
    ompi_list_t *test_results;
    ompi_buffer_t cmd, answer;
    char **string1, **string2;
    int i;
    int32_t num_responses, test_level;
    ompi_registry_internal_test_results_t *newptr;
    mca_gpr_cmd_flag_t command;
    int recv_tag;


    test_results = OBJ_NEW(ompi_list_t);
    test_level = (int32_t)level;

    command = MCA_GPR_TEST_INTERNALS_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return test_results;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &command, 1, MCA_GPR_OOB_PACK_CMD)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, &test_level, 1, OMPI_INT32)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }


    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_TEST_INTERNALS_CMD != command)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &num_responses, 1, OMPI_INT32)) ||
	(0 >= num_responses)) {
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

    for (i=0; i<num_responses; i++) {
	if (0 > ompi_unpack_string(answer, string1)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	}
	if (0 > ompi_unpack_string(answer, string2)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	}
	newptr = OBJ_NEW(ompi_registry_internal_test_results_t);
	newptr->test = strdup(*string1);
	newptr->message = strdup(*string2);
	ompi_list_append(test_results, &newptr->item);
    }
    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return test_results;
}
