/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/gpr/base/base.h"
#include "gpr_proxy.h"

/**
 * globals
 */

/*
 * Implemented registry functions
 */

int gpr_proxy_define_segment(char *segment)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    mca_gpr_cmd_flag_t command;
    int recv_tag;
    int32_t response;

    command = MCA_GPR_DEFINE_SEGMENT_CMD;
    recv_tag = MCA_OOB_TAG_GPR;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)&command, 1, MCA_GPR_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)segment, 1, OMPI_STRING)) {
	return OMPI_ERROR;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	return OMPI_ERROR;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	return OMPI_ERROR;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	|| (MCA_GPR_DEFINE_SEGMENT_CMD != command)) {
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
}


int gpr_proxy_delete_segment(char *segment)
{
    return OMPI_SUCCESS;
}


int gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		  char **tokens, ompi_registry_object_t *object,
		  int size)
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

    if (OMPI_SUCCESS != ompi_pack(cmd, segment, 1, OMPI_STRING)) {
	return OMPI_ERROR;
    }

    /* compute number of tokens */
    tokptr = tokens;
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
    return OMPI_SUCCESS;
}


ompi_list_t* gpr_proxy_index(char *segment)
{
    ompi_list_t *answer;

    answer = OBJ_NEW(ompi_list_t);

    return answer;
}


int gpr_proxy_subscribe(ompi_process_name_t *caller, ompi_registry_mode_t mode,
			ompi_registry_notify_action_t action,
			char *segment, char **tokens)
{
    return OMPI_SUCCESS;
}


int gpr_proxy_unsubscribe(ompi_process_name_t *caller, ompi_registry_mode_t mode,
			  char *segment, char **tokens)
{
    return OMPI_SUCCESS;
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

    test_results = OBJ_NEW(ompi_list_t);

    return test_results;
}
