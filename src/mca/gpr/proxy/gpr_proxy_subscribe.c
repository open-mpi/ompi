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
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "ompi_config.h"

#include "gpr_proxy.h"

ompi_registry_notify_id_t
mca_gpr_proxy_subscribe(ompi_registry_mode_t mode,
			ompi_registry_notify_action_t action,
			char *segment, char **tokens,
			ompi_registry_notify_cb_fn_t cb_func, void *user_tag)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;
    ompi_registry_notify_id_t idtag, response, remote_idtag;

    /* need to protect against errors */
    if (NULL == segment) {
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }

    if (mca_gpr_proxy_compound_cmd_mode) {
	if (OMPI_SUCCESS != mca_gpr_base_pack_subscribe(mca_gpr_proxy_compound_cmd,
							mode, action, segment, tokens)) {
	    return OMPI_REGISTRY_NOTIFY_ID_MAX;
	}

	OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);

	/* store callback function and user_tag in local list for lookup */
	/* generate id_tag to send to replica to identify lookup entry */
	idtag = mca_gpr_proxy_enter_notify_request(segment, action, cb_func, user_tag);

	OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);

	if (OMPI_SUCCESS != ompi_pack(mca_gpr_proxy_compound_cmd, &idtag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	    return OMPI_REGISTRY_NOTIFY_ID_MAX;
	}
	return idtag;
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }

    response = OMPI_REGISTRY_NOTIFY_ID_MAX;

    if (OMPI_SUCCESS != mca_gpr_base_pack_subscribe(cmd, mode, action, segment, tokens)) {
	goto CLEANUP;
    }

    OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);

    /* store callback function and user_tag in local list for lookup */
    /* generate id_tag to send to replica to identify lookup entry */
    idtag = mca_gpr_proxy_enter_notify_request(segment, action, cb_func, user_tag);

    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);

    if (OMPI_SUCCESS != ompi_pack(cmd, &idtag, 1, MCA_GPR_OOB_PACK_NOTIFY_ID)) {
	goto CLEANUP;
    }

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "[%d,%d,%d] gpr proxy subscribe: subscribing to segment %s local idtag %d",
				OMPI_NAME_ARGS(*ompi_rte_get_self()), segment, (int)idtag);
    }


    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != mca_gpr_base_unpack_subscribe(answer, &remote_idtag)) {
	if (mca_gpr_proxy_debug) {
	    ompi_output(0, "proxy_subscribe: unable to unpack");
	}
	OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);
	mca_gpr_proxy_remove_notify_request(idtag);
	OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);

	response = OMPI_REGISTRY_NOTIFY_ID_MAX;

    } else {
	response = remote_idtag;
	mca_gpr_proxy_set_remote_idtag(idtag, remote_idtag);
    }

    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return response;
}


int mca_gpr_proxy_unsubscribe(ompi_registry_notify_id_t sub_number)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;
    int response;
    ompi_registry_notify_id_t remote_idtag;

    OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);
    remote_idtag = mca_gpr_proxy_remove_notify_request(sub_number);
    if (OMPI_REGISTRY_NOTIFY_ID_MAX == remote_idtag) {
	return OMPI_ERROR;
    }
    response = mca_gpr_base_pack_unsubscribe(mca_gpr_proxy_compound_cmd,
					     mca_gpr_proxy_silent_mode,
					     remote_idtag);
    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);

    if (mca_gpr_proxy_compound_cmd_mode || OMPI_SUCCESS != response) {
	return response;
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }


    if (OMPI_SUCCESS != mca_gpr_base_pack_unsubscribe(cmd, mca_gpr_proxy_silent_mode,
						      remote_idtag)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (mca_gpr_proxy_silent_mode) {
	goto COMPLETE;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != mca_gpr_base_unpack_unsubscribe(answer)) {  /* got an error on replica */
	ompi_buffer_free(answer);
	goto CLEANUP;
    }

 COMPLETE:
    ompi_buffer_free(answer);
    ompi_buffer_free(cmd);
    return OMPI_SUCCESS;

 CLEANUP:
    ompi_buffer_free(cmd);
    return OMPI_ERROR;
}
