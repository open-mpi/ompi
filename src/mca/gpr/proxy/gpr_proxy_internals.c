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

#include "gpr_proxy.h"

ompi_registry_notify_id_t
mca_gpr_proxy_enter_notify_request(char *segment,
				   ompi_registry_notify_action_t action,
				   ompi_registry_notify_cb_fn_t cb_func,
				   void *user_tag)
{
    mca_gpr_proxy_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;

    trackptr = OBJ_NEW(mca_gpr_proxy_notify_request_tracker_t);
    trackptr->segment = strdup(segment);
    trackptr->action = action;
    trackptr->callback = cb_func;
    trackptr->user_tag = user_tag;
    trackptr->remote_idtag = OMPI_REGISTRY_NOTIFY_ID_MAX;
    if (ompi_list_is_empty(&mca_gpr_proxy_free_notify_id_tags)) {
	trackptr->local_idtag = mca_gpr_proxy_last_notify_id_tag;
	mca_gpr_proxy_last_notify_id_tag++;
    } else {
	ptr_free_id = (mca_gpr_idtag_list_t*)ompi_list_remove_first(&mca_gpr_proxy_free_notify_id_tags);
	trackptr->local_idtag = ptr_free_id->id_tag;
    }
    ompi_list_append(&mca_gpr_proxy_notify_request_tracker, &trackptr->item);

    return trackptr->local_idtag;
}


ompi_registry_notify_id_t
mca_gpr_proxy_remove_notify_request(ompi_registry_notify_id_t local_idtag)
{
    mca_gpr_proxy_notify_request_tracker_t *trackptr;
    mca_gpr_idtag_list_t *ptr_free_id;
    ompi_registry_notify_id_t remote_idtag;

    /* locate corresponding entry on proxy tracker list and remove it */
    for (trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_proxy_notify_request_tracker);
	 trackptr != (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_proxy_notify_request_tracker) &&
	     trackptr->local_idtag != local_idtag;
	 trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_next(trackptr));

    if (trackptr == (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_proxy_notify_request_tracker)) {
	return OMPI_REGISTRY_NOTIFY_ID_MAX;
    }

    remote_idtag = trackptr->remote_idtag;
    ompi_list_remove_item(&mca_gpr_proxy_notify_request_tracker, &trackptr->item);

    /* put id tag on free list */
    ptr_free_id = OBJ_NEW(mca_gpr_idtag_list_t);
    ptr_free_id->id_tag = trackptr->local_idtag;
    ompi_list_append(&mca_gpr_proxy_free_notify_id_tags, &ptr_free_id->item);

    /* release tracker item */
    OBJ_RELEASE(trackptr);

    return remote_idtag;
}


int mca_gpr_proxy_set_remote_idtag(ompi_registry_notify_id_t local_idtag,
				   ompi_registry_notify_id_t remote_idtag)
{
    mca_gpr_proxy_notify_request_tracker_t *trackptr;

    /* locate corresponding entry on proxy tracker list  */
    for (trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_first(&mca_gpr_proxy_notify_request_tracker);
	 trackptr != (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_proxy_notify_request_tracker) &&
	     trackptr->local_idtag != local_idtag;
	 trackptr = (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_next(trackptr));

    if (trackptr == (mca_gpr_proxy_notify_request_tracker_t*)ompi_list_get_end(&mca_gpr_proxy_notify_request_tracker)) {
	return OMPI_ERROR;
    }

    trackptr->remote_idtag = remote_idtag;
    return OMPI_SUCCESS;
}


ompi_list_t* mca_gpr_proxy_test_internals(int level)
{
    ompi_list_t *test_results=NULL;
    ompi_buffer_t cmd, answer;
    int recv_tag;

    test_results = OBJ_NEW(ompi_list_t);

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_test_internals(mca_gpr_proxy_compound_cmd, level);
	return test_results;
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return test_results;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_test_internals(cmd, level)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }


    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    if (OMPI_SUCCESS != mca_gpr_base_unpack_test_internals(answer, test_results)) {
	/* clear any partial results from the list */
    }

    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return test_results;
}
