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

int mca_gpr_proxy_put(ompi_registry_mode_t mode, char *segment,
		      char **tokens, ompi_registry_object_t object,
		      ompi_registry_object_size_t size)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;
    int response;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "gpr_proxy_put: entered for segment %s 1st token %s", segment, *tokens);
    }

    if (mca_gpr_proxy_compound_cmd_mode) {
	return mca_gpr_base_pack_put(mca_gpr_proxy_compound_cmd, mca_gpr_proxy_silent_mode,
				     mode, segment, tokens, object, size);
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    response = OMPI_ERROR;

    if (OMPI_SUCCESS != mca_gpr_base_pack_put(cmd, mca_gpr_proxy_silent_mode,
					      mode, segment, tokens, object, size)) {
	goto CLEANUP;
    }

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "[%d,%d,%d] gpr_proxy_put: initiating send",
                    OMPI_NAME_ARGS(*ompi_rte_get_self()));
	if (NULL == mca_gpr_my_replica) {
	    ompi_output(0, "\tBAD REPLICA");
	}
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	if (mca_gpr_proxy_debug) {
	    ompi_output(0, "gpr_proxy_put: send failed");
	}
	goto CLEANUP;
    }

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "[%d,%d,%d] gpr_proxy_put: send complete", OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    if (mca_gpr_proxy_silent_mode) {
	ompi_buffer_free(cmd);
	return OMPI_SUCCESS;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    response = mca_gpr_base_unpack_put(answer);
    ompi_buffer_free(answer);

CLEANUP:
    ompi_buffer_free(cmd);
    return response;
}


ompi_list_t* mca_gpr_proxy_get(ompi_registry_mode_t mode, char *segment, char **tokens)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;
    ompi_list_t *returned_list;

    returned_list = OBJ_NEW(ompi_list_t);

    /* need to protect against errors */
    if (NULL == segment || NULL == tokens || NULL == *tokens) {
	return returned_list;
    }

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_get(mca_gpr_proxy_compound_cmd, mode, segment, tokens);
	return returned_list;
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return returned_list;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_get(cmd, mode, segment, tokens)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }


    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    mca_gpr_base_unpack_get(answer, returned_list);
    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return returned_list;
}
