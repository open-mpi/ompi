/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"

#include "gpr_proxy.h"

/**
 * globals
 */

/*
 * Implemented registry functions
 */


int mca_gpr_proxy_delete_segment(char *segment)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR, response;

    if (mca_gpr_proxy_compound_cmd_mode) {
	return mca_gpr_base_pack_delete_segment(mca_gpr_proxy_compound_cmd,
						mca_gpr_proxy_silent_mode, segment);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    response = OMPI_ERROR;

    if (OMPI_SUCCESS != mca_gpr_base_pack_delete_segment(cmd, mca_gpr_proxy_silent_mode,
							 segment)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (mca_gpr_proxy_silent_mode) {
	ompi_buffer_free(cmd);
	return OMPI_SUCCESS;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    response = mca_gpr_base_unpack_delete_segment(answer);
    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return response;
}


int mca_gpr_proxy_delete_object(ompi_registry_mode_t mode,
			    char *segment, char **tokens)
{
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;
    int response;

    if (mca_gpr_proxy_debug) {
	    ompi_output(0, "[%d,%d,%d] gpr_proxy_delete_object", OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    /* need to protect against errors */
    if (NULL == segment || NULL == tokens || NULL == *tokens) {
	return OMPI_ERROR;
    }

    if (mca_gpr_proxy_compound_cmd_mode) {
	return mca_gpr_base_pack_delete_object(mca_gpr_proxy_compound_cmd,
					       mca_gpr_proxy_silent_mode,
					       mode, segment, tokens);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    response = OMPI_ERROR;

    if (OMPI_SUCCESS != mca_gpr_base_pack_delete_object(cmd, mca_gpr_proxy_silent_mode,
							mode, segment, tokens)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (mca_gpr_proxy_silent_mode) {
	ompi_buffer_free(cmd);
	return OMPI_SUCCESS;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    response = mca_gpr_base_unpack_delete_object(answer);
    ompi_buffer_free(answer);


 CLEANUP:
    if (mca_gpr_proxy_debug) {
	   ompi_output(0, "[%d,%d,%d] gpr_proxy_delete_object: cleanup\n", OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }
    ompi_buffer_free(cmd);
    return response;
}


ompi_list_t* mca_gpr_proxy_index(char *segment)
{
    ompi_list_t *return_list;
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;

    return_list = OBJ_NEW(ompi_list_t);

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_index(mca_gpr_proxy_compound_cmd, segment);
	return return_list;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return return_list;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_index(cmd, segment)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    mca_gpr_base_unpack_index(answer, return_list);
    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return return_list;
}
