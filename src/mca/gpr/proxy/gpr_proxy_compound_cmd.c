/*
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


int mca_gpr_proxy_begin_compound_cmd(void)
{
    size_t size;

    OMPI_THREAD_LOCK(&mca_gpr_proxy_wait_for_compound_mutex);

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_proxy_compound_cmd_waiting++;
	ompi_condition_wait(&mca_gpr_proxy_compound_cmd_condition, &mca_gpr_proxy_wait_for_compound_mutex);
	mca_gpr_proxy_compound_cmd_waiting--;
    }

    mca_gpr_proxy_compound_cmd_mode = true;
    if (NULL != mca_gpr_proxy_compound_cmd) {  /* first time through, pointer is NULL, so just perform init */
        ompi_buffer_size(mca_gpr_proxy_compound_cmd, &size);
        if (0 < size) {
	    ompi_buffer_free(mca_gpr_proxy_compound_cmd);
        }
    }

    ompi_buffer_init(&mca_gpr_proxy_compound_cmd, 0);

    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_wait_for_compound_mutex);
    return OMPI_SUCCESS;
}


int mca_gpr_proxy_stop_compound_cmd(void)
{
    size_t size;

    OMPI_THREAD_LOCK(&mca_gpr_proxy_wait_for_compound_mutex);

    mca_gpr_proxy_compound_cmd_mode = false;
    if (NULL != mca_gpr_proxy_compound_cmd) {
	ompi_buffer_size(mca_gpr_proxy_compound_cmd, &size);
	if (0 < size) {
	    ompi_buffer_free(mca_gpr_proxy_compound_cmd);
	}
    }

    if (mca_gpr_proxy_compound_cmd_waiting) {
	ompi_condition_signal(&mca_gpr_proxy_compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_wait_for_compound_mutex);
    return OMPI_SUCCESS;
}


ompi_list_t* mca_gpr_proxy_exec_compound_cmd(bool return_requested)
{
    uint8_t tmp;
    mca_gpr_cmd_flag_t command;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;
    size_t size;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "[%d,%d,%d] transmitting compound command",
		    OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }

    OMPI_THREAD_LOCK(&mca_gpr_proxy_wait_for_compound_mutex);

    /**
     * pack the exec_compound_cmd command and return_requested flag at the end of the buffer
     * then send command off to be processed
     */

    command = MCA_GPR_COMPOUND_CMD;
    ompi_pack(mca_gpr_proxy_compound_cmd, &command, 1, MCA_GPR_OOB_PACK_CMD);

    tmp = (uint8_t)return_requested;
    ompi_pack(mca_gpr_proxy_compound_cmd, &tmp, 1, MCA_GPR_OOB_PACK_BOOL);

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, mca_gpr_proxy_compound_cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (return_requested) {
	if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	    goto CLEANUP;
	}

	/* RHC = need to figure out how to unpack the return message */
    }

 CLEANUP:
    mca_gpr_proxy_compound_cmd_mode = false;
    if (NULL != mca_gpr_proxy_compound_cmd) {  /* shouldn't be any way this could be true, but just to be safe... */
	ompi_buffer_size(mca_gpr_proxy_compound_cmd, &size);
	if (0 < size) {
	    ompi_buffer_free(mca_gpr_proxy_compound_cmd);
	}
    }

    if (mca_gpr_proxy_compound_cmd_waiting) {
	ompi_condition_signal(&mca_gpr_proxy_compound_cmd_condition);
    }

    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_wait_for_compound_mutex);

    return OMPI_SUCCESS;
}


