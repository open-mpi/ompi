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

#include "gpr_proxy.h"


void mca_gpr_proxy_silent_mode_on(void)
{
    OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);
    mca_gpr_proxy_silent_mode = true;
    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);
}

void mca_gpr_proxy_silent_mode_off(void)
{
    OMPI_THREAD_LOCK(&mca_gpr_proxy_mutex);
    mca_gpr_proxy_silent_mode = false;
    OMPI_THREAD_UNLOCK(&mca_gpr_proxy_mutex);
}

void mca_gpr_proxy_notify_off(ompi_registry_notify_id_t sub_number)
{
    ompi_buffer_t cmd;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "gpr_proxy_notify_off entered for sub_number %X", sub_number);
    }

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_notify_off(mca_gpr_proxy_compound_cmd,
				     ompi_rte_get_self(), sub_number);
	return;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) {
	return;
    }
    if (OMPI_SUCCESS != mca_gpr_base_pack_notify_off(cmd,
						     ompi_rte_get_self(), sub_number)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

 CLEANUP:
    ompi_buffer_free(cmd);
    return;
}

void mca_gpr_proxy_notify_on(ompi_registry_notify_id_t sub_number)
{
    ompi_buffer_t cmd;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "gpr_proxy_notify_on entered for sub_number %X", sub_number);
    }

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_notify_on(mca_gpr_proxy_compound_cmd,
				    ompi_rte_get_self(), sub_number);
	return;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) {
	return;
    }
    if (OMPI_SUCCESS != mca_gpr_base_pack_notify_on(cmd,
						    ompi_rte_get_self(), sub_number)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

 CLEANUP:
    ompi_buffer_free(cmd);
    return;
}

void mca_gpr_proxy_triggers_active(mca_ns_base_jobid_t jobid)
{
    ompi_buffer_t cmd;

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_triggers_active_cmd(mca_gpr_proxy_compound_cmd, jobid);
	return;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_triggers_active_cmd(cmd, jobid)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

 CLEANUP:
    if (mca_gpr_proxy_debug) {
	   ompi_output(0, "[%d,%d,%d] gpr_proxy_triggers_active: cleanup\n",
		       OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }
    ompi_buffer_free(cmd);
    return;
}

void mca_gpr_proxy_triggers_inactive(mca_ns_base_jobid_t jobid)
{
    ompi_buffer_t cmd;

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_triggers_inactive_cmd(mca_gpr_proxy_compound_cmd, jobid);
	return;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_triggers_inactive_cmd(cmd, jobid)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

 CLEANUP:
    if (mca_gpr_proxy_debug) {
	   ompi_output(0, "[%d,%d,%d] gpr_proxy_triggers_active: cleanup\n",
		       OMPI_NAME_ARGS(*ompi_rte_get_self()));
    }
    ompi_buffer_free(cmd);
    return;
}


int mca_gpr_proxy_assume_ownership(char *segment)
{
    ompi_buffer_t cmd, answer;
    mca_gpr_cmd_flag_t command;
    int recv_tag=MCA_OOB_TAG_GPR;
    int32_t response;
    mca_ns_base_jobid_t jobid;

    if (mca_gpr_proxy_debug) {
	ompi_output(0, "gpr_proxy_assume_ownership entered for segment %s", segment);
    }

    jobid = ompi_name_server.get_jobid(ompi_rte_get_self());

    if (mca_gpr_proxy_compound_cmd_mode) {
	return mca_gpr_base_pack_assume_ownership(mca_gpr_proxy_compound_cmd,
						  mca_gpr_proxy_silent_mode,
						  jobid, segment);
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) {
	return OMPI_ERROR;
    }
    if (OMPI_SUCCESS != mca_gpr_base_pack_assume_ownership(cmd, mca_gpr_proxy_silent_mode,
							   jobid, segment)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }


    if (mca_gpr_proxy_silent_mode) {
	return OMPI_SUCCESS;
    } else {
	if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	    goto CLEANUP;
	}

	if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD))
	    || (MCA_GPR_ASSUME_OWNERSHIP_CMD != command)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	}

	if (OMPI_SUCCESS != ompi_unpack(answer, &response, 1, OMPI_INT32)) {
	    ompi_buffer_free(answer);
	    goto CLEANUP;
	} else {
	    ompi_buffer_free(cmd);
	    ompi_buffer_free(answer);
	    return (int)response;
	}
    }

 CLEANUP:
    ompi_buffer_free(cmd);
    return OMPI_ERROR;
}
