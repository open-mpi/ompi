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
 * The Open MPI General Purpose Registry - proxy component
 *
 */

/*
 * includes
 */
#include "ompi_config.h"

#include "gpr_proxy.h"

ompi_buffer_t mca_gpr_proxy_get_startup_msg(mca_ns_base_jobid_t jobid,
					    ompi_list_t *recipients)
{
    ompi_buffer_t msg, cmd, answer;
    int recv_tag=MCA_OOB_TAG_GPR;

    if (mca_gpr_proxy_compound_cmd_mode) {
    		if (mca_gpr_proxy_debug) {
    			ompi_output(0, "[%d,%d,%d] gpr_proxy: getting startup msg - compound cmd",
    						OMPI_NAME_ARGS(*ompi_rte_get_self()));
    		}
	mca_gpr_base_pack_get_startup_msg(mca_gpr_proxy_compound_cmd, jobid);
	return NULL;
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return NULL;
    }

    msg = NULL;

    if (OMPI_SUCCESS != mca_gpr_base_pack_get_startup_msg(cmd, jobid)) {
	goto CLEANUP;
    }

	if (mca_gpr_proxy_debug) {
		ompi_output(0, "[%d,%d,%d] gpr_proxy: getting startup msg",
					OMPI_NAME_ARGS(*ompi_rte_get_self()));
	}

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	goto CLEANUP;
    }

    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	goto CLEANUP;
    }

    msg = mca_gpr_base_unpack_get_startup_msg(answer, recipients);
    ompi_buffer_free(answer);

 CLEANUP:
    ompi_buffer_free(cmd);
    return msg;
}
