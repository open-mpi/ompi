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


void mca_gpr_proxy_cleanup_job(mca_ns_base_jobid_t jobid)
{
    ompi_buffer_t cmd;

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_cleanup_job(mca_gpr_proxy_compound_cmd, jobid);
	return;
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_cleanup_job(cmd, jobid)) {
	return;
    }

    mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0);

    ompi_buffer_free(cmd);

}


void mca_gpr_proxy_cleanup_proc(bool purge, ompi_process_name_t *proc)
{
    ompi_buffer_t cmd;

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_cleanup_proc(mca_gpr_proxy_compound_cmd, purge, proc);
	return;
    }

    if (mca_gpr_proxy_debug) {
        ompi_output(0, "[%d,%d,%d] cleanup_process: function entered for proc [%d,%d,%d]",
            OMPI_NAME_ARGS(*ompi_rte_get_self()), OMPI_NAME_ARGS(*proc));
    }

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_cleanup_proc(cmd, purge, proc)) {
	return;
    }

    mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0);

    ompi_buffer_free(cmd);

}
