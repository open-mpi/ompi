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

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "include/constants.h"

#include "threads/mutex.h"

#include "util/output.h"
#include "util/proc_info.h"
#include "util/sys_info.h"

#include "mca/gpr/base/base.h"
#include "gpr_proxy.h"

void mca_gpr_proxy_dump(int output_id)
{
    mca_gpr_cmd_flag_t command;
    ompi_buffer_t cmd;
    ompi_buffer_t answer;
    int recv_tag=MCA_OOB_TAG_GPR;

    if (mca_gpr_proxy_compound_cmd_mode) {
	mca_gpr_base_pack_dump(mca_gpr_proxy_compound_cmd);
	return;
    }


    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return;
    }

    if (OMPI_SUCCESS != mca_gpr_base_pack_dump(cmd)) {
	return;
    }

    if (0 > mca_oob_send_packed(mca_gpr_my_replica, cmd, MCA_OOB_TAG_GPR, 0)) {
	return;
    }


    if (0 > mca_oob_recv_packed(mca_gpr_my_replica, &answer, &recv_tag)) {
	return;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_GPR_OOB_PACK_CMD)) ||
	(MCA_GPR_DUMP_CMD != command)) {
	return;
    }

    mca_gpr_base_print_dump(answer, output_id);
    ompi_buffer_free(answer);
    return;
}
