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
 */

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/mca.h"
#include "util/bufpack.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "ns_proxy.h"

/**
 * globals
 */

/*
 * functions
 */

mca_ns_base_cellid_t mca_ns_proxy_create_cellid(void)
{
    ompi_buffer_t cmd;
    mca_ns_base_cellid_t cell;
    ompi_buffer_t answer;
    mca_ns_cmd_flag_t command;
    int recv_tag;

    command = MCA_NS_CREATE_CELLID_CMD;
    recv_tag = MCA_OOB_TAG_NS;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) {
	return OMPI_ERROR;
    }

    if (0 > mca_oob_send_packed(mca_ns_my_replica, cmd, MCA_OOB_TAG_NS, 0)) {
	return MCA_NS_BASE_CELLID_MAX;
    }

    if (0 > mca_oob_recv_packed(mca_ns_my_replica, &answer, &recv_tag)) {
	return MCA_NS_BASE_CELLID_MAX;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_NS_OOB_PACK_CMD))
	|| (MCA_NS_CREATE_CELLID_CMD != command)) {
	ompi_buffer_free(answer);
	return MCA_NS_BASE_CELLID_MAX;
    }

    if (OMPI_SUCCESS != ompi_unpack(answer, &cell, 1, MCA_NS_OOB_PACK_CELLID)) {
	ompi_buffer_free(answer);
	return MCA_NS_BASE_CELLID_MAX;
    } else {
	ompi_buffer_free(answer);
	return cell;
    }
}


mca_ns_base_jobid_t mca_ns_proxy_create_jobid(void)
{
    ompi_buffer_t cmd;
    mca_ns_base_jobid_t job;
    ompi_buffer_t answer;
    mca_ns_cmd_flag_t command;
    int recv_tag;

    command = MCA_NS_CREATE_JOBID_CMD;
    recv_tag = MCA_OOB_TAG_NS;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (0 > mca_oob_send_packed(mca_ns_my_replica, cmd, MCA_OOB_TAG_NS, 0)) {
	return MCA_NS_BASE_JOBID_MAX;
    }

    if (0 > mca_oob_recv_packed(mca_ns_my_replica, &answer, &recv_tag)) {
	return MCA_NS_BASE_JOBID_MAX;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_NS_OOB_PACK_CMD))
	|| (MCA_NS_CREATE_JOBID_CMD != command)) {
	ompi_buffer_free(answer);
	return MCA_NS_BASE_JOBID_MAX;
    }

    if (OMPI_SUCCESS != ompi_unpack(answer, &job, 1, MCA_NS_OOB_PACK_JOBID)) {
	ompi_buffer_free(answer);
	return MCA_NS_BASE_JOBID_MAX;
    } else {
	ompi_buffer_free(answer);
	return job;
    }
}


mca_ns_base_vpid_t mca_ns_proxy_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range)
{
    ompi_buffer_t cmd;
    mca_ns_base_vpid_t starting_vpid;
    ompi_buffer_t answer;
    mca_ns_cmd_flag_t command;
    int recv_tag;

    command = MCA_NS_RESERVE_RANGE_CMD;
    recv_tag = MCA_OOB_TAG_NS;

    if (OMPI_SUCCESS != ompi_buffer_init(&cmd, 0)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)&command, 1, MCA_NS_OOB_PACK_CMD)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)&job, 1, MCA_NS_OOB_PACK_JOBID)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (OMPI_SUCCESS != ompi_pack(cmd, (void*)&range, 1, MCA_NS_OOB_PACK_VPID)) { /* got a problem */
	return OMPI_ERROR;
    }

    if (0 > mca_oob_send_packed(mca_ns_my_replica, cmd, MCA_OOB_TAG_NS, 0)) {
	return MCA_NS_BASE_VPID_MAX;
    }

    if (0 > mca_oob_recv_packed(mca_ns_my_replica, &answer, &recv_tag)) {
	return MCA_NS_BASE_VPID_MAX;
    }

    if ((OMPI_SUCCESS != ompi_unpack(answer, &command, 1, MCA_NS_OOB_PACK_CMD))
	|| (MCA_NS_RESERVE_RANGE_CMD != command)) {
	ompi_buffer_free(answer);
	return MCA_NS_BASE_VPID_MAX;
    }

    if (OMPI_SUCCESS != ompi_unpack(answer, &starting_vpid, 1, MCA_NS_OOB_PACK_VPID)) {
	ompi_buffer_free(answer);
	return MCA_NS_BASE_VPID_MAX;
    } else {
	ompi_buffer_free(answer);
	return starting_vpid;
    }
}
