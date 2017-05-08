/* -*- Mode: C; c-basic-offset:2 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2006      The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2006      The Technical University of Chemnitz. All
 *                         rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * Author(s): Torsten Hoefler <htor@cs.indiana.edu>
 *
 * Modified for LIBPNBC by Bradley Morgan <bradley@auburn.edu>
 *
 */

#include "pnbc_internal.h"

int ompi_coll_libpnbc_start(ompi_request_t ** request) {

	PNBC_DEBUG(5, " ** ompi_coll_libpnbc_start **\n");

	PNBC_Handle *handle;
	PNBC_Schedule *schedule;

	int res;

	handle = (PNBC_Handle *) *request;

	schedule = handle->schedule;

	PNBC_DEBUG(5, "--------------------------------\n");
	PNBC_DEBUG(5, "schedule %p size %u\n", &schedule, sizeof(schedule));
	PNBC_DEBUG(5, "handle %p size %u\n", &handle, sizeof(handle));
	PNBC_DEBUG(5, "data %p size %u\n", &schedule->data, sizeof(schedule->data));
	PNBC_DEBUG(5, "req_array %p size %u\n", &handle->req_array, sizeof(handle->req_array));
	PNBC_DEBUG(5, "row_offset=%u address=%p size=%u\n", handle->row_offset, &handle->row_offset, sizeof(handle->row_offset));
	PNBC_DEBUG(5, "req_count=%u address=%p size=%u\n", handle->req_count, &handle->req_count, sizeof(handle->req_count));
	PNBC_DEBUG(5, "tmpbuf address=%p size=%u\n", handle->tmpbuf, sizeof(handle->tmpbuf));
	PNBC_DEBUG(5, "--------------------------------\n");

	res = PNBC_Start_internal(handle, schedule);
	if (OPAL_UNLIKELY(OMPI_SUCCESS != res)) {
		PNBC_DEBUG(5, " ** bad result from pnbc_start_internal **\n");
	    PNBC_Return_handle (handle);
	    return res;
	}

	PNBC_DEBUG(5, " ** LEAVING ompi_coll_libpnbc_start **\n");

	return OMPI_SUCCESS;

}
