/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010      Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/communicator/communicator.h"

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_endpoint.h"

static int
completion_fn(ptl_event_t *ev, ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    ompi_mtl_portals4_probe_request_t *ptl_request = 
        (ompi_mtl_portals4_probe_request_t*) ptl_base_request;

    if (ev->ni_fail_type == PTL_OK) {
        /* set the status */
        ptl_request->status.MPI_SOURCE = PTL_GET_SOURCE(ev->match_bits);
        ptl_request->status.MPI_TAG = PTL_GET_TAG(ev->match_bits);
        ptl_request->status.MPI_ERROR = MPI_SUCCESS;
        ptl_request->status._ucount = MTL_PORTALS4_GET_LENGTH(ev->hdr_data);
        ptl_request->found_match = 1;
    } else {
        ptl_request->found_match = 0;
    }
    opal_atomic_wmb();
    ptl_request->req_complete = 1;

    return OMPI_SUCCESS;
}

int
ompi_mtl_portals4_iprobe(struct mca_mtl_base_module_t* mtl,
                         struct ompi_communicator_t *comm,
                         int src,
                         int tag,
                         int *flag,
                         struct ompi_status_public_t *status)
{
    struct ompi_mtl_portals4_probe_request_t request;
    mca_mtl_base_endpoint_t *endpoint = NULL;
    ptl_me_t me;
    ptl_process_t remote_proc;
    ptl_match_bits_t match_bits, ignore_bits;
    int ret;

    if  (MPI_ANY_SOURCE == src) {
        remote_proc.phys.nid = PTL_NID_ANY;
        remote_proc.phys.pid = PTL_PID_ANY;
    } else {
        ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, src );
        endpoint = (mca_mtl_base_endpoint_t*) ompi_proc->proc_pml;
        remote_proc = endpoint->ptl_proc;
    }

    PTL_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid,
                      src, tag);

    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT;
    me.match_id = remote_proc;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    request.super.event_callback = completion_fn;
    request.req_complete = 0;
    request.found_match = 0;

    opal_atomic_wmb();

    ret = PtlMESearch(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.send_idx,
                      &me,
                      PTL_SEARCH_ONLY,
                      &request);
    if (PTL_OK != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlMESearch failed: %d",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    while (0 == request.req_complete) {
        opal_progress();
    }

    *flag = request.found_match;
    if (1 == *flag) {
        *status = request.status;
    }

    return OMPI_ERR_NOT_IMPLEMENTED;
}
