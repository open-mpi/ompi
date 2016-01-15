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
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/communicator/communicator.h"
#include "ompi/message/message.h"

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_message.h"

static int
completion_fn(ptl_event_t *ev, ompi_mtl_portals4_base_request_t *ptl_base_request)
{
    ompi_mtl_portals4_probe_request_t *ptl_request =
        (ompi_mtl_portals4_probe_request_t*) ptl_base_request;

    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "%s:%d: completion_fn: %d %d",
                        __FILE__, __LINE__, ev->type, ev->ni_fail_type);

    if (OPAL_UNLIKELY(ev->ni_fail_type == PTL_OK)) {
        ptl_request->found_match = 1;
        ptl_request->status.MPI_SOURCE = MTL_PORTALS4_GET_SOURCE(ev->match_bits);
        ptl_request->status.MPI_TAG = MTL_PORTALS4_GET_TAG(ev->match_bits);
        ptl_request->status.MPI_ERROR = MPI_SUCCESS;
        ptl_request->status._ucount += ev->mlength;
        if (ev->type != PTL_EVENT_SEARCH) {
            ptl_request->message = ompi_mtl_portals4_message_alloc(ev);
        }
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
    ptl_me_t me;
    ptl_process_t remote_proc;
    ptl_match_bits_t match_bits, ignore_bits;
    int ret;

    if  (MPI_ANY_SOURCE == src) {
        if (ompi_mtl_portals4.use_logical) {
            remote_proc.rank = PTL_RANK_ANY;
        } else {
            remote_proc.phys.nid = PTL_NID_ANY;
            remote_proc.phys.pid = PTL_PID_ANY;
        }
    } else if ((ompi_mtl_portals4.use_logical) && (MPI_COMM_WORLD == comm)) {
        remote_proc.rank = src;
    } else {
        ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, src );
        remote_proc = *((ptl_process_t*) ompi_mtl_portals4_get_endpoint (mtl, ompi_proc));
    }

    MTL_PORTALS4_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid,
                               src, tag);

    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_USE_ONCE;
    me.match_id = remote_proc;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    request.super.type = portals4_req_probe;
    request.super.event_callback = completion_fn;
    request.req_complete = 0;
    request.found_match = 0;

    opal_atomic_wmb();

    ret = PtlMESearch(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.recv_idx,
                      &me,
                      PTL_SEARCH_ONLY,
                      &request);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
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

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_improbe(struct mca_mtl_base_module_t *mtl,
                          struct ompi_communicator_t *comm,
                          int src,
                          int tag,
                          int *matched,
                          struct ompi_message_t **message,
                          struct ompi_status_public_t *status)
{
    struct ompi_mtl_portals4_probe_request_t request;
    ptl_me_t me;
    ptl_process_t remote_proc;
    ptl_match_bits_t match_bits, ignore_bits;
    int ret;

    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "%s:%d: improbe %d %d %d",
                        __FILE__, __LINE__, comm->c_contextid, src, tag);

    if  (MPI_ANY_SOURCE == src) {
        if (ompi_mtl_portals4.use_logical) {
            remote_proc.rank = PTL_RANK_ANY;
        } else {
            remote_proc.phys.nid = PTL_NID_ANY;
            remote_proc.phys.pid = PTL_PID_ANY;
        }
    } else if ((ompi_mtl_portals4.use_logical) && (MPI_COMM_WORLD == comm)) {
        remote_proc.rank = src;
    } else {
        ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, src );
        remote_proc = *((ptl_process_t*) ompi_mtl_portals4_get_endpoint (mtl, ompi_proc));
    }

    MTL_PORTALS4_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid,
                               src, tag);

    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
    me.options = PTL_ME_OP_PUT | PTL_ME_USE_ONCE;
    me.match_id = remote_proc;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    request.super.type = portals4_req_probe;
    request.super.event_callback = completion_fn;
    request.req_complete = 0;
    request.found_match = 0;

    opal_atomic_wmb();

    ret = PtlMESearch(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.recv_idx,
                      &me,
                      PTL_SEARCH_DELETE,
                      &request);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMESearch failed: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    while (0 == request.req_complete) {
        opal_progress();
    }

    *matched = request.found_match;
    if (1 == *matched) {
        *status = request.status;

        (*message) = ompi_message_alloc();
        if (NULL == (*message)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        (*message)->comm = comm;
        (*message)->req_ptr = request.message;
        (*message)->peer = status->MPI_SOURCE;
        (*message)->count = status->_ucount;

        if (NULL == (*message)->req_ptr) {
            ompi_message_return(*message);
            *message = NULL;
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    } else {
        (*message) = MPI_MESSAGE_NULL;
    }

    return OMPI_SUCCESS;
}
