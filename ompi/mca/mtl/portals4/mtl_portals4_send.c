/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_portals4.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_endpoint.h"
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
#include "mtl_portals4_flowctl.h"
#endif


static inline int
ompi_mtl_portals4_callback(ptl_event_t *ev, 
                           ompi_mtl_portals4_base_request_t* ptl_base_request,
                           bool *complete)
{
    int retval = OMPI_SUCCESS, ret, val, add = 1;
    ompi_mtl_portals4_isend_request_t* ptl_request = 
        (ompi_mtl_portals4_isend_request_t*) ptl_base_request;
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    ompi_mtl_portals4_pending_request_t *pending = 
        ptl_request->pending;

    if (ev->ni_fail_type == PTL_NI_FLOW_CTRL) {
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                             "send %lu hit flow control",
                             ptl_request->opcount));

        ompi_mtl_portals4_flowctl_start_recover();
        opal_list_remove_item(&ompi_mtl_portals4.flowctl.active_sends, 
                              &pending->super.super);
        opal_list_append(&ompi_mtl_portals4.flowctl.pending_sends, 
                         &pending->super.super);
        return OMPI_SUCCESS;
    }
#endif
    if (ev->ni_fail_type != PTL_NI_OK) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: send callback ni_fail_type: %d",
                            __FILE__, __LINE__, ev->ni_fail_type);
        *complete = true;
        return OMPI_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "send %lu got event of type %d",
                         ptl_request->opcount, ev->type));

    /* If we received an ack in the priority list, that's worth two
       messages.  If it hit the overflow list, that's only one.  Since
       we start eager messages at one count, have to compare >=
       instead of just == */
    if (ev->type == PTL_EVENT_ACK) {
        if (ev->ptl_list == PTL_PRIORITY_LIST) {
            /* ack on the priority list, so will never see an event on the me */
            if (PTL_OK != PtlHandleIsEqual(ptl_request->me_h, PTL_INVALID_HANDLE)) {
                ret = PtlMEUnlink(ptl_request->me_h);
                if (PTL_OK != ret) {
                    opal_output_verbose(1, ompi_mtl_base_output,
                                        "%s:%d: send callback PtlMDUnlink returned %d",
                                        __FILE__, __LINE__, ret);
                }
            }
            add++;
        }
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
        /* once the ack is received, we're out of flow control problem
           regions, so we can remove this list entry */
        opal_list_remove_item(&ompi_mtl_portals4.flowctl.active_sends,
                              &pending->super.super);
        OPAL_FREE_LIST_RETURN(&ompi_mtl_portals4.flowctl.pending_fl,
                              &pending->super);
#endif
    }
    val = opal_atomic_add_32((int32_t*)&ptl_request->event_count, add);

    if (val >= 3) {
        if (NULL != ptl_request->buffer_ptr) {
            free(ptl_request->buffer_ptr);
        }
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: send callback PtlMDRelease returned %d",
                                __FILE__, __LINE__, ret);
            retval = OMPI_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output, "send %lu completed",
                             ptl_request->opcount));
        *complete = true;
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
        opal_atomic_add_32(&ompi_mtl_portals4.flowctl.slots, 1);
        ompi_mtl_portals4_pending_list_progress();
#endif
    }
    
    return retval;
}


static int
ompi_mtl_portals4_send_callback(ptl_event_t *ev,
                                ompi_mtl_portals4_base_request_t* ptl_base_request)
{
    bool complete = false;
    int ret;
    ompi_mtl_portals4_send_request_t* ptl_request = 
        (ompi_mtl_portals4_send_request_t*) ptl_base_request;

    ret = ompi_mtl_portals4_callback(ev, ptl_base_request, &complete);
    if (complete) {
        ptl_request->retval = ret;
        opal_atomic_wmb();
        ptl_request->complete = true;
    }

    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_isend_callback(ptl_event_t *ev,
                                 ompi_mtl_portals4_base_request_t* ptl_base_request)
{
    bool complete = false;
    int ret;
    ompi_mtl_portals4_isend_request_t* ptl_request = 
        (ompi_mtl_portals4_isend_request_t*) ptl_base_request;

    ret = ompi_mtl_portals4_callback(ev, ptl_base_request, &complete);
    if (complete) {
        ptl_request->super.super.ompi_req->req_status.MPI_ERROR = ret;
        ptl_request->super.super.completion_callback(&ptl_request->super.super);
    }

    return OMPI_SUCCESS;
}


static inline int
ompi_mtl_portals4_short_isend(mca_pml_base_send_mode_t mode, 
                              void *start, int length, int contextid, int tag,
                              int localrank,
                              mca_mtl_base_endpoint_t *endpoint,
                              ompi_mtl_portals4_isend_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_me_t me;
    ptl_hdr_data_t hdr_data;

    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, 
                               MTL_PORTALS4_SHORT_MSG);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 
                              (MCA_PML_BASE_SEND_SYNCHRONOUS == mode) ? 1 : 0);
    
    md.start = start;
    md.length = length;
    md.options = 0;
    md.eq_handle = ompi_mtl_portals4.send_eq_h;
    md.ct_handle = PTL_CT_NONE;
    
    ret = PtlMDBind(ompi_mtl_portals4.ni_h,
		    &md,
		    &ptl_request->md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMDBind failed: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    if (MCA_PML_BASE_SEND_SYNCHRONOUS == mode) {
        me.start = NULL;
        me.length = 0;
        me.ct_handle = PTL_CT_NONE;
        me.min_free = 0;
        me.uid = PTL_UID_ANY;
        me.options = 
            PTL_ME_OP_PUT | 
            PTL_ME_USE_ONCE | 
            PTL_ME_EVENT_LINK_DISABLE |
            PTL_ME_EVENT_UNLINK_DISABLE;
        me.match_id = endpoint->ptl_proc;
        me.match_bits = hdr_data;
        me.ignore_bits = 0;

        ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                          ompi_mtl_portals4.read_idx,
                          &me,
                          PTL_PRIORITY_LIST,
                          ptl_request,
                          &ptl_request->me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMEAppend failed: %d",
                                __FILE__, __LINE__, ret);
            PtlMDRelease(ptl_request->md_h);
            return ompi_mtl_portals4_get_error(ret);
        }

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                             "Send %lu short sync send with hdr_data 0x%lx (0x%lx)",
                             ptl_request->opcount, hdr_data, match_bits));
    } else {
        ptl_request->event_count = 1;
        ptl_request->me_h = PTL_INVALID_HANDLE;

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                             "Send %lu short send with hdr_data 0x%lx (0x%lx)",
                             ptl_request->opcount, hdr_data, match_bits));
    }

    ret = PtlPut(ptl_request->md_h,
                 0,
                 length,
		 PTL_ACK_REQ,
		 endpoint->ptl_proc,
		 ompi_mtl_portals4.recv_idx,
		 match_bits,
		 0,
                 ptl_request,
		 hdr_data);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
        if (MCA_PML_BASE_SEND_SYNCHRONOUS == mode) {
            PtlMEUnlink(ptl_request->me_h);
        }
	PtlMDRelease(ptl_request->md_h);
        return ompi_mtl_portals4_get_error(ret);
    }
    
    return OMPI_SUCCESS;
}

static inline int
ompi_mtl_portals4_long_isend(void *start, int length, int contextid, int tag,
                             int localrank, 
                             mca_mtl_base_endpoint_t *endpoint,
                             ompi_mtl_portals4_isend_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_me_t me;
    ptl_hdr_data_t hdr_data;
    ptl_size_t put_length;

    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, 
                               MTL_PORTALS4_LONG_MSG);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 0);

    md.start = start;
    md.length = length;
    md.options = 0;
    md.eq_handle = ompi_mtl_portals4.send_eq_h;
    md.ct_handle = PTL_CT_NONE;

    ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                    &md,
                    &ptl_request->md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMDBind failed: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    me.start = start;
    me.length = length;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = PTL_UID_ANY;
    me.options = 
        PTL_ME_OP_GET | 
        PTL_ME_USE_ONCE | 
        PTL_ME_EVENT_LINK_DISABLE |
        PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id = endpoint->ptl_proc;
    me.match_bits = hdr_data;
    me.ignore_bits = 0;

    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.read_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      ptl_request,
                      &ptl_request->me_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlMEAppend failed: %d",
                            __FILE__, __LINE__, ret);
        PtlMDRelease(ptl_request->md_h);
        return ompi_mtl_portals4_get_error(ret);
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "Send %lu long send with hdr_data 0x%lx (0x%lx)",
                         ptl_request->opcount, hdr_data, match_bits));

    put_length = (rndv == ompi_mtl_portals4.protocol) ? 
        (ptl_size_t) ompi_mtl_portals4.eager_limit : (ptl_size_t) length;
    ret = PtlPut(ptl_request->md_h,
                 0,
                 put_length,
                 PTL_ACK_REQ,
                 endpoint->ptl_proc,
                 ompi_mtl_portals4.recv_idx,
                 match_bits,
                 0,
                 ptl_request,
                 hdr_data);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
	PtlMEUnlink(ptl_request->me_h);
        PtlMDRelease(ptl_request->md_h);
        return ompi_mtl_portals4_get_error(ret);
    }

    return OMPI_SUCCESS;
}


#if OMPI_MTL_PORTALS4_FLOW_CONTROL
void
ompi_mtl_portals4_pending_list_progress()
{
    int ret, val;
    opal_list_item_t *item;
    ompi_mtl_portals4_pending_request_t *pending;

    while (!ompi_mtl_portals4.flowctl.flowctl_active) {
        val = opal_atomic_add_32(&ompi_mtl_portals4.flowctl.slots, -1);
        if (val <= 0) {
            opal_atomic_add_32(&ompi_mtl_portals4.flowctl.slots, 1);
            return;
        }

        item = opal_list_remove_first(&ompi_mtl_portals4.flowctl.pending_sends);
        if (NULL == item) {
            opal_atomic_add_32(&ompi_mtl_portals4.flowctl.slots, 1);
            return;
        }

        pending = (ompi_mtl_portals4_pending_request_t*) item;
        opal_list_append(&ompi_mtl_portals4.flowctl.active_sends,
                         &pending->super.super);
        if (pending->length <= ompi_mtl_portals4.eager_limit) {
            ret = ompi_mtl_portals4_short_isend(pending->mode,
                                                pending->start,
                                                pending->length,
                                                pending->contextid,
                                                pending->tag,
                                                pending->my_rank,
                                                pending->endpoint,
                                                pending->ptl_request);
        } else {
            ret = ompi_mtl_portals4_long_isend(pending->start,
                                               pending->length,
                                               pending->contextid,
                                               pending->tag,
                                               pending->my_rank,
                                               pending->endpoint,
                                               pending->ptl_request);
        }
        if (OMPI_SUCCESS != ret) {
            opal_list_prepend(&ompi_mtl_portals4.flowctl.pending_sends,
                              &pending->super.super);
        }
    }
}
#endif


static inline int
ompi_mtl_portals4_start_send(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t* comm,
                             int dest,
                             int tag,
                             struct opal_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode,
                             ompi_mtl_portals4_isend_request_t* ptl_request)
{
    int ret;
    void *start;
    size_t length;
    bool free_after;
    ompi_proc_t *ompi_proc = ompi_comm_peer_lookup(comm, dest);
    mca_mtl_base_endpoint_t *endpoint = 
        (mca_mtl_base_endpoint_t*) ompi_proc->proc_pml;

    ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) return ret;

    ptl_request->opcount = opal_atomic_add_64((int64_t*)&ompi_mtl_portals4.opcount, 1);
    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->event_count = 0;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "Send %lu to %x,%x of length %d\n",
                         ptl_request->opcount,
                         endpoint->ptl_proc.phys.nid, 
                         endpoint->ptl_proc.phys.pid, 
                         (int)length));

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    {
        opal_free_list_item_t *item;
        ompi_mtl_portals4_pending_request_t *pending;

        OPAL_FREE_LIST_GET(&ompi_mtl_portals4.flowctl.pending_fl, item, ret);
        if (NULL == item) return OMPI_ERR_OUT_OF_RESOURCE;

        pending = (ompi_mtl_portals4_pending_request_t*) item;
        pending->mode = mode;
        pending->start = start;
        pending->length = length;
        pending->contextid = comm->c_contextid;
        pending->tag = tag;
        pending->my_rank = comm->c_my_rank;
        pending->endpoint = endpoint;
        pending->ptl_request = ptl_request;
        ptl_request->pending = pending;

        opal_list_append(&ompi_mtl_portals4.flowctl.pending_sends, 
                         &pending->super.super);
        ompi_mtl_portals4_pending_list_progress();
    }
#else
    if (length <= ompi_mtl_portals4.eager_limit) {
        ret = ompi_mtl_portals4_short_isend(mode,
                                            start,
                                            length,
                                            comm->c_contextid,
                                            tag,
                                            comm->c_my_rank,
                                            endpoint,
                                            ptl_request);
    } else {
        ret = ompi_mtl_portals4_long_isend(start,
                                           length,
                                           comm->c_contextid,
                                           tag,
                                           comm->c_my_rank,
                                           endpoint,
                                           ptl_request);
    }
#endif
    
    return ret;
}


int
ompi_mtl_portals4_send(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t* comm,
                       int dest,
                       int tag,
                       struct opal_convertor_t *convertor,
                       mca_pml_base_send_mode_t mode)
{
    int ret = OMPI_SUCCESS;
    ompi_mtl_portals4_send_request_t ptl_request;

    ptl_request.complete = false;
    ptl_request.retval = OMPI_SUCCESS;
    ptl_request.super.super.type = portals4_req_send;
    ptl_request.super.super.event_callback = ompi_mtl_portals4_send_callback;

    ret = ompi_mtl_portals4_start_send(mtl, comm, dest, tag,
                                       convertor, mode, &ptl_request.super);
    if (OMPI_SUCCESS != ret) goto cleanup;

    while (false == ptl_request.complete) {
        opal_atomic_mb();
        ompi_mtl_portals4_progress();
    }
    ret = ptl_request.retval;

 cleanup:
    if (NULL != ptl_request.super.buffer_ptr) {
        free(ptl_request.super.buffer_ptr);
    }

    return ret;
}


int
ompi_mtl_portals4_isend(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t* comm,
                        int dest,
                        int tag,
                        struct opal_convertor_t *convertor,
                        mca_pml_base_send_mode_t mode,
                        bool blocking,
                        mca_mtl_request_t *mtl_request)
{
    int ret = OMPI_SUCCESS;
    ompi_mtl_portals4_isend_request_t *ptl_request =
        (ompi_mtl_portals4_isend_request_t*) mtl_request;

    ptl_request->super.type = portals4_req_isend;
    ptl_request->super.event_callback = ompi_mtl_portals4_isend_callback;

    ret = ompi_mtl_portals4_start_send(mtl, comm, dest, tag,
                                       convertor, mode, ptl_request);

    if (OMPI_SUCCESS != ret && NULL != ptl_request->buffer_ptr) {
        free(ptl_request->buffer_ptr);
    }

    return ret;
}

