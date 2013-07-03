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


static int ompi_mtl_portals4_try_to_use_fixed_md(void *start,
                    int length,
                    ptl_handle_md_t *md_h,
                    int64_t *offset,
                    ompi_mtl_portals4_isend_request_t *ptl_request,
                    bool unlink_me);
static inline int
ompi_mtl_portals4_callback(ptl_event_t *ev, 
                           ompi_mtl_portals4_base_request_t* ptl_base_request,
                           bool *complete)
{
    int retval = OMPI_SUCCESS, ret, val, add = 1;
    ompi_mtl_portals4_isend_request_t* ptl_request = 
        (ompi_mtl_portals4_isend_request_t*) ptl_base_request;

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    if (OPAL_UNLIKELY(ev->ni_fail_type == PTL_NI_PT_DISABLED)) {
        ompi_mtl_portals4_pending_request_t *pending = 
            ptl_request->pending;

        OPAL_OUTPUT_VERBOSE((10, ompi_mtl_base_framework.framework_output,
                             "send %lu hit flow control (%d)",
                             ptl_request->opcount, ev->type));

        /* BWB: FIX ME: this is a hack.. */
        if (pending->fc_notified) {
            return OMPI_SUCCESS;
        }
        pending->fc_notified = 1;

        if (PTL_INVALID_HANDLE != ptl_request->md_h) PtlMDRelease(ptl_request->md_h);

        if (!PtlHandleIsEqual(ptl_request->me_h, PTL_INVALID_HANDLE)) {
            ret = PtlMEUnlink(ptl_request->me_h);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: send callback PtlMEUnlink returned %d",
                                    __FILE__, __LINE__, ret);
            }
        }

        opal_list_append(&ompi_mtl_portals4.flowctl.pending_sends, 
                         &pending->super.super);
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        ompi_mtl_portals4_flowctl_trigger();

        return OMPI_SUCCESS;
    }
#endif

    if (OPAL_UNLIKELY(ev->ni_fail_type != PTL_NI_OK)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: send callback ni_fail_type: %d",
                            __FILE__, __LINE__, ev->ni_fail_type);
        *complete = true;
        return OMPI_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "send %lu got event of type %d",
                         ptl_request->opcount, ev->type));

    if ((PTL_EVENT_ACK == ev->type) &&
        (PTL_PRIORITY_LIST == ev->ptl_list) &&
        (eager == ompi_mtl_portals4.protocol) &&
        (!PtlHandleIsEqual(ptl_request->me_h, PTL_INVALID_HANDLE))) {
        /* long expected messages with the eager protocol won't see a
           get event to complete the message.  Give them an extra
           count to cause the message to complete with just the SEND
           and ACK events and remove the ME. (we wait for the counter
           to reach 3 events, but short messages start the counter at
           1, so they don't need to enter this path) */
        ret = PtlMEUnlink(ptl_request->me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: send callback PtlMEUnlink returned %d",
                                __FILE__, __LINE__, ret);
        }
        add++;
    }
    val = OPAL_THREAD_ADD32((int32_t*)&ptl_request->event_count, add);

    assert(val <= 3);

    if (val == 3) {
        if (NULL != ptl_request->buffer_ptr) {
            free(ptl_request->buffer_ptr);
        }
        if (PTL_INVALID_HANDLE != ptl_request->md_h) {
            ret = PtlMDRelease(ptl_request->md_h);
            if (PTL_OK != ret) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                    "%s:%d: send callback PtlMDRelease returned %d",
                                    __FILE__, __LINE__, ret);
                retval = OMPI_ERROR;
            }
        }

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output, "send %lu completed",
                             ptl_request->opcount));

        *complete = true;
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        OPAL_FREE_LIST_RETURN(&ompi_mtl_portals4.flowctl.pending_fl,
                              &ptl_request->pending->super);

        if (OPAL_UNLIKELY(0 != opal_list_get_size(&ompi_mtl_portals4.flowctl.pending_sends))) {
            ompi_mtl_portals4_pending_list_progress();
        }
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
    ptl_me_t me;
    ptl_hdr_data_t hdr_data;
    int64_t offset;
    ptl_handle_md_t md_h;

    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, 
                               MTL_PORTALS4_SHORT_MSG);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 
                              (MCA_PML_BASE_SEND_SYNCHRONOUS == mode) ? 1 : 0);

    if (MCA_PML_BASE_SEND_SYNCHRONOUS == mode) {
        me.start = NULL;
        me.length = 0;
        me.ct_handle = PTL_CT_NONE;
        me.min_free = 0;
        me.uid = ompi_mtl_portals4.uid;
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
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlMEAppend failed: %d",
                                __FILE__, __LINE__, ret);
            return ompi_mtl_portals4_get_error(ret);
        }

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "Send %lu short sync send with hdr_data 0x%lx (0x%lx)",
                             ptl_request->opcount, hdr_data, match_bits));
    } else {
        ptl_request->event_count = 1;
        ptl_request->me_h = PTL_INVALID_HANDLE;

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "Send %lu short send with hdr_data 0x%lx (0x%lx)",
                             ptl_request->opcount, hdr_data, match_bits));
    }

    ret = ompi_mtl_portals4_try_to_use_fixed_md(start, length, &md_h, &offset, ptl_request, MCA_PML_BASE_SEND_SYNCHRONOUS == mode ? true : false);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = PtlPut(md_h,
                 (ptl_size_t) offset,
                 length,
		 PTL_ACK_REQ,
		 endpoint->ptl_proc,
		 ompi_mtl_portals4.recv_idx,
		 match_bits,
		 0,
                 ptl_request,
		 hdr_data);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
        if (MCA_PML_BASE_SEND_SYNCHRONOUS == mode) {
            PtlMEUnlink(ptl_request->me_h);
        }
        if (PTL_INVALID_HANDLE != ptl_request->md_h) PtlMDRelease(ptl_request->md_h);
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
    ptl_me_t me;
    ptl_hdr_data_t hdr_data;
    ptl_size_t put_length;
    ptl_handle_md_t md_h;
    int64_t offset;

    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, 
                               MTL_PORTALS4_LONG_MSG);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 0);

    me.start = start;
    me.length = length;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
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
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "Send %lu long send with hdr_data 0x%lx (0x%lx)",
                         ptl_request->opcount, hdr_data, match_bits));

    put_length = (rndv == ompi_mtl_portals4.protocol) ? 
        (ptl_size_t) ompi_mtl_portals4.eager_limit : (ptl_size_t) length;

    ompi_mtl_portals4_try_to_use_fixed_md(start, put_length, &md_h, &offset, ptl_request, true);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ret = PtlPut(md_h,
                 (ptl_size_t) offset,
                 put_length,
                 PTL_ACK_REQ,
                 endpoint->ptl_proc,
                 ompi_mtl_portals4.recv_idx,
                 match_bits,
                 0,
                 ptl_request,
                 hdr_data);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
	PtlMEUnlink(ptl_request->me_h);
        if (PTL_INVALID_HANDLE != ptl_request->md_h) PtlMDRelease(ptl_request->md_h);
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

    while ((!ompi_mtl_portals4.flowctl.flowctl_active) &&
           (0 != opal_list_get_size(&ompi_mtl_portals4.flowctl.pending_sends))) {
        val = OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, -1);
        if (val < 0) {
            OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
            return;
        }

        item = opal_list_remove_first(&ompi_mtl_portals4.flowctl.pending_sends);
        if (OPAL_UNLIKELY(NULL == item)) {
            OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
            return;
        }

        pending = (ompi_mtl_portals4_pending_request_t*) item;
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
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            opal_list_prepend(&ompi_mtl_portals4.flowctl.pending_sends,
                              &pending->super.super);
            OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        }
    }
}
#endif


static inline int
ompi_mtl_portals4_send_start(struct mca_mtl_base_module_t* mtl,
                             struct ompi_communicator_t* comm,
                             int dest,
                             int tag,
                             struct opal_convertor_t *convertor,
                             mca_pml_base_send_mode_t mode,
                             ompi_mtl_portals4_isend_request_t* ptl_request)
{
    int ret= OMPI_SUCCESS;
    void *start;
    size_t length;
    bool free_after;
    ompi_proc_t *ompi_proc = ompi_comm_peer_lookup(comm, dest);
    mca_mtl_base_endpoint_t *endpoint = 
        (mca_mtl_base_endpoint_t*) ompi_proc->proc_pml;
#if OMPI_MTL_PORTALS4_FLOW_CONTROL
        opal_free_list_item_t *item;
        ompi_mtl_portals4_pending_request_t *pending;
#endif

    ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) return ret;

    ptl_request->opcount = OPAL_THREAD_ADD64((int64_t*)&ompi_mtl_portals4.opcount, 1);
    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->event_count = 0;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "Send %lu to %x,%x of length %d\n",
                         ptl_request->opcount,
                         endpoint->ptl_proc.phys.nid, 
                         endpoint->ptl_proc.phys.pid, 
                         (int)length));

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    OPAL_FREE_LIST_GET(&ompi_mtl_portals4.flowctl.pending_fl, item, ret);
    if (NULL == item) return OMPI_ERR_OUT_OF_RESOURCE;

    pending = (ompi_mtl_portals4_pending_request_t*) item;
    ptl_request->pending = pending;
    pending->mode = mode;
    pending->start = start;
    pending->length = length;
    pending->contextid = comm->c_contextid;
    pending->tag = tag;
    pending->my_rank = comm->c_my_rank;
    pending->fc_notified = 0;
    pending->endpoint = endpoint;
    pending->ptl_request = ptl_request;

    if (OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, -1) < 0) {
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        opal_list_append(&ompi_mtl_portals4.flowctl.pending_sends,
                         &pending->super.super);
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(0 != opal_list_get_size(&ompi_mtl_portals4.flowctl.pending_sends))) {
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        opal_list_append(&ompi_mtl_portals4.flowctl.pending_sends,
                         &pending->super.super);
        ompi_mtl_portals4_pending_list_progress();
        return OMPI_SUCCESS;
    }

    if (OPAL_UNLIKELY(ompi_mtl_portals4.flowctl.flowctl_active)) {
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        opal_list_append(&ompi_mtl_portals4.flowctl.pending_sends,
                         &pending->super.super);
        return OMPI_SUCCESS;
    }
#endif
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

    ret = ompi_mtl_portals4_send_start(mtl, comm, dest, tag,
                                       convertor, mode, &ptl_request.super);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        if (NULL != ptl_request.super.buffer_ptr) {
            free(ptl_request.super.buffer_ptr);
        }
        return ret;
    }

    while (false == ptl_request.complete) {
        ompi_mtl_portals4_progress();
    }
    ret = ptl_request.retval;

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

    ret = ompi_mtl_portals4_send_start(mtl, comm, dest, tag,
                                       convertor, mode, ptl_request);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret && NULL != ptl_request->buffer_ptr)) {
        free(ptl_request->buffer_ptr);
    }

    return ret;
}

static int
ompi_mtl_portals4_try_to_use_fixed_md(void *start,
                    int length,
                    ptl_handle_md_t *md_h,
                    int64_t *offset,
                    ompi_mtl_portals4_isend_request_t *ptl_request,
                    bool unlink_me)
{
    int ret;
    ptl_md_t md;
    int64_t addr;

    addr = ((int64_t)start & ~EXTENDED_ADDR);

    /* If fixed_md_distance is defined for MD and if the memory buffer is strictly contained in one of them, then use one */
    if ((0 != ompi_mtl_portals4.fixed_md_distance) &&
        (((addr % ompi_mtl_portals4.fixed_md_distance) + length) < ompi_mtl_portals4.fixed_md_distance)) {
		if (0 == length) OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output, "          Memory  : [ %16lx -   (len = 0)      ] is in fixed MD number: %d\n\n",
                           start, addr / ompi_mtl_portals4.fixed_md_distance));
                else OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output, "          Memory  : [ %16lx - %16lx ] is in fixed MD number: %d\n\n",
                           start, (long int)start + length - 1, addr / ompi_mtl_portals4.fixed_md_distance));
                /* Use the fixed MD */
                *md_h = ompi_mtl_portals4.fixed_md_h[addr / ompi_mtl_portals4.fixed_md_distance];
                *offset = (addr % ompi_mtl_portals4.fixed_md_distance);
                ptl_request->md_h = PTL_INVALID_HANDLE;
    }
    else {
        if (0 == ompi_mtl_portals4.fixed_md_distance)
             OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output, "\nWARNING: Memory cannot be connected to a fixed MD\n"));
        else OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output, "\nWARNING: Memory outside the scope of the fixed MD %x\n\n", addr / ompi_mtl_portals4.fixed_md_distance));

        /* Bind the MD (and unbind it where necessary) */
        md.start     = start;
        md.length    = length;
        md.options   = 0;
        md.eq_handle = ompi_mtl_portals4.send_eq_h;
        md.ct_handle = PTL_CT_NONE;

        ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                        &md,
                        &ptl_request->md_h); 
        if (OPAL_UNLIKELY(PTL_OK != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlMDBind failed: %d\n",
                                __FILE__, __LINE__, ret);
            if (unlink_me) {
                PtlMEUnlink(ptl_request->me_h);
            }
            return ompi_mtl_portals4_get_error(ret);
        }
        *md_h = ptl_request->md_h;
        *offset = 0;
    }
    return OMPI_SUCCESS;
}
