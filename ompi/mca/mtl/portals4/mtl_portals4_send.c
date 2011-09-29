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


static int
ompi_mtl_portals4_send_callback(ptl_event_t *ev, struct ompi_mtl_portals4_base_request_t* ptl_base_request)
{
    int ret;
    ompi_mtl_portals4_send_request_t* ptl_request = 
        (ompi_mtl_portals4_send_request_t*) ptl_base_request;

    assert(NULL != ptl_request->super.super.ompi_req);

    if (ev->ni_fail_type != PTL_NI_OK) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: send callback ni_fail_type: %d",
                    __FILE__, __LINE__, ev->ni_fail_type);
        ptl_request->super.super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        ptl_request->super.super.completion_callback(&ptl_request->super.super);
        abort();
        return OMPI_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "Send %d got event of type %d",
                         ptl_request->opcount, ev->type));

    /* we only receive an ack if the message was received into an
       expected message.  Otherwise, we don't get an ack, but mark
       completion when the message was pulled (long message).  A short
       message starts at count 1, so the send event completes it. */
    if ( ++(ptl_request->event_count) == 2 ) {
        if (NULL != ptl_request->buffer_ptr) {
            free(ptl_request->buffer_ptr);
        }
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: send callback PtlMDRelease returned %d",
                                __FILE__, __LINE__, ret);
            ptl_request->super.super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output, "Send %d completed",
                             ptl_request->opcount));
        ptl_request->super.super.completion_callback(&ptl_request->super.super);
    }

    /* received an ack - unlink the me */
    if (ev->type == PTL_EVENT_ACK) {
        ret = PtlMEUnlink(ptl_request->me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: send callback PtlMDUnlink returned %d",
                                __FILE__, __LINE__, ret);
        }
    }
    
    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_short_isend(mca_pml_base_send_mode_t mode, 
                              void *start, int length, int contextid, int tag,
                              int localrank,
                              mca_mtl_base_endpoint_t *endpoint,
                              ompi_mtl_portals4_send_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t mode_bits, match_bits;
    ptl_hdr_data_t hdr_data;
    ptl_md_t md;

    ptl_request->super.event_callback = ompi_mtl_portals4_send_callback;
    ptl_request->event_count = 1;

    mode_bits = (MCA_PML_BASE_SEND_READY != mode) ? MTL_PORTALS4_SHORT_MSG : MTL_PORTALS4_READY_MSG;
    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, mode_bits); 

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 0);
    
    md.start = start;
    md.length = length;
    md.options = 0;
    md.eq_handle = ompi_mtl_portals4.eq_h;
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

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "Send %d short send with hdr_data 0x%lx (0x%lx)",
                         ptl_request->opcount, hdr_data, match_bits));

    ret = PtlPut(ptl_request->md_h,
                 0,
                 length,
		 PTL_NO_ACK_REQ,
		 endpoint->ptl_proc,
		 ompi_mtl_portals4.send_idx,
		 match_bits,
		 0,
                 ptl_request,
		 hdr_data);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
	PtlMDRelease(ptl_request->md_h);
        return ompi_mtl_portals4_get_error(ret);
    }
    
    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_sync_isend(void *start, int length, int contextid, int tag,
                             int localrank,
                             mca_mtl_base_endpoint_t *endpoint, 
                             ompi_mtl_portals4_send_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_me_t me;
    ptl_hdr_data_t hdr_data;

    ptl_request->super.event_callback = ompi_mtl_portals4_send_callback;

    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, 
                               MTL_PORTALS4_SHORT_MSG);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 1);

    md.start = start;
    md.length = length;
    md.options = 0;
    md.eq_handle = ompi_mtl_portals4.eq_h;
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

    me.start = NULL;
    me.length = 0;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_USE_ONCE | PTL_ME_EVENT_UNLINK_DISABLE;
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
                         "Send %d short sync send with hdr_data 0x%lx (0x%lx)",
                         ptl_request->opcount, hdr_data, match_bits));

    ret = PtlPut(ptl_request->md_h,
                 0,
                 length,
                 PTL_ACK_REQ,
                 endpoint->ptl_proc,
                 ompi_mtl_portals4.send_idx,
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


static int
ompi_mtl_portals4_long_isend(void *start, int length, int contextid, int tag,
                             int localrank, int destrank, 
                             mca_mtl_base_endpoint_t *endpoint,
                             ompi_mtl_portals4_send_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_me_t me;
    ptl_hdr_data_t hdr_data;

    ptl_request->super.event_callback = ompi_mtl_portals4_send_callback;

    MTL_PORTALS4_SET_SEND_BITS(match_bits, contextid, localrank, tag, MTL_PORTALS4_LONG_MSG);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, ptl_request->opcount, length, 0);

    md.start = start;
    md.length = length;
    md.options = 0;
    md.eq_handle = ompi_mtl_portals4.eq_h;
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
    me.options = PTL_ME_OP_GET | PTL_ME_USE_ONCE | PTL_ME_EVENT_UNLINK_DISABLE;
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
                         "Send %d long send with hdr_data 0x%lx (0x%lx)",
                         ptl_request->opcount, hdr_data, match_bits));

    if (ompi_mtl_portals4.protocol == rndv) {
        ret = PtlPut(ptl_request->md_h,
                     0,
                     ompi_mtl_portals4.eager_limit,
                     PTL_NO_ACK_REQ,
                     endpoint->ptl_proc,
                     ompi_mtl_portals4.send_idx,
                     match_bits,
                     0,
                     ptl_request,
                     hdr_data);
    } else {
        ret = PtlPut(ptl_request->md_h,
                     0,
                     length,
                     PTL_ACK_REQ,
                     endpoint->ptl_proc,
                     ompi_mtl_portals4.send_idx,
                     match_bits,
                     0,
                     ptl_request,
                     hdr_data);
    }
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
    ompi_proc_t *ompi_proc = ompi_comm_peer_lookup( comm, dest );
    mca_mtl_base_endpoint_t *endpoint = (mca_mtl_base_endpoint_t*)ompi_proc->proc_pml;
    ompi_mtl_portals4_send_request_t *ptl_request = (ompi_mtl_portals4_send_request_t*)mtl_request;
    void *start;
    size_t length;
    bool free_after;

    ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) return ret;

    ptl_request->opcount = ++ompi_mtl_portals4.opcount;
    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->event_count = 0;
    ptl_request->super.super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "Send %d to %x,%x of length %d\n",
                         ptl_request->opcount,
                         endpoint->ptl_proc.phys.nid, endpoint->ptl_proc.phys.pid, 
                         (int)length));

    switch (mode) {
    case MCA_PML_BASE_SEND_READY:
        ret = ompi_mtl_portals4_short_isend(mode,
                                            start,
                                            length,
                                            comm->c_contextid,
                                            tag,
                                            comm->c_my_rank,
                                            endpoint,
                                            ptl_request);
        break;

    case MCA_PML_BASE_SEND_STANDARD:
    case MCA_PML_BASE_SEND_BUFFERED:
    case MCA_PML_BASE_SEND_COMPLETE:
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
                                               dest,
                                               endpoint,
                                               ptl_request);
        }
        break;

    case MCA_PML_BASE_SEND_SYNCHRONOUS:
        if (length <= ompi_mtl_portals4.eager_limit) {
            ret = ompi_mtl_portals4_sync_isend(start,
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
                                               dest,
                                               endpoint,
                                               ptl_request);
        }
        break;

    case MCA_PML_BASE_SEND_SIZE:
        abort();
        break;
    }

    if (OMPI_SUCCESS != ret && NULL != ptl_request->buffer_ptr) {
        free(ptl_request->buffer_ptr);
    }

    return ret;
}
