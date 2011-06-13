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


/* called when no ack is necessary */
static int
ompi_mtl_portals4_short_callback(ptl_event_t *ev, ompi_mtl_portals4_request_t *ptl_request)
{
    int ret;

    assert(ev->type == PTL_EVENT_SEND);
    assert(NULL != ptl_request->super.ompi_req);

    if (ev->ni_fail_type != PTL_NI_OK) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: short send callback ni_fail_type: %d",
                            __FILE__, __LINE__, ev->ni_fail_type);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
    }
    if (NULL != ptl_request->buffer_ptr) {
        free(ptl_request->buffer_ptr);
    }
    ret = PtlMDRelease(ptl_request->md_h);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: short send callback PtlMDRelease returned %d",
                            __FILE__, __LINE__, ret);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
    }
    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output, "send completed"));
    ptl_request->super.completion_callback(&ptl_request->super);

    return OMPI_SUCCESS;
}


/* called when send should wait for an ack or get */
static int
ompi_mtl_portals4_long_callback(ptl_event_t *ev, struct ompi_mtl_portals4_request_t* ptl_request)
{
    int ret;

    assert(ev->type == PTL_EVENT_SEND || ev->type == PTL_EVENT_ACK || ev->type == PTL_EVENT_GET);
    assert(NULL != ptl_request->super.ompi_req);

    if (ev->ni_fail_type != PTL_NI_OK) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: long send callback ni_fail_type: %d",
                            __FILE__, __LINE__, ev->ni_fail_type);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
    }

    /* we only receive an ack if the message was received into an
       expected message.  Otherwise, we don't get an ack, but mark
       completion when the message was pulled (long message). */
    if ( ++(ptl_request->event_count) == 2 ) {
        if (NULL != ptl_request->buffer_ptr) {
            free(ptl_request->buffer_ptr);
        }
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: long send callback PtlMDRelease returned %d",
                                __FILE__, __LINE__, ret);
            ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output, "send completed"));
        ptl_request->super.completion_callback(&ptl_request->super);
    }

    /* received an ack - unlink the me */
    if (ev->type == PTL_EVENT_ACK) {
        PtlMEUnlink(ptl_request->me_h);
    }
    
    return OMPI_SUCCESS;
}


/* called when sync send should wait for an ack or put */
static int
ompi_mtl_portals4_sync_callback(ptl_event_t *ev, struct ompi_mtl_portals4_request_t* ptl_request)
{
    int ret;

    assert(ev->type == PTL_EVENT_SEND || ev->type == PTL_EVENT_ACK || ev->type == PTL_EVENT_PUT);
    assert(NULL != ptl_request->super.ompi_req);

    if (ev->ni_fail_type != PTL_NI_OK) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: sync send callback ni_fail_type: %d",
                            __FILE__, __LINE__, ev->ni_fail_type);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
    }

    /* we only receive an ack if the message was received into an
       expected message.  Otherwise, we don't get an ack, but mark
       completion when a zero-length put arrrives. */
    if ( ++(ptl_request->event_count) == 2 ) {
        if (NULL != ptl_request->buffer_ptr) {
            free(ptl_request->buffer_ptr);
        }
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: sync send callback PtlMDRelease returned %d",
                                __FILE__, __LINE__, ret);
            ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output, "send completed"));
        ptl_request->super.completion_callback(&ptl_request->super);
    }
    
    /* received an ack - unlink the me */
    if (ev->type == PTL_EVENT_ACK) {
        ret = PtlMEUnlink(ptl_request->me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: sync send callback PtlMEUnlink returned %d",
                                __FILE__, __LINE__, ret);
            ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        }
    }
    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_short_isend(mca_pml_base_send_mode_t mode, void *start, int length,
                              int contextid, int localrank, int tag, mca_mtl_base_endpoint_t *endpoint,
                              ompi_mtl_portals4_request_t *ptl_request )
{
    int ret;
    ptl_match_bits_t mode_bits;
    ptl_match_bits_t match_bits;
    ptl_md_t md;

    ptl_request->event_callback = ompi_mtl_portals4_short_callback;

    mode_bits = (MCA_PML_BASE_SEND_READY != mode) ? PTL_SHORT_MSG : PTL_READY_MSG;
    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, mode_bits); 
    
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

    ret = PtlPut(ptl_request->md_h,
                 0,
                 length,
		 PTL_NO_ACK_REQ,
		 endpoint->ptl_proc,
		 ompi_mtl_portals4.send_idx,
		 match_bits,
		 0,
                 ptl_request,
		 0);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
	ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMDRelease failed: %d",
                                __FILE__, __LINE__, ret);
        }
        return ompi_mtl_portals4_get_error(ret);
    }
    
    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_long_isend(void *start, int length, int contextid, int localrank, int tag,
                             mca_mtl_base_endpoint_t *endpoint,
                             ompi_mtl_portals4_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_me_t me;

    ptl_request->event_callback = ompi_mtl_portals4_long_callback;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, PTL_LONG_MSG);

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
    me.ac_id.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_GET | PTL_ME_USE_ONCE;
    me.match_id = endpoint->ptl_proc;
    if (ompi_mtl_portals4.protocol == rndv) {
        me.match_bits = ((uint64_t) endpoint->send_count << 32) | length;
    } else {
        me.match_bits = endpoint->send_count;
    }
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
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMDRelease failed: %d",
                                __FILE__, __LINE__, ret);
        }
        return ompi_mtl_portals4_get_error(ret);
    }

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
                     me.match_bits);
    } else if (ompi_mtl_portals4.protocol == triggered) {
        ret = PtlPut(ptl_request->md_h,
                     0,
                     ompi_mtl_portals4.eager_limit + 1,
                     PTL_NO_ACK_REQ,
                     endpoint->ptl_proc,
                     ompi_mtl_portals4.send_idx,
                     match_bits,
                     0,
                     ptl_request,
                     me.match_bits);
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
                     me.match_bits);
    }
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
	ret = PtlMEUnlink(ptl_request->me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMEUnlink failed: %d",
                                __FILE__, __LINE__, ret);
        }
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMDRelease failed: %d",
                                __FILE__, __LINE__, ret);
        }
        return ompi_mtl_portals4_get_error(ret);
    }

    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals4_sync_isend(void *start, int length, int contextid, int localrank, int tag,
                             mca_mtl_base_endpoint_t *endpoint, ompi_mtl_portals4_request_t *ptl_request)
{
    int ret;
    ptl_match_bits_t match_bits;
    ptl_md_t md;
    ptl_me_t me;

    ptl_request->event_callback = ompi_mtl_portals4_sync_callback;

    PTL_SET_SEND_BITS(match_bits, contextid, localrank, tag, PTL_SHORT_MSG);

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
    me.ac_id.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_USE_ONCE;
    me.match_id = endpoint->ptl_proc;
    me.match_bits = endpoint->send_count;
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
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMDRelease failed: %d",
                                __FILE__, __LINE__, ret);
        }
        return ompi_mtl_portals4_get_error(ret);
    }

    ret = PtlPut(ptl_request->md_h,
                 0,
                 length,
                 PTL_ACK_REQ,
                 endpoint->ptl_proc,
                 ompi_mtl_portals4.send_idx,
                 match_bits,
                 0,
                 ptl_request,
                 (ptl_hdr_data_t)(uintptr_t)ptl_request);
    if (PTL_OK != ret) {
        opal_output_verbose(1, ompi_mtl_base_output,
                            "%s:%d: PtlPut failed: %d",
                            __FILE__, __LINE__, ret);
	ret = PtlMEUnlink(ptl_request->me_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMEUnlink failed: %d",
                                __FILE__, __LINE__, ret);
        }
        ret = PtlMDRelease(ptl_request->md_h);
        if (PTL_OK != ret) {
            opal_output_verbose(1, ompi_mtl_base_output,
                                "%s:%d: PtlMDRelease failed: %d",
                                __FILE__, __LINE__, ret);
        }
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
    ompi_mtl_portals4_request_t *ptl_request = (ompi_mtl_portals4_request_t*)mtl_request;
    void *start;
    size_t length;
    bool free_after;

    ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) return ret;

    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->event_count = 0;
    ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;

    endpoint->send_count++;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "Send %d to %x,%x of length %d\n",
                         endpoint->send_count,
                         endpoint->ptl_proc.phys.nid, endpoint->ptl_proc.phys.pid, 
                         (int)length));

    switch (mode) {
    case MCA_PML_BASE_SEND_STANDARD:
    case MCA_PML_BASE_SEND_READY:
    case MCA_PML_BASE_SEND_BUFFERED:
        if ((length <= ompi_mtl_portals4.eager_limit) ||
            (MCA_PML_BASE_SEND_READY == mode)) {
            ret = ompi_mtl_portals4_short_isend(mode,
                                                start,
                                                length,
                                                comm->c_contextid,
                                                comm->c_my_rank,
                                                tag,
                                                endpoint,
                                                ptl_request);
            break;
        }

        /* long standard send case falls through */					
    case MCA_PML_BASE_SEND_SYNCHRONOUS:
        if (length <= ompi_mtl_portals4.eager_limit) {
            ret = ompi_mtl_portals4_sync_isend(start,
                                               length,
                                               comm->c_contextid,
                                               comm->c_my_rank,
                                               tag,
                                               endpoint,
                                               ptl_request);
        } else {
	    /* if we got this far, we're either a standard or synchronous long send */
            ret = ompi_mtl_portals4_long_isend(start,
                                               length,
                                               comm->c_contextid,
                                               comm->c_my_rank,
                                               tag,
                                               endpoint,
                                               ptl_request);
        }
        break;

    default:
        opal_output(ompi_mtl_base_output, "Unexpected msg type %dn", mode);
        ret = OMPI_ERR_NOT_SUPPORTED;
    }

    if (OMPI_SUCCESS != ret && NULL != ptl_request->buffer_ptr) {
        free(ptl_request->buffer_ptr);
    }

    return ret;
}
