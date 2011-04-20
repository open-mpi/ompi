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

#include "opal/class/opal_list.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_recv_short.h"

/* called when a receive should be progressed */
int
ompi_mtl_portals4_recv_progress(ptl_event_t *ev,
                                ompi_mtl_portals4_request_t* ptl_request)
{
    int ret;

    switch (ev->type) {
    case PTL_EVENT_PUT:
        if (ev->ni_fail_type == PTL_NI_OK) {
            /* make sure the data is in the right place */
            ret = ompi_mtl_datatype_unpack(ptl_request->convertor,
                                           ev->start,
                                           ev->mlength);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_mtl_base_output,
                            "%s:%d: ompi_mtl_datatype_unpack failed: %d",
                            __FILE__, __LINE__, ret);
                ptl_request->super.ompi_req->req_status.MPI_ERROR = ret;
            }
            /* set the status */
            ptl_request->super.ompi_req->req_status.MPI_SOURCE =
                PTL_GET_SOURCE(ev->match_bits);
            ptl_request->super.ompi_req->req_status.MPI_TAG = 
                PTL_GET_TAG(ev->match_bits);
            if (ev->rlength > ev->mlength) {
                ptl_request->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_TRUNCATE;
            }
            ptl_request->super.ompi_req->req_status._ucount =
                ev->mlength;
        } else {
            opal_output(ompi_mtl_base_output,
                        "%s:%d: recv(PTL_EVENT_PUT) ni_fail_type: %d",
                        __FILE__, __LINE__, ret);
            ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        }
        ptl_request->super.completion_callback(&ptl_request->super);
        break;

    case PTL_EVENT_REPLY:
        if (ev->ni_fail_type == PTL_NI_OK) {
            /* make sure the data is in the right place */
            ret = ompi_mtl_datatype_unpack(ptl_request->convertor, 
                                           ev->start,
                                           ev->mlength);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_mtl_base_output,
                            "%s:%d: ompi_mtl_datatype_unpack failed: %d",
                            __FILE__, __LINE__, ret);
                ptl_request->super.ompi_req->req_status.MPI_ERROR = ret;
            }
            /* set the status - most of this filled in right after issuing
               the PtlGet */
            ptl_request->super.ompi_req->req_status._ucount = 
                ev->mlength;
        } else {
            opal_output(ompi_mtl_base_output,
                        "%s:%d: recv(PTL_EVENT_REPLY) ni_fail_type: %d",
                        __FILE__, __LINE__, ret);
            ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
        }
        PtlMDRelease(ptl_request->md_h);
        ptl_request->super.completion_callback(&ptl_request->super);
        break;

    case PTL_EVENT_PUT_OVERFLOW:
        /* overflow case.  Short messages have the buffer stashed
           somewhere.  Long messages left in buffer at the source */
        if (PTL_IS_SHORT_MSG(ev->match_bits)) {
            if (ev->ni_fail_type == PTL_NI_OK) {
                ptl_request->super.ompi_req->req_status.MPI_SOURCE =
                    PTL_GET_SOURCE(ev->match_bits);
                ptl_request->super.ompi_req->req_status.MPI_TAG = 
                    PTL_GET_TAG(ev->match_bits);
                if (ev->rlength > ptl_request->delivery_len) {
                    ptl_request->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_TRUNCATE;
                }
                ptl_request->super.ompi_req->req_status._ucount = 
                    ev->mlength;
                if (ev->mlength > 0) {
                    struct iovec iov;
                    uint32_t iov_count = 1;
                    size_t max_data;
                    iov.iov_base = (char*) ev->start;
                    iov.iov_len = ev->mlength;
                    max_data = iov.iov_len;

                    ret = opal_convertor_unpack(ptl_request->convertor, 
                                                &iov, &iov_count,
                                                &max_data );
                    if (ret < 0) {
                        opal_output(ompi_mtl_base_output,
                                    "%s:%d: opal_convertor_unpack failed: %d",
                                    __FILE__, __LINE__, ret);
                        if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
                        ptl_request->super.ompi_req->req_status.MPI_ERROR = ret;
                        ptl_request->super.completion_callback(&ptl_request->super);
                        return OMPI_SUCCESS;
                    }
                }
                if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);

                /* if it's a sync, send the ack */
                if (PTL_IS_SYNC_MSG(ev)) {
                    ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                                 0,
                                 0,
                                 PTL_NO_ACK_REQ,
                                 ev->initiator,
                                 PTL_READ_TABLE_ID,
                                 ev->hdr_data,
                                 0,
                                 NULL,
                                 0);
                    if (PTL_OK != ret) {
                        opal_output(ompi_mtl_base_output,
                                    "%s:%d: PtlPut failed: %d",
                                    __FILE__, __LINE__, ret);
                        ptl_request->super.ompi_req->req_status.MPI_ERROR = 
                            ompi_mtl_portals4_get_error(ret);;
                        ptl_request->super.completion_callback(&ptl_request->super);
                        return OMPI_SUCCESS;
                    }
                }
            } else {
                opal_output(ompi_mtl_base_output,
                            "%s:%d: recv(PTL_EVENT_PUT_OVERFLOW) ni_fail_type: %d",
                            __FILE__, __LINE__, ret);
                ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
            }
            ptl_request->super.completion_callback(&ptl_request->super);

        } else {
            ptl_md_t md;

            if (ev->ni_fail_type == PTL_NI_OK) {
                /* set the status */
                ptl_request->super.ompi_req->req_status.MPI_SOURCE =
                    PTL_GET_SOURCE(ev->match_bits);
                ptl_request->super.ompi_req->req_status.MPI_TAG = 
                    PTL_GET_TAG(ev->match_bits);
                if (ev->rlength > ptl_request->delivery_len) {
                    ptl_request->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_TRUNCATE;
                }

                md.start = ptl_request->delivery_ptr;
                md.length = (ev->rlength > ptl_request->delivery_len) ?
                    ptl_request->delivery_len : ev->rlength;
                md.options = 0;
                md.eq_handle = ompi_mtl_portals4.eq_h;
                md.ct_handle = PTL_CT_NONE;

                ret = PtlMDBind(ompi_mtl_portals4.ni_h,
                                &md,
                                &ptl_request->md_h);
                if (PTL_OK != ret) {
                    if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
                    opal_output(ompi_mtl_base_output,
                                "%s:%d: PtlMDBind failed: %d",
                                __FILE__, __LINE__, ret);
                    ptl_request->super.ompi_req->req_status.MPI_ERROR = 
                        ompi_mtl_portals4_get_error(ret);;
                    ptl_request->super.completion_callback(&ptl_request->super);
                    return OMPI_SUCCESS;
                }

                ret = PtlGet(ompi_mtl_portals4.ni_h,
                             0,
                             md.length,
                             ev->initiator,
                             PTL_READ_TABLE_ID,
                             ev->hdr_data,
                             0,
                             ptl_request);
                if (PTL_OK != ret) {
                    PtlMDRelease(ptl_request->md_h);
                    if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
                    opal_output(ompi_mtl_base_output,
                                "%s:%d: PtlGet failed: %d",
                                __FILE__, __LINE__, ret);
                    ptl_request->super.ompi_req->req_status.MPI_ERROR = 
                        ompi_mtl_portals4_get_error(ret);;
                    ptl_request->super.completion_callback(&ptl_request->super);
                    return OMPI_SUCCESS;
                }
            } else {
                opal_output(ompi_mtl_base_output,
                            "%s:%d: recv(PTL_EVENT_PUT_OVERFLOW) ni_fail_type: %d",
                            __FILE__, __LINE__, ret);
                ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_ERROR;
                ptl_request->super.completion_callback(&ptl_request->super);
            }
        }
        break;

    default:
        opal_output(ompi_mtl_base_output,
                    "Unhandled receive callback with event type %d",
                    ev->type);
        return OMPI_ERROR;
    }
    
    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_irecv(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm,
                        int src,
                        int tag,
                        struct opal_convertor_t *convertor,
                        mca_mtl_request_t *mtl_request)
{
    ptl_match_bits_t match_bits, ignore_bits;
    int ret = OMPI_SUCCESS;
    ptl_process_t remote_proc;
    mca_mtl_base_endpoint_t *endpoint = NULL;
    ompi_mtl_portals4_request_t *ptl_request = 
        (ompi_mtl_portals4_request_t*) mtl_request;
    void *start;
    size_t length;
    bool free_after;
    ptl_me_t me;

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

    ret = ompi_mtl_datatype_recv_buf(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) {
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlMEAppend failed: %d",
                    __FILE__, __LINE__, ret);
        return ret;
    }

    ptl_request->event_callback = ompi_mtl_portals4_recv_progress;
    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->convertor = convertor;
    ptl_request->delivery_ptr = start;
    ptl_request->delivery_len = length;
    ptl_request->super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;

    me.start = start;
    me.length = length;
    me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.ac_id.uid = PTL_UID_ANY;
    me.options = PTL_ME_OP_PUT | PTL_ME_USE_ONCE | PTL_ME_EVENT_UNLINK_DISABLE;
    me.match_id = remote_proc;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      PTL_SEND_TABLE_ID,
                      &me,
                      PTL_PRIORITY_LIST,
                      ptl_request,
                      &ptl_request->me_h);
    if (PTL_OK != ret) {
        if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
        opal_output(ompi_mtl_base_output,
                    "%s:%d: PtlMEAppend failed: %d",
                    __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    return OMPI_SUCCESS; 
}
