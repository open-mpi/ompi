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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "ompi/request/request.h"
#include "ompi/datatype/datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/pml/base/pml_base_recvreq.h"

#include "mtl_portals.h"
#include "mtl_portals_endpoint.h"
#include "mtl_portals_request.h"

/* called when a receive should be progressed */
static int
ompi_mtl_portals_recv_progress(ptl_event_t *ev,
                               struct ompi_mtl_portals_request_t* ptl_request)
{
    switch (ev->type) {
    case PTL_EVENT_PUT_END:
    case PTL_EVENT_REPLY_END:
        {
            mca_pml_base_recv_request_t *recvreq = 
                (mca_pml_base_recv_request_t*) ptl_request->super.ompi_req;

            /* make sure the data is in the right place */
            ompi_mtl_datatype_unpack(&recvreq->req_convertor,
                                     ev->md.start, ev->md.length);

            /* set the status */
            recvreq->req_base.req_ompi.req_status.MPI_SOURCE =
                PTL_GET_SOURCE(ev->match_bits);
            recvreq->req_base.req_ompi.req_status.MPI_TAG = 
                PTL_GET_TAG(ev->match_bits);
            /* BWB - fix me - this is right for put but not for
               unexpected, I think */ 
            recvreq->req_base.req_ompi.req_status.MPI_ERROR = 
                (ev->rlength > ev->mlength) ?
                MPI_ERR_TRUNCATE : MPI_SUCCESS;
            recvreq->req_base.req_ompi.req_status._count = 
                ev->mlength;

            ptl_request->super.completion_callback(&ptl_request->super);
        }
        break;

    default:
        break;
    }
    
    return OMPI_SUCCESS;
}


static int
ompi_mtl_portals_get_data(ompi_mtl_portals_event_t *recv_event, 
                          struct ompi_convertor_t *convertor,
                          ompi_mtl_portals_request_t  *ptl_request)
{
    int ret;
    ptl_md_t md;
    ptl_handle_md_t md_h;

    PTL_IS_SHORT_MSG(recv_event->ev.match_bits, ret);
    if (ret) {
        /* the buffer is sitting in the short message queue */
        abort();
    } else {
        size_t buflen;
        ret = ompi_mtl_datatype_recv_buf(convertor, &md.start, &buflen,
                                         &ptl_request->free_after);
        if (OMPI_SUCCESS != ret) abort();
        md.length = buflen;
        md.threshold = 2; /* send and get */
        md.options = PTL_MD_EVENT_START_DISABLE;
        md.user_ptr = ptl_request;
        md.eq_handle = ompi_mtl_portals.ptl_eq_h;

        ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h, md,
                        PTL_UNLINK, &md_h);
        if (PTL_OK != ret) abort();

        ret = PtlGet(md_h, 
                     recv_event->ev.initiator, 
                     OMPI_MTL_PORTALS_READ_TABLE_ID,
                     0, 
                     recv_event->ev.hdr_data,
                     0);
        if (PTL_OK != ret) abort();

        ptl_request->event_callback = ompi_mtl_portals_recv_progress;
    }

    return OMPI_SUCCESS;
}

int
ompi_mtl_portals_irecv(struct mca_mtl_base_module_t* mtl,
                       struct ompi_communicator_t *comm,
                       int src,
                       int tag,
                       struct ompi_convertor_t *convertor,
                       mca_mtl_request_t *mtl_request)
{
    ptl_match_bits_t match_bits, ignore_bits;
    ptl_md_t md;
    ptl_handle_md_t md_h;
    ptl_handle_me_t me_h;
    int ret;
    ptl_process_id_t remote_proc;
    mca_mtl_base_endpoint_t *endpoint = NULL;
    opal_list_item_t *list_item;
    ompi_mtl_portals_request_t *ptl_request = 
        (ompi_mtl_portals_request_t*) mtl_request;
    size_t buflen;

    if  (MPI_ANY_SOURCE == src) {
        remote_proc.nid = PTL_NID_ANY;
        remote_proc.pid = PTL_PID_ANY;
    } else {
        endpoint = (mca_mtl_base_endpoint_t*) comm->c_pml_procs[src]->proc_ompi->proc_pml;
        remote_proc = endpoint->ptl_proc;
    }

    PTL_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid,
                      src, tag);
#if OMPI_MTL_PORTALS_DEBUG
    printf("recv bits: 0x%016llx 0x%016llx\n", match_bits, ignore_bits);
#endif

    /* first, check the queue of processed unexpected messages */
    list_item = opal_list_get_first(&ompi_mtl_portals.unexpected_messages);
    while (list_item != opal_list_get_end(&ompi_mtl_portals.unexpected_messages)) {
        opal_list_item_t *next_item = opal_list_get_next(list_item);
        ompi_mtl_portals_event_t *recv_event = 
            (ompi_mtl_portals_event_t*) list_item;

        if ((recv_event->ev.match_bits & ~ignore_bits) == 
            (match_bits & ~ignore_bits)) {
            /* we have a match... */
            opal_list_remove_item(&(ompi_mtl_portals.unexpected_messages),
                                  list_item);
            ompi_mtl_portals_get_data(recv_event, convertor, ptl_request);
            OMPI_FREE_LIST_RETURN(&ompi_mtl_portals.event_fl, list_item);
            goto cleanup;
        }
        list_item = next_item;
    }

    /* now check the unexpected queue */
 restart_search:
    while (true) {
        ompi_mtl_portals_event_t *recv_event;
        OMPI_FREE_LIST_GET(&ompi_mtl_portals.event_fl, recv_event, ret);
        ret = PtlEQGet(ompi_mtl_portals.ptl_unexpected_recv_eq_h,
                       &recv_event->ev);
        if (PTL_OK == ret) {
            if ((recv_event->ev.match_bits & ~ignore_bits) == 
                (match_bits & ~ignore_bits)) {
                /* we have a match... */
                ompi_mtl_portals_get_data(recv_event, convertor, ptl_request);
                goto cleanup;
            } else {
                /* not ours - put in unexpected queue */
                opal_list_append(&(ompi_mtl_portals.unexpected_messages),
                                 (opal_list_item_t*) recv_event);
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break;
        } else {
            abort();
        }
    }

    /* now post the receive */
    ret = ompi_mtl_datatype_recv_buf(convertor, &md.start, &buflen,
                                     &ptl_request->free_after);
    md.length = buflen;

    PtlMEInsert(ompi_mtl_portals.ptl_unexpected_me_h,
                remote_proc,
                match_bits,
                ignore_bits,
                PTL_UNLINK,
                PTL_INS_BEFORE,
                &me_h);

    md.threshold = 0;
    md.options = PTL_MD_OP_PUT | PTL_MD_TRUNCATE | PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = ptl_request;
    md.eq_handle = ompi_mtl_portals.ptl_eq_h;
    PtlMDAttach(me_h, md, PTL_UNLINK, &md_h);

    /* now try to make active */
    md.threshold = 1;

    ret = PtlMDUpdate(md_h, NULL, &md,
                      ompi_mtl_portals.ptl_unexpected_recv_eq_h);
    if (ret == PTL_MD_NO_UPDATE) {
        /* a message has arrived since we searched - look again */
        PtlMDUnlink(md_h);
        if (ptl_request->free_after) { free(md.start); }
        goto restart_search;
    }

    ptl_request->event_callback = ompi_mtl_portals_recv_progress;

 cleanup:

    return OMPI_SUCCESS;
}

