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
#include "ompi/mca/mtl/base/mtl_base_datatype.h"

#include "mtl_portals.h"
#include "mtl_portals_endpoint.h"
#include "mtl_portals_request.h"
#include "mtl_portals_recv_short.h"

#define CHECK_MATCH(incoming_bits, match_bits, ignore_bits)     \
    (((incoming_bits ^ match_bits) & ~ignore_bits) == 0)

/* called when a receive should be progressed */
static int
ompi_mtl_portals_recv_progress(ptl_event_t *ev,
                               struct ompi_mtl_portals_request_t* ptl_request)
{
    int ret;

    switch (ev->type) {
    case PTL_EVENT_PUT_END:
        /* make sure the data is in the right place */
        ompi_mtl_datatype_unpack(ptl_request->convertor,
                                 ev->md.start, ev->mlength);

        /* set the status */
        ptl_request->super.ompi_req->req_status.MPI_SOURCE =
            PTL_GET_SOURCE(ev->match_bits);
        ptl_request->super.ompi_req->req_status.MPI_TAG = 
                PTL_GET_TAG(ev->match_bits);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = 
            (ev->rlength > ev->mlength) ?
            MPI_ERR_TRUNCATE : MPI_SUCCESS;
        ptl_request->super.ompi_req->req_status._count = 
            ev->mlength;

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                             "recv complete: 0x%016llx\n", ev->match_bits));
        
        ptl_request->super.completion_callback(&ptl_request->super);
        break;

    case PTL_EVENT_REPLY_END:
        /* make sure the data is in the right place */
        ompi_mtl_datatype_unpack(ptl_request->convertor,
                                 ev->md.start, ev->mlength);

        ret=PtlMDUnlink(ev->md_handle);
        if( ret !=PTL_OK) {
            return ompi_common_portals_error_ptl_to_ompi(ret);
        }

        /* set the status - most of this filled in right after issuing
           the PtlGet*/
        ptl_request->super.ompi_req->req_status._count = 
            ev->mlength;

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                             "recv complete: 0x%016llx\n", ev->match_bits));
        
        ptl_request->super.completion_callback(&ptl_request->super);
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
    size_t buflen;
    
    if (PTL_IS_SHORT_MSG(recv_event->ev.match_bits)) {
        /* the buffer is sitting in the short message queue */

        struct iovec iov;
        uint32_t iov_count = 1;
        size_t max_data;

        ompi_mtl_portals_recv_short_block_t *block = 
            recv_event->ev.md.user_ptr;

        iov.iov_base = (((char*) recv_event->ev.md.start) + recv_event->ev.offset);
        iov.iov_len = recv_event->ev.mlength;
        max_data = iov.iov_len;

        /* see if this message filled the receive block */
        if (recv_event->ev.md.length - (recv_event->ev.offset + 
                                        recv_event->ev.mlength) <
            recv_event->ev.md.max_size) {
            block->full = true;
        }

        /* pull out the data */
        if (iov.iov_len > 0) {
            ompi_convertor_unpack(convertor, &iov, &iov_count,
                                  &max_data );
        }

        /* if synchronous, return an ack */
        if (PTL_IS_SYNC_MSG(recv_event->ev)) {
            md.length = 0;
            md.start = (((char*) recv_event->ev.md.start) + recv_event->ev.offset);
            md.threshold = 1; /* send */
            md.options = PTL_MD_EVENT_START_DISABLE;
            md.user_ptr = NULL;
            md.eq_handle = ompi_mtl_portals.ptl_eq_h;

            ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h, md,
                            PTL_UNLINK, &md_h);
            if (PTL_OK != ret) {
                opal_output(fileno(stderr)," Error returned from PtlMDBind.  Error code - %d \n",ret);
                abort();
            }

            OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                                 "acking recv: 0x%016llx\n", 
                                 recv_event->ev.match_bits));

            ret = PtlPut(md_h,
                         PTL_NO_ACK_REQ,
                         recv_event->ev.initiator,
                         OMPI_MTL_PORTALS_ACK_TABLE_ID,
                         0,
                         recv_event->ev.hdr_data,
                         0,
                         0);
            if (PTL_OK != ret) {
                opal_output(fileno(stderr)," Error returned from PtlPut.  Error code - %d \n",ret);
                abort();
            }
        }

        /* finished with our buffer space */
        ompi_mtl_portals_return_block_part(&ompi_mtl_portals, block);

        ompi_convertor_get_packed_size(convertor, &buflen);

        ptl_request->super.ompi_req->req_status.MPI_SOURCE =
            PTL_GET_SOURCE(recv_event->ev.match_bits);
        ptl_request->super.ompi_req->req_status.MPI_TAG = 
            PTL_GET_TAG(recv_event->ev.match_bits);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = 
            (recv_event->ev.rlength > buflen) ?
            MPI_ERR_TRUNCATE : MPI_SUCCESS;
        ptl_request->super.ompi_req->req_status._count = 
            recv_event->ev.mlength;

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                             "recv complete: 0x%016llx\n", 
                             recv_event->ev.match_bits));
        
        ptl_request->super.completion_callback(&ptl_request->super);

    } else {
        ret = ompi_mtl_datatype_recv_buf(convertor, &md.start, &buflen,
                                         &ptl_request->free_after);
        if (OMPI_SUCCESS != ret) {
            opal_output(fileno(stderr)," Error returned from ompi_mtl_datatype_recv_buf.  Error code - %d \n",ret);
            abort();
        }
        md.length = (recv_event->ev.rlength > buflen) ? buflen : recv_event->ev.rlength;
        md.threshold = 2; /* send and get */
        md.options = PTL_MD_EVENT_START_DISABLE;
        md.user_ptr = ptl_request;
        md.eq_handle = ompi_mtl_portals.ptl_eq_h;

        /* retain because it's unclear how many events we'll get here.
           Some implementations give just the REPLY, others give SEND
           and REPLY */
        ret = PtlMDBind(ompi_mtl_portals.ptl_ni_h, md,
                        PTL_RETAIN, &md_h);
        if (PTL_OK != ret) {
            opal_output(fileno(stderr)," Error returned from ompi_mtl_datatype_recv_buf.  Error code - %d \n",ret);
            abort();
        }

        ptl_request->event_callback = ompi_mtl_portals_recv_progress;

        ret = PtlGet(md_h, 
                     recv_event->ev.initiator, 
                     OMPI_MTL_PORTALS_READ_TABLE_ID,
                     0, 
                     recv_event->ev.hdr_data,
                     0);
        if (PTL_OK != ret) {
            opal_output(fileno(stderr)," Error returned from PtlGet.  Error code - %d \n",ret);
            abort();
        }

        ptl_request->super.ompi_req->req_status.MPI_SOURCE =
            PTL_GET_SOURCE(recv_event->ev.match_bits);
        ptl_request->super.ompi_req->req_status.MPI_TAG = 
            PTL_GET_TAG(recv_event->ev.match_bits);
        ptl_request->super.ompi_req->req_status.MPI_ERROR = 
            (recv_event->ev.rlength > buflen) ?
            MPI_ERR_TRUNCATE : MPI_SUCCESS;
    }

    return OMPI_SUCCESS;
}


static void
ompi_mtl_portals_match_up_put_end(ptl_seq_t link)
{
    opal_list_item_t *list_item;

    /* match up a PUT_END event with its corresponding PUT_START event */
    list_item = opal_list_get_first(&ompi_mtl_portals.unexpected_messages);
    while (list_item != opal_list_get_end(&ompi_mtl_portals.unexpected_messages)) {
        opal_list_item_t *next_item = opal_list_get_next(list_item);
        ompi_mtl_portals_event_t *recv_event = (ompi_mtl_portals_event_t*) list_item;
        if (recv_event->ev.link == link) {
            recv_event->is_complete = true;
            return;
        }
        list_item = next_item;
    }

    /* should never get here */
    opal_output(fileno(stderr)," ompi_mtl_portals_match_up_put_end failed \n");
    abort(); 
}


static void 
ompi_mtl_portals_wait_for_put_end(ptl_seq_t link)
{
    ptl_event_t ev;
    int ret;

    /* wait for a PUT_END event that matches the message we're looking for */
    while (true) {
        ret = PtlEQWait(ompi_mtl_portals.ptl_unexpected_recv_eq_h,&ev);
        if (PTL_OK == ret) {
            if (PTL_EVENT_PUT_START == ev.type) {
                ompi_free_list_item_t *item;
                ompi_mtl_portals_event_t *recv_event;

                OMPI_FREE_LIST_GET(&ompi_mtl_portals.event_fl, item, ret);
                recv_event = (ompi_mtl_portals_event_t*) item;
                recv_event->ev = ev;
                recv_event->is_complete = false;
                opal_list_append(&(ompi_mtl_portals.unexpected_messages),
                                 (opal_list_item_t*) recv_event);

               if (PTL_IS_SHORT_MSG(recv_event->ev.match_bits)) {
                    ompi_mtl_portals_recv_short_block_t *block =
                        recv_event->ev.md.user_ptr;
                    OPAL_THREAD_ADD32(&block->pending, 1);
               }

            } else if (PTL_EVENT_PUT_END == ev.type) {
                if (link == ev.link) {
                    /* the one we want */
                    return;
                }
                /* otherwise match it up */
                ompi_mtl_portals_match_up_put_end(ev.link);
            } else {
                opal_output(fileno(stderr)," Unrecognised event type - %d - ompi_mtl_portals_wait_for_put_end : %d \n",ev.type,ret);
                abort(); 
            }
        } else {
            opal_output(fileno(stderr)," Error returned in ompi_mtl_portals_wait_for_put_end from PtlEQWait : %d \n",ret);
            abort();
        }
    }
}


static ompi_mtl_portals_event_t*
ompi_mtl_portals_search_unex_events(ptl_match_bits_t match_bits,
                                    ptl_match_bits_t ignore_bits)
{
    ptl_event_t ev;
    int ret;

    /* check to see if there are any events in the unexpected event queue */ 
    while (true) {
        ret = PtlEQGet(ompi_mtl_portals.ptl_unexpected_recv_eq_h,&ev);
        if (PTL_OK == ret) {
            if (PTL_EVENT_PUT_START == ev.type) {
                ompi_free_list_item_t *item;
                ompi_mtl_portals_event_t *recv_event;

                OMPI_FREE_LIST_GET(&ompi_mtl_portals.event_fl, item, ret);
                recv_event = (ompi_mtl_portals_event_t*) item;
                recv_event->ev = ev;
                recv_event->is_complete = false;

               if (PTL_IS_SHORT_MSG(recv_event->ev.match_bits)) {
                    ompi_mtl_portals_recv_short_block_t *block =
                        recv_event->ev.md.user_ptr;
                    OPAL_THREAD_ADD32(&block->pending, 1);
               }
                if (CHECK_MATCH(recv_event->ev.match_bits, match_bits, ignore_bits)) {
                    /* the one we want */
                    ompi_mtl_portals_wait_for_put_end(recv_event->ev.link);
                    return recv_event; 
                } else {
                    /* not the one we want, so add it to the unex list */
                    opal_list_append(&(ompi_mtl_portals.unexpected_messages),
                                     (opal_list_item_t*) recv_event);
                }
            } else if (PTL_EVENT_PUT_END == ev.type) {
                /* can't be the one we want */
                ompi_mtl_portals_match_up_put_end(ev.link);
            } else {
                opal_output(fileno(stderr)," Unrecognised event type - %d - ompi_mtl_portals_search_unex_events : %d \n",ev.type,ret);
                abort();
            }
        } else if (PTL_EQ_EMPTY == ret) {
            break; 
        } else {
            opal_output(fileno(stderr)," Error returned in ompi_mtl_portals_search_unex_events from PtlEQWait : %d \n",ret);
            abort();
        }
    }

    return NULL;
}


static ompi_mtl_portals_event_t*
ompi_mtl_portals_search_unex_q( ptl_match_bits_t match_bits,
                                ptl_match_bits_t ignore_bits )
{
    opal_list_item_t *list_item;
    ompi_mtl_portals_event_t *recv_event = NULL;

    /* check the queue of processed unexpected messages */
    list_item = opal_list_get_first(&ompi_mtl_portals.unexpected_messages);
    while (list_item != opal_list_get_end(&ompi_mtl_portals.unexpected_messages)) {
        opal_list_item_t *next_item = opal_list_get_next(list_item);

        recv_event = (ompi_mtl_portals_event_t*) list_item;
        if (CHECK_MATCH(recv_event->ev.match_bits, match_bits, ignore_bits)) {
            /* we have a match... */
            if ( false == recv_event->is_complete) {
                /* wait for put end event */
                ompi_mtl_portals_wait_for_put_end(recv_event->ev.link);
            }
            opal_list_remove_item(&(ompi_mtl_portals.unexpected_messages),
                                  list_item);
            return recv_event;
        }
        list_item = next_item;
    }

    /* didn't find it */
    return NULL;
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
    ompi_mtl_portals_request_t *ptl_request = 
        (ompi_mtl_portals_request_t*) mtl_request;
    ompi_mtl_portals_event_t *recv_event = NULL;
    size_t buflen;

    ptl_request->convertor = convertor;

    if  (MPI_ANY_SOURCE == src) {
        remote_proc.nid = PTL_NID_ANY;
        remote_proc.pid = PTL_PID_ANY;
    } else {
        ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, src );
        endpoint = (mca_mtl_base_endpoint_t*) ompi_proc->proc_pml;
        remote_proc = endpoint->ptl_proc;
    }

    PTL_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid,
                      src, tag);

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_output,
                         "recv bits: 0x%016llx 0x%016llx\n",
                         match_bits, ignore_bits));

    /* first, check the queue of processed unexpected messages */
    recv_event = ompi_mtl_portals_search_unex_q(match_bits, ignore_bits);
    if (NULL != recv_event) {
        /* found it */
        ompi_mtl_portals_get_data(recv_event, convertor, ptl_request);
        OMPI_FREE_LIST_RETURN(&ompi_mtl_portals.event_fl,
                              (ompi_free_list_item_t*)recv_event);
        goto cleanup;
    } else {
restart_search:
        /* check unexpected events */
        recv_event = ompi_mtl_portals_search_unex_events(match_bits, ignore_bits);
        if (NULL != recv_event) {
            /* found it */
            ompi_mtl_portals_get_data(recv_event, convertor, ptl_request);
            OMPI_FREE_LIST_RETURN(&ompi_mtl_portals.event_fl,
                                  (ompi_free_list_item_t*)recv_event);
            goto cleanup;
        }
    }

    /* didn't find it, now post the receive */
    ret = ompi_mtl_datatype_recv_buf(convertor, &md.start, &buflen,
                                     &ptl_request->free_after);
    md.length = buflen;

    /* create ME entry */
    ret = PtlMEInsert(ompi_mtl_portals.ptl_match_ins_me_h,
                remote_proc,
                match_bits,
                ignore_bits,
                PTL_UNLINK,
                PTL_INS_BEFORE,
                &me_h);
    if( ret !=PTL_OK) {
        return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    /* associate a memory descriptor with the Match list Entry */
    md.threshold = 0;
    md.options = PTL_MD_OP_PUT | PTL_MD_TRUNCATE | PTL_MD_EVENT_START_DISABLE;
    md.user_ptr = ptl_request;
    md.eq_handle = ompi_mtl_portals.ptl_eq_h;
    ret=PtlMDAttach(me_h, md, PTL_UNLINK, &md_h);
    if( ret !=PTL_OK) {
        return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    /* now try to make active */
    md.threshold = 1;

    /* enable the memory descritor, if the ptl_unexpected_recv_eq_h
     *   queue is empty */
    ret = PtlMDUpdate(md_h, NULL, &md,
                      ompi_mtl_portals.ptl_unexpected_recv_eq_h);
    if (ret == PTL_MD_NO_UPDATE) {
        /* a message has arrived since we searched - look again */
        PtlMDUnlink(md_h);
        if (ptl_request->free_after) { free(md.start); }
        goto restart_search;
    } else if( PTL_OK != ret ) {
        return ompi_common_portals_error_ptl_to_ompi(ret);
    }

    ptl_request->event_callback = ompi_mtl_portals_recv_progress;

 cleanup:

    return OMPI_SUCCESS;
}

