/* 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "portals_config.h"

#include "ptl_portals.h"
#include "ptl_portals_compat.h"
#include "ptl_portals_recv.h"
#include "ptl_portals_send.h"
#include "mca/ptl/base/ptl_base_sendreq.h"


OBJ_CLASS_INSTANCE(mca_ptl_portals_recv_frag_t,
                   mca_ptl_base_recv_frag_t,
                   NULL, NULL);


int
ptl_portals_post_recv_md(struct mca_ptl_portals_module_t *ptl, void *data_ptr)
{
    ptl_handle_me_t me_handle;
    ptl_handle_md_t md_handle;
    ptl_md_t md;
    void *mem;
    int ret;
    ptl_process_id_t proc = { PTL_NID_ANY, PTL_PID_ANY };

    /* create match entry */
    ret = PtlMEAttach(ptl->ni_handle,
                      PTL_PORTALS_FRAG_TABLE_ID,
                      proc,
                      0, /* match bits */
                      0, /* ignore bits */
                      PTL_UNLINK,
                      PTL_INS_AFTER,
                      &me_handle);
    if (PTL_OK != ret) return OMPI_ERROR;

    if (NULL == data_ptr) {
        /* and some memory */
        mem = malloc(ptl->first_frag_entry_size);
        if (NULL == mem) {
            PtlMEUnlink(me_handle);
            return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
        }
    } else {
        /* re-use the memory from the (now unlinked) memory descriptor */
        mem = data_ptr;
    }

    /* and the memory descriptor */
    md.start = mem;
    md.length = ptl->first_frag_entry_size;
    md.threshold = PTL_MD_THRESH_INF;
    md.max_size = md.length - ptl->super.ptl_first_frag_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = NULL;
    md.eq_handle = ptl->frag_eq_handle;

    ret = PtlMDAttach(me_handle,
                      md,
                      PTL_UNLINK,
                      &md_handle);
    if (PTL_OK != ret) {
        PtlMEUnlink(me_handle);
        return OMPI_ERROR;
    }

    ompi_output_verbose(50, mca_ptl_portals_component.portals_output,
                        "new receive buffer posted");

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_process_recv_event(struct mca_ptl_portals_module_t *ptl,
                                   ptl_event_t *ev)
{
    int ret;

    if (ev->type == PTL_EVENT_PUT_START) {
        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "PUT_START event received (%ld)", ev->link);
    } else if (ev->type == PTL_EVENT_PUT_END) {
        ompi_list_item_t *item;
        mca_ptl_portals_recv_frag_t *recvfrag;
        mca_ptl_base_header_t *hdr;

        ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                            "message %ld received, start: %p, mlength: %lld, offset: %lld",
                            ev->link, ev->md.start, ev->mlength, ev->offset);

        /* buffer is going to be header followed by data */
        hdr = (mca_ptl_base_header_t*) (((char*) ev->md.start) + ev->offset);
        switch (hdr->hdr_common.hdr_type) {

        case MCA_PTL_HDR_TYPE_MATCH:
            /* get a fragment header */
            OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_recv_frags, item, ret);
            recvfrag = (mca_ptl_portals_recv_frag_t*) item;
            if (OMPI_SUCCESS != ret) {
                ompi_output(mca_ptl_portals_component.portals_output,
                            "unable to allocate resources");
                return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            }

            /* save the sender */
            recvfrag->frag_source = ev->initiator;

            recvfrag->frag_data = ((mca_ptl_base_match_header_t*) hdr) + 1;
            recvfrag->frag_size = ev->mlength - sizeof(mca_ptl_base_match_header_t);
            memcpy(&(recvfrag->frag_recv.frag_base.frag_header),
                   hdr, sizeof(mca_ptl_base_match_header_t));
            recvfrag->frag_recv.frag_base.frag_owner =
                (struct mca_ptl_base_module_t*) ptl;
            recvfrag->frag_recv.frag_base.frag_peer = NULL; /* BWB - fix me */
            recvfrag->frag_recv.frag_base.frag_size = 0;
            recvfrag->frag_recv.frag_base.frag_addr = recvfrag->frag_data;
            recvfrag->frag_recv.frag_is_buffered = true;
            recvfrag->frag_recv.frag_request = NULL;

            ptl->super.ptl_match(&ptl->super, &recvfrag->frag_recv, 
                                 &hdr->hdr_match);
            break;

        case MCA_PTL_HDR_TYPE_RNDV:
            /* get a fragment header */
            OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_recv_frags, item, ret);
            recvfrag = (mca_ptl_portals_recv_frag_t*) item;
            if (OMPI_SUCCESS != ret) {
                ompi_output(mca_ptl_portals_component.portals_output,
                            "unable to allocate resources");
                return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
            }

            /* save the sender */
            recvfrag->frag_source = ev->initiator;

            recvfrag->frag_data = ((mca_ptl_base_rendezvous_header_t*) hdr) + 1;
            recvfrag->frag_size = ev->mlength - sizeof(mca_ptl_base_rendezvous_header_t);
            memcpy(&(recvfrag->frag_recv.frag_base.frag_header),
                   hdr, sizeof(mca_ptl_base_rendezvous_header_t));
            recvfrag->frag_recv.frag_base.frag_owner =
                (struct mca_ptl_base_module_t*) ptl;
            recvfrag->frag_recv.frag_base.frag_peer = NULL; /* BWB - fix me */
            recvfrag->frag_recv.frag_base.frag_size = 0;
            recvfrag->frag_recv.frag_base.frag_addr = recvfrag->frag_data;
            recvfrag->frag_recv.frag_is_buffered = true;
            recvfrag->frag_recv.frag_request = NULL;

            ptl->super.ptl_match(&ptl->super, &recvfrag->frag_recv, 
                                 &hdr->hdr_match);

            break;

        case MCA_PTL_HDR_TYPE_FRAG:
            {
                unsigned int bytes_delivered;
                mca_ptl_base_recv_request_t* request;

                /* get a fragment header */
                OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_recv_frags, item, ret);
                recvfrag = (mca_ptl_portals_recv_frag_t*) item;
                if (OMPI_SUCCESS != ret) {
                    ompi_output(mca_ptl_portals_component.portals_output,
                                "unable to allocate resources");
                    return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
                }

                /* save the sender */
                recvfrag->frag_source = ev->initiator;

                recvfrag->frag_data = ((mca_ptl_base_frag_header_t*) hdr) + 1;
                recvfrag->frag_size = ev->mlength - sizeof(mca_ptl_base_frag_header_t);
                memcpy(&(recvfrag->frag_recv.frag_base.frag_header),
                       hdr, sizeof(mca_ptl_base_frag_header_t));
                recvfrag->frag_recv.frag_base.frag_owner =
                    (struct mca_ptl_base_module_t*) ptl;
                recvfrag->frag_recv.frag_base.frag_peer = NULL; /* BWB - fix me */
                recvfrag->frag_recv.frag_base.frag_size = 0;
                recvfrag->frag_recv.frag_base.frag_addr = recvfrag->frag_data;
                recvfrag->frag_recv.frag_is_buffered = true;
                recvfrag->frag_recv.frag_request = hdr->hdr_frag.hdr_dst_ptr.pval;
                bytes_delivered = recvfrag->frag_size;
                request = recvfrag->frag_recv.frag_request;

                if(recvfrag->frag_size > 0) {
                    struct iovec iov;
                    unsigned int iov_count = 1;
                    int free_after = 0;
                    ompi_proc_t *proc = ompi_comm_peer_lookup(request->req_recv.req_base.req_comm,
                                                              request->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
                    ompi_convertor_t* convertor = &(recvfrag->frag_recv.frag_base.frag_convertor);

                    /* initialize receive convertor */
                    ompi_convertor_copy(proc->proc_convertor, convertor);
                    ompi_convertor_init_for_recv(
                                 convertor,                      /* convertor */
                                 0,                              /* flags */
                                 request->req_recv.req_base.req_datatype, /* datatype */
                                 request->req_recv.req_base.req_count,    /* count elements */
                                 request->req_recv.req_base.req_addr,     /* users buffer */
                                 hdr->hdr_frag.hdr_frag_offset,                              /* offset in bytes into packed buffer */
                                 NULL );                         /* not allocating memory */
                    /*ompi_convertor_get_packed_size(convertor, &request->req_bytes_packed); */

                    iov.iov_base = recvfrag->frag_data;
                    iov.iov_len = recvfrag->frag_size;
                    ompi_convertor_unpack(convertor, &iov, &iov_count, &bytes_delivered, &free_after );
                }

                /* update request status */
                ptl->super.ptl_recv_progress(&ptl->super,
                                             request,
                                             recvfrag->frag_size,
                                             bytes_delivered);

            }
            break;

        case MCA_PTL_HDR_TYPE_ACK:
            {
                mca_ptl_portals_send_frag_t *sendfrag;
                mca_ptl_base_send_request_t *sendreq;
                sendfrag = hdr->hdr_ack.hdr_src_ptr.pval;
                sendreq = sendfrag->frag_send.frag_request;

                sendreq->req_peer_match = hdr->hdr_ack.hdr_dst_match;

                ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                                    "received ack for request %p",
                                    hdr->hdr_ack.hdr_dst_match);

                sendfrag->frag_send.frag_base.frag_owner->
                    ptl_send_progress(sendfrag->frag_send.frag_base.frag_owner,
                                      sendfrag->frag_send.frag_request,
                                      sendfrag->frag_send.frag_base.frag_size);

                /* return frag to freelist if not part of request */
                if (sendfrag->frag_send.frag_request->req_cached == false) {
                    if (sendfrag->frag_send.frag_base.frag_addr == NULL) {
                        free(sendfrag->frag_send.frag_base.frag_addr);
                    }
                    OMPI_FREE_LIST_RETURN(&mca_ptl_portals_component.portals_send_frags,
                                          (ompi_list_item_t*) sendfrag);
                }
            }

            break;

        default:
            ompi_output(mca_ptl_portals_component.portals_output,
                        "unable to deal with header of type %d",
                        hdr->hdr_common.hdr_type);
            break;
        }

        /* see if we need to repost an md */
        if (ev->offset > ev->md.length - ev->md.max_size) {
            ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                                "must repost event: %lld, %lld, %lld",
                                ev->offset, ev->md.length, ev->md.max_size);
            /* use the same memory as the old md - it's not using it anymore */
            ret = ptl_portals_post_recv_md(ptl, ev->md.start);
            if (OMPI_SUCCESS != ret) {
                ompi_output(mca_ptl_portals_component.portals_output,
                            "failed to allocate receive memory descriptor");
                /* BWB - ok, what do I do now? */
            }
        }

    } else {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "unknown event: %d (%ld)",
                            ev->type, ev->link);
    }

    return OMPI_SUCCESS;
}


void
mca_ptl_portals_matched(struct mca_ptl_base_module_t *ptl_base,
			struct mca_ptl_base_recv_frag_t *frag_base)
{
    mca_ptl_base_header_t* hdr = &frag_base->frag_base.frag_header;
    mca_ptl_base_recv_request_t* request = frag_base->frag_request;
    mca_ptl_portals_module_t* ptl = (mca_ptl_portals_module_t*) ptl_base;
    mca_ptl_portals_recv_frag_t* recvfrag = (mca_ptl_portals_recv_frag_t*) frag_base;
    unsigned int bytes_delivered = recvfrag->frag_size;

    /* generate an acknowledgment if required */
    if(hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK) {
        mca_ptl_portals_send_ack(ptl, recvfrag);
    }

    /* copy data into users buffer */
    if(recvfrag->frag_size > 0) {
        struct iovec iov;
        unsigned int iov_count = 1;
        int free_after = 0;
        ompi_proc_t *proc = ompi_comm_peer_lookup(request->req_recv.req_base.req_comm,
            request->req_recv.req_base.req_ompi.req_status.MPI_SOURCE);
        ompi_convertor_t* convertor = &frag_base->frag_base.frag_convertor;

        /* initialize receive convertor */
        ompi_convertor_copy(proc->proc_convertor, convertor);
        ompi_convertor_init_for_recv(
            convertor,                      /* convertor */
            0,                              /* flags */
            request->req_recv.req_base.req_datatype, /* datatype */
            request->req_recv.req_base.req_count,    /* count elements */
            request->req_recv.req_base.req_addr,     /* users buffer */
            0,                              /* offset in bytes into packed buffer */
            NULL );                         /* not allocating memory */
        /*ompi_convertor_get_packed_size(convertor, &request->req_bytes_packed); */

        iov.iov_base = recvfrag->frag_data;
        iov.iov_len = recvfrag->frag_size;
        ompi_convertor_unpack(convertor, &iov, &iov_count, &bytes_delivered, &free_after );
    }

    /* update request status */
    ptl->super.ptl_recv_progress(&ptl->super,
                                 request,
                                 recvfrag->frag_size,
                                 bytes_delivered);

    /* release resources */
#if 0
    if(ack_pending == false)
        MCA_PTL_PORTALS_RECV_FRAG_RETURN(recvfrag);
#endif
    return;
}
