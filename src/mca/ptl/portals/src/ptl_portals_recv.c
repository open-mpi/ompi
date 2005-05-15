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
    md.max_size = ptl->super.ptl_first_frag_size;
    md.options = PTL_MD_OP_PUT | PTL_MD_MAX_SIZE;
    md.user_ptr = NULL;
    md.eq_handle = ptl->eq_handles[MCA_PTL_PORTALS_EQ_RECV];

    ret = PtlMDAttach(me_handle,
                      md,
                      PTL_UNLINK,
                      &md_handle);
    if (PTL_OK != ret) {
        PtlMEUnlink(me_handle);
        return OMPI_ERROR;
    }

    OMPI_OUTPUT_VERBOSE((100, mca_ptl_portals_component.portals_output,
                         "new receive buffer posted"));

    return OMPI_SUCCESS;
}


int
mca_ptl_portals_process_recv_event(struct mca_ptl_portals_module_t *ptl,
                                   ptl_event_t *ev)
{
    int ret;

    if (ev->type == PTL_EVENT_PUT_START) {
        OMPI_OUTPUT_VERBOSE((101, mca_ptl_portals_component.portals_output,
                             "starting to receive message", ev->link));
    } else if (ev->type == PTL_EVENT_PUT_END) {
        mca_ptl_base_header_t *hdr;

        OMPI_OUTPUT_VERBOSE((101, mca_ptl_portals_component.portals_output,
                             "message %ld received, start: %p, mlength: %lld,"
                             " offset: %lld",
                             ev->link, ev->md.start, ev->mlength, ev->offset));

        /* buffer is going to be header followed by data */
        hdr = (mca_ptl_base_header_t*) (((char*) ev->md.start) + ev->offset);
        switch (hdr->hdr_common.hdr_type) {

        case MCA_PTL_HDR_TYPE_MATCH:
            ret = mca_ptl_portals_process_first_frag(ptl, hdr, ev,
                                  sizeof(mca_ptl_base_match_header_t));
            if (OMPI_SUCCESS != ret) return ret;
            break;

        case MCA_PTL_HDR_TYPE_RNDV:
            ret = mca_ptl_portals_process_first_frag(ptl, hdr, ev,
                                  sizeof(mca_ptl_base_rendezvous_header_t));
            if (OMPI_SUCCESS != ret) return ret;
            break;

        case MCA_PTL_HDR_TYPE_FRAG:
            ret = mca_ptl_portals_process_frag_frag(ptl, hdr, ev);
            if (OMPI_SUCCESS != ret) return ret;
            break;

        case MCA_PTL_HDR_TYPE_ACK:
            {
                mca_ptl_portals_send_frag_t *sendfrag;
                mca_ptl_base_send_request_t *sendreq;

                sendfrag = hdr->hdr_ack.hdr_src_ptr.pval;
                sendreq = sendfrag->frag_send.frag_request;
                sendreq->req_peer_match = hdr->hdr_ack.hdr_dst_match;

                OMPI_OUTPUT_VERBOSE((100, 
                                     mca_ptl_portals_component.portals_output,
                                     "received ack for recv request %p (msg %d)",
                                     hdr->hdr_ack.hdr_dst_match,
                                     sendreq->req_send.req_base.req_sequence));

                mca_ptl_portals_complete_send_event(sendfrag);
            }
            break;

        default:
            ompi_output(mca_ptl_portals_component.portals_output,
                        "*** unable to deal with header of type %d",
                        hdr->hdr_common.hdr_type);
            break;
        }

        if (ev->md.length - (ev->offset + ev->mlength) < ev->md.max_size) {
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
                            "*** unknown event: %d (%ld)",
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
