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
/**
 * @file
 */

#ifndef MCA_PTL_PORTALS_RECV_FRAG_H
#define MCA_PTL_PORTALS_RECV_FRAG_H

#include "mca/ptl/base/ptl_base_recvfrag.h"

/**
 *  PORTALS received fragment derived type.
 */
struct mca_ptl_portals_recv_frag_t {
    mca_ptl_base_recv_frag_t frag_recv;  /**< base receive fragment descriptor */
    void *frag_data;
    size_t frag_size;
    ptl_process_id_t frag_source;
};
typedef struct mca_ptl_portals_recv_frag_t mca_ptl_portals_recv_frag_t;

OBJ_CLASS_DECLARATION(mca_ptl_portals_recv_frag_t);


int ptl_portals_post_recv_md(struct mca_ptl_portals_module_t *ptl,
                             void *data_ptr);
int mca_ptl_portals_process_recv_event(struct mca_ptl_portals_module_t *ptl, 
                                       ptl_event_t *ev);

static inline mca_ptl_portals_recv_frag_t *
mca_ptl_portals_recv_get_frag(struct mca_ptl_portals_module_t *ptl,
                               mca_ptl_base_header_t *hdr, 
                               ptl_event_t *ev,
                               size_t header_size)
{
    mca_ptl_portals_recv_frag_t * recvfrag;
    opal_list_item_t *item;
    int ret;

    /* get a fragment header */
    OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_recv_frags,
                       item, ret);
    recvfrag = (mca_ptl_portals_recv_frag_t*) item;
    if (OMPI_SUCCESS != ret) {
        opal_output(mca_ptl_portals_component.portals_output,
                    "unable to allocate resources");
        return NULL;
    }

    /* save the sender */
    recvfrag->frag_source = ev->initiator;

    recvfrag->frag_data = ((char*) hdr) + header_size;
    recvfrag->frag_size = ev->mlength - header_size;
    memcpy(&(recvfrag->frag_recv.frag_base.frag_header),
           hdr, header_size);
    recvfrag->frag_recv.frag_base.frag_owner = &(ptl->super);
    recvfrag->frag_recv.frag_base.frag_peer = NULL; /* BWB - fix me */
    recvfrag->frag_recv.frag_base.frag_size = 0;
    recvfrag->frag_recv.frag_base.frag_addr = recvfrag->frag_data;
    recvfrag->frag_recv.frag_is_buffered = true;

    return recvfrag;
}


static inline int
mca_ptl_portals_process_first_frag(struct mca_ptl_portals_module_t *ptl,
                                   mca_ptl_base_header_t *hdr, 
                                   ptl_event_t *ev,
                                   size_t header_size)
{
    mca_ptl_portals_recv_frag_t *recvfrag;

    recvfrag = mca_ptl_portals_recv_get_frag(ptl, hdr, ev, header_size);
    if (NULL == recvfrag) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    OPAL_OUTPUT_VERBOSE((100, mca_ptl_portals_component.portals_output,
                         "recving first frag of size %d for msg %d from %lu",
                         recvfrag->frag_size,
                         (int) hdr->hdr_match.hdr_msg_seq,
                         ev->initiator.pid));

    recvfrag->frag_recv.frag_request = NULL;
    ptl->super.ptl_match(&ptl->super, &recvfrag->frag_recv, 
                         &hdr->hdr_match);

    return OMPI_SUCCESS;
}

static inline int
mca_ptl_portals_process_frag_frag(struct mca_ptl_portals_module_t *ptl,
                                  mca_ptl_base_header_t *hdr, 
                                  ptl_event_t *ev)
{
    size_t bytes_delivered;
    mca_ptl_base_recv_request_t* request;
    mca_ptl_portals_recv_frag_t *recvfrag;

    /* get a frag and fill it in */
    recvfrag = mca_ptl_portals_recv_get_frag(ptl, hdr, ev,
                                         sizeof(mca_ptl_base_frag_header_t));
    if (NULL == recvfrag) return OMPI_ERR_TEMP_OUT_OF_RESOURCE;

    recvfrag->frag_recv.frag_request = hdr->hdr_frag.hdr_dst_ptr.pval;
    bytes_delivered = recvfrag->frag_size;
    request = recvfrag->frag_recv.frag_request;

    if (recvfrag->frag_size > 0) {
        struct iovec iov;
        unsigned int iov_count = 1;
        int free_after = 0;
        ompi_convertor_t* convertor = 
                    &(recvfrag->frag_recv.frag_base.frag_convertor);

        /* clone receive convertor and set to correct position */
        ompi_convertor_clone_with_position(&(request->req_recv.req_convertor),
                                            convertor, 1,
                                            &(hdr->hdr_frag.hdr_frag_offset));

        iov.iov_base = recvfrag->frag_data;
        iov.iov_len = recvfrag->frag_size;
        ompi_convertor_unpack(convertor, &iov, &iov_count, 
                              &bytes_delivered, &free_after );
    }

    OPAL_OUTPUT_VERBOSE((100, mca_ptl_portals_component.portals_output,
                         "recving secnd frag of size %d for msg %d, offset %lld from %lu, %p",
                         recvfrag->frag_size,
                         (int) hdr->hdr_match.hdr_msg_seq,
                         hdr->hdr_frag.hdr_frag_offset,
                         ev->initiator.pid,
                         request));

    /* update request status */
    ptl->super.ptl_recv_progress(&ptl->super,
                                 request,
                                 recvfrag->frag_size,
                                 bytes_delivered);

    return OMPI_SUCCESS;
}

#endif
