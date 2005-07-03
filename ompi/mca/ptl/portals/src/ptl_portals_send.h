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


#ifndef MCA_PTL_PORTALS_SENDFRAG_H_
#define MCA_PTL_PORTALS_SENDFRAG_H_

#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/ptl/base/ptl_base_sendreq.h"
#include "ptl_portals_recv.h" 

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    struct mca_ptl_portals_send_frag_t {
        mca_ptl_base_send_frag_t frag_send;
        ptl_md_iovec_t frag_vector[2];
        int32_t free_data;
    };
    typedef struct mca_ptl_portals_send_frag_t mca_ptl_portals_send_frag_t;
    OBJ_CLASS_DECLARATION (mca_ptl_portals_send_frag_t);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

static inline int
mca_ptl_portals_send_frag(struct mca_ptl_portals_module_t *ptl,
                          mca_ptl_portals_send_frag_t* sendfrag)
{
    ptl_md_t md;
    ptl_handle_md_t md_handle;
    int ret;

    /* setup the send and go */
    md.start = sendfrag->frag_vector;
    md.length = 2; /* header + data */
    md.threshold = PTL_MD_THRESH_INF; /* unlink based on protocol */
    md.max_size = 0;
    md.options = PTL_MD_IOVEC; /* BWB - can we optimize? */
    md.user_ptr = sendfrag;
    md.eq_handle = ptl->eq_handles[MCA_PTL_PORTALS_EQ_SEND];

    /* make a free-floater */
    ret = PtlMDBind(ptl->ni_handle,
                    md,
                    PTL_UNLINK,
                    &md_handle);
    if (ret != PTL_OK) {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "PtlMDBind failed with error %d", ret);
        return OMPI_ERROR;
    }

    ret = PtlPut(md_handle,
                 PTL_ACK_REQ,
                 *((ptl_process_id_t*) sendfrag->frag_send.frag_base.frag_peer),
                 PTL_PORTALS_FRAG_TABLE_ID,
                 0, /* ac_index */
                 0, /* match bits */
                 0, /* remote offset - not used */
                 0); /* hdr_data - not used */
    if (ret != PTL_OK) {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "PtlPut failed with error %d", ret);
        PtlMDUnlink(md_handle);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}


static inline int
mca_ptl_portals_send_ack(struct mca_ptl_portals_module_t *ptl,
                         mca_ptl_portals_recv_frag_t* recvfrag)
{
    mca_ptl_base_header_t* hdr;
    mca_ptl_portals_send_frag_t* sendfrag;
    opal_list_item_t *item;
    mca_ptl_base_recv_request_t* request = recvfrag->frag_recv.frag_request;
    int ret;

    /* get a fragment */
    OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_send_frags,
                       item, ret);
    if (NULL == item) return ret;
    sendfrag = (mca_ptl_portals_send_frag_t *) item;

    /* no payload */
    sendfrag->frag_vector[1].iov_base = NULL;
    sendfrag->frag_vector[1].iov_len = 0;

    /* setup message header */
    hdr = &sendfrag->frag_send.frag_base.frag_header;

    hdr->hdr_ack.hdr_common.hdr_type = MCA_PTL_HDR_TYPE_ACK;
    hdr->hdr_ack.hdr_common.hdr_flags = 0;

#if OMPI_ENABLE_MEM_DEBUG
    hdr->hdr_ack.hdr_dst_match.lval = 0;
    hdr->hdr_ack.hdr_dst_addr.lval = 0;
#endif

    hdr->hdr_ack.hdr_src_ptr = 
        recvfrag->frag_recv.frag_base.frag_header.hdr_rndv.hdr_src_ptr;
    hdr->hdr_ack.hdr_dst_match.pval = request;
    hdr->hdr_ack.hdr_dst_addr.pval = request->req_recv.req_base.req_addr;
    hdr->hdr_ack.hdr_dst_size = request->req_recv.req_bytes_packed;

    /* can ignore most of the fragment, but need to make sure the
       request is NULL so that process_send_event knows it's an ack
       completing */
    sendfrag->frag_send.frag_request = NULL;
    sendfrag->frag_send.frag_base.frag_peer = 
        (struct mca_ptl_base_peer_t*) &(recvfrag->frag_source);

    sendfrag->frag_vector[0].iov_len = sizeof(mca_ptl_base_ack_header_t);

    OMPI_OUTPUT_VERBOSE((100, mca_ptl_portals_component.portals_output,
                         "sending ack for recv request %p", request));
        
    return mca_ptl_portals_send_frag(ptl, sendfrag);
}


static inline void
mca_ptl_portals_complete_send_event(mca_ptl_portals_send_frag_t* frag)
{
    frag->frag_send.frag_base.frag_owner->
        ptl_send_progress(frag->frag_send.frag_base.frag_owner,
                          frag->frag_send.frag_request,
                          frag->frag_send.frag_base.frag_size);

    /* return frag to freelist if not part of request */
    if (frag->frag_send.frag_request->req_cached == false || 
        frag->frag_send.frag_base.frag_header.hdr_common.hdr_type == 
        MCA_PTL_HDR_TYPE_FRAG) {

        if (frag->free_data) {
            free(frag->frag_vector[1].iov_base);
        }
        OMPI_FREE_LIST_RETURN(&mca_ptl_portals_component.portals_send_frags,
                              (opal_list_item_t*) frag);
    }
}                        

#endif /* MCA_PTL_PORTALS_SENDFRAG_H_ */
