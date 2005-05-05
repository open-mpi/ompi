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
#include "ptl_portals_send.h"
#include "mca/pml/pml.h"
#include "mca/pml/base/pml_base_sendreq.h"

OBJ_CLASS_INSTANCE(mca_ptl_portals_send_frag_t,
                   mca_ptl_base_send_frag_t,
                   NULL, NULL);


int
mca_ptl_portals_send(struct mca_ptl_base_module_t *ptl_base,
		     struct mca_ptl_base_peer_t *ptl_peer,
		     struct mca_pml_base_send_request_t *sendreq,
		     size_t offset, size_t size, int flags)
{
    mca_ptl_portals_module_t* ptl = (mca_ptl_portals_module_t*) ptl_base;
    ptl_process_id_t *peer_id = (ptl_process_id_t*) ptl_peer;
    mca_ptl_portals_send_frag_t* sendfrag;
    mca_ptl_base_header_t* hdr;
    int ret;
    ptl_md_t md;
    ptl_handle_md_t md_handle;

    ompi_output_verbose(100, mca_ptl_portals_component.portals_output,
                        "mca_ptl_portals_send to %lu, %lu",
                        peer_id->nid, peer_id->pid);

    if (sendreq->req_cached) {
        sendfrag = (mca_ptl_portals_send_frag_t*)(sendreq+1);
    } else {
        ompi_list_item_t *item;
        OMPI_FREE_LIST_GET(&mca_ptl_portals_component.portals_send_frags,
                           item, ret);
        if (NULL == item) return ret;
        sendfrag = (mca_ptl_portals_send_frag_t *) item;
    }

    /* initialize convertor */
    if (size > 0) {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "request size > 0, not implemented");
        return OMPI_ERROR;
    } else {
        sendfrag->frag_send.frag_base.frag_addr = NULL;
        sendfrag->frag_send.frag_base.frag_size = 0;
    }

    /* setup message header */
    hdr = &sendfrag->frag_send.frag_base.frag_header;
    if(offset == 0) {
        hdr->hdr_common.hdr_flags = flags;
        hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
        hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
        hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
        hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
        hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;
        hdr->hdr_match.hdr_msg_seq = sendreq->req_base.req_sequence;
    } else {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "offset > 0, not implemented");
        return OMPI_ERROR;
    }

    /* fragment state */
    sendfrag->frag_send.frag_base.frag_owner = ptl_base;
    sendfrag->frag_send.frag_request = sendreq;
    sendfrag->frag_send.frag_base.frag_peer = ptl_peer;


    /* must update the offset after actual fragment size is determined 
     * before attempting to send the fragment
     */
    mca_pml_base_send_request_offset(sendreq,
        sendfrag->frag_send.frag_base.frag_size);

    md.start = hdr;
    md.length = sizeof(mca_ptl_base_header_t);
    md.threshold = 2; /* we do a put, we get out of here */
    md.max_size = 0;
    md.options = 0;
    md.user_ptr = 123;
    md.eq_handle = ptl->frag_eq_handle;

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
                 PTL_NO_ACK_REQ,
                 *((ptl_process_id_t*) ptl_peer),
                 PTL_PORTALS_FRAG_TABLE_ID,
                 0, /* ac_index */
                 0, /* match bits */
                 0, /* remote offset - not used */
                 321); /* hdr_data - not used */
    if (ret != PTL_OK) {
        ompi_output(mca_ptl_portals_component.portals_output,
                    "PtlPut failed with error %d", ret);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
