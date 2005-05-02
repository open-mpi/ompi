/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004 The Ohio State University.
 *                    All rights reserved.
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
#include "include/types.h"
#include "datatype/datatype.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "ptl_ib.h"
#include "ptl_ib_peer.h"
#include "ptl_ib_proc.h"
#include "ptl_ib_sendfrag.h"
#include "ptl_ib_priv.h"
#include "ptl_ib_memory.h"

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag);
static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag);

OBJ_CLASS_INSTANCE(mca_ptl_ib_send_frag_t, 
        mca_ptl_base_send_frag_t,
        mca_ptl_ib_send_frag_construct, 
        mca_ptl_ib_send_frag_destruct);

/*
 * Placeholders for send fragment constructor/destructors.
 */

static void mca_ptl_ib_send_frag_construct(mca_ptl_ib_send_frag_t* frag)
{
    frag->frag_progressed = 0;
    frag->frag_ack_pending = 0;
}

static void mca_ptl_ib_send_frag_destruct(mca_ptl_ib_send_frag_t* frag)
{
}

/*
 * Allocate a IB send descriptor
 *
 */
mca_ptl_ib_send_frag_t* mca_ptl_ib_alloc_send_frag(
        mca_ptl_ib_module_t* ib_ptl,
        mca_pml_base_send_request_t* request)
{
    ompi_free_list_t *flist = &ib_ptl->send_free;
    ompi_list_item_t *item;
    mca_ptl_ib_send_frag_t *ib_send_frag;

    item = ompi_list_remove_first(&((flist)->super));
    while(NULL == item) {

        mca_ptl_tstamp_t tstamp = 0;

        D_PRINT("Gone one NULL descriptor ... trying again");

        mca_ptl_ib_component_progress(0);
        item = ompi_list_remove_first (&((flist)->super));
    }

    ib_send_frag = (mca_ptl_ib_send_frag_t *)item;
    return ib_send_frag;
}


int mca_ptl_ib_send_frag_register(mca_ptl_ib_module_t *ib_ptl)
{
    int i, rc, num_send_frags;
    ompi_list_item_t *item;
    ompi_free_list_t *flist = &ib_ptl->send_free;
    ib_buffer_t *ib_buf_ptr;
    mca_ptl_ib_send_frag_t *ib_send_frag;

    num_send_frags = ompi_list_get_size(&(flist->super));
    item = ompi_list_get_first(&((flist)->super));

    /* Register the buffers */
    for(i = 0; i < num_send_frags; 
            item = ompi_list_get_next(item), i++) {

        ib_send_frag = (mca_ptl_ib_send_frag_t *) item;

        ib_send_frag->frag_progressed = 0;

        ib_buf_ptr = (ib_buffer_t *) &ib_send_frag->ib_buf;

        rc = mca_ptl_ib_register_mem(ib_ptl->nic, ib_ptl->ptag,
                (void*) ib_buf_ptr->buf, 
                MCA_PTL_IB_FIRST_FRAG_SIZE,
                &ib_buf_ptr->hndl);
        if(rc != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        IB_PREPARE_SEND_DESC(ib_buf_ptr, 0, 
                MCA_PTL_IB_FIRST_FRAG_SIZE, ib_buf_ptr);
    }

    return OMPI_SUCCESS;
}


/*
 * Process send completions
 *
 */

void mca_ptl_ib_send_frag_send_complete(mca_ptl_ib_module_t *ib_ptl, mca_ptl_ib_send_frag_t* sendfrag)
{
    mca_ptl_base_header_t *hdr;
    mca_pml_base_send_request_t* req = sendfrag->frag_send.frag_request;
    hdr = (mca_ptl_base_header_t *) sendfrag->ib_buf.buf;

    switch(hdr->hdr_common.hdr_type) {
        case MCA_PTL_HDR_TYPE_MATCH:
            if (0 == (hdr->hdr_common.hdr_flags & MCA_PTL_FLAGS_ACK)
                || mca_pml_base_send_request_matched(req)) {

                ib_ptl->super.ptl_send_progress(&ib_ptl->super,
                    sendfrag->frag_send.frag_request,
                    hdr->hdr_rndv.hdr_frag_length);
                if(req->req_cached == false) {
                    OMPI_FREE_LIST_RETURN(&ib_ptl->send_free, 
                        ((ompi_list_item_t *) sendfrag));
                }
            }
            break;

        case MCA_PTL_HDR_TYPE_ACK:

            OMPI_FREE_LIST_RETURN(&ib_ptl->send_free, 
                ((ompi_list_item_t *) sendfrag));
            break;

        case MCA_PTL_HDR_TYPE_FIN:

            ib_ptl->super.ptl_send_progress(&ib_ptl->super,
               sendfrag->frag_send.frag_request,
               hdr->hdr_frag.hdr_frag_length);
            OMPI_FREE_LIST_RETURN(&ib_ptl->send_free, 
               ((ompi_list_item_t *) sendfrag));
            break;
    }
}

