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

#ifndef MCA_BMI_IB_SEND_FRAG_H
#define MCA_BMI_IB_SEND_FRAG_H

#include "ompi_config.h"
#include "mca/bmi/base/bmi_base_sendreq.h"
#include "mca/bmi/base/bmi_base_sendfrag.h"

#include "bmi_ib_priv.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif
OBJ_CLASS_DECLARATION(mca_bmi_ib_send_frag_t);

typedef enum { 
    MCA_BMI_IB_FRAG_SEND, 
    MCA_BMI_IB_FRAG_PUT, 
    MCA_BMI_IB_FRAG_GET, 
    MCA_BMI_IB_FRAG_ACK 
} mca_bmi_ib_frag_type_t; 

/**
 * IB send fragment derived type.
 */
struct mca_bmi_ib_frag_t {
    mca_bmi_base_descriptor_t base; 
    mca_bmi_base_segment_t segment; 
    struct mca_bmi_base_endpoint_t *endpoint; 
    mca_bmi_ib_frag_type_t type; 
    mca_bmi_base_tag_t tag; 
    
    size_t size; 
    int rc; 
    bool                                     frag_ack_pending;
}; 
typedef struct mca_bmi_ib_frag_t mca_bmi_ib_frag_t; 
    
    

/*
 * Allocate an IB send descriptor
 *
 */
#define MCA_BMI_IB_FRAG_ALLOC1(frag, rc)                               \
{                                                                      

    ompi_list_item_t *item; 
    OMPI_FREE_LIST_WAIT(&mca_bmi_ib_module.ib_frags1, item, rc); 
    frag = (mca_bmi_ib_frag_t*) item; 
    
}


int mca_bmi_ib_send_frag_register(mca_bmi_ib_module_t *ib_bmi)
{
    int i, rc, num_send_frags;
    ompi_list_item_t *item;
    ompi_free_list_t *flist = &ib_bmi->ib_frags1;
    ib_buffer_t *ib_buf_ptr;
    mca_bmi_ib_frag_t *ib_frag;

    num_send_frags = ompi_list_get_size(&(flist->super));
    item = ompi_list_get_first(&((flist)->super));

    /* Register the buffers */
    for(i = 0; i < num_send_frags; 
            item = ompi_list_get_next(item), i++) {

        ib_send_frag = (mca_bmi_ib_send_frag_t *) item;

        ib_send_frag->frag_progressed = 0;

        ib_buf_ptr = (ib_buffer_t *) &ib_send_frag->ib_buf;

        rc = mca_bmi_ib_register_mem(ib_bmi->nic, ib_bmi->ptag,
                (void*) ib_buf_ptr->buf, 
                MCA_BMI_IB_FIRST_FRAG_SIZE,
                &ib_buf_ptr->hndl);
        if(rc != OMPI_SUCCESS) {
            return OMPI_ERROR;
        }

        IB_PREPARE_SEND_DESC(ib_buf_ptr, 0, 
                MCA_BMI_IB_FIRST_FRAG_SIZE, ib_buf_ptr);
    }

    return OMPI_SUCCESS;
}




struct mca_bmi_ib_module_t;

mca_bmi_ib_send_frag_t* mca_bmi_ib_alloc_send_frag(
        struct mca_bmi_ib_module_t* ib_bmi,
        mca_bmi_base_send_request_t* request);

int  mca_bmi_ib_send_frag_register(struct mca_bmi_ib_module_t *bmi);
void mca_bmi_ib_send_frag_send_complete(struct mca_bmi_ib_module_t *bmi, mca_bmi_ib_send_frag_t*);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif
