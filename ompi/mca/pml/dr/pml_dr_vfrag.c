/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
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
#include "pml_dr_vfrag.h"
#include "pml_dr_sendreq.h"
#include "orte/mca/errmgr/errmgr.h"

void mca_pml_dr_vfrag_wdog_timeout(int fd, short event, void* vfrag); 
void mca_pml_dr_vfrag_ack_timeout(int fd, short event, void* vfrag);

static void mca_pml_dr_vfrag_construct(mca_pml_dr_vfrag_t* vfrag)
{
    vfrag->vf_send.pval = NULL;
    vfrag->vf_recv.pval = NULL;
    vfrag->vf_id = 0;
    vfrag->vf_idx = 0;
    vfrag->vf_len = 0; 
    vfrag->vf_offset = 0; 
    vfrag->vf_size = 0;
    vfrag->vf_max_send_size = 0;
    vfrag->vf_ack = 0;
    vfrag->vf_mask = 0;
    vfrag->vf_send_cnt = 1;
    vfrag->tv_wdog.tv_sec = mca_pml_dr.timer_wdog_sec;
    vfrag->tv_wdog.tv_usec = mca_pml_dr.timer_wdog_usec;
    vfrag->tv_ack.tv_sec = mca_pml_dr.timer_ack_usec;
    vfrag->tv_ack.tv_usec = mca_pml_dr.timer_ack_usec;
    opal_evtimer_set(&vfrag->ev_wdog, mca_pml_dr_vfrag_wdog_timeout, (void*) vfrag);
    opal_evtimer_set(&vfrag->ev_ack, mca_pml_dr_vfrag_ack_timeout, (void*) vfrag);
}


static void mca_pml_dr_vfrag_destruct(mca_pml_dr_vfrag_t* vfrag)
{

}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_vfrag_t,
    opal_list_item_t,
    mca_pml_dr_vfrag_construct,
    mca_pml_dr_vfrag_destruct
);


/** 
 * The wdog timer expired, better do something about it, like resend the current part of the vfrag 
 */
void mca_pml_dr_vfrag_wdog_timeout(int fd, short event, void* data) 
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*) data;
    mca_pml_dr_send_request_t* sendreq = vfrag->vf_send.pval;
    OPAL_THREAD_LOCK(&ompi_request_lock);
    vfrag->vf_send_cnt++;
    if(vfrag->vf_send_cnt > mca_pml_dr.timer_wdog_max_count) { 
        opal_output(0, "wdog retry count exceeded! %s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }
    vfrag->vf_idx = 1;
    vfrag->vf_mask_processed = 0;
    vfrag->vf_ack = 0;
    opal_list_append(&sendreq->req_retrans, (opal_list_item_t*)vfrag);
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
    mca_pml_dr_send_request_schedule(sendreq);
}

/** 
 * The ack timer expired, better do something about it, like resend the entire vfrag? 
 */
void mca_pml_dr_vfrag_ack_timeout(int fd, short event, void* data) { 
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*) data;
    mca_pml_dr_send_request_t* sendreq = vfrag->vf_send.pval;
    OPAL_THREAD_LOCK(&ompi_request_lock);
    vfrag->vf_send_cnt++;
    if(vfrag->vf_send_cnt > mca_pml_dr.timer_ack_max_count) { 
        opal_output(0, "ack retry count exceeded! %s:%d FATAL", __FILE__, __LINE__);
        orte_errmgr.abort();
    }
    vfrag->vf_idx = 1;
    vfrag->vf_mask_processed = 0;
    vfrag->vf_ack = 0;
    if(0 == vfrag->vf_offset) { /* this is the first part of the message
                                   that we need to resend */
        mca_bml_base_btl_t* bml_btl = sendreq->descriptor->des_context;
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        mca_bml_base_send(bml_btl, sendreq->descriptor, MCA_BTL_TAG_PML);
        
    } else { 
        opal_list_append(&sendreq->req_retrans, (opal_list_item_t*)vfrag);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        mca_pml_dr_send_request_schedule(sendreq);
    }
}


