/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
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
#include "ompi/mca/bml/base/base.h"
#include "orte/mca/errmgr/errmgr.h"

static void mca_pml_dr_vfrag_wdog_timeout(int fd, short event, void* vfrag); 
static void mca_pml_dr_vfrag_ack_timeout(int fd, short event, void* vfrag);

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
    vfrag->vf_mask = 1;
    vfrag->vf_state = 0;
    vfrag->vf_wdog_tv = mca_pml_dr.wdog_timer;
    vfrag->vf_ack_tv = mca_pml_dr.ack_timer;
    vfrag->vf_wdog_cnt = 0;
    vfrag->vf_ack_cnt = 0;
    OBJ_CONSTRUCT(&vfrag->vf_wdog_ev, opal_event_t);
    opal_event.evtimer_set(&vfrag->vf_wdog_ev, mca_pml_dr_vfrag_wdog_timeout, (void*) vfrag);
    OBJ_CONSTRUCT(&vfrag->vf_ack_ev, opal_event_t);
    opal_event.evtimer_set(&vfrag->vf_ack_ev, mca_pml_dr_vfrag_ack_timeout, (void*) vfrag);
}


static void mca_pml_dr_vfrag_destruct(mca_pml_dr_vfrag_t* vfrag)
{
    OBJ_DESTRUCT(&vfrag->vf_wdog_ev);
    OBJ_DESTRUCT(&vfrag->vf_ack_ev);
}


OBJ_CLASS_INSTANCE(
    mca_pml_dr_vfrag_t,
    ompi_free_list_item_t,
    mca_pml_dr_vfrag_construct,
    mca_pml_dr_vfrag_destruct
);


/** 
 * The wdog timer expired, better do something about it, like resend the current part of the vfrag 
 */
static void mca_pml_dr_vfrag_wdog_timeout(int fd, short event, void* data) 
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*) data;
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;

    MCA_PML_DR_DEBUG(0,(0, "%s:%d:%s: wdog timeout: %p vid: %d",
                        __FILE__, __LINE__, __func__, (void*)vfrag, vfrag->vf_id));

    /* update pending counts */
    OPAL_THREAD_ADD_SIZE_T(&sendreq->req_pipeline_depth,-vfrag->vf_pending);
    OPAL_THREAD_ADD64(&vfrag->vf_pending,-vfrag->vf_pending);

    /* check for hung btl */
    if(++vfrag->vf_wdog_cnt == mca_pml_dr.wdog_retry_max) {
        /* declare btl dead */
        if(vfrag->bml_btl->btl) { 
            opal_output(0, "%s:%d:%s: failing BTL: %s", __FILE__, __LINE__, __func__, 
                        vfrag->bml_btl->btl->btl_component->btl_version.mca_component_name);
            mca_pml_dr_sendreq_cleanup_active(vfrag->bml_btl->btl);
            mca_bml.bml_del_btl(vfrag->bml_btl->btl);
        } else { 
            opal_output(0, "%s:%d:%s: failing already failed BTL", __FILE__, __LINE__, __func__);
        }
        mca_pml_dr_vfrag_reset(vfrag);
    } else if(NULL == vfrag->bml_btl->btl) { 
        mca_pml_dr_vfrag_reset(vfrag);
    }

    /* back off watchdog timer */
    vfrag->vf_wdog_tv.tv_sec = 
          mca_pml_dr.wdog_timer.tv_sec + 
          mca_pml_dr.wdog_timer.tv_sec * mca_pml_dr.wdog_timer_multiplier  * 
          vfrag->vf_wdog_cnt;
    vfrag->vf_wdog_tv.tv_usec =
          mca_pml_dr.wdog_timer.tv_usec +
          mca_pml_dr.wdog_timer.tv_usec * mca_pml_dr.wdog_timer_multiplier  *
          vfrag->vf_wdog_cnt;

    /* reschedule vfrag */
    mca_pml_dr_vfrag_reschedule(vfrag);
}


/** 
 * The ack timer expired, better do something about it, like resend the entire vfrag? 
 */
static void mca_pml_dr_vfrag_ack_timeout(int fd, short event, void* data) 
{
    mca_pml_dr_vfrag_t* vfrag = (mca_pml_dr_vfrag_t*) data;
    MCA_PML_DR_DEBUG(0,(0, "%s:%d:%s: ack timeout: %p", 
                        __FILE__, __LINE__, __func__, (void*)vfrag));

    /* stop ack timer */
    MCA_PML_DR_VFRAG_ACK_STOP(vfrag);

    /* check for hung btl */
    if(++vfrag->vf_ack_cnt == mca_pml_dr.ack_retry_max) {
        /* declare btl dead */
        if(vfrag->bml_btl->btl) { 
            opal_output(0, "%s:%d:%s: failing BTL: %s", __FILE__, __LINE__, __func__, 
                        vfrag->bml_btl->btl->btl_component->btl_version.mca_component_name);
            mca_pml_dr_sendreq_cleanup_active(vfrag->bml_btl->btl);
            mca_bml.bml_del_btl(vfrag->bml_btl->btl);
        } else { 
            opal_output(0, "%s:%d:%s: failing already failed BTL", __FILE__, __LINE__, __func__);
        }   
        mca_pml_dr_vfrag_reset(vfrag);
    } else if(NULL == vfrag->bml_btl->btl) { 
        mca_pml_dr_vfrag_reset(vfrag);
    }

    /* back off ack timer */
    vfrag->vf_ack_tv.tv_sec = 
          mca_pml_dr.ack_timer.tv_sec + 
          mca_pml_dr.ack_timer.tv_sec * mca_pml_dr.ack_timer_multiplier  * 
          vfrag->vf_ack_cnt;
    vfrag->vf_ack_tv.tv_usec =
          mca_pml_dr.ack_timer.tv_usec +
          mca_pml_dr.ack_timer.tv_usec * mca_pml_dr.ack_timer_multiplier  *
          vfrag->vf_ack_cnt;

    /* reschedule vfrag */
    mca_pml_dr_vfrag_reschedule(vfrag);
}

/**
 * Vfrag failure - declare btl dead and try to resend on an alternate btl
 */

void mca_pml_dr_vfrag_reset(mca_pml_dr_vfrag_t* vfrag)
{
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;

    /* update counters - give new BTL a fair chance :-) */
    vfrag->vf_ack_cnt = 0;
    vfrag->vf_wdog_cnt = 0;

    /* lookup new bml_btl data structure */
    sendreq->req_endpoint = (mca_pml_dr_endpoint_t*)sendreq->req_send.req_base.req_proc->proc_pml; 

    /* make sure a path is available */
    if(mca_bml_base_btl_array_get_size(&sendreq->req_endpoint->bml_endpoint->btl_eager) == 0 ||
       mca_bml_base_btl_array_get_size(&sendreq->req_endpoint->bml_endpoint->btl_eager) == 0) {
        opal_output(0, "%s:%d:%s: no path to peer", __FILE__, __LINE__, __func__);
        orte_errmgr.abort(-1, NULL);
    }
    if(vfrag->vf_offset == 0) {
        vfrag->bml_btl = mca_bml_base_btl_array_get_next(&sendreq->req_endpoint->bml_endpoint->btl_eager);
    } else {
        vfrag->bml_btl = mca_bml_base_btl_array_get_next(&sendreq->req_endpoint->bml_endpoint->btl_send);
    }
    opal_output(0, "%s:%d:%s: selected new BTL: %s", __FILE__, __LINE__, __func__, 
        vfrag->bml_btl->btl->btl_component->btl_version.mca_component_name);
}


/**
 *  Reschedule vfrag that has timed out 
 */

void mca_pml_dr_vfrag_reschedule(mca_pml_dr_vfrag_t* vfrag)
{
    mca_pml_dr_send_request_t* sendreq = (mca_pml_dr_send_request_t*)vfrag->vf_send.pval;

    /* start wdog timer */
    MCA_PML_DR_VFRAG_WDOG_START(vfrag);

    /* first frag within send request */
    OPAL_THREAD_LOCK(&ompi_request_lock);
    if(vfrag == &sendreq->req_vfrag0) {
        if(vfrag->vf_state & MCA_PML_DR_VFRAG_RNDV) { 
            MCA_PML_DR_SEND_REQUEST_RNDV_PROBE(sendreq, vfrag);
        } else { 
            MCA_PML_DR_SEND_REQUEST_EAGER_RETRY(sendreq, vfrag);
        }
        OPAL_THREAD_UNLOCK(&ompi_request_lock);

    /* reschedule unacked portion of vfrag */
    } else { 
        MCA_PML_DR_SEND_REQUEST_VFRAG_RETRANS(sendreq, vfrag);
        OPAL_THREAD_UNLOCK(&ompi_request_lock);
        mca_pml_dr_send_request_schedule(sendreq);
    }
}

