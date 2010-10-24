/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
/**
 * @file
 */
#ifndef OMPI_PML_DR_VFRAG_H_
#define OMPI_PML_DR_VFRAG_H_

#include "ompi_config.h"
#include "opal/mca/event/event.h"
#include "opal/types.h"
#include "pml_dr.h"

BEGIN_C_DECLS

#define MCA_PML_DR_VFRAG_NACKED  0x01
#define MCA_PML_DR_VFRAG_RNDV    0x02
#define MCA_PML_DR_VFRAG_RETRANS 0x04

struct mca_pml_dr_vfrag_t {
    ompi_free_list_item_t super;
    ompi_ptr_t vf_send;
    ompi_ptr_t vf_recv;
    uint32_t   vf_id;
    uint16_t   vf_idx;
    uint16_t   vf_len;
    size_t     vf_offset;
    size_t     vf_size;
    size_t     vf_max_send_size;
    uint64_t   vf_ack;
    uint64_t   vf_mask;
    int64_t    vf_pending;
    uint32_t   vf_state;
    struct mca_bml_base_btl_t* bml_btl;

    /* we need a timer for the vfrag for: 
       1) a watchdog timer for local completion of the current 
          operation
       2) a timeout for ACK of the VRAG
    */
    struct timeval vf_wdog_tv;
    opal_event_t   vf_wdog_ev;
    uint8_t        vf_wdog_cnt;

    struct timeval vf_ack_tv;
    opal_event_t   vf_ack_ev;
    uint8_t        vf_ack_cnt;
};
typedef struct mca_pml_dr_vfrag_t mca_pml_dr_vfrag_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_vfrag_t);

#define MCA_PML_DR_VFRAG_ALLOC(vfrag,rc)                                   \
do {                                                                       \
    ompi_free_list_item_t* item;                                           \
    OMPI_FREE_LIST_WAIT(&mca_pml_dr.vfrags, item, rc);                     \
    vfrag = (mca_pml_dr_vfrag_t*)item;                                     \
} while(0)


#define MCA_PML_DR_VFRAG_RETURN(vfrag)                                     \
do {                                                                       \
    OMPI_FREE_LIST_RETURN(&mca_pml_dr.vfrags,                              \
                          (ompi_free_list_item_t*)vfrag);                  \
} while(0)

#define MCA_PML_DR_VFRAG_INIT(vfrag)                                       \
do {                                                                       \
    (vfrag)->vf_idx = 0;                                                   \
    (vfrag)->vf_ack = 0;                                                   \
    (vfrag)->vf_wdog_cnt = 0;                                              \
    (vfrag)->vf_ack_cnt = 0;                                               \
    (vfrag)->vf_recv.pval = NULL;                                          \
    (vfrag)->vf_state = 0;                                                 \
    (vfrag)->vf_pending = 0;                                               \
    (vfrag)->vf_wdog_tv = mca_pml_dr.wdog_timer;                           \
    (vfrag)->vf_ack_tv = mca_pml_dr.ack_timer;                             \
} while(0)


/*
 * Watchdog Timer
 */ 

#define MCA_PML_DR_VFRAG_WDOG_START(vfrag)                                 \
do {                                                                       \
    opal_event.add(&(vfrag)->vf_wdog_ev, &(vfrag)->vf_wdog_tv);            \
} while(0)                                                                          

#define MCA_PML_DR_VFRAG_WDOG_STOP(vfrag)                                  \
do {                                                                       \
   opal_event.del(&(vfrag)->vf_wdog_ev);                                   \
} while(0)

#define MCA_PML_DR_VFRAG_WDOG_RESET(vfrag)                                 \
do {                                                                       \
    opal_event.del(&(vfrag)->vf_wdog_ev);                                  \
    opal_event.add(&(vfrag)->vf_wdog_ev, &vfrag->vf_wdog_tv);              \
} while(0)                                                                          


/*
 * Ack Timer
 */

#define MCA_PML_DR_VFRAG_ACK_START(vfrag)                                  \
do {                                                                       \
    opal_event.add(&(vfrag)->vf_ack_ev, &(vfrag)->vf_ack_tv);              \
} while(0)                                                                          

#define MCA_PML_DR_VFRAG_ACK_STOP(vfrag)                                   \
do {                                                                       \
   opal_event.del(&vfrag->vf_ack_ev);                                      \
} while(0)

#define MCA_PML_DR_VFRAG_ACK_RESET(vfrag)                                  \
do {                                                                       \
    MCA_PML_DR_VFRAG_ACK_STOP(vfrag);                                      \
    MCA_PML_DR_VFRAG_ACK_START(vfrag);                                     \
} while(0)                                                                          


/**
 * Reset a VFRAG to use a new BTL
 */

void mca_pml_dr_vfrag_reset(mca_pml_dr_vfrag_t*);

/**
 * Reschedule a vfrag that has timed out
 */

void mca_pml_dr_vfrag_reschedule(mca_pml_dr_vfrag_t*);

END_C_DECLS
#endif

