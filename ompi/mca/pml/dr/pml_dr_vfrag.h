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
/**
 * @file
 */
#ifndef OMPI_PML_DR_VFRAG_H_
#define OMPI_PML_DR_VFRAG_H_

#include "ompi_config.h"
#include "opal/event/event.h"
#include "pml_dr.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif


struct mca_pml_dr_vfrag_t {
    opal_list_item_t super;
    ompi_ptr_t vf_send;
    ompi_ptr_t vf_recv;
    uint32_t   vf_id;
    uint16_t   vf_idx;
    uint16_t   vf_len;
    uint8_t    vf_retry_cnt;
    size_t     vf_offset;
    size_t     vf_size;
    size_t     vf_max_send_size;
    uint64_t   vf_ack;
    uint64_t   vf_mask;
    uint64_t   vf_mask_processed;
    uint64_t   vf_retrans;
    struct mca_bml_base_btl_t* bml_btl;

    /* we need a timer for the vfrag for: 
       1) a watchdog timer for local completion of the current 
          operation
       2) a timeout for ACK of the VRAG
    */
    struct timeval tv_wdog;
    struct timeval tv_ack;
    opal_event_t ev_ack;
    opal_event_t ev_wdog;
    uint8_t cnt_wdog;
    uint8_t cnt_ack;
    uint8_t cnt_nack;
};
typedef struct mca_pml_dr_vfrag_t mca_pml_dr_vfrag_t;

OBJ_CLASS_DECLARATION(mca_pml_dr_vfrag_t);

#define MCA_PML_DR_VFRAG_ALLOC(vfrag,rc)                                   \
do {                                                                       \
    opal_list_item_t* item;                                                \
    OMPI_FREE_LIST_WAIT(&mca_pml_dr.vfrags, item, rc);                     \
    vfrag = (mca_pml_dr_vfrag_t*)item;                                     \
} while(0)


#define MCA_PML_DR_VFRAG_RETURN(vfrag)                                     \
do {                                                                       \
    OMPI_FREE_LIST_RETURN(&mca_pml_dr.vfrags, (opal_list_item_t*)vfrag);   \
} while(0)

#define MCA_PML_DR_VFRAG_RESET(vfrag)                                      \
do {                                                                       \
    vfrag->vf_idx = 0;                                                     \
    vfrag->vf_mask_processed = 0;                                          \
    vfrag->vf_ack = 0;                                                     \
    vfrag->vf_retrans = 0;                                                 \
} while(0)
#if 1

#define MCA_PML_DR_VFRAG_WDOG_START(vfrag)                                 \
do {                                                                       \
    opal_event_add(&vfrag->ev_wdog, &vfrag->tv_wdog);                      \
} while(0)                                                                          

#define MCA_PML_DR_VFRAG_WDOG_RESET(vfrag)                                 \
do {                                                                       \
    opal_event_del(&vfrag->ev_wdog);                                       \
    vfrag->tv_wdog.tv_sec =                                                \
          mca_pml_dr.timer_wdog_sec +                                      \
          mca_pml_dr.timer_wdog_sec * mca_pml_dr.timer_wdog_multiplier  *  \
          vfrag->vf_retry_cnt;                                             \
    vfrag->tv_wdog.tv_usec =                                               \
          mca_pml_dr.timer_wdog_usec +                                     \
          mca_pml_dr.timer_wdog_usec * mca_pml_dr.timer_wdog_multiplier  * \
          vfrag->vf_retry_cnt;                                             \
    opal_event_add(&vfrag->ev_wdog, &vfrag->tv_wdog);                      \
} while(0)                                                                          

#define MCA_PML_DR_VFRAG_WDOG_STOP(vfrag)                                  \
do {                                                                       \
   opal_event_del(&vfrag->ev_wdog);                                        \
                                                                           \
} while(0)

#define MCA_PML_DR_VFRAG_ACK_START(vfrag)                                  \
do {                                                                       \
    (vfrag)->tv_ack.tv_sec =                                               \
          mca_pml_dr.timer_ack_sec +                                       \
          mca_pml_dr.timer_ack_sec * mca_pml_dr.timer_ack_multiplier  *    \
          (vfrag)->vf_retry_cnt;                                           \
    (vfrag)->tv_ack.tv_usec =                                              \
          mca_pml_dr.timer_ack_usec +                                      \
          mca_pml_dr.timer_ack_usec * mca_pml_dr.timer_ack_multiplier  *   \
          (vfrag)->vf_retry_cnt;                                           \
    opal_event_add(&(vfrag)->ev_ack, &(vfrag)->tv_ack);                    \
                                                                           \
} while(0)                                                                          

#define MCA_PML_DR_VFRAG_ACK_STOP(vfrag)                                   \
do {                                                                       \
   opal_event_del(&vfrag->ev_ack);                                         \
                                                                           \
} while(0)


#endif

#if 0

#define MCA_PML_DR_VFRAG_WDOG_START(vfrag)
#define MCA_PML_DR_VFRAG_WDOG_RESET(vfrag)
#define MCA_PML_DR_VFRAG_WDOG_STOP(vfrag)
#define MCA_PML_DR_VFRAG_ACK_START(vfrag)
#define MCA_PML_DR_VFRAG_ACK_STOP(vfrag)

#endif 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

