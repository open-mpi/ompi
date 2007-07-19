#ifndef MCA_BTL_SM_FIFO_H
#define MCA_BTL_SM_FIFO_H

#include "btl_sm.h"
#include "btl_sm_endpoint.h"

#define MCA_BTL_SM_FIFO_WRITE(endpoint_peer, my_smp_rank,peer_smp_rank,frag,rc) \
do { \
    ompi_fifo_t* fifo; \
    fifo=&(mca_btl_sm_component.fifo[my_smp_rank][peer_smp_rank]); \
 \
    /* thread lock */ \
    if(opal_using_threads()) \
        opal_atomic_lock(&fifo->head_lock); \
    if(OMPI_CB_FREE == fifo->head) { \
        /* no queues have been allocated - allocate now */ \
        rc=ompi_fifo_init_same_base_addr( \
            mca_btl_sm_component.size_of_cb_queue, \
            mca_btl_sm_component.cb_lazy_free_freq, \
            /* at this stage we are not doing anything with memory \
            * locality */ \
            0,0,0, \
            fifo, mca_btl_sm_component.sm_mpool); \
        if( rc != OMPI_SUCCESS ) { \
            if(opal_using_threads()) \
                opal_atomic_unlock(&(fifo->head_lock)); \
            break; \
        } \
    } \
    \
    /* post fragment */ \
    while(ompi_fifo_write_to_head_same_base_addr(frag, fifo, \
        mca_btl_sm_component.sm_mpool) != OMPI_SUCCESS) \
        opal_progress(); \
    MCA_BTL_SM_SIGNAL_PEER(endpoint_peer); \
    rc=OMPI_SUCCESS; \
    if(opal_using_threads()) \
        opal_atomic_unlock(&fifo->head_lock); \
} while(0) 


#endif

