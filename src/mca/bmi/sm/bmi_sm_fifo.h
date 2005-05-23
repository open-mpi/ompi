#ifndef MCA_BMI_SM_FIFO_H
#define MCA_BMI_SM_FIFO_H

#include "bmi_sm.h"



#define MCA_BMI_SM_FIFO_WRITE(my_smp_rank,peer_smp_rank,frag,rc) \
do { \
    ompi_fifo_t* fifo; \
    fifo=&(mca_bmi_sm_component.fifo[my_smp_rank][peer_smp_rank]); \
 \
    /* thread lock */ \
    if(ompi_using_threads()) \
        ompi_atomic_lock(&fifo->head_lock); \
    if(OMPI_CB_FREE == fifo->head) { \
        /* no queues have been allocated - allocate now */ \
        rc=ompi_fifo_init_same_base_addr( \
            mca_bmi_sm_component.size_of_cb_queue, \
            mca_bmi_sm_component.cb_lazy_free_freq, \
            /* at this stage we are not doing anything with memory \
            * locality */ \
            0,0,0, \
            fifo, mca_bmi_sm_component.sm_mpool); \
        if( rc != OMPI_SUCCESS ) { \
            if(ompi_using_threads()) \
                ompi_atomic_unlock(&(fifo->head_lock)); \
            break; \
        } \
    } \
    \
    /* post fragment */ \
    rc=ompi_fifo_write_to_head_same_base_addr(frag, fifo, \
        mca_bmi_sm_component.sm_mpool); \
    if(  0 <= rc ) { \
        MCA_BMI_SM_SIGNAL_PEER(bmi_peer); \
        rc=OMPI_SUCCESS; \
    } \
    if(ompi_using_threads()) \
        ompi_atomic_unlock(&fifo->head_lock); \
} while(0) 


#endif

