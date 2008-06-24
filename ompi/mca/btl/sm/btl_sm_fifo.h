#ifndef MCA_BTL_SM_FIFO_H
#define MCA_BTL_SM_FIFO_H

#include "btl_sm.h"
#include "btl_sm_endpoint.h"

#define MCA_BTL_SM_FIFO_WRITE(endpoint_peer, my_smp_rank,                   \
        peer_smp_rank, hdr, resend, rc)                                     \
do {                                                                        \
    ompi_fifo_t* fifo;                                                      \
    fifo=&(mca_btl_sm_component.fifo[peer_smp_rank][my_smp_rank]);          \
                                                                            \
    /* thread lock */                                                       \
    if(opal_using_threads())                                                \
        opal_atomic_lock(fifo->head_lock);                                  \
    /* post fragment */                                                     \
    if(OMPI_CB_ERROR ==                                                     \
       ompi_cb_fifo_write_to_head(hdr, &fifo->head->cb_fifo)) {             \
        rc = OMPI_ERR_RESOURCE_BUSY;                                        \
    } else {                                                                \
        MCA_BTL_SM_SIGNAL_PEER(endpoint_peer);                              \
        rc = OMPI_SUCCESS;                                                  \
    }                                                                       \
    if(opal_using_threads())                                                \
        opal_atomic_unlock(fifo->head_lock);                                \
} while(0)

#endif
