/*
 * Copyright (c) 2013-2016 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_ACK_H
#define BTL_USNIC_ACK_H

#include "opal_config.h"

#include "opal/class/opal_hotel.h"

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_endpoint.h"
#include "btl_usnic_compat.h"

/* Invoke the descriptor callback for a (non-PUT) send frag, updating
 * stats and clearing the _CALLBACK flag in the process. */
#define OPAL_BTL_USNIC_DO_SEND_FRAG_CB(module, send_frag, comment)            \
    do {                                                                      \
        MSGDEBUG1_OUT("%s:%d: %s SEND callback for module=%p frag=%p\n",      \
                      __func__, __LINE__,                                     \
                      (comment), (void *)(module), (void *)(send_frag));      \
        (send_frag)->sf_base.uf_base.des_cbfunc(                              \
            &(module)->super,                                                 \
            (send_frag)->sf_endpoint,                                         \
            &(send_frag)->sf_base.uf_base,                                    \
            OPAL_SUCCESS);                                                    \
        frag->sf_base.uf_base.des_flags &= ~MCA_BTL_DES_SEND_ALWAYS_CALLBACK; \
        ++((module)->stats.pml_send_callbacks);                               \
    } while (0)

#if BTL_VERSION == 30
/* Invoke the descriptor callback for a send frag that was a PUT,
 * updating stats and clearing the _CALLBACK flag in the process. */
#define OPAL_BTL_USNIC_DO_PUT_FRAG_CB(module, send_frag, comment)             \
    do {                                                                      \
        MSGDEBUG1_OUT("%s:%d: %s PUT callback for module=%p frag=%p\n",       \
                      __func__, __LINE__,                                     \
                      (comment), (void *)(module), (void *)(send_frag));      \
        mca_btl_base_rdma_completion_fn_t func =                        \
            (mca_btl_base_rdma_completion_fn_t)                         \
            (send_frag)->sf_base.uf_base.des_cbfunc;                    \
        func(&(module)->super,                                          \
             (send_frag)->sf_endpoint,                                  \
             (send_frag)->sf_base.uf_local_seg[0].seg_addr.pval,        \
             NULL,                                                      \
             (send_frag)->sf_base.uf_base.des_context,                  \
             (send_frag)->sf_base.uf_base.des_cbdata,                   \
             OPAL_SUCCESS);                                             \
        ++((module)->stats.pml_send_callbacks);                         \
    } while (0)
#endif

/*
 * Reap an ACK send that is complete
 */
void opal_btl_usnic_ack_complete(opal_btl_usnic_module_t *module,
                                   opal_btl_usnic_ack_segment_t *ack);


/*
 * Send an ACK
 */
void opal_btl_usnic_ack_send(opal_btl_usnic_module_t *module,
                               opal_btl_usnic_endpoint_t *endpoint);

/*
 * Callback for when a send times out without receiving a
 * corresponding ACK
 */
void opal_btl_usnic_ack_timeout(opal_hotel_t *hotel, int room_num,
                                  void *occupant);

/*
 * Handle an incoming ACK
 */
void opal_btl_usnic_handle_ack(opal_btl_usnic_endpoint_t *endpoint,
                               opal_btl_usnic_seq_t ack_seq);

static inline void
opal_btl_usnic_piggyback_ack(
    opal_btl_usnic_endpoint_t *endpoint,
    opal_btl_usnic_send_segment_t *sseg)
{
    /* If ACK is needed, piggy-back it here and send it on */
    if (endpoint->endpoint_ack_needed) {
        opal_btl_usnic_remove_from_endpoints_needing_ack(endpoint);
        sseg->ss_base.us_btl_header->ack_seq =
            SEQ_DIFF(endpoint->endpoint_next_contig_seq_to_recv, 1);
        sseg->ss_base.us_btl_header->ack_present = 1;
#if MSGDEBUG1
        opal_output(0, "Piggy-backing ACK for sequence %"UDSEQ"\n",
                sseg->ss_base.us_btl_header->ack_seq);
#endif
    } else {
        sseg->ss_base.us_btl_header->ack_present = 0;
    }
}


#endif /* BTL_USNIC_ACK_H */
