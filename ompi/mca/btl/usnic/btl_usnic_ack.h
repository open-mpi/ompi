/*
 * Copyright (c) 2013-2014 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_ACK_H
#define BTL_USNIC_ACK_H

#include "ompi_config.h"

#include "opal/class/opal_hotel.h"

#include "btl_usnic.h"
#include "btl_usnic_frag.h"
#include "btl_usnic_endpoint.h"

/* Invoke the descriptor callback for the frag, updating stats and clearing the
 * _CALLBACK flag in the process. */
#define OMPI_BTL_USNIC_DO_SEND_FRAG_CB(module, send_frag, comment)            \
    do {                                                                      \
        MSGDEBUG1_OUT("%s:%d: %s send callback for module=%p frag=%p\n",      \
                      __func__, __LINE__,                                     \
                      (comment), (void *)(module), (void *)(send_frag));      \
        (send_frag)->sf_base.uf_base.des_cbfunc(                              \
            &(module)->super,                                                 \
            (send_frag)->sf_endpoint,                                         \
            &(send_frag)->sf_base.uf_base,                                    \
            OMPI_SUCCESS);                                                    \
        frag->sf_base.uf_base.des_flags &= ~MCA_BTL_DES_SEND_ALWAYS_CALLBACK; \
        ++((module)->stats.pml_send_callbacks);                               \
    } while (0)

/*
 * Reap an ACK send that is complete 
 */
void ompi_btl_usnic_ack_complete(ompi_btl_usnic_module_t *module,
                                   ompi_btl_usnic_ack_segment_t *ack);


/*
 * Send an ACK
 */
void ompi_btl_usnic_ack_send(ompi_btl_usnic_module_t *module,
                               ompi_btl_usnic_endpoint_t *endpoint);

/*
 * Callback for when a send times out without receiving a
 * corresponding ACK
 */
void ompi_btl_usnic_ack_timeout(opal_hotel_t *hotel, int room_num, 
                                  void *occupant);

/*
 * Handle an incoming ACK
 */
void ompi_btl_usnic_handle_ack(ompi_btl_usnic_endpoint_t *endpoint,
                               ompi_btl_usnic_seq_t ack_seq);

static inline void
ompi_btl_usnic_piggyback_ack(
    ompi_btl_usnic_endpoint_t *endpoint,
    ompi_btl_usnic_send_segment_t *sseg)
{
    /* If ACK is needed, piggy-back it here and send it on */
    if (endpoint->endpoint_ack_needed) {
        ompi_btl_usnic_remove_from_endpoints_needing_ack(endpoint);
        sseg->ss_base.us_btl_header->ack_seq = 
            endpoint->endpoint_next_contig_seq_to_recv - 1;
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
