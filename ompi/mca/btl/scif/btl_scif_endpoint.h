/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2013      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BTL_SCIF_ENDPOINT_H 
#define MCA_BTL_SCIF_ENDPOINT_H

#include "btl_scif.h"

typedef enum mca_btl_scif_endpoint_state_t {
    MCA_BTL_SCIF_EP_STATE_INIT,
    MCA_BTL_SCIF_EP_STATE_CONNECTING,
    MCA_BTL_SCIF_EP_STATE_CONNECTED
} mca_btl_scif_endpoint_state_t;

typedef struct mca_btl_scif_endpoint_buffer_t {
    unsigned char *buffer;
    off_t scif_offset;
    unsigned int start, end;
    uint32_t *startp, *endp;
} mca_btl_scif_endpoint_buffer_t;

typedef struct mca_btl_base_endpoint_t {
    opal_list_item_t super;
    mca_btl_scif_module_t *btl;

    /* location in the module endpoints array */
    int id;

    opal_mutex_t lock;

    /* scif endpoint */
    scif_epd_t scif_epd;

    /* connection information */
    struct scif_portID port_id;

    /* buffer information */
    mca_btl_scif_endpoint_buffer_t send_buffer;
    mca_btl_scif_endpoint_buffer_t recv_buffer;

    /* current connect state */
    mca_btl_scif_endpoint_state_t state;

    /* frags waiting for resources */
    opal_list_t frag_wait_list;

    /* associated process */
    ompi_proc_t *peer_proc;

#if defined(SCIF_USE_SEQ)
    uint32_t seq_next;
    uint32_t seq_expected;
#endif
} mca_btl_base_endpoint_t;

typedef mca_btl_base_endpoint_t  mca_btl_scif_endpoint_t;

OBJ_CLASS_DECLARATION(mca_btl_scif_endpoint_t);

int mca_btl_scif_ep_connect (mca_btl_scif_endpoint_t *ep);
int mca_btl_scif_ep_connect_start_passive (void);

static inline int mca_btl_scif_ep_init (mca_btl_scif_endpoint_t *endpoint,
                                        mca_btl_scif_module_t *btl,
                                        ompi_proc_t *peer_proc) {
    mca_btl_scif_modex_t *modex;
    size_t msg_size;
    int rc;

    OBJ_CONSTRUCT(endpoint, mca_btl_scif_endpoint_t);
    endpoint->state = MCA_BTL_SCIF_EP_STATE_INIT;

    rc = ompi_modex_recv (&mca_btl_scif_component.super.btl_version, peer_proc,
                          (void **) &modex, &msg_size);
    if (OMPI_SUCCESS != rc) {
        return rc;
    }
    assert (msg_size == sizeof (endpoint->port_id));

    endpoint->port_id = modex->port_id;
    endpoint->peer_proc = peer_proc;
    endpoint->btl = btl;

#if defined(SCIF_USE_SEQ)
    endpoint->seq_next = 0x00001010;
    endpoint->seq_expected = 0x00001010;
#endif

    free (modex);

    return OMPI_SUCCESS;
}

static inline int mca_btl_scif_ep_release (mca_btl_scif_endpoint_t *ep)
{
    OBJ_DESTRUCT(ep);
    return OMPI_SUCCESS;
}

#endif /* MCA_BTL_SCIF_ENDPOINT_H */
