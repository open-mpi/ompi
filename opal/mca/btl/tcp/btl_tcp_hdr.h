/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2020 The University of Tennessee and The University
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

#ifndef MCA_BTL_TCP_HDR_H
#define MCA_BTL_TCP_HDR_H

#include "opal_config.h"
#include "btl_tcp.h"
#include "opal/mca/btl/base/base.h"

BEGIN_C_DECLS

/**
 * TCP header.
 */

#define MCA_BTL_TCP_HDR_TYPE_SEND 1
#define MCA_BTL_TCP_HDR_TYPE_PUT  2
#define MCA_BTL_TCP_HDR_TYPE_GET  3
#define MCA_BTL_TCP_HDR_TYPE_FIN  4
/* The MCA_BTL_TCP_HDR_TYPE_FIN is a special kind of message sent during normal
 * connexion closing. Before the endpoint closes the socket, it performs a
 * 1-way handshake by sending a FIN message in the socket. This lets the other
 * end of the connexion discriminate between the case in which the peer has
 * closed intentionnally (e.g., during MPI_FINALIZE), or unintentionally (e.g.,
 * as the result of some transmission or process failure).
 * The process initiating the close sends the FIN message but does not wait
 * for a 2-way handshake and closes the socket immediately. Thus, the recipient
 * of a FIN message can simply close the socket and mark the endpoint as closed
 * without error, and without answering a FIN message itself.
 */

struct mca_btl_tcp_hdr_t {
    mca_btl_base_header_t base;
    uint8_t type;
    uint16_t count;
    uint32_t size;
};
typedef struct mca_btl_tcp_hdr_t mca_btl_tcp_hdr_t;

#define MCA_BTL_TCP_HDR_HTON(hdr)     \
    do {                              \
        hdr.count = htons(hdr.count); \
        hdr.size = htonl(hdr.size);   \
    } while (0)

#define MCA_BTL_TCP_HDR_NTOH(hdr)     \
    do {                              \
        hdr.count = ntohs(hdr.count); \
        hdr.size = ntohl(hdr.size);   \
    } while (0)

END_C_DECLS
#endif
