/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

/* *****************************************************************************
 *  General purpose routines.
 * ***************************************************************************
 */


#include "btl_sctp_utils.h"

/**
 * struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_frag
 * --------------------------------------------------------
 *  Returns a sockaddr_in struct associated with the mca_btl_sctp_frag_t *frag.
 */
struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_frag(struct mca_btl_sctp_frag_t *frag) {
    struct sockaddr_in btl_sockaddr;
    bzero(&btl_sockaddr, sizeof(struct sockaddr_in));
    btl_sockaddr.sin_family = AF_INET;
    btl_sockaddr.sin_port = frag->endpoint->endpoint_addr->addr_port;
    btl_sockaddr.sin_addr.s_addr = frag->endpoint->endpoint_addr->addr_inet.s_addr;

    return btl_sockaddr;
}

/**
 * struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_endpoint
 * ------------------------------------------------------------
 *  Returns a sockaddr_in struct associated with the mca_btl_base_endpoint_t *ep.
 */
struct sockaddr_in mca_btl_sctp_utils_sockaddr_from_endpoint(struct mca_btl_base_endpoint_t *ep) {
    struct sockaddr_in btl_sockaddr;
    bzero(&btl_sockaddr, sizeof(struct sockaddr_in));
    btl_sockaddr.sin_family = AF_INET;
    btl_sockaddr.sin_port = ep->endpoint_addr->addr_port;
    btl_sockaddr.sin_addr.s_addr = ep->endpoint_addr->addr_inet.s_addr;

    return btl_sockaddr;
}
