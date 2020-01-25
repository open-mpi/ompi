/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
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
#ifndef MCA_BTL_TCP_ADDR_H
#define MCA_BTL_TCP_ADDR_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif


/**
 * Modex address structure.
 *
 * One of these structures will be sent for every btl module in use by
 * the local BTL TCP component.
 */
struct mca_btl_tcp_modex_addr_t {
    uint8_t     addr[16];       /* endpoint address.  for addr_family
                                   of MCA_BTL_TCP_AF_INET, only the
                                   first 4 bytes have meaning. */
    uint32_t    addr_ifkindex;  /* endpoint kernel index */
    uint16_t    addr_port;      /* endpoint listen port */
    uint8_t     addr_family;    /* endpoint address family.  Note that
                                   this is
                                   MCA_BTL_TCP_AF_{INET,INET6}, not
                                   the traditional
                                   AF_INET/AF_INET6. */
    uint8_t     padding[1];     /* padd out to an 8-byte word */
};
typedef struct mca_btl_tcp_modex_addr_t mca_btl_tcp_modex_addr_t;


/**
 * Remote peer address structure
 *
 * One of these structures will be allocated for every remote endpoint
 * associated with a remote proc.  The data is pulled from the
 * mca_btl_tcp_modex_addr_t structure, except for the addr_inuse
 * field, which is local.
 */
struct mca_btl_tcp_addr_t {
    union {
        struct in_addr  addr_inet;      /* IPv6 listen address */
#if OPAL_ENABLE_IPV6
        struct in6_addr addr_inet6;     /* IPv6 listen address */
#endif
    } addr_union;
    in_port_t   addr_port;     /**< listen port */
    int         addr_ifkindex; /**< remote interface index assigned with
                                    this address */
    uint8_t     addr_family;   /**< AF_INET or AF_INET6 */
    bool        addr_inuse;    /**< local meaning only */
};
typedef struct mca_btl_tcp_addr_t mca_btl_tcp_addr_t;

#define MCA_BTL_TCP_AF_INET     0
#define MCA_BTL_TCP_AF_INET6    1

#endif

