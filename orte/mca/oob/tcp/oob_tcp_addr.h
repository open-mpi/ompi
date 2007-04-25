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
/** @file:
 *
 *  Contains header used by tcp oob.
 */

#ifndef _MCA_OOB_TCP_ADDR_H_
#define _MCA_OOB_TCP_ADDR_H_

#include "orte_config.h"
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#include "orte/dss/dss.h"
#include "opal/class/opal_object.h"
#include "orte/mca/ns/ns_types.h"
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define MCA_OOB_TCP_ADDR_UNCLASSIFIED	0  /* we don't know anything */
#define MCA_OOB_TCP_ADDR_MATCHED	1  /* peer has IP on the same LAN */
#define MCA_OOB_TCP_ADDR_IPV6		2  /* peer has an IPv6 address */
#define MCA_OOB_TCP_ADDR_IPV4public	4  /* peer has public IPv4 address */

#define MCA_OOB_TCP_ADDR_TYPE_AFINET   0x01
#if OPAL_WANT_IPV6
# define MCA_OOB_TCP_ADDR_TYPE_AFINET6 0x02
#endif

/**
 * Address info published to registry
 */
struct mca_oob_tcp_addr_t {
    opal_object_t super;
    orte_process_name_t addr_name;
    orte_std_cntr_t addr_count;               
    orte_std_cntr_t addr_next;
    orte_std_cntr_t addr_alloc;
    orte_std_cntr_t addr_matched;/* status of already tried address classes */
#if OPAL_WANT_IPV6
    struct sockaddr_in6* addr_inet;
#else
    struct sockaddr_in* addr_inet;    
#endif
};
typedef struct mca_oob_tcp_addr_t mca_oob_tcp_addr_t;

OBJ_CLASS_DECLARATION(mca_oob_tcp_addr_t);

/**
 * Unpack the contact information posted by the peer.
 */

mca_oob_tcp_addr_t* mca_oob_tcp_addr_unpack(orte_buffer_t*);

/**
 * Pack this hosts addressing info into a buffer for posting
 * into the registry.
 */

int mca_oob_tcp_addr_pack(orte_buffer_t*);

/**
 *
 */

#if OPAL_WANT_IPV6
int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t*, const struct sockaddr_in6*);
#else
int mca_oob_tcp_addr_insert(mca_oob_tcp_addr_t*, const struct sockaddr_in*);
#endif

/**
 * 
 */
 
int mca_oob_tcp_addr_get_next(mca_oob_tcp_addr_t*, struct sockaddr_storage*);

/**
 * Returns true if the given address is a public IPv4 address.
 */
bool mca_oob_tcp_addr_isipv4public(struct sockaddr_storage*);


#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif 

