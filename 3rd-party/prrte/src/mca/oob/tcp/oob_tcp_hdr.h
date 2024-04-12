/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_HDR_H_
#define _MCA_OOB_TCP_HDR_H_

#include "prte_config.h"

/* define several internal-only message
 * types this component uses for its own
 * handshake operations, plus one indicating
 * the message came from an external (to
 * this component) source
 */
typedef uint8_t prte_oob_tcp_msg_type_t;

#define MCA_OOB_TCP_IDENT 1
#define MCA_OOB_TCP_PROBE 2
#define MCA_OOB_TCP_PING  3
#define MCA_OOB_TCP_USER  4

#define PRTE_MAX_RTD_SIZE 31

/* header for tcp msgs */
typedef struct {
    /* the originator of the message - if we are routing,
     * it could be someone other than me
     */
    pmix_proc_t origin;
    /* the intended final recipient - if we don't have
     * a path directly to that process, then we will
     * attempt to route. If we have no route to that
     * process, then we should have rejected the message
     * and let some other module try to send it
     */
    pmix_proc_t dst;
    /* the rml tag where this message is headed */
    prte_rml_tag_t tag;
    /* the seq number of this message */
    uint32_t seq_num;
    /* number of bytes in message */
    uint32_t nbytes;
    /* type of message */
    prte_oob_tcp_msg_type_t type;
    /* routed module to be used */
    char routed[PRTE_MAX_RTD_SIZE + 1];
} prte_oob_tcp_hdr_t;
/**
 * Convert the message header to host byte order
 */
#define MCA_OOB_TCP_HDR_NTOH(h)                 \
    (h)->origin.rank = ntohl((h)->origin.rank); \
    (h)->dst.rank = ntohl((h)->dst.rank);       \
    (h)->tag = PRTE_RML_TAG_NTOH((h)->tag);     \
    (h)->nbytes = ntohl((h)->nbytes);

/**
 * Convert the message header to network byte order
 */
#define MCA_OOB_TCP_HDR_HTON(h)                 \
    (h)->origin.rank = htonl((h)->origin.rank); \
    (h)->dst.rank = htonl((h)->dst.rank);       \
    (h)->tag = PRTE_RML_TAG_HTON((h)->tag);     \
    (h)->nbytes = htonl((h)->nbytes);

#endif /* _MCA_OOB_TCP_HDR_H_ */
