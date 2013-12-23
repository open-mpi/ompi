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
 * Copyright (c) 2010-2011 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_HDR_H_
#define _MCA_OOB_TCP_HDR_H_

#include "orte_config.h"

/* define several internal-only message
 * types this component uses for its own
 * handshake operations, plus one indicating
 * the message came from an external (to
 * this component) source
 */
typedef enum {
    MCA_OOB_TCP_IDENT,
    MCA_OOB_TCP_PROBE,
    MCA_OOB_TCP_PING,
    MCA_OOB_TCP_USER
} mca_oob_tcp_msg_type_t;

/* header for tcp msgs */
typedef struct {
    /* the originator of the message - if we are routing,
     * it could be someone other than me
     */
    orte_process_name_t     origin;
    /* the intended final recipient - if we don't have
     * a path directly to that process, then we will
     * attempt to route. If we have no route to that
     * process, then we should have rejected the message
     * and let some other module try to send it
     */
    orte_process_name_t     dst;
    /* type of message */
    mca_oob_tcp_msg_type_t type;
    /* the rml tag where this message is headed */
    orte_rml_tag_t tag;
    /* number of bytes in message */
    uint32_t nbytes;
} mca_oob_tcp_hdr_t;
/**
 * Convert the message header to host byte order
 */
#define MCA_OOB_TCP_HDR_NTOH(h)                \
    ORTE_PROCESS_NAME_NTOH((h)->origin);        \
    ORTE_PROCESS_NAME_NTOH((h)->dst);           \
    (h)->type = ntohl((h)->type);               \
    (h)->tag = ORTE_RML_TAG_NTOH((h)->tag);     \
    (h)->nbytes = ntohl((h)->nbytes);

/**
 * Convert the message header to network byte order
 */
#define MCA_OOB_TCP_HDR_HTON(h)                \
    ORTE_PROCESS_NAME_HTON((h)->origin);        \
    ORTE_PROCESS_NAME_HTON((h)->dst);           \
    (h)->type = htonl((h)->type);               \
    (h)->tag = ORTE_RML_TAG_HTON((h)->tag);     \
    (h)->nbytes = htonl((h)->nbytes);

#endif /* _MCA_OOB_TCP_HDR_H_ */
