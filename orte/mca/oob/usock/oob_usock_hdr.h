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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_USOCK_HDR_H_
#define _MCA_OOB_USOCK_HDR_H_

#include "orte_config.h"

/* define several internal-only message
 * types this component uses for its own
 * handshake operations, plus one indicating
 * the message came from an external (to
 * this component) source
 */
typedef enum {
    MCA_OOB_USOCK_IDENT,
    MCA_OOB_USOCK_PROBE,
    MCA_OOB_USOCK_PING,
    MCA_OOB_USOCK_USER
} mca_oob_usock_msg_type_t;

/* header for usock msgs */
typedef struct {
    /* the original sender */
    orte_process_name_t origin;
    /* the intended final recipient */
    orte_process_name_t dst;
    /* type of message */
    mca_oob_usock_msg_type_t type;
    /* the rml tag where this message is headed */
    orte_rml_tag_t tag;
    /* number of bytes in message */
    uint32_t nbytes;
} mca_oob_usock_hdr_t;

#endif /* _MCA_OOB_USOCK_HDR_H_ */
