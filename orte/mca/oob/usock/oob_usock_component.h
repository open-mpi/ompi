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
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _MCA_OOB_USOCK_COMPONENT_H_
#define _MCA_OOB_USOCK_COMPONENT_H_

#include "orte_config.h"

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/class/opal_bitmap.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/mca/oob/oob.h"
#include "oob_usock_peer.h"
#include "oob_usock.h"

/**
 *  OOB USOCK Component
 */
typedef struct {
    mca_oob_base_component_t  super;          /**< base OOB component */
    int                       max_retries;    /**< max number of retries before declaring peer gone */
    struct sockaddr_un        address;        /**< address of our rendezvous point */
} mca_oob_usock_component_t;

ORTE_MODULE_DECLSPEC extern mca_oob_usock_component_t mca_oob_usock_component;

ORTE_MODULE_DECLSPEC char* mca_oob_usock_state_print(mca_oob_usock_state_t state);
ORTE_MODULE_DECLSPEC void mca_oob_usock_component_set_module(int fd, short args, void *cbdata);
ORTE_MODULE_DECLSPEC void mca_oob_usock_component_lost_connection(int fd, short args, void *cbdata);
ORTE_MODULE_DECLSPEC void mca_oob_usock_component_failed_to_connect(int fd, short args, void *cbdata);
ORTE_MODULE_DECLSPEC mca_oob_usock_peer_t* mca_oob_usock_peer_lookup(const orte_process_name_t *name);
ORTE_MODULE_DECLSPEC void mca_oob_usock_component_cannot_send(int fd, short args, void *cbdata);

#endif /* _MCA_OOB_USOCK_COMPONENT_H_ */
