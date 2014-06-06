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
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef _MCA_OOB_TCP_PING_H_
#define _MCA_OOB_TCP_PING_H_

#include "orte_config.h"

#include "opal/mca/event/event.h"

#include "oob_tcp.h"
#include "oob_tcp_sendrecv.h"

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_process_name_t peer;
} mca_oob_tcp_ping_t;
OBJ_CLASS_DECLARATION(mca_oob_tcp_ping_t);

#define ORTE_ACTIVATE_TCP_PING(p, cbfunc)                               \
    do {                                                                \
        mca_oob_tcp_ping_t *pop;                                        \
        pop = OBJ_NEW(mca_oob_tcp_ping_t);                              \
        pop->peer.jobid = (p)->jobid;                                   \
        pop->peer.vpid = (p)->vpid;                                     \
        opal_event_set(mca_oob_tcp_module.ev_base, &pop->ev, -1,        \
                       OPAL_EV_WRITE, (cbfunc), pop);                   \
        opal_event_set_priority(&pop->ev, ORTE_MSG_PRI);                \
        opal_event_active(&pop->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);

#endif /* _MCA_OOB_TCP_PING_H_ */
