/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 *               2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef _MCA_OOB_UD_PING_H_
#define _MCA_OOB_UD_PING_H_

#include "oob_ud_component.h"

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_process_name_t peer;
} mca_oob_ud_ping_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_ping_t);

#define ORTE_ACTIVATE_UD_PING(p, cbfunc)                                \
    do {                                                                \
        mca_oob_ud_ping_t *pop;                                         \
        pop = OBJ_NEW(mca_oob_ud_ping_t);                               \
        pop->peer.jobid = (p)->jobid;                                   \
        pop->peer.vpid = (p)->vpid;                                     \
        opal_event_set(mca_oob_ud_module.ev_base, &pop->ev, -1,         \
                       OPAL_EV_WRITE, (cbfunc), pop);                   \
        opal_event_set_priority(&pop->ev, ORTE_MSG_PRI);                \
        opal_event_active(&pop->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);

#endif /* _MCA_OOB_UD_PING_H_ */
