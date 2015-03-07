/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014 Mellanox Technologies, Inc.
 *                    All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#if !defined(MCA_OOB_UD_SEND_H)
#define MCA_OOB_UD_SEND_H

#include "oob_ud_component.h"

#define min(a,b) ((a) < (b) ? (a) : (b))

#define MCA_OOB_UD_IOV_SIZE(msg, size)                                  \
    do {                                                                \
        if (msg->iov != NULL) {                                         \
            int i;                                                      \
            for (i = 0, (size) = 0 ; i < (msg->count) ; ++i) {          \
                (size) += (msg->iov)[i].iov_len;                        \
            }                                                           \
        } else {                                                        \
            (size) = msg->buffer->bytes_used;                           \
        }                                                               \
    } while (0);

/* State machine for processing message */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_rml_send_t *msg;
} mca_oob_ud_msg_op_t;
OBJ_CLASS_DECLARATION(mca_oob_ud_msg_op_t);

#define ORTE_ACTIVATE_UD_POST_SEND(ms, cbfunc)                          \
    do {                                                                \
        mca_oob_ud_msg_op_t *mop;                                       \
        opal_output_verbose(5, orte_oob_base_framework.framework_output,\
                            "%s:[%s:%d] post send to %s",               \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__,                         \
                            ORTE_NAME_PRINT(&((ms)->dst)));             \
        mop = OBJ_NEW(mca_oob_ud_msg_op_t);                             \
        mop->msg = (ms);                                                \
        opal_event_set(mca_oob_ud_module.ev_base, &mop->ev, -1,         \
                       OPAL_EV_WRITE, (cbfunc), mop);                   \
        opal_event_set_priority(&mop->ev, ORTE_MSG_PRI);                \
        opal_event_active(&mop->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);
#endif
