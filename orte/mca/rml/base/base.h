/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2014 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * RML Framework maintenence interface
 *
 * Interface for starting / stopping / controlling the RML framework,307
 * as well as support for modifying RML datatypes.
 *
 * @note The only RML datatype exposed to the user is the RML tag.
 * This will always be an integral value, so the only datatype support
 * really required is the internal DSS functions for packing /
 * unpacking / comparing tags.  The user should never need to deal
 * with these.
 */

#ifndef MCA_RML_BASE_H
#define MCA_RML_BASE_H

#include "orte_config.h"

#include "opal/dss/dss_types.h"
#include "orte/mca/mca.h"
#include "opal/util/timings.h"
#include "opal/class/opal_pointer_array.h"

#include "orte/runtime/orte_globals.h"
#include "orte/mca/routed/routed.h"

#include "orte/mca/rml/rml.h"


BEGIN_C_DECLS

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_rml_base_framework;
/* select a component */
ORTE_DECLSPEC int orte_rml_base_select(void);

/*
 *  globals that might be needed
 */

/* a global struct containing framework-level values */
typedef struct {
    opal_list_t posted_recvs;
    opal_list_t unmatched_msgs;
    int max_retries;
#if OPAL_ENABLE_TIMING
    bool timing;
#endif
} orte_rml_base_t;
ORTE_DECLSPEC extern orte_rml_base_t orte_rml_base;


/* structure to send RML messages - used internally */
typedef struct {
    opal_list_item_t super;
    orte_process_name_t dst;     // targeted recipient
    orte_process_name_t origin;
    int status;                  // returned status on send
    orte_rml_tag_t tag;          // targeted tag
    int retries;                 // #times we have tried to send it

    /* user's send callback functions and data */
    union {
        orte_rml_callback_fn_t        iov;
        orte_rml_buffer_callback_fn_t buffer;
    } cbfunc;
    void *cbdata;

    /* pointer to the user's iovec array */
    struct iovec *iov;
    int count;
    /* pointer to the user's buffer */
    opal_buffer_t *buffer;
    /* msg seq number */
    uint32_t seq_num;
    /* pointer to raw data for cross-transport
     * transfers
     */
    char *data;
} orte_rml_send_t;
OBJ_CLASS_DECLARATION(orte_rml_send_t);

/* define an object for transferring send requests to the event lib */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    orte_rml_send_t send;
} orte_rml_send_request_t;
OBJ_CLASS_DECLARATION(orte_rml_send_request_t);

/* structure to recv RML messages - used internally */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    orte_process_name_t sender;  // sender
    orte_rml_tag_t tag;          // targeted tag
    uint32_t seq_num;             //sequence number
    struct iovec iov;            // the recvd data
} orte_rml_recv_t;
OBJ_CLASS_DECLARATION(orte_rml_recv_t);

typedef struct {
    opal_list_item_t super;
    bool buffer_data;
    orte_process_name_t peer;
    orte_rml_tag_t tag;
    bool persistent;
    union {
        orte_rml_callback_fn_t        iov;
        orte_rml_buffer_callback_fn_t buffer;
    } cbfunc;
    void *cbdata;
} orte_rml_posted_recv_t;
OBJ_CLASS_DECLARATION(orte_rml_posted_recv_t);

/* define an object for transferring recv requests to the list of posted recvs */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    bool cancel;
    orte_rml_posted_recv_t *post;
} orte_rml_recv_request_t;
OBJ_CLASS_DECLARATION(orte_rml_recv_request_t);

/* define a structure for sending a message to myself */
typedef struct {
    opal_object_t object;
    opal_event_t ev;
    orte_rml_tag_t tag;
    struct iovec* iov;
    int count;
    opal_buffer_t *buffer;
    union {
        orte_rml_callback_fn_t        iov;
        orte_rml_buffer_callback_fn_t buffer;
    } cbfunc;
    void *cbdata;
} orte_self_send_xfer_t;
OBJ_CLASS_DECLARATION(orte_self_send_xfer_t);

#define ORTE_RML_POST_MESSAGE(p, t, s, b, l)                            \
    do {                                                                \
        orte_rml_recv_t *msg;                                           \
        opal_output_verbose(5, orte_rml_base_framework.framework_output, \
                            "%s Message posted at %s:%d for tag %d",    \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__, (t));                   \
        msg = OBJ_NEW(orte_rml_recv_t);                                 \
        msg->sender.jobid = (p)->jobid;                                 \
        msg->sender.vpid = (p)->vpid;                                   \
        msg->tag = (t);                                                 \
        msg->seq_num = (s);                                             \
        msg->iov.iov_base = (IOVBASE_TYPE*)(b);                         \
        msg->iov.iov_len = (l);                                         \
        /* setup the event */                                           \
        opal_event_set(orte_event_base, &msg->ev, -1,                   \
                       OPAL_EV_WRITE,                                   \
                       orte_rml_base_process_msg, msg);                 \
        opal_event_set_priority(&msg->ev, ORTE_MSG_PRI);                \
        opal_event_active(&msg->ev, OPAL_EV_WRITE, 1);                  \
    } while(0);

#define ORTE_RML_ACTIVATE_MESSAGE(m)                            \
    do {                                                        \
        /* setup the event */                                   \
        opal_event_set(orte_event_base, &(m)->ev, -1,           \
                       OPAL_EV_WRITE,                           \
                       orte_rml_base_process_msg, (m));         \
        opal_event_set_priority(&(m)->ev, ORTE_MSG_PRI);        \
        opal_event_active(&(m)->ev, OPAL_EV_WRITE, 1);          \
    } while(0);

#define ORTE_RML_SEND_COMPLETE(m)                                       \
    do {                                                                \
        opal_output_verbose(5, orte_rml_base_framework.framework_output, \
                            "%s-%s Send message complete at %s:%d",     \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            ORTE_NAME_PRINT(&((m)->dst)),               \
                            __FILE__, __LINE__);                        \
        if (NULL != (m)->iov) {                                         \
            if (NULL != (m)->cbfunc.iov) {                              \
                (m)->cbfunc.iov((m)->status,                            \
                            &((m)->dst),                                \
                            (m)->iov, (m)->count,                       \
                            (m)->tag, (m)->cbdata);                     \
            }                                                           \
         } else if (NULL != (m)->cbfunc.buffer) {                       \
            /* non-blocking buffer send */                              \
            (m)->cbfunc.buffer((m)->status, &((m)->dst),                \
                           (m)->buffer,                                 \
                           (m)->tag, (m)->cbdata);                      \
         }                                                              \
        OBJ_RELEASE(m);                                                 \
    }while(0);

/* common implementations */
ORTE_DECLSPEC void orte_rml_base_post_recv(int sd, short args, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_process_msg(int fd, short flags, void *cbdata);


END_C_DECLS

#endif /* MCA_RML_BASE_H  */
