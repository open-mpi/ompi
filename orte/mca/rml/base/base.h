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
 * Copyright (c) 2014-2015 Intel, Inc. All rights reserved.
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
 * Interface for starting / stopping / controlling the RML framework,
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

#include "orte/mca/rml/rml.h"


BEGIN_C_DECLS

OPAL_TIMING_DECLARE_EXT(ORTE_DECLSPEC, tm_rml)

/*
 * MCA Framework
 */
ORTE_DECLSPEC extern mca_base_framework_t orte_rml_base_framework;
/* select a component */
ORTE_DECLSPEC int orte_rml_base_select(void);

/**
 * Post receive to get updates regarding contact information
 *
 * Post a non-blocking receive (likely during orte_init()) to receive
 * updated contact information from the HNP when it becomes available.
 * This should be called in any process that needs such updates, and
 * the receive will continue to get update callbacks until
 * orte_rml_base_comm_stop() is called.
 */
ORTE_DECLSPEC void orte_rml_base_comm_start(void);


/**
 * Stop receiving contact information updates
 *
 * Shut down the receive posted during orte_rml_base_comm_start(),
 * likely during orte_finalize().
 */
ORTE_DECLSPEC void orte_rml_base_comm_stop(void);


/* a global struct containing framework-level values */
typedef struct {
    opal_list_t posted_recvs;
    opal_list_t unmatched_msgs;
    opal_pointer_array_t open_channels;
#if OPAL_ENABLE_TIMING
    bool timing;
#endif
} orte_rml_base_t;
ORTE_DECLSPEC extern orte_rml_base_t orte_rml_base;


/**
 * List of components that are available to the RML
 *
 * List of components that are currently available to the RML
 * framework.  Useable between calls to orte_rml_base_open() and
 * orte_rml_base_close().
 *
 * @note This list should not be used by code outside the RML base.
 */
ORTE_DECLSPEC extern opal_list_t orte_rml_base_components;


/**
 * Component structure for the selected RML component
 *
 * Component structure pointer for the currently selected RML
 * component.  Useable between calls to orte_rml_base_select() and
 * orte_rml_base_close().
 * @note This pointer should not be used outside the RML base.  It is
 * available outside the RML base only for the F/T component.
 */
ORTE_DECLSPEC extern orte_rml_component_t *orte_rml_component;

typedef enum  {
    orte_rml_channel_opening = 0,
    orte_rml_channel_open = 1,
    orte_rml_channel_closing = 2,
    orte_rml_channel_closed = 3,
}orte_rml_channel_state_t;

/**
 * RML channel structure.
 * The RML only needs basic channel information as the rest of the book keeping information
 * is stored in the QoS module specific channel object.
 * It contains a pointer to the QoS module that handles requests on the channel.
 * It contains a pointer to a struct that contains the QoS specific channel data.
 */
typedef struct {
    opal_list_item_t super;
    orte_rml_channel_num_t  channel_num; // the channel number reference (exposed to the user).
    orte_process_name_t   peer; //  the other end point (peer) of the channel
    orte_rml_channel_num_t  peer_channel; // peer channel number
    void * qos;  // pointer to QoS component specific module
    void * qos_channel_ptr; // pointer to QoS component specific channel struct
    orte_rml_channel_state_t state; // channel state
    bool recv; // set to true if this is a receive (peer opened) channel. (Default is send channel)
} orte_rml_channel_t;
OBJ_CLASS_DECLARATION(orte_rml_channel_t);


/* structure to send RML messages - used internally */
typedef struct {
    opal_list_item_t super;
    orte_process_name_t dst;     // targeted recipient
    orte_process_name_t origin;
    int status;                  // returned status on send
    orte_rml_tag_t tag;          // targeted tag

    /* user's send callback functions and data */
    union {
        orte_rml_callback_fn_t        iov;
        orte_rml_buffer_callback_fn_t buffer;
        orte_rml_send_channel_callback_fn_t iov_chan;
        orte_rml_send_buffer_channel_callback_fn_t buf_chan;
    } cbfunc;
    void *cbdata;

    /* pointer to the user's iovec array */
    struct iovec *iov;
    int count;
    /* pointer to the user's buffer */
    opal_buffer_t *buffer;
    /*** TODO : need to move channel specific data to a channel struct */
    /* pointer to the channel object */
    orte_rml_channel_t *channel;
    /* destination channel number */
    orte_rml_channel_num_t dst_channel;
    /* msg seq number */
    uint32_t seq_num;
    /* pointer to raw data for cross-transport
     * transfers
     */
    char *data;
} orte_rml_send_t;
OBJ_CLASS_DECLARATION(orte_rml_send_t);

/* structure to send RML channel open messages - used internally */
typedef struct {
    opal_list_item_t super;
    /* peer process */
    orte_process_name_t dst;
    /* msg send status */
    int status;
    /* channel object */
    orte_rml_channel_t *channel;
    /* attributes of the channel */
    opal_list_t *qos_attributes;
    /* user's callback function */
    orte_rml_channel_callback_fn_t cbfunc;
    /* user's cbdata */
    void *cbdata;
} orte_rml_open_channel_t;
OBJ_CLASS_DECLARATION(orte_rml_open_channel_t);

/* structure to send RML channel close messages - used internally */
typedef struct {
    opal_list_item_t super;
    /* msg send status */
    int status;
     /* channel object */
    orte_rml_channel_t *channel;
    /* user's callback function */
    orte_rml_channel_callback_fn_t cbfunc;
    /* user's cbdata */
    void *cbdata;
} orte_rml_close_channel_t;
OBJ_CLASS_DECLARATION(orte_rml_close_channel_t);

/* define an object for transferring send requests to the event lib */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
    union {
        orte_rml_send_t send;
        orte_rml_open_channel_t open_channel;
        orte_rml_close_channel_t close_channel;
    }post;
} orte_rml_send_request_t;
OBJ_CLASS_DECLARATION(orte_rml_send_request_t);

/* structure to recv RML messages - used internally */
typedef struct {
    opal_list_item_t super;
    opal_event_t ev;
    orte_process_name_t sender;  // sender
    orte_rml_tag_t tag;          // targeted tag
    orte_rml_channel_num_t channel_num; // channel number
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

#define ORTE_RML_POST_MESSAGE(p, t, c, s, b, l)                               \
    do {                                                                \
        orte_rml_recv_t *msg;                                           \
        opal_output_verbose(5, orte_rml_base_framework.framework_output, \
                            "%s Message posted at %s:%d",               \
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                            __FILE__, __LINE__);                        \
        msg = OBJ_NEW(orte_rml_recv_t);                                 \
        msg->sender.jobid = (p)->jobid;                                 \
        msg->sender.vpid = (p)->vpid;                                   \
        msg->tag = (t);                                                 \
        msg->channel_num = (c);                                         \
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

/*
 reactivates rcv msg on the unposted rcvd list when a match occurs
 need a different path as the QoS recv processing was already done
 for this process
*/
#define ORTE_RML_REACTIVATE_MESSAGE(m)                         \
    do {                                                      \
    /* setup the event */                                   \
    opal_event_set(orte_event_base, &(m)->ev, -1,           \
                   OPAL_EV_WRITE,                           \
                   orte_rml_base_reprocess_msg, (m));         \
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
        if( NULL == (m)->channel) {                                     \
            if (NULL != (m)->iov) {                                     \
                if (NULL != (m)->cbfunc.iov) {                          \
                    (m)->cbfunc.iov((m)->status,                        \
                                &((m)->dst),                            \
                                (m)->iov, (m)->count,                   \
                                (m)->tag, (m)->cbdata);                 \
                }                                                       \
             } else {                                                   \
                /* non-blocking buffer send */                          \
                (m)->cbfunc.buffer((m)->status, &((m)->origin),         \
                               (m)->buffer,                             \
                               (m)->tag, (m)->cbdata);                  \
             }                                                          \
        } else {                                                        \
            if (NULL != (m)->iov) {                                     \
                if (NULL != (m)->cbfunc.iov_chan) {                     \
                    (m)->cbfunc.iov_chan((m)->status,                   \
                        (m)->channel->channel_num,                      \
                        (m)->iov, (m)->count,                           \
                        (m)->tag, (m)->cbdata);                         \
                }                                                       \
            } else {                                                    \
                /* non-blocking buffer send */                          \
                (m)->cbfunc.buf_chan((m)->status,                       \
                       (m)->channel->channel_num,                       \
                       (m)->buffer,                                     \
                       (m)->tag, (m)->cbdata);                          \
            }                                                           \
        }                                                               \
        OBJ_RELEASE(m);                                                 \
    }while(0);


#define ORTE_RML_OPEN_CHANNEL_COMPLETE(m)                               \
 do {                                                                  \
        opal_output_verbose(5, orte_rml_base_framework.framework_output, \
                              "%s-%s open channel message complete at %s:%d", \
                               ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                               ORTE_NAME_PRINT(&((m)->dst)),               \
                               __FILE__, __LINE__);                     \
        /* call the callback function  */                              \
        (m)->cbfunc((m)->status,  (m)->channel->channel_num,            \
                           &((m)->dst),                            \
                           NULL, (m)->cbdata) ;                     \
    }while(0);

#define ORTE_RML_CLOSE_CHANNEL_COMPLETE(m)                           \
    do {                                                             \
    opal_output_verbose(5, orte_rml_base_framework.framework_output, \
                    "%s-%d close channel message complete at %s:%d", \
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),         \
                        (m)->channel->channel_num,                  \
                        __FILE__, __LINE__);                        \
    /* call the callback function  */                               \
    (m)->cbfunc((m)->status,  (m)->channel->channel_num,            \
                NULL, NULL, (m)->cbdata) ;                          \
}while(0);
/*
 * This is the base priority for a RML wrapper component
 * If there exists more than one wrapper, then the one with
 * the lowest priority wins.
 */
#define RML_SELECT_WRAPPER_PRIORITY -128

#define ORTE_RML_INVALID_CHANNEL_NUM  UINT32_MAX
ORTE_DECLSPEC orte_rml_channel_t * orte_rml_base_get_channel (orte_rml_channel_num_t chan_num);


/* common implementations */
ORTE_DECLSPEC void orte_rml_base_post_recv(int sd, short args, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_process_msg(int fd, short flags, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_process_error(int fd, short flags, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_open_channel(int fd, short flags, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_close_channel(int fd, short flags, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_open_channel_send_callback ( int status, orte_process_name_t* sender,
                                                         opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                         void* cbdata);
ORTE_DECLSPEC void orte_rml_base_open_channel_resp_callback (int status, orte_process_name_t* peer,
                                                        struct opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                        void* cbdata);
ORTE_DECLSPEC void orte_rml_base_open_channel_reply_send_callback ( int status, orte_process_name_t* sender,
                                                               opal_buffer_t* buffer, orte_rml_tag_t tag,
                                                               void* cbdata);
ORTE_DECLSPEC void orte_rml_base_prep_send_channel (orte_rml_channel_t *channel,
                                      orte_rml_send_t *send);
ORTE_DECLSPEC int orte_rml_base_process_recv_channel (orte_rml_channel_t *channel,
                                      orte_rml_recv_t *recv);
ORTE_DECLSPEC void orte_rml_base_close_channel_send_callback ( int status, orte_process_name_t* sender,
                                             opal_buffer_t* buffer, orte_rml_tag_t tag, void* cbdata);
ORTE_DECLSPEC void orte_rml_base_send_close_channel ( orte_rml_close_channel_t *close_chan);
ORTE_DECLSPEC void orte_rml_base_reprocess_msg(int fd, short flags, void *cbdata);
ORTE_DECLSPEC void orte_rml_base_complete_recv_msg (orte_rml_recv_t **recv_msg);
END_C_DECLS

#endif /* MCA_RML_BASE_H */
