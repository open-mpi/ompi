/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2012 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * Contains the typedefs for the use of the rml
 */

#ifndef PRTE_RML_TYPES_H_
#define PRTE_RML_TYPES_H_

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <limits.h>
#ifdef HAVE_SYS_UIO_H
/* for struct iovec */
#    include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#    include <net/uio.h>
#endif

#include "src/class/pmix_bitmap.h"
#include "src/class/pmix_list.h"
#include "src/pmix/pmix-internal.h"

BEGIN_C_DECLS

/**
 * Message matching tag
 *
 * Message matching tag.  Unlike MPI, there is no wildcard receive,
 * all messages must match exactly. Tag values less than
 * PRTE_RML_TAG_DYNAMIC are reserved and may only be referenced using
 * a defined constant.
 */
typedef uint32_t prte_rml_tag_t;


/**
 * Function prototype for callback from non-blocking buffer send and receive
 *
 * Function prototype for callback from non-blocking buffer send and
 * receive. On send, the buffer will be the same pointer passed to
 * send_buffer_nb. On receive, the buffer will be allocated and owned
 * by the RML, not the process receiving the callback.
 *
 * @note The parameter in/out parameters are relative to the user's callback
 * function.
 *
 * @param[in] status  Completion status
 * @param[in] peer    Name of peer process
 * @param[in] buffer  Message buffer
 * @param[in] tag     User defined tag for matching send/recv
 * @param[in] cbdata  User data passed to send_buffer_nb() or recv_buffer_nb()
 */
typedef void (*prte_rml_buffer_callback_fn_t)(int status, pmix_proc_t *peer,
                                              pmix_data_buffer_t *buffer,
                                              prte_rml_tag_t tag,
                                              void *cbdata);

/* Convenience def for readability */
#define PRTE_RML_PERSISTENT     true
#define PRTE_RML_NON_PERSISTENT false

/**
 * Constant tag values for well-known services
 */

#define PRTE_RML_TAG PMIX_UINT32

#define PRTE_RML_TAG_INVALID              0
#define PRTE_RML_TAG_DAEMON               1
#define PRTE_RML_TAG_IOF_HNP              2
#define PRTE_RML_TAG_IOF_PROXY            3
#define PRTE_RML_TAG_XCAST_BARRIER        4
#define PRTE_RML_TAG_PLM                  5
#define PRTE_RML_TAG_LAUNCH_RESP          6
#define PRTE_RML_TAG_ERRMGR               7
#define PRTE_RML_TAG_WIREUP               8
#define PRTE_RML_TAG_RML_INFO_UPDATE      9
#define PRTE_RML_TAG_PRTED_CALLBACK       10
#define PRTE_RML_TAG_ROLLUP               11
#define PRTE_RML_TAG_REPORT_REMOTE_LAUNCH 12

#define PRTE_RML_TAG_CKPT 13

#define PRTE_RML_TAG_RML_ROUTE 14
#define PRTE_RML_TAG_XCAST     15

#define PRTE_RML_TAG_UPDATE_ROUTE_ACK 19
#define PRTE_RML_TAG_SYNC             20

/* For FileM Base */
#define PRTE_RML_TAG_FILEM_BASE      21
#define PRTE_RML_TAG_FILEM_BASE_RESP 22

/* For FileM RSH Component */
#define PRTE_RML_TAG_FILEM_RSH 23

#define PRTE_RML_TAG_JOBID_RESP      24

/* For tools */
#define PRTE_RML_TAG_TOOL 26

/* support data store/lookup */
#define PRTE_RML_TAG_DATA_SERVER 27
#define PRTE_RML_TAG_DATA_CLIENT 28

/* timing related */
#define PRTE_RML_TAG_COLLECTIVE_TIMER 29

/* collectives */
#define PRTE_RML_TAG_COLLECTIVE       30
#define PRTE_RML_TAG_COLL_RELEASE     31
#define PRTE_RML_TAG_DAEMON_COLL      32
#define PRTE_RML_TAG_ALLGATHER_DIRECT 33
#define PRTE_RML_TAG_ALLGATHER_BRUCKS 34
#define PRTE_RML_TAG_ALLGATHER_RCD    35

/* debugger release */
#define PRTE_RML_TAG_DEBUGGER_RELEASE 37

/* bootstrap */
#define PRTE_RML_TAG_BOOTSTRAP 38

/* report a missed msg */
#define PRTE_RML_TAG_MISSED_MSG 39

/* tag for receiving ack of abort msg */
#define PRTE_RML_TAG_ABORT 40

/* tag for receiving heartbeats */
#define PRTE_RML_TAG_HEARTBEAT 41

/* Process Migration Tool Tag */
#define PRTE_RML_TAG_MIGRATE 42

/* For SStore Framework */
#define PRTE_RML_TAG_SSTORE          43
#define PRTE_RML_TAG_SSTORE_INTERNAL 44

#define PRTE_RML_TAG_SUBSCRIBE 45

/* Notify of failed processes */
#define PRTE_RML_TAG_FAILURE_NOTICE 46

/* distributed file system */
#define PRTE_RML_TAG_DFS_CMD  47
#define PRTE_RML_TAG_DFS_DATA 48

/* sensor data */
#define PRTE_RML_TAG_SENSOR_DATA 49

/* direct modex support */
#define PRTE_RML_TAG_DIRECT_MODEX      50
#define PRTE_RML_TAG_DIRECT_MODEX_RESP 51

/* notifier support */
#define PRTE_RML_TAG_NOTIFIER_HNP    52
#define PRTE_RML_TAG_NOTIFY_COMPLETE 53

/*** QOS specific  RML TAGS ***/
#define PRTE_RML_TAG_OPEN_CHANNEL_REQ     54
#define PRTE_RML_TAG_OPEN_CHANNEL_RESP    55
#define PRTE_RML_TAG_MSG_ACK              56
#define PRTE_RML_TAG_CLOSE_CHANNEL_REQ    57
#define PRTE_RML_TAG_CLOSE_CHANNEL_ACCEPT 58

/* error notifications */
#define PRTE_RML_TAG_NOTIFICATION 59

/* stacktrace for debug */
#define PRTE_RML_TAG_STACK_TRACE 60

/* memory profile */
#define PRTE_RML_TAG_MEMPROFILE 61

/* topology report */
#define PRTE_RML_TAG_TOPOLOGY_REPORT 62

/* warmup connection - simply establishes the connection */
#define PRTE_RML_TAG_WARMUP_CONNECTION 63

/* node regex report */
#define PRTE_RML_TAG_NODE_REGEX_REPORT 64

/* pmix log requests */
#define PRTE_RML_TAG_LOGGING 65

/* error propagate  */
#define PRTE_RML_TAG_RBCAST 66

/* heartbeat request */
#define PRTE_RML_TAG_HEARTBEAT_REQUEST 70

/* error propagate  */
#define PRTE_RML_TAG_PROPAGATE 71

/* scheduler requests */
#define PRTE_RML_TAG_SCHED 72


#define PRTE_RML_TAG_MAX 100

#define PRTE_RML_TAG_NTOH(t) ntohl(t)
#define PRTE_RML_TAG_HTON(t) htonl(t)

/*** length of the tag. change this when type of prte_rml_tag_t is changed ***/
/*** max valu in unit32_t is 0xFFFF_FFFF when converted to char this is 8  **
#define PRTE_RML_TAG_T_CHAR_LEN   8
#define PRTE_RML_TAG_T_SPRINT    "%8x" */

/* ******************************************************************** */

/*
 * RML proxy commands
 */
typedef uint8_t prte_rml_cmd_flag_t;
#define PRTE_RML_CMD        PMIX_UINT8
#define PRTE_RML_UPDATE_CMD 1

typedef struct {
    pmix_object_t super;
    pmix_proc_t name;
    pmix_data_buffer_t data;
    bool active;
} prte_rml_recv_cb_t;
PMIX_CLASS_DECLARATION(prte_rml_recv_cb_t);

/* structure to send RML messages - used internally */
typedef struct {
    pmix_list_item_t super;
    pmix_proc_t dst; // targeted recipient
    pmix_proc_t origin;
    int status;         // returned status on send
    prte_rml_tag_t tag; // targeted tag
    int retries;        // #times we have tried to send it

    /* user's send callback functions and data */
    prte_rml_buffer_callback_fn_t cbfunc;
    void *cbdata;

    /* data buffer */
    pmix_data_buffer_t *dbuf;
    /* msg seq number */
    uint32_t seq_num;
} prte_rml_send_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_rml_send_t);

/* define an object for transferring send requests to the event lib */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    prte_rml_send_t send;
} prte_rml_send_request_t;
PMIX_CLASS_DECLARATION(prte_rml_send_request_t);

/* structure to recv RML messages - used internally */
typedef struct {
    pmix_list_item_t super;
    prte_event_t ev;
    pmix_proc_t sender;      // sender
    prte_rml_tag_t tag;      // targeted tag
    uint32_t seq_num;        // sequence number
    pmix_data_buffer_t *dbuf; // the recvd data
} prte_rml_recv_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_rml_recv_t);

typedef struct {
    pmix_list_item_t super;
    bool buffer_data;
    pmix_proc_t peer;
    prte_rml_tag_t tag;
    bool persistent;
    prte_rml_buffer_callback_fn_t cbfunc;
    void *cbdata;
} prte_rml_posted_recv_t;
PMIX_CLASS_DECLARATION(prte_rml_posted_recv_t);

/* define an object for transferring recv requests to the list of posted recvs */
typedef struct {
    pmix_object_t super;
    prte_event_t ev;
    bool cancel;
    prte_rml_posted_recv_t *post;
} prte_rml_recv_request_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_rml_recv_request_t);

/* struct for tracking routing trees */
typedef struct {
    pmix_list_item_t super;
    pmix_rank_t rank;
    pmix_bitmap_t relatives;
} prte_routed_tree_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_routed_tree_t);

END_C_DECLS

#endif /* RML_TYPES */
