/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * Contains the typedefs for the use of the rmcast
 */

#ifndef MCA_RMCAST_TYPES_H_
#define MCA_RMCAST_TYPES_H_

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#include "opal/dss/dss_types.h"

BEGIN_C_DECLS

/* Data structure for passing messages to recv processing */
typedef struct {
    opal_object_t super;
    orte_process_name_t sender;
    opal_buffer_t *buf;
} orte_rmcast_msg_t;
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(orte_rmcast_msg_t);

/* channel type */
typedef uint32_t orte_rmcast_channel_t;
#define ORTE_RMCAST_CHANNEL_T   OPAL_UINT32

/* ORTE IP multicast channels */
#define ORTE_RMCAST_GROUP_INPUT_CHANNEL     0
#define ORTE_RMCAST_DIRECT_CHANNEL          1
#define ORTE_RMCAST_GROUP_OUTPUT_CHANNEL    2
#define ORTE_RMCAST_WILDCARD_CHANNEL        3
#define ORTE_RMCAST_INVALID_CHANNEL         4
#define ORTE_RMCAST_SYS_CHANNEL             5
#define ORTE_RMCAST_APP_PUBLIC_CHANNEL      6
#define ORTE_RMCAST_DATA_SERVER_CHANNEL     7
#define ORTE_RMCAST_ERROR_CHANNEL           8
#define ORTE_RMCAST_HEARTBEAT_CHANNEL       9

#define ORTE_RMCAST_DYNAMIC_CHANNELS       10


/* define channel directions */
#define ORTE_RMCAST_XMIT    0x01
#define ORTE_RMCAST_RECV    0x02
#define ORTE_RMCAST_BIDIR   0x03

/* define channel flags */
#define ORTE_RMCAST_MY_INPUT   0x10
#define ORTE_RMCAST_MY_OUTPUT  0x20

/* Message matching tag */
typedef int32_t orte_rmcast_tag_t;
#define ORTE_RMCAST_TAG_T   OPAL_INT32

/* tag values for well-known services */
#define ORTE_RMCAST_TAG_WILDCARD       0
#define ORTE_RMCAST_TAG_INVALID        1
#define ORTE_RMCAST_TAG_BOOTSTRAP      2
#define ORTE_RMCAST_TAG_ANNOUNCE       3
#define ORTE_RMCAST_TAG_OUTPUT         4
#define ORTE_RMCAST_TAG_PS             5
#define ORTE_RMCAST_TAG_MSG            6
#define ORTE_RMCAST_TAG_TOOL           7
#define ORTE_RMCAST_TAG_IOF            8
#define ORTE_RMCAST_TAG_DATA           9
#define ORTE_RMCAST_TAG_CMD_ACK       10
#define ORTE_RMCAST_TAG_HEARTBEAT     11
#define ORTE_RMCAST_TAG_COMMAND       12
#define ORTE_RMCAST_TAG_ERRMGR        13
#define ORTE_RMCAST_TAG_UPDATE_STATE  14
#define ORTE_RMCAST_TAG_TERMINATE     15

/* starting value for dynamically assignable tags */
#define ORTE_RMCAST_TAG_DYNAMIC     100


/* persistence of recv requests */
typedef uint8_t orte_rmcast_flag_t;
#define ORTE_RMCAST_NON_PERSISTENT  0x00
#define ORTE_RMCAST_PERSISTENT      0x01

/* message sequence number */
typedef int32_t orte_rmcast_seq_t;
#define ORTE_RMCAST_SEQ_MAX         INT32_MAX
#define ORTE_RMCAST_SEQ_INVALID     -1
#define ORTE_RMCAST_SEQ_T   OPAL_INT32

/**
 * Function prototypes for callback from receiving multicast messages
 */
typedef void (*orte_rmcast_callback_buffer_fn_t)(int status,
                                                 orte_rmcast_channel_t channel,
                                                 orte_rmcast_seq_t seq_num,
                                                 orte_rmcast_tag_t tag,
                                                 orte_process_name_t *sender,
                                                 opal_buffer_t *buf, void* cbdata);

typedef void (*orte_rmcast_callback_fn_t)(int status,
                                          orte_rmcast_channel_t channel,
                                          orte_rmcast_seq_t seq_num,
                                          orte_rmcast_tag_t tag,
                                          orte_process_name_t *sender,
                                          struct iovec *msg, int count, void* cbdata);


END_C_DECLS


#endif  /* MCA_RMCAST_TYPES_H_ */
