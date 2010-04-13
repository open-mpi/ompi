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


BEGIN_C_DECLS

/* channel type */
typedef int32_t orte_rmcast_channel_t;
#define ORTE_RMCAST_CHANNEL_T   OPAL_INT32

/* ORTE IP multicast channels */
#define ORTE_RMCAST_GROUP_OUTPUT_CHANNEL   -2
#define ORTE_RMCAST_WILDCARD_CHANNEL       -1
#define ORTE_RMCAST_INVALID_CHANNEL         0
#define ORTE_RMCAST_SYS_CHANNEL             1
#define ORTE_RMCAST_APP_PUBLIC_CHANNEL      2

#define ORTE_RMCAST_DYNAMIC_CHANNELS        3


/* define channel directions */
#define ORTE_RMCAST_XMIT    0x01
#define ORTE_RMCAST_RECV    0x02
#define ORTE_RMCAST_BIDIR   0x03

/* Message matching tag */
typedef int32_t orte_rmcast_tag_t;
#define ORTE_RMCAST_TAG_T   OPAL_INT32

/* tag values for well-known services */
#define ORTE_RMCAST_TAG_WILDCARD     0
#define ORTE_RMCAST_TAG_INVALID      1
#define ORTE_RMCAST_TAG_BOOTSTRAP    2
#define ORTE_RMCAST_TAG_ANNOUNCE     3
#define ORTE_RMCAST_TAG_OUTPUT       4
#define ORTE_RMCAST_TAG_PS           5
#define ORTE_RMCAST_TAG_MSG          6
#define ORTE_RMCAST_TAG_TOOL         7
#define ORTE_RMCAST_TAG_IOF          8

/* starting value for dynamically assignable tags */
#define ORTE_RMCAST_TAG_DYNAMIC     100


/* persistence of recv requests */
typedef uint8_t orte_rmcast_flag_t;
#define ORTE_RMCAST_NON_PERSISTENT  0x00
#define ORTE_RMCAST_PERSISTENT      0x01

/* message sequence number */
typedef uint32_t orte_rmcast_seq_t;
#define ORTE_RMCAST_SEQ_MAX         UINT32_MAX-1
#define ORTE_RMCAST_SEQ_INVALID     UINT32_MAX
#define ORTE_RMCAST_SEQ_T   OPAL_UINT32

END_C_DECLS


#endif  /* MCA_RMCAST_TYPES_H_ */
