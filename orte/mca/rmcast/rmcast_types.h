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

/* ORTE IP multicast channels */
#define ORTE_RMCAST_SYS_ADDR            1
#define ORTE_RMCAST_APP_PUBLIC_ADDR     2


#define ORTE_RMCAST_DYNAMIC_CHANNELS    100


/* define channel directions */
#define ORTE_RMCAST_XMIT    0x01
#define ORTE_RMCAST_RECV    0x02
#define ORTE_RMCAST_BIDIR   0x03

/* Message matching tag */
typedef uint16_t orte_rmcast_tag_t;
#define ORTE_RMCAST_TAG_T   OPAL_UINT16

/* tag values for well-known services */
#define ORTE_RMCAST_TAG_INVALID     0
#define ORTE_RMCAST_TAG_BOOTSTRAP   1

/* starting value for dynamicall assignable tags */
#define ORTE_RMCAST_TAG_DYNAMIC     100


/* persistence of recv requests */
typedef uint8_t orte_rmcast_flag_t;
#define ORTE_RMCAST_NON_PERSISTENT  0x00
#define ORTE_RMCAST_PERSISTENT      0x01

END_C_DECLS


#endif  /* MCA_RMCAST_TYPES_H_ */
