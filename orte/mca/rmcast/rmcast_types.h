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

/* define the default ORTE multicast subnet */
#define ORTE_RMCAST_DEFAULT_SUBNET      239

/* default IP multicast scope - site-local, per IANA 2009-03-17 */
#define ORTE_RMCAST_DEFAULT_SCOPE       255

/* ORTE IP multicast channels */
#define ORTE_RMCAST_SYS_ADDR            1
#define ORTE_RMCAST_APP_PUBLIC_ADDR     2


#define ORTE_RMCAST_DYNAMIC_CHANNELS    100


#define ORTE_RMCAST_XMIT    0x01
#define ORTE_RMCAST_RECV    0x02
#define ORTE_RMCAST_BIDIR   0x03


END_C_DECLS


#endif  /* MCA_RMCAST_TYPES_H_ */
