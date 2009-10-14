/*
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 */

#ifndef ORTE_MCA_RMCAST_BASE_H
#define ORTE_MCA_RMCAST_BASE_H

/*
 * includes
 */
#include "orte_config.h"

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/event/event.h"

#include "orte/mca/rmcast/rmcast.h"

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_rmcast_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * function definitions
 */
ORTE_DECLSPEC int orte_rmcast_base_select(void);
ORTE_DECLSPEC int orte_rmcast_base_close(void);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
