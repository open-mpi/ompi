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

#include "orte/mca/rmcast/rmcast.h"

BEGIN_C_DECLS

ORTE_DECLSPEC int orte_rmcast_base_open(void);

#if !ORTE_DISABLE_FULL_SUPPORT

/*
 * globals that might be needed
 */
typedef struct {
    int rmcast_output;
    opal_list_t rmcast_opened;
    uint8_t subnet;
    uint8_t scope;
    uint16_t af_family;
    uint32_t base_ip_addr;
    uint16_t ports[256];
} orte_rmcast_base_t;

ORTE_DECLSPEC extern orte_rmcast_base_t orte_rmcast_base;


/*
 * function definitions
 */
ORTE_DECLSPEC int orte_rmcast_base_select(void);
ORTE_DECLSPEC int orte_rmcast_base_close(void);

#endif /* ORTE_DISABLE_FULL_SUPPORT */

END_C_DECLS

#endif
