/*
 * Copyright (c) 2013-2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_HWLOC_H
#define BTL_USNIC_HWLOC_H

#include "opal_config.h"

#include "btl_usnic_module.h"


#if OPAL_HAVE_HWLOC
int opal_btl_usnic_hwloc_distance(opal_btl_usnic_module_t *module);
#endif

#endif /* BTL_USNIC_HWLOC_H */
