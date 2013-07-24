/*
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef BTL_USNIC_HWLOC_H
#define BTL_USNIC_HWLOC_H

#include "ompi_config.h"

#include "btl_usnic_module.h"


#if OPAL_HAVE_HWLOC
int ompi_btl_usnic_hwloc_distance(ompi_btl_usnic_module_t *module);
#endif

#endif /* BTL_USNIC_HWLOC_H */
