/*
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_REACHABLE_NETLINK_H
#define MCA_REACHABLE_NETLINK_H

#include "opal_config.h"

#include "opal/mca/reachable/reachable.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern opal_reachable_base_component_t
    mca_reachable_netlink_component;

OPAL_DECLSPEC extern const opal_reachable_base_module_t
    opal_reachable_netlink_module;

END_C_DECLS

#endif /* MCA_REACHABLE_NETLINK_H */
