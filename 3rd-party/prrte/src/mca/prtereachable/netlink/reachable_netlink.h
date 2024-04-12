/*
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_MCA_REACHABLE_NETLINK_H
#define PRTE_MCA_REACHABLE_NETLINK_H

#include "prte_config.h"

#include "src/mca/prtereachable/prtereachable.h"

BEGIN_C_DECLS

PRTE_EXPORT extern prte_reachable_base_component_t prte_mca_prtereachable_netlink_component;

PRTE_EXPORT extern const prte_reachable_base_module_t prte_prtereachable_netlink_module;

END_C_DECLS

#endif /* MCA_REACHABLE_NETLINK_H */
