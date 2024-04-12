/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_MCA_REACHABLE_WEIGHTED_H
#define PRTE_MCA_REACHABLE_WEIGHTED_H

#include "prte_config.h"

#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif

#include "src/event/event-internal.h"
#include "src/mca/mca.h"
#include "src/mca/prtereachable/prtereachable.h"
#include "src/util/proc_info.h"

BEGIN_C_DECLS

typedef struct {
    prte_reachable_base_component_t super;
} prte_mca_prtereachable_weighted_component_t;

PRTE_EXPORT extern prte_mca_prtereachable_weighted_component_t prte_mca_prtereachable_weighted_component;

PRTE_EXPORT extern const prte_reachable_base_module_t prte_prtereachable_weighted_module;

END_C_DECLS

#endif /* MCA_REACHABLE_WEIGHTED_H */
