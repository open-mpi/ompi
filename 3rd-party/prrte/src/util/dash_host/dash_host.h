/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 */
#ifndef PRTE_UTIL_DASH_HOST_H
#define PRTE_UTIL_DASH_HOST_H

#include "prte_config.h"

#include "src/class/pmix_list.h"

#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

PRTE_EXPORT int prte_util_add_dash_host_nodes(pmix_list_t *nodes, char *hosts, bool allocating);

PRTE_EXPORT int prte_util_filter_dash_host_nodes(pmix_list_t *nodes, char *hosts, bool remove);

PRTE_EXPORT int prte_util_get_ordered_dash_host_list(pmix_list_t *nodes, char *hosts);

PRTE_EXPORT int prte_util_dash_host_compute_slots(prte_node_t *node, char *hosts);

END_C_DECLS

#endif
