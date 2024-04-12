/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017-2019 Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRTE_REACHABLE_H
#define PRTE_REACHABLE_H

#include "prte_config.h"

#include "src/pmix/pmix-internal.h"
#include "src/class/pmix_list.h"
#include "src/include/types.h"

#include "src/mca/mca.h"

BEGIN_C_DECLS

/**
 * Reachability matrix between endpoints of a given pair of hosts
 *
 * The output of the reachable() call is a prte_reachable_t, which
 * gives an matrix of the connectivity between local and remote
 * ethernet endpoints.  Any given value in weights is the connectivity
 * between the local endpoint index (first index) and the remote
 * endpoint index (second index), and is a value between 0 and INT_MAX
 * representing a relative connectivity.
 */
struct prte_reachable_t {
    pmix_object_t super;
    /** number of local interfaces passed to reachable() */
    int num_local;
    /** number of remote interfaces passed to reachable() */
    int num_remote;
    /** matric of connectivity weights */
    int **weights;
    /** \internal */
    void *memory;
};
typedef struct prte_reachable_t prte_reachable_t;
PMIX_CLASS_DECLARATION(prte_reachable_t);

/* Init */
typedef int (*prte_reachable_base_module_init_fn_t)(void);

/* Finalize */
typedef int (*prte_reachable_base_module_fini_fn_t)(void);

/* Build reachability matrix between local and remote ethernet
 * interfaces
 *
 * @param local_ifs (IN)     Local list of pmix_pif_t objects
 *                           The pmix_pif_t objects must be fully populated
 * @param remote_ifs (IN)    Remote list of pmix_pif_t objects
 *                           The pmix_pif_t objects must have the following fields populated:
 *                              uint16_t                 af_family;
 *                              struct sockaddr_storage  if_addr;
 *                              uint32_t                 if_mask;
 *                              uint32_t                 if_bandwidth;
 * @return prte_reachable_t  The reachability matrix was successfully created
 * @return NULL              The reachability matrix could not be constructed
 *
 * Given a list of local interfaces and remote interfaces from a
 * single peer, build a reachability matrix between the two peers.
 * This function does not select the best pairing of local and remote
 * interfaces, but only a (comparable) reachability between any pair
 * of local/remote interfaces.
 *
 *
 */
typedef prte_reachable_t *(*prte_reachable_base_module_reachable_fn_t)(pmix_list_t *local_ifs,
                                                                       pmix_list_t *remote_ifs);

/*
 * the standard public API data structure
 */
typedef struct {
    /* currently used APIs */
    prte_reachable_base_module_init_fn_t init;
    prte_reachable_base_module_fini_fn_t finalize;
    prte_reachable_base_module_reachable_fn_t reachable;
} prte_reachable_base_module_t;

typedef struct {
    pmix_mca_base_component_t base_version;
    int priority;
} prte_reachable_base_component_t;

/*
 * Macro for use in components that are of type reachable
 */
#define PRTE_REACHABLE_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("prtereachable", 2, 0, 0)

/* Global structure for accessing reachability functions */
PRTE_EXPORT extern prte_reachable_base_module_t prte_reachable;

END_C_DECLS

#endif
