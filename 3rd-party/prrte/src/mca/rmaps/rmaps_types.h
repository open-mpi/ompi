/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/* Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef PRTE_MCA_RMAPS_TYPES_H
#define PRTE_MCA_RMAPS_TYPES_H

#include "prte_config.h"
#include "constants.h"

#include "src/class/pmix_pointer_array.h"
#include "src/hwloc/hwloc-internal.h"

#include "src/runtime/prte_globals.h"

/*
 * General MAP types - instanced in runtime/prte_globals_class_instances.h
 */

BEGIN_C_DECLS

typedef uint16_t prte_mapping_policy_t;
#define PRTE_MAPPING_POLICY PRTE_UINT16
typedef uint16_t prte_ranking_policy_t;
#define PRTE_RANKING_POLICY PRTE_UINT16

/*
 * Structure that represents the mapping of a job to an
 * allocated set of resources.
 */
struct prte_job_map_t {
    pmix_object_t super;
    /* user-specified mapping params */
    char *req_mapper;  /* requested mapper */
    char *last_mapper; /* last mapper used */
    prte_mapping_policy_t mapping;
    prte_ranking_policy_t ranking;
    prte_binding_policy_t binding;
    bool rtos_set;
    /* *** */
    /* number of new daemons required to be launched
     * to support this job map
     */
    int32_t num_new_daemons;
    /* starting vpid of the new daemons - they will
     * be sequential from that point
     */
    pmix_rank_t daemon_vpid_start;
    /* number of nodes participating in this job */
    int32_t num_nodes;
    /* array of pointers to nodes in this map for this job */
    pmix_pointer_array_t *nodes;
};
typedef struct prte_job_map_t prte_job_map_t;
PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_job_map_t);

typedef struct {
    /* input info */
    uint16_t cpus_per_rank;
    bool use_hwthreads;
    int stream;
    int verbosity;
    char *cpuset;
    hwloc_cpuset_t job_cpuset;
    bool bindsupport;
    bool donotlaunch;
    bool membind_warned;
    bool oversubscribe;
    bool overload;

    /* mapping values */
    prte_mapping_policy_t map;
    bool mapspan;
    bool ordered;
    prte_binding_policy_t mapdepth;
    unsigned ncpus;
    int nprocs;
    hwloc_obj_type_t maptype;
    unsigned cmaplvl;
    /* #procs/resource as per PPR */
    int pprn;

    /* ranking values */
    prte_ranking_policy_t rank;
    bool userranked;
    unsigned nnodes;
    unsigned total_nobjs;
    unsigned nobjs;

    /* binding values */
    prte_binding_policy_t bind;
    bool dobind;
    hwloc_obj_type_t hwb;
    unsigned clvl;

    /* usage tracking */
    hwloc_cpuset_t target;
    hwloc_obj_t obj;

} prte_rmaps_options_t;


/*
 **
 * Macro for use in components that are of type rmaps
 */
#define PRTE_RMAPS_BASE_VERSION_4_0_0 PRTE_MCA_BASE_VERSION_3_0_0("rmaps", 4, 0, 0)

/* define map-related directives */
#define PRTE_MAPPING_NO_USE_LOCAL     0x0100
#define PRTE_MAPPING_NO_OVERSUBSCRIBE 0x0200
#define PRTE_MAPPING_SUBSCRIBE_GIVEN  0x0400
#define PRTE_MAPPING_SPAN             0x0800
#define PRTE_MAPPING_ORDERED          0x1000
/* directives given */
#define PRTE_MAPPING_LOCAL_GIVEN 0x2000
#define PRTE_MAPPING_GIVEN       0x4000
/* macros to set/get/unset mapping directive */
#define PRTE_SET_MAPPING_DIRECTIVE(target, pol)   (target) |= (pol)
#define PRTE_UNSET_MAPPING_DIRECTIVE(target, pol) (target) &= ~(pol)
#define PRTE_GET_MAPPING_DIRECTIVE(pol)           ((pol) &0xff00)

/* round-robin policies */
/* start with hardware-based options
 * so the values match the corresponding
 * levels in src/hwloc/hwloc-internal.h
 */
#define PRTE_MAPPING_BYNODE      1
#define PRTE_MAPPING_BYNUMA      2
#define PRTE_MAPPING_BYPACKAGE   3
#define PRTE_MAPPING_BYL3CACHE   4
#define PRTE_MAPPING_BYL2CACHE   5
#define PRTE_MAPPING_BYL1CACHE   6
#define PRTE_MAPPING_BYCORE      7
#define PRTE_MAPPING_BYHWTHREAD  8
/* now take the other round-robin options */
#define PRTE_MAPPING_BYSLOT      9
#define PRTE_MAPPING_BYDIST     10
#define PRTE_MAPPING_PELIST     11
/* convenience - declare anything <= 15 to be round-robin*/
#define PRTE_MAPPING_RR         16

/* sequential policy */
#define PRTE_MAPPING_SEQ        20
#define PRTE_MAPPING_COLOCATE   21
/* rank file and other user-defined mapping */
#define PRTE_MAPPING_BYUSER     22
/* pattern-based mapping */
#define PRTE_MAPPING_PPR        23
/* macro to separate out the mapping policy
 * from the directives
 */
#define PRTE_GET_MAPPING_POLICY(pol) ((pol) & 0x00ff)
/* macro to determine if mapping policy is set */
#define PRTE_MAPPING_POLICY_IS_SET(pol)      ((pol) & 0x00ff)
#define PRTE_SET_MAPPING_POLICY(target, pol) (target) = (pol) | ((target) & 0xff00)

/* define ranking directives */
#define PRTE_RANKING_GIVEN                        0x1000
#define PRTE_SET_RANKING_DIRECTIVE(target, pol)   (target) |= (pol)
#define PRTE_UNSET_RANKING_DIRECTIVE(target, pol) (target) &= ~(pol)
#define PRTE_GET_RANKING_DIRECTIVE(pol)           ((pol) & 0xf000)

/* define ranking policies */
#define PRTE_RANK_BY_NODE            1
#define PRTE_RANK_BY_SLOT            2
#define PRTE_RANK_BY_FILL            3
#define PRTE_RANK_BY_SPAN            4
#define PRTE_RANKING_BYUSER          5

#define PRTE_GET_RANKING_POLICY(pol) ((pol) &0x0fff)
/* macro to determine if ranking policy is set */
#define PRTE_RANKING_POLICY_IS_SET(pol)      ((pol) & 0x0fff)
#define PRTE_SET_RANKING_POLICY(target, pol) (target) = (pol) | ((target) & 0xf000)

END_C_DECLS

#endif
