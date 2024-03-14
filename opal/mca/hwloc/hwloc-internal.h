/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * In days of old, hwloc was packaged as multiple MCA components, and
 * grew an extensive set of base code to support Open MPI's use of
 * hwloc.  When internal builds of libevent, hwloc, and hwloc were
 * moved out of components into base code so that they could be shared
 * between Open MPI and PRRTE without incurring linking hell, we left
 * the base code active.  This MCA framework is essentially defunct;
 * its only purpose is to allow continued use of the base code.
 */

#ifndef OPAL_MCA_HWLOC_H
#define OPAL_MCA_HWLOC_H

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#include <stdarg.h>
#include <stdint.h>

#include "opal/class/opal_list.h"
#include "opal/class/opal_value_array.h"

#include "opal/mca/base/base.h"
#include "opal/mca/mca.h"

BEGIN_C_DECLS

#ifdef WIN32
#    define WIN32_LEAN_AND_MEAN
#    include <windows.h>
#    undef WIN32_LEAN_AND_MEAN
typedef unsigned char u_char;
typedef unsigned short u_short;
#endif

/**
 * Structure for hwloc components.
 */
struct opal_hwloc_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;
};

/**
 * Convenience typedef
 */
typedef struct opal_hwloc_base_component_2_0_0_t opal_hwloc_base_component_2_0_0_t;
typedef struct opal_hwloc_base_component_2_0_0_t opal_hwloc_component_t;

/**
 * Macro for use in components that are of type hwloc
 */
#define OPAL_HWLOC_BASE_VERSION_2_0_0 OPAL_MCA_BASE_VERSION_2_1_0("hwloc", 2, 0, 0)

/* ******************************************************************** */
/* Although we cannot bind if --without-hwloc is set,
 * we do still need to know some basic locality data
 * like on_node and not_on_node. So ensure that we
 * always have access to that much info by including
 * the definitions here, outside the if-have-hwloc test
 */
typedef uint16_t opal_hwloc_locality_t;
#define OPAL_HWLOC_LOCALITY_T OPAL_UINT16

/** Process locality definitions */
enum {
    OPAL_PROC_LOCALITY_UNKNOWN = 0x0000,
    OPAL_PROC_NON_LOCAL = 0x8000,
    OPAL_PROC_ON_CLUSTER = 0x0001,
    OPAL_PROC_ON_CU = 0x0002,
    OPAL_PROC_ON_HOST = 0x0004,
    OPAL_PROC_ON_BOARD = 0x0008,
    OPAL_PROC_ON_NODE = 0x000c, // same host and board
    OPAL_PROC_ON_NUMA = 0x0010,
    OPAL_PROC_ON_SOCKET = 0x0020,
    OPAL_PROC_ON_L3CACHE = 0x0040,
    OPAL_PROC_ON_L2CACHE = 0x0080,
    OPAL_PROC_ON_L1CACHE = 0x0100,
    OPAL_PROC_ON_CORE = 0x0200,
    OPAL_PROC_ON_HWTHREAD = 0x0400,
    OPAL_PROC_ALL_LOCAL = 0x0fff,
};

/** Process locality macros */
#define OPAL_PROC_ON_LOCAL_CLUSTER(n)  (!!((n) &OPAL_PROC_ON_CLUSTER))
#define OPAL_PROC_ON_LOCAL_CU(n)       (!!((n) &OPAL_PROC_ON_CU))
#define OPAL_PROC_ON_LOCAL_HOST(n)     (!!((n) &OPAL_PROC_ON_HOST))
#define OPAL_PROC_ON_LOCAL_BOARD(n)    (!!((n) &OPAL_PROC_ON_BOARD))
#define OPAL_PROC_ON_LOCAL_NODE(n)     (OPAL_PROC_ON_LOCAL_HOST(n) && OPAL_PROC_ON_LOCAL_BOARD(n))
#define OPAL_PROC_ON_LOCAL_NUMA(n)     (!!((n) &OPAL_PROC_ON_NUMA))
#define OPAL_PROC_ON_LOCAL_SOCKET(n)   (!!((n) &OPAL_PROC_ON_SOCKET))
#define OPAL_PROC_ON_LOCAL_L3CACHE(n)  (!!((n) &OPAL_PROC_ON_L3CACHE))
#define OPAL_PROC_ON_LOCAL_L2CACHE(n)  (!!((n) &OPAL_PROC_ON_L2CACHE))
#define OPAL_PROC_ON_LOCAL_L1CACHE(n)  (!!((n) &OPAL_PROC_ON_L1CACHE))
#define OPAL_PROC_ON_LOCAL_CORE(n)     (!!((n) &OPAL_PROC_ON_CORE))
#define OPAL_PROC_ON_LOCAL_HWTHREAD(n) (!!((n) &OPAL_PROC_ON_HWTHREAD))

/* ******************************************************************** */

/**
 * Struct used to describe a section of memory (starting address
 * and length). This is really the same thing as an iovec, but
 * we include a separate type for it for at least 2 reasons:
 *
 * 1. Some OS's iovec definitions are exceedingly lame (e.g.,
 * Solaris 9 has the length argument as an int, instead of a
 * size_t).
 *
 * 2. We reserve the right to expand/change this struct in the
 * future.
 */
typedef struct {
    /** Starting address of segment */
    void *mbs_start_addr;
    /** Length of segment */
    size_t mbs_len;
} opal_hwloc_base_memory_segment_t;

/* include implementation to call */
#include <hwloc.h>
#if defined(OPAL_HWLOC_WANT_SHMEM) && OPAL_HWLOC_WANT_SHMEM
#    if HWLOC_API_VERSION >= 0x20000
#        include <hwloc/shmem.h>
#    endif
/* Do nothing in the 1.x case because the caller doesn't know HWLOC_API_VERSION when it sets
 * OPAL_HWLOC_WANT_SHMEM. Calls to hwloc/shmem.h are protected by HWLOC_API_VERSION >= 0x20000 in
 * the actual code.
 */
#endif

#if HWLOC_API_VERSION < 0x00010b00
#    define HWLOC_OBJ_NUMANODE HWLOC_OBJ_NODE
#    define HWLOC_OBJ_PACKAGE  HWLOC_OBJ_SOCKET
#endif

/* define type of processor info requested */
typedef uint8_t opal_hwloc_resource_type_t;
#define OPAL_HWLOC_PHYSICAL  1
#define OPAL_HWLOC_LOGICAL   2
#define OPAL_HWLOC_AVAILABLE 3

/* structs for storing info on objects */
typedef struct {
    opal_object_t super;
    bool npus_calculated;
    unsigned int npus;
    unsigned int idx;
    unsigned int num_bound;
} opal_hwloc_obj_data_t;
OBJ_CLASS_DECLARATION(opal_hwloc_obj_data_t);

typedef struct {
    opal_list_item_t super;
    hwloc_obj_type_t type;
    unsigned cache_level;
    unsigned int num_objs;
    opal_hwloc_resource_type_t rtype;
    opal_list_t sorted_by_dist_list;
} opal_hwloc_summary_t;
OBJ_CLASS_DECLARATION(opal_hwloc_summary_t);

typedef struct {
    opal_object_t super;
    hwloc_cpuset_t available;
    opal_list_t summaries;

    /** \brief Additional space for custom data */
    void *userdata;
} opal_hwloc_topo_data_t;
OPAL_DECLSPEC OBJ_CLASS_DECLARATION(opal_hwloc_topo_data_t);

/* some global values */
OPAL_DECLSPEC extern hwloc_topology_t opal_hwloc_topology;
OPAL_DECLSPEC extern hwloc_cpuset_t opal_hwloc_my_cpuset;

END_C_DECLS

#endif /* OPAL_HWLOC_H_ */
