/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 */

#ifndef OPAL_HWLOC_H
#define OPAL_HWLOC_H

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <stdint.h>
#include <stdarg.h>
#include <hwloc.h>
/* If the including file requested it, also include the hwloc verbs
   helper file.  We can't just always include this file (even if we
   know we have <infiniband/verbs.h>) because there are some inline
   functions in that file that invoke ibv_* functions.  Some linkers
   (e.g., Solaris Studio Compilers) will instantiate those static
   inline functions even if we don't use them, and therefore we need
   to be able to resolve the ibv_* symbols at link time.

   Since -libverbs is only specified in places where we use other
   ibv_* functions (e.g., the OpenFabrics-based BTLs), that means that
   linking random executables can/will fail (e.g., orterun).
 */
#if defined(OPAL_HWLOC_WANT_VERBS_HELPER) && OPAL_HWLOC_WANT_VERBS_HELPER
#    if defined(HAVE_INFINIBAND_VERBS_H)
#        include hwloc_external_openfabrics_header
#    else
#        error Tried to include hwloc verbs helper file, but hwloc was compiled with no OpenFabrics support
#    endif
#endif


#include "opal/class/opal_list.h"
#include "opal/class/opal_value_array.h"

BEGIN_C_DECLS

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
typedef unsigned char u_char;
typedef unsigned short u_short;
#endif

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
    OPAL_PROC_LOCALITY_UNKNOWN  = 0x0000,
    OPAL_PROC_NON_LOCAL         = 0x8000,
    OPAL_PROC_ON_CLUSTER        = 0x0001,
    OPAL_PROC_ON_CU             = 0x0002,
    OPAL_PROC_ON_HOST           = 0x0004,
    OPAL_PROC_ON_BOARD          = 0x0008,
    OPAL_PROC_ON_NODE           = 0x000c,   // same host and board
    OPAL_PROC_ON_NUMA           = 0x0010,
    OPAL_PROC_ON_SOCKET         = 0x0020,
    OPAL_PROC_ON_L3CACHE        = 0x0040,
    OPAL_PROC_ON_L2CACHE        = 0x0080,
    OPAL_PROC_ON_L1CACHE        = 0x0100,
    OPAL_PROC_ON_CORE           = 0x0200,
    OPAL_PROC_ON_HWTHREAD       = 0x0400,
    OPAL_PROC_ALL_LOCAL         = 0x0fff,
};

/** Process locality macros */
#define OPAL_PROC_ON_LOCAL_CLUSTER(n)   (!!((n) & OPAL_PROC_ON_CLUSTER))
#define OPAL_PROC_ON_LOCAL_CU(n)        (!!((n) & OPAL_PROC_ON_CU))
#define OPAL_PROC_ON_LOCAL_HOST(n)      (!!((n) & OPAL_PROC_ON_HOST))
#define OPAL_PROC_ON_LOCAL_BOARD(n)     (!!((n) & OPAL_PROC_ON_BOARD))
#define OPAL_PROC_ON_LOCAL_NODE(n)      (OPAL_PROC_ON_LOCAL_HOST(n) && OPAL_PROC_ON_LOCAL_BOARD(n))
#define OPAL_PROC_ON_LOCAL_NUMA(n)      (!!((n) & OPAL_PROC_ON_NUMA))
#define OPAL_PROC_ON_LOCAL_SOCKET(n)    (!!((n) & OPAL_PROC_ON_SOCKET))
#define OPAL_PROC_ON_LOCAL_L3CACHE(n)   (!!((n) & OPAL_PROC_ON_L3CACHE))
#define OPAL_PROC_ON_LOCAL_L2CACHE(n)   (!!((n) & OPAL_PROC_ON_L2CACHE))
#define OPAL_PROC_ON_LOCAL_L1CACHE(n)   (!!((n) & OPAL_PROC_ON_L1CACHE))
#define OPAL_PROC_ON_LOCAL_CORE(n)      (!!((n) & OPAL_PROC_ON_CORE))
#define OPAL_PROC_ON_LOCAL_HWTHREAD(n)  (!!((n) & OPAL_PROC_ON_HWTHREAD))

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
} opal_hwloc_memory_segment_t;

/* define type of processor info requested */
typedef uint8_t opal_hwloc_resource_type_t;
#define OPAL_HWLOC_PHYSICAL   1
#define OPAL_HWLOC_LOGICAL    2
#define OPAL_HWLOC_AVAILABLE  3

/* structs for storing info on objects */
typedef struct {
    opal_object_t super;
    hwloc_cpuset_t available;
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

/* define binding policies */
typedef uint16_t opal_binding_policy_t;
#define OPAL_BINDING_POLICY OPAL_UINT16

/* binding directives */
#define OPAL_BIND_IF_SUPPORTED      0x1000
#define OPAL_BIND_ALLOW_OVERLOAD    0x2000
#define OPAL_BIND_GIVEN             0x4000
/* binding policies - any changes in these
 * values must be reflected in orte/mca/rmaps/rmaps.h
 */
#define OPAL_BIND_TO_NONE           1
#define OPAL_BIND_TO_BOARD          2
#define OPAL_BIND_TO_NUMA           3
#define OPAL_BIND_TO_SOCKET         4
#define OPAL_BIND_TO_L3CACHE        5
#define OPAL_BIND_TO_L2CACHE        6
#define OPAL_BIND_TO_L1CACHE        7
#define OPAL_BIND_TO_CORE           8
#define OPAL_BIND_TO_HWTHREAD       9
#define OPAL_BIND_TO_CPUSET         10
#define OPAL_GET_BINDING_POLICY(pol) \
    ((pol) & 0x0fff)
#define OPAL_SET_BINDING_POLICY(target, pol) \
    (target) = (pol) | (((target) & 0x2000) | OPAL_BIND_GIVEN)
#define OPAL_SET_DEFAULT_BINDING_POLICY(target, pol)            \
    do {                                                        \
        if (!OPAL_BINDING_POLICY_IS_SET((target))) {            \
            (target) = (pol) | (((target) & 0xf000) |           \
                                OPAL_BIND_IF_SUPPORTED);        \
        }                                                       \
    } while(0);

/* check if policy is set */
#define OPAL_BINDING_POLICY_IS_SET(pol) \
    ((pol) & 0x4000)
/* macro to detect if binding was qualified */
#define OPAL_BINDING_REQUIRED(n) \
    (!(OPAL_BIND_IF_SUPPORTED & (n)))
/* macro to detect if binding is forced */
#define OPAL_BIND_OVERLOAD_ALLOWED(n) \
    (OPAL_BIND_ALLOW_OVERLOAD & (n))

/* some global values */
OPAL_DECLSPEC extern hwloc_topology_t opal_hwloc_topology;
OPAL_DECLSPEC extern opal_binding_policy_t opal_hwloc_binding_policy;
OPAL_DECLSPEC extern hwloc_cpuset_t opal_hwloc_my_cpuset;
OPAL_DECLSPEC extern bool opal_hwloc_report_bindings;
OPAL_DECLSPEC extern hwloc_obj_type_t opal_hwloc_levels[];
OPAL_DECLSPEC extern bool opal_hwloc_use_hwthreads_as_cpus;

/* some required open/close functions */
OPAL_DECLSPEC int opal_hwloc_register(void);
OPAL_DECLSPEC int opal_hwloc_open(void);
OPAL_DECLSPEC int opal_hwloc_close(void);


END_C_DECLS

#endif /* OPAL_HWLOC_H_ */
