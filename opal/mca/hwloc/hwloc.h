/*
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 */

#ifndef OPAL_MCA_HWLOC_H
#define OPAL_MCA_HWLOC_H

#include "opal_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/class/opal_value_array.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

BEGIN_C_DECLS

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN
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
#define OPAL_HWLOC_BASE_VERSION_2_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "hwloc", 2, 0, 0

END_C_DECLS

/* Define a hierarchical level value that
 * helps resolve the hwloc behavior of
 * treating caches as a single type of
 * entity - must always be available
 */
typedef enum uint8_t {
    OPAL_HWLOC_NODE_LEVEL=0,
    OPAL_HWLOC_NUMA_LEVEL,
    OPAL_HWLOC_SOCKET_LEVEL,
    OPAL_HWLOC_L3CACHE_LEVEL,
    OPAL_HWLOC_L2CACHE_LEVEL,
    OPAL_HWLOC_L1CACHE_LEVEL,
    OPAL_HWLOC_CORE_LEVEL,
    OPAL_HWLOC_HWTHREAD_LEVEL
} opal_hwloc_level_t;
#define OPAL_HWLOC_LEVEL_T OPAL_UINT8

/* include implementation to call */
#if OPAL_HAVE_HWLOC
#include MCA_hwloc_IMPLEMENTATION_HEADER


/* define type of processor info requested */
typedef uint8_t opal_hwloc_resource_type_t;
#define OPAL_HWLOC_PHYSICAL   1
#define OPAL_HWLOC_LOGICAL    2
#define OPAL_HWLOC_AVAILABLE  3

/* structs for storing info on objects */
typedef struct {
    opal_object_t super;
    hwloc_cpuset_t available;
    unsigned int npus;
    unsigned int idx;
} opal_hwloc_obj_data_t;
OBJ_CLASS_DECLARATION(opal_hwloc_obj_data_t);

typedef struct {
    opal_list_item_t super;
    hwloc_obj_type_t type;
    unsigned cache_level;
    unsigned int num_objs;
    opal_hwloc_resource_type_t rtype;
} opal_hwloc_summary_t;
OBJ_CLASS_DECLARATION(opal_hwloc_summary_t);

typedef struct {
    opal_object_t super;
    hwloc_cpuset_t available;
    opal_list_t summaries;
} opal_hwloc_topo_data_t;
OBJ_CLASS_DECLARATION(opal_hwloc_topo_data_t);

/* define binding policies */
typedef uint16_t opal_binding_policy_t;
#define OPAL_BINDING_POLICY OPAL_UINT16

/* binding directives */
#define OPAL_BIND_IF_SUPPORTED      0x1000
#define OPAL_BIND_ALLOW_OVERLOAD    0x2000
#define OPAL_BIND_GIVEN             0x4000
/* binding policies */
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
    (target) = (pol) | ((target) & 0xf000)
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

#endif

#endif /* OPAL_HWLOC_H_ */
