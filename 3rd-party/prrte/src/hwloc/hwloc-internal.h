/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 */

#ifndef PRTE_MCA_HWLOC_H
#define PRTE_MCA_HWLOC_H

#include "prte_config.h"

#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif
#include <stdarg.h>
#include <stdint.h>
#include <hwloc.h>
#if HWLOC_API_VERSION >= 0x20000
#   include <hwloc/shmem.h>
#endif

#if HWLOC_API_VERSION < 0x10b00
#define HWLOC_OBJ_NUMANODE HWLOC_OBJ_NODE
#define HWLOC_OBJ_PACKAGE HWLOC_OBJ_SOCKET
#endif
#if HWLOC_API_VERSION < 0x10a00
static inline hwloc_obj_t hwloc_get_numanode_obj_by_os_index(hwloc_topology_t topology, unsigned os_index)
{
    hwloc_obj_t obj = NULL;
    while ((obj = hwloc_get_next_obj_by_type(topology, HWLOC_OBJ_NUMANODE, obj)) != NULL)
        if (obj->os_index == os_index)
            return obj;
    return NULL;
}
#endif

#include "src/class/pmix_list.h"
#include "src/class/pmix_value_array.h"

BEGIN_C_DECLS

/* ******************************************************************** */
/* Although we cannot bind if --without-hwloc is set,
 * we do still need to know some basic locality data
 * like on_node and not_on_node. So ensure that we
 * always have access to that much info by including
 * the definitions here, outside the if-have-hwloc test
 */
typedef uint16_t prte_hwloc_locality_t;
#define PRTE_HWLOC_LOCALITY_T PRTE_UINT16

/** Process locality definitions */
enum {
    PRTE_PROC_LOCALITY_UNKNOWN = 0x0000,
    PRTE_PROC_NON_LOCAL = 0x8000,
    PRTE_PROC_ON_CLUSTER = 0x0001,
    PRTE_PROC_ON_CU = 0x0002,
    PRTE_PROC_ON_HOST = 0x0004,
    PRTE_PROC_ON_NODE = 0x000c, // same host
    PRTE_PROC_ON_PACKAGE = 0x0020,
    PRTE_PROC_ON_NUMA = 0x0040,
    PRTE_PROC_ON_L3CACHE = 0x0080,
    PRTE_PROC_ON_L2CACHE = 0x0100,
    PRTE_PROC_ON_L1CACHE = 0x0200,
    PRTE_PROC_ON_CORE = 0x0400,
    PRTE_PROC_ON_HWTHREAD = 0x0800,
    PRTE_PROC_ALL_LOCAL = 0x0fff,
};

/** Process locality macros */
#define PRTE_PROC_ON_LOCAL_CLUSTER(n)  (!!((n) &PRTE_PROC_ON_CLUSTER))
#define PRTE_PROC_ON_LOCAL_CU(n)       (!!((n) &PRTE_PROC_ON_CU))
#define PRTE_PROC_ON_LOCAL_HOST(n)     (!!((n) &PRTE_PROC_ON_HOST))
#define PRTE_PROC_ON_LOCAL_NODE(n)     (!!((n) &PRTE_PROC_ON_LOCAL_HOST(n)))
#define PRTE_PROC_ON_LOCAL_PACKAGE(n)  (!!((n) &PRTE_PROC_ON_PACKAGE))
#define PRTE_PROC_ON_LOCAL_NUMA(n)     (!!((n) &PRTE_PROC_ON_NUMA))
#define PRTE_PROC_ON_LOCAL_L3CACHE(n)  (!!((n) &PRTE_PROC_ON_L3CACHE))
#define PRTE_PROC_ON_LOCAL_L2CACHE(n)  (!!((n) &PRTE_PROC_ON_L2CACHE))
#define PRTE_PROC_ON_LOCAL_L1CACHE(n)  (!!((n) &PRTE_PROC_ON_L1CACHE))
#define PRTE_PROC_ON_LOCAL_CORE(n)     (!!((n) &PRTE_PROC_ON_CORE))
#define PRTE_PROC_ON_LOCAL_HWTHREAD(n) (!!((n) &PRTE_PROC_ON_HWTHREAD))

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
} prte_hwloc_base_memory_segment_t;

/* define binding policies */
typedef uint16_t prte_binding_policy_t;
#define PRTE_BINDING_POLICY PRTE_UINT16

/* binding directives */
#define PRTE_BIND_IF_SUPPORTED   0x1000
#define PRTE_BIND_ALLOW_OVERLOAD 0x2000
#define PRTE_BIND_GIVEN          0x4000
// overload policy was given
#define PRTE_BIND_OVERLOAD_GIVEN 0x0100

/* binding policies - any changes in these
 * values must be reflected in prte/mca/rmaps/rmaps.h
 */
#define PRTE_BIND_TO_NONE            1
#define PRTE_BIND_TO_PACKAGE         2
#define PRTE_BIND_TO_NUMA            3
#define PRTE_BIND_TO_L3CACHE         4
#define PRTE_BIND_TO_L2CACHE         5
#define PRTE_BIND_TO_L1CACHE         6
#define PRTE_BIND_TO_CORE            7
#define PRTE_BIND_TO_HWTHREAD        8
#define PRTE_GET_BINDING_POLICY(pol) ((pol) &0x00ff)
#define PRTE_SET_BINDING_POLICY(target, pol) \
    (target) = (pol) | (((target) & 0xff00) | PRTE_BIND_GIVEN)
#define PRTE_SET_DEFAULT_BINDING_POLICY(target, pol)                           \
    do {                                                                       \
        if (!PRTE_BINDING_POLICY_IS_SET((target))) {                           \
            (target) = (pol) | (((target) & 0xff00) | PRTE_BIND_IF_SUPPORTED); \
        }                                                                      \
    } while (0);

/* check if policy is set */
#define PRTE_BINDING_POLICY_IS_SET(pol) ((pol) &0x4000)
/* macro to detect if binding was qualified */
#define PRTE_BINDING_REQUIRED(n) (!(PRTE_BIND_IF_SUPPORTED & (n)))
/* macro to detect if binding is forced */
#define PRTE_BIND_OVERLOAD_ALLOWED(n)  (PRTE_BIND_ALLOW_OVERLOAD & (n))
#define PRTE_BIND_OVERLOAD_SET(n) (PRTE_BIND_OVERLOAD_GIVEN & (n))

/* some global values */
PRTE_EXPORT extern hwloc_topology_t prte_hwloc_topology;
PRTE_EXPORT extern prte_binding_policy_t prte_hwloc_default_binding_policy;
PRTE_EXPORT extern hwloc_obj_type_t prte_hwloc_levels[];
PRTE_EXPORT extern char *prte_hwloc_default_cpu_list;
PRTE_EXPORT extern bool prte_hwloc_default_use_hwthread_cpus;

#if HWLOC_API_VERSION < 0x20000
#    define HWLOC_OBJ_L3CACHE HWLOC_OBJ_CACHE
#    define HWLOC_OBJ_L2CACHE HWLOC_OBJ_CACHE
#    define HWLOC_OBJ_L1CACHE HWLOC_OBJ_CACHE
#    if HWLOC_API_VERSION < 0x10a00
#        define HWLOC_OBJ_PACKAGE HWLOC_OBJ_SOCKET
#    endif
#    define HAVE_DECL_HWLOC_OBJ_OSDEV_COPROC 0
#    define HAVE_HWLOC_TOPOLOGY_DUP          0
#else
#    define HAVE_DECL_HWLOC_OBJ_OSDEV_COPROC 1
#    define HAVE_HWLOC_TOPOLOGY_DUP          1
#endif

/**
 * Debugging output stream
 */
PRTE_EXPORT extern int prte_hwloc_base_output;
PRTE_EXPORT extern bool prte_hwloc_base_inited;

/* we always must have some minimal locality support */
#define PRTE_HWLOC_PRINT_MAX_SIZE 50
#define PRTE_HWLOC_PRINT_NUM_BUFS 16
typedef struct {
    char *buffers[PRTE_HWLOC_PRINT_NUM_BUFS];
    int cntr;
} prte_hwloc_print_buffers_t;
prte_hwloc_print_buffers_t *prte_hwloc_get_print_buffer(void);
extern char *prte_hwloc_print_null;
PRTE_EXPORT char *prte_hwloc_base_print_locality(prte_hwloc_locality_t locality);

PRTE_EXPORT extern char *prte_hwloc_base_topo_file;
PRTE_EXPORT extern bool prte_hwloc_synthetic_topo;

/* convenience macro for debugging */
#define PRTE_HWLOC_SHOW_BINDING(n, v, t)                                                        \
    do {                                                                                        \
        char tmp1[1024];                                                                        \
        hwloc_cpuset_t bind;                                                                    \
        bind = prte_hwloc_alloc();                                                              \
        if (hwloc_get_cpubind(t, bind, HWLOC_CPUBIND_PROCESS) < 0) {                            \
            pmix_output_verbose(n, v, "CANNOT DETERMINE BINDING AT %s:%d", __FILE__, __LINE__); \
        } else {                                                                                \
            prte_hwloc_base_cset2mapstr(tmp1, sizeof(tmp1), t, bind);                           \
            pmix_output_verbose(n, v, "BINDINGS AT %s:%d: %s", __FILE__, __LINE__, tmp1);       \
        }                                                                                       \
        hwloc_bitmap_free(bind);                                                                \
    } while (0);

#if HWLOC_API_VERSION < 0x20000
#    define PRTE_HWLOC_MAKE_OBJ_CACHE(level, obj, cache_level) \
        do {                                                   \
            obj = HWLOC_OBJ_CACHE;                             \
            cache_level = level;                               \
        } while (0)
#else
#    define PRTE_HWLOC_MAKE_OBJ_CACHE(level, obj, cache_level) \
        do {                                                   \
            obj = HWLOC_OBJ_L##level##CACHE;                   \
            cache_level = 0;                                   \
        } while (0)
#endif

PRTE_EXPORT prte_hwloc_locality_t prte_hwloc_base_get_relative_locality(hwloc_topology_t topo,
                                                                        char *cpuset1,
                                                                        char *cpuset2);

PRTE_EXPORT int prte_hwloc_base_set_default_binding(void *jdata,
                                                    void *options);
PRTE_EXPORT int prte_hwloc_base_set_binding_policy(void *jdata, char *spec);

struct prte_rmaps_numa_node_t {
    pmix_list_item_t super;
    int index;
    float dist_from_closed;
};
typedef struct prte_rmaps_numa_node_t prte_rmaps_numa_node_t;
PMIX_CLASS_DECLARATION(prte_rmaps_numa_node_t);

/**
 * Enum for what memory allocation policy we want for user allocations.
 * MAP = memory allocation policy.
 */
typedef enum { PRTE_HWLOC_BASE_MAP_NONE, PRTE_HWLOC_BASE_MAP_LOCAL_ONLY } prte_hwloc_base_map_t;

/**
 * Global reflecting the MAP (set by MCA param).
 */
PRTE_EXPORT extern prte_hwloc_base_map_t prte_hwloc_base_map;

/**
 * Enum for what to do if the hwloc framework tries to bind memory
 * and fails.  BFA = bind failure action.
 */
typedef enum {
    PRTE_HWLOC_BASE_MBFA_SILENT,
    PRTE_HWLOC_BASE_MBFA_WARN,
    PRTE_HWLOC_BASE_MBFA_ERROR
} prte_hwloc_base_mbfa_t;

/**
 * Global reflecting the BFA (set by MCA param).
 */
PRTE_EXPORT extern prte_hwloc_base_mbfa_t prte_hwloc_base_mbfa;

/**
 * Discover / load the hwloc topology (i.e., call hwloc_topology_init() and
 * hwloc_topology_load()).
 */
PRTE_EXPORT int prte_hwloc_base_get_topology(void);
PRTE_EXPORT hwloc_cpuset_t prte_hwloc_base_setup_summary(hwloc_topology_t topo);

/**
 * Set the hwloc topology to that from the given topo file
 */
PRTE_EXPORT int prte_hwloc_base_set_topology(char *topofile);

PRTE_EXPORT hwloc_cpuset_t prte_hwloc_base_generate_cpuset(hwloc_topology_t topo,
                                                           bool use_hwthread_cpus, char *cpulist);

PRTE_EXPORT hwloc_cpuset_t prte_hwloc_base_filter_cpus(hwloc_topology_t topo);

/**
 * Free the hwloc topology.
 */
PRTE_EXPORT unsigned int prte_hwloc_base_get_nbobjs_by_type(hwloc_topology_t topo,
                                                            hwloc_obj_type_t target,
                                                            unsigned cache_level);

PRTE_EXPORT hwloc_obj_t prte_hwloc_base_get_obj_by_type(hwloc_topology_t topo,
                                                        hwloc_obj_type_t target,
                                                        unsigned cache_level,
                                                        unsigned int instance);
PRTE_EXPORT unsigned int prte_hwloc_base_get_obj_idx(hwloc_topology_t topo, hwloc_obj_t obj);

/**
 * Get the number of pu's under a given hwloc object.
 */
PRTE_EXPORT unsigned int prte_hwloc_base_get_npus(hwloc_topology_t topo, bool use_hwthread_cpus,
                                                  hwloc_cpuset_t envelope, hwloc_obj_t target);
PRTE_EXPORT char *prte_hwloc_base_print_binding(prte_binding_policy_t binding);

/**
 * Determine if there is a single cpu in a bitmap.
 */
PRTE_EXPORT bool prte_hwloc_base_single_cpu(hwloc_cpuset_t cpuset);

/**
 * Provide a utility to parse a slot list against the local
 * cpus of given type, and produce a cpuset for the described binding
 */
PRTE_EXPORT int prte_hwloc_base_cpu_list_parse(const char *slot_str, hwloc_topology_t topo,
                                                bool use_hwthread_cpus, hwloc_cpuset_t cpumask);

PRTE_EXPORT char *prte_hwloc_base_find_coprocessors(hwloc_topology_t topo);
PRTE_EXPORT char *prte_hwloc_base_check_on_coprocessor(void);

/**
 * Report a bind failure using the normal mechanisms if a component
 * fails to bind memory -- according to the value of the
 * hwloc_base_bind_failure_action MCA parameter.
 */
PRTE_EXPORT int prte_hwloc_base_report_bind_failure(const char *file, int line, const char *msg,
                                                    int rc);

/**
 * This function sets the process-wide memory affinity policy
 * according to prte_hwloc_base_map and prte_hwloc_base_mbfa.  It needs
 * to be a separate, standalone function (as opposed to being done
 * during prte_hwloc_base_open()) because prte_hwloc_topology is not
 * loaded by prte_hwloc_base_open().  Hence, an upper layer needs to
 * invoke this function after prte_hwloc_topology has been loaded.
 */
PRTE_EXPORT int prte_hwloc_base_set_process_membind_policy(void);

PRTE_EXPORT int prte_hwloc_base_membind(prte_hwloc_base_memory_segment_t *segs, size_t count,
                                        int node_id);

PRTE_EXPORT int prte_hwloc_base_node_name_to_id(char *node_name, int *id);

PRTE_EXPORT int prte_hwloc_base_memory_set(prte_hwloc_base_memory_segment_t *segments,
                                           size_t num_segments);

/**
 * Make a prettyprint string for a hwloc_cpuset_t (e.g., "package
 * 2[core 3]").
 */
PRTE_EXPORT char *prte_hwloc_base_cset2str(hwloc_const_cpuset_t cpuset,
                                           bool use_hwthread_cpus,
                                           hwloc_topology_t topo);

PRTE_EXPORT void prte_hwloc_get_binding_info(hwloc_const_cpuset_t cpuset,
                                             bool use_hwthread_cpus,
                                             hwloc_topology_t topo, int *pkgnum, 
                                             char *cores, int sz);

/* get the hwloc object that corresponds to the given processor id  and type */
PRTE_EXPORT hwloc_obj_t prte_hwloc_base_get_pu(hwloc_topology_t topo, bool use_hwthread_cpus,
                                               int lid);

/* get the topology "signature" so we can check for differences - caller
 * if responsible for freeing the returned string */
PRTE_EXPORT char *prte_hwloc_base_get_topo_signature(hwloc_topology_t topo);

/* get a string describing the locality of a given process */
PRTE_EXPORT char *prte_hwloc_base_get_locality_string(hwloc_topology_t topo, char *bitmap);

/* extract a location from the locality string */
PRTE_EXPORT char *prte_hwloc_base_get_location(char *locality, hwloc_obj_type_t type,
                                               unsigned index);

PRTE_EXPORT prte_hwloc_locality_t prte_hwloc_compute_relative_locality(char *loc1, char *loc2);

PRTE_EXPORT int prte_hwloc_base_topology_export_xmlbuffer(hwloc_topology_t topology, char **xmlpath,
                                                          int *buflen);

PRTE_EXPORT int prte_hwloc_base_topology_set_flags(hwloc_topology_t topology, unsigned long flags,
                                                   bool io);

PRTE_EXPORT int prte_hwloc_base_open(void);
PRTE_EXPORT void prte_hwloc_base_close(void);
PRTE_EXPORT int prte_hwloc_base_register(void);
PRTE_EXPORT int prte_hwloc_print(char **output, char *prefix, hwloc_topology_t src);

PRTE_EXPORT void prte_hwloc_build_map(hwloc_topology_t topo,
                                      hwloc_cpuset_t avail,
                                      bool use_hwthread_cpus,
                                      hwloc_bitmap_t coreset);

PRTE_EXPORT bool prte_hwloc_base_core_cpus(hwloc_topology_t topo);

END_C_DECLS

#endif /* PRTE_HWLOC_H_ */
