/*
 * Copyright (c) 2011-2017 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2021      Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_HWLOC_BASE_H
#define OPAL_HWLOC_BASE_H

#include "opal_config.h"

#include "opal/mca/hwloc/hwloc-internal.h"

#if HWLOC_API_VERSION < 0x20000
#    define HWLOC_OBJ_L3CACHE HWLOC_OBJ_CACHE
#    define HWLOC_OBJ_L2CACHE HWLOC_OBJ_CACHE
#    define HWLOC_OBJ_L1CACHE HWLOC_OBJ_CACHE
#endif

/*
 * Global functions for MCA overall hwloc open and close
 */

BEGIN_C_DECLS

/* ******************************************************************** */

/**
 * Note that the open function does NOT fill the global variable
 * opal_hwloc_topology, nor does it set the process-wide memory
 * affinity policy.  Filling opal_hwloc_topology via
 * hwloc_topology_load() can be expensive (and/or serialized by the
 * OS); it may not be desireable to call this function in every MPI
 * process on a machine.  Hence, it is the responsibility for an upper
 * layer to both fill opal_hwloc_topology in some scalable way, as
 * well as to invoke opal_hwloc_base_set_process_membind_policy()
 * (after opal_hwloc_topology has been loaded) to set the process-wide
 * memory affinity policy.
 */

/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern bool opal_hwloc_base_inited;
OPAL_DECLSPEC extern bool opal_hwloc_topology_inited;

OPAL_DECLSPEC extern mca_base_framework_t opal_hwloc_base_framework;

/* we always must have some minimal locality support */
#define OPAL_HWLOC_PRINT_MAX_SIZE 50
#define OPAL_HWLOC_PRINT_NUM_BUFS 16
typedef struct {
    char *buffers[OPAL_HWLOC_PRINT_NUM_BUFS];
    int cntr;
} opal_hwloc_print_buffers_t;
opal_hwloc_print_buffers_t *opal_hwloc_get_print_buffer(void);

/* convenience macro for debugging */
#define OPAL_HWLOC_SHOW_BINDING(n, v, t)                                                        \
    do {                                                                                        \
        char tmp1[1024];                                                                        \
        hwloc_cpuset_t bind;                                                                    \
        bind = opal_hwloc_alloc();                                                              \
        if (hwloc_get_cpubind(t, bind, HWLOC_CPUBIND_PROCESS) < 0) {                            \
            opal_output_verbose(n, v, "CANNOT DETERMINE BINDING AT %s:%d", __FILE__, __LINE__); \
        } else {                                                                                \
            opal_hwloc_base_cset2mapstr(tmp1, sizeof(tmp1), t, bind);                           \
            opal_output_verbose(n, v, "BINDINGS AT %s:%d: %s", __FILE__, __LINE__, tmp1);       \
        }                                                                                       \
        hwloc_bitmap_free(bind);                                                                \
    } while (0);

#if HWLOC_API_VERSION < 0x20000
#    define OPAL_HWLOC_MAKE_OBJ_CACHE(level, obj, cache_level) \
        do {                                                   \
            obj = HWLOC_OBJ_CACHE;                             \
            cache_level = level;                               \
        } while (0)
#else
#    define OPAL_HWLOC_MAKE_OBJ_CACHE(level, obj, cache_level) \
        do {                                                   \
            obj = HWLOC_OBJ_L##level##CACHE;                   \
            cache_level = 0;                                   \
        } while (0)
#endif

struct opal_rmaps_numa_node_t {
    opal_list_item_t super;
    int index;
    float dist_from_closed;
};
typedef struct opal_rmaps_numa_node_t opal_rmaps_numa_node_t;
OBJ_CLASS_DECLARATION(opal_rmaps_numa_node_t);

/**
 * Enum for what memory allocation policy we want for user allocations.
 * MAP = memory allocation policy.
 */
typedef enum { OPAL_HWLOC_BASE_MAP_NONE, OPAL_HWLOC_BASE_MAP_LOCAL_ONLY } opal_hwloc_base_map_t;

/**
 * Global reflecting the MAP (set by MCA param).
 */
OPAL_DECLSPEC extern opal_hwloc_base_map_t opal_hwloc_base_map;

/**
 * Enum for what to do if the hwloc framework tries to bind memory
 * and fails.  BFA = bind failure action.
 */
typedef enum {
    OPAL_HWLOC_BASE_MBFA_SILENT,
    OPAL_HWLOC_BASE_MBFA_WARN,
    OPAL_HWLOC_BASE_MBFA_ERROR
} opal_hwloc_base_mbfa_t;

/**
 * Global reflecting the BFA (set by MCA param).
 */
OPAL_DECLSPEC extern opal_hwloc_base_mbfa_t opal_hwloc_base_mbfa;

/**
 * Discover / load the hwloc topology (i.e., call hwloc_topology_init() and
 * hwloc_topology_load()).
 */
OPAL_DECLSPEC int opal_hwloc_base_get_topology(void);

/**
 * Free the hwloc topology.
 */
OPAL_DECLSPEC void opal_hwloc_base_free_topology(hwloc_topology_t topo);
OPAL_DECLSPEC unsigned int opal_hwloc_base_get_nbobjs_by_type(hwloc_topology_t topo,
                                                              hwloc_obj_type_t target,
                                                              unsigned cache_level,
                                                              opal_hwloc_resource_type_t rtype);

OPAL_DECLSPEC hwloc_obj_t opal_hwloc_base_get_obj_by_type(hwloc_topology_t topo,
                                                          hwloc_obj_type_t target,
                                                          unsigned cache_level,
                                                          unsigned int instance,
                                                          opal_hwloc_resource_type_t rtype);

/**
 * This function sets the process-wide memory affinity policy
 * according to opal_hwloc_base_map and opal_hwloc_base_mbfa.  It needs
 * to be a separate, standalone function (as opposed to being done
 * during opal_hwloc_base_open()) because opal_hwloc_topology is not
 * loaded by opal_hwloc_base_open().  Hence, an upper layer needs to
 * invoke this function after opal_hwloc_topology has been loaded.
 */
OPAL_DECLSPEC int opal_hwloc_base_set_process_membind_policy(void);

OPAL_DECLSPEC int opal_hwloc_base_membind(opal_hwloc_base_memory_segment_t *segs, size_t count,
                                          int node_id);

OPAL_DECLSPEC int opal_hwloc_base_memory_set(opal_hwloc_base_memory_segment_t *segments,
                                             size_t num_segments);

/**
 * Make a prettyprint string for a hwloc_cpuset_t (e.g., "socket
 * 2[core 3]").
 */
OPAL_DECLSPEC int opal_hwloc_base_cset2str(char *str, int len, hwloc_topology_t topo,
                                           hwloc_cpuset_t cpuset);

/* extract a location from the locality string */
OPAL_DECLSPEC char *opal_hwloc_base_get_location(char *locality, hwloc_obj_type_t type,
                                                 unsigned index);

OPAL_DECLSPEC opal_hwloc_locality_t opal_hwloc_compute_relative_locality(char *loc1, char *loc2);

END_C_DECLS

#endif /* OPAL_HWLOC_BASE_H */
