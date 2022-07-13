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
 * OS); it may not be desirable to call this function in every MPI
 * process on a machine.  Hence, it is the responsibility for an upper
 * layer to both fill opal_hwloc_topology in some scalable way.
 */

/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern bool opal_hwloc_base_inited;
OPAL_DECLSPEC extern bool opal_hwloc_topology_inited;

OPAL_DECLSPEC extern mca_base_framework_t opal_hwloc_base_framework;

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
 * Discover / load the hwloc topology (i.e., call hwloc_topology_init() and
 * hwloc_topology_load()).
 */
OPAL_DECLSPEC int opal_hwloc_base_get_topology(void);

OPAL_DECLSPEC unsigned int opal_hwloc_base_get_nbobjs_by_type(hwloc_topology_t topo,
                                                              hwloc_obj_type_t target,
                                                              unsigned cache_level,
                                                              opal_hwloc_resource_type_t rtype);

OPAL_DECLSPEC hwloc_obj_t opal_hwloc_base_get_obj_by_type(hwloc_topology_t topo,
                                                          hwloc_obj_type_t target,
                                                          unsigned cache_level,
                                                          unsigned int instance,
                                                          opal_hwloc_resource_type_t rtype);

OPAL_DECLSPEC int opal_hwloc_base_membind(opal_hwloc_base_memory_segment_t *segs, size_t count,
                                          int node_id);

OPAL_DECLSPEC int opal_hwloc_base_memory_set(opal_hwloc_base_memory_segment_t *segments,
                                             size_t num_segments);

/* extract a location from the locality string */
OPAL_DECLSPEC char *opal_hwloc_base_get_location(char *locality, hwloc_obj_type_t type,
                                                 unsigned index);

OPAL_DECLSPEC opal_hwloc_locality_t opal_hwloc_compute_relative_locality(char *loc1, char *loc2);

END_C_DECLS

#endif /* OPAL_HWLOC_BASE_H */
