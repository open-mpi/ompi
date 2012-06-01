/*
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef OPAL_HWLOC_BASE_H
#define OPAL_HWLOC_BASE_H

#include "opal_config.h"

#include "opal/dss/dss_types.h"

#include "opal/mca/hwloc/hwloc.h"

/*
 * Global functions for MCA overall hwloc open and close
 */

BEGIN_C_DECLS

/* ******************************************************************** */

/**
 * Initialize the hwloc MCA framework
 *
 * @retval OPAL_SUCCESS Upon success
 * @retval OPAL_ERROR Upon failure
 *
 * This must be the first function invoked in the hwloc MCA
 * framework.  It initializes the hwloc MCA framework, finds
 * and opens hwloc components, etc.
 *
 * This function is invoked during opal_init().
 * 
 * This function fills in the internal global variable
 * opal_hwloc_base_components_opened, which is a list of all
 * hwloc components that were successfully opened.  This
 * variable should \em only be used by other hwloc base
 * functions -- it is not considered a public interface member --
 * and is only mentioned here for completeness.
 *
 * Note that this function does NOT fill the global variable
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
OPAL_DECLSPEC int opal_hwloc_base_open(void);

/**
 * Shut down the hwloc MCA framework.
 *
 * @retval OPAL_SUCCESS Always
 *
 * This function shuts down everything in the hwloc MCA
 * framework, and is called during opal_finalize().
 *
 * It must be the last function invoked on the hwloc MCA
 * framework.
 */
OPAL_DECLSPEC int opal_hwloc_base_close(void);

/**
 * Debugging output stream
 */
OPAL_DECLSPEC extern int opal_hwloc_base_output;
OPAL_DECLSPEC extern opal_list_t opal_hwloc_base_components;
OPAL_DECLSPEC extern bool opal_hwloc_base_inited;
OPAL_DECLSPEC extern bool opal_hwloc_topology_inited;

#if OPAL_HAVE_HWLOC
OPAL_DECLSPEC extern char *opal_hwloc_base_slot_list;
OPAL_DECLSPEC extern char *opal_hwloc_base_cpu_set;
OPAL_DECLSPEC extern hwloc_cpuset_t opal_hwloc_base_given_cpus;

OPAL_DECLSPEC opal_hwloc_locality_t opal_hwloc_base_get_relative_locality(hwloc_topology_t topo,
                                                                              opal_hwloc_level_t level1,
                                                                              unsigned int peer1,
                                                                              opal_hwloc_level_t level2,
                                                                              unsigned int peer2);

/**
 * Loads opal_hwloc_my_cpuset (global variable in
 * opal/mca/hwloc/hwloc.h) for this process.  opal_hwloc_my_cpuset
 * will be loaded with this process' binding, or, if the process is
 * not bound, use the hwloc root object's (available and online)
 * cpuset.
 */
OPAL_DECLSPEC void opal_hwloc_base_get_local_cpuset(void);

/**
 * Enum for what memory allocation policy we want for user allocations.
 * MAP = memory allocation policy.
 */
typedef enum {
    OPAL_HWLOC_BASE_MAP_NONE,
    OPAL_HWLOC_BASE_MAP_LOCAL_ONLY
} opal_hwloc_base_map_t;

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

/* some critical helper functions */
OPAL_DECLSPEC int opal_hwloc_base_filter_cpus(hwloc_topology_t topo);

/**
 * Discover / load the hwloc topology (i.e., call hwloc_topology_init() and
 * hwloc_topology_load()).
 */
OPAL_DECLSPEC int opal_hwloc_base_get_topology(void);

/**
 * Free the hwloc topology.
 */
OPAL_DECLSPEC void opal_hwloc_base_free_topology(hwloc_topology_t topo);
OPAL_DECLSPEC hwloc_cpuset_t opal_hwloc_base_get_available_cpus(hwloc_topology_t topo,
                                                                hwloc_obj_t obj);
OPAL_DECLSPEC unsigned int opal_hwloc_base_get_nbobjs_by_type(hwloc_topology_t topo,
                                                              hwloc_obj_type_t target,
                                                              unsigned cache_level,
                                                              opal_hwloc_resource_type_t rtype);
OPAL_DECLSPEC hwloc_obj_t opal_hwloc_base_get_obj_by_type(hwloc_topology_t topo,
                                                          hwloc_obj_type_t target,
                                                          unsigned cache_level,
                                                          unsigned int instance,
                                                          opal_hwloc_resource_type_t rtype);
OPAL_DECLSPEC unsigned int opal_hwloc_base_get_obj_idx(hwloc_topology_t topo,
                                                       hwloc_obj_t obj,
                                                       opal_hwloc_resource_type_t rtype);
OPAL_DECLSPEC hwloc_obj_t opal_hwloc_base_get_level_and_index(hwloc_cpuset_t cpus,
                                                              opal_hwloc_level_t *bind_level,
                                                              unsigned int *bind_idx);
OPAL_DECLSPEC int opal_hwloc_base_get_local_index(hwloc_obj_type_t type,
                                                  unsigned cache_level,
                                                  unsigned int *idx);

/**
 * Get the number of pu's under a given hwloc object.
 */
OPAL_DECLSPEC unsigned int opal_hwloc_base_get_npus(hwloc_topology_t topo,
                                                    hwloc_obj_t target);
OPAL_DECLSPEC char* opal_hwloc_base_print_binding(opal_binding_policy_t binding);
OPAL_DECLSPEC char* opal_hwloc_base_print_locality(opal_hwloc_locality_t locality);
OPAL_DECLSPEC char* opal_hwloc_base_print_level(opal_hwloc_level_t level);

/**
 * Determine if there is a single cpu in a bitmap.
 */
OPAL_DECLSPEC bool opal_hwloc_base_single_cpu(hwloc_cpuset_t cpuset);

/**
 * Provide a utility to parse a slot list against the local
 * logical cpus, and produce a cpuset for the described binding
 */
OPAL_DECLSPEC int opal_hwloc_base_slot_list_parse(const char *slot_str,
                                                  hwloc_topology_t topo,
                                                  hwloc_cpuset_t cpumask);

/**
 * Report a bind failure using the normal mechanisms if a component
 * fails to bind memory -- according to the value of the
 * hwloc_base_bind_failure_action MCA parameter.
 */
OPAL_DECLSPEC int opal_hwloc_base_report_bind_failure(const char *file,
                                                      int line,
                                                      const char *msg, 
                                                      int rc);

/**
 * This function sets the process-wide memory affinity policy
 * according to opal_hwloc_base_map and opal_hwloc_base_mbfa.  It needs
 * to be a separate, standalone function (as opposed to being done
 * during opal_hwloc_base_open()) because opal_hwloc_topology is not
 * loaded by opal_hwloc_base_open().  Hence, an upper layer needs to
 * invoke this function after opal_hwloc_topology has been loaded.
 */
OPAL_DECLSPEC int opal_hwloc_base_set_process_membind_policy(void);

OPAL_DECLSPEC int opal_hwloc_base_membind(opal_hwloc_base_memory_segment_t *segs,
                                          size_t count, int node_id);

OPAL_DECLSPEC int opal_hwloc_base_node_name_to_id(char *node_name, int *id);

OPAL_DECLSPEC int opal_hwloc_base_memory_set(opal_hwloc_base_memory_segment_t *segments,
                                             size_t num_segments);

/* datatype support */
OPAL_DECLSPEC int opal_hwloc_pack(opal_buffer_t *buffer, const void *src,
                                  int32_t num_vals,
                                  opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_unpack(opal_buffer_t *buffer, void *dest,
                                    int32_t *num_vals,
                                    opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_copy(hwloc_topology_t *dest,
                                  hwloc_topology_t src,
                                  opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_compare(const hwloc_topology_t topo1,
                                     const hwloc_topology_t topo2,
                                     opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_print(char **output, char *prefix,
                                   hwloc_topology_t src,
                                   opal_data_type_t type);
OPAL_DECLSPEC int opal_hwloc_size(size_t *size,
                                  hwloc_topology_t src,
                                  opal_data_type_t type);
OPAL_DECLSPEC void opal_hwloc_release(opal_dss_value_t *value);

/**
 * Make a prettyprint string for a hwloc_cpuset_t (e.g., "socket
 * 2[core 3]").
 */
int opal_hwloc_base_cset2str(char *str, int len, hwloc_cpuset_t cpuset);

/**
 * Make a prettyprint string for a cset in a map format.  
 * Example: [B./..]
 * Key:  [] - signifies socket
 *        / - divider between cores
 *        . - signifies PU a process not bound to
 *        B - signifies PU a process is bound to
 */
int opal_hwloc_base_cset2mapstr(char *str, int len, hwloc_cpuset_t cpuset);

#endif

END_C_DECLS

#endif /* OPAL_HWLOC_BASE_H */
