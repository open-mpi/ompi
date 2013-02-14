/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "opal/util/output.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/util/nidmap.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ess/base/base.h"

int orte_ess_base_proc_binding(void)
{
#if OPAL_HAVE_HWLOC
    hwloc_obj_t node, obj;
    hwloc_cpuset_t cpus, nodeset;
    hwloc_obj_type_t target;
    unsigned int cache_level = 0;
    struct hwloc_topology_support *support;
    char *map;
    int ret;
    char *error;

    /* Determine if we were pre-bound or not */
    if (NULL != getenv("OMPI_MCA_orte_bound_at_launch")) {
        orte_proc_is_bound = true;
        if (NULL != (map = getenv("OMPI_MCA_orte_base_applied_binding"))) {
            orte_proc_applied_binding = hwloc_bitmap_alloc();
            if (0 != (ret = hwloc_bitmap_list_sscanf(orte_proc_applied_binding, map))) {
                error = "applied_binding parse";
                goto error;
            }
        }
    }

    /* see if we were bound when launched */
    if (!orte_proc_is_bound) {
        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                             "%s Not bound at launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* we were not bound at launch */
        if (NULL != opal_hwloc_topology) {
            support = (struct hwloc_topology_support*)hwloc_topology_get_support(opal_hwloc_topology);
            /* get our node object */
            node = hwloc_get_root_obj(opal_hwloc_topology);
            nodeset = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, node);
            /* get our bindings */
            cpus = hwloc_bitmap_alloc();
            if (hwloc_get_cpubind(opal_hwloc_topology, cpus, HWLOC_CPUBIND_PROCESS) < 0) {
                /* we are NOT bound if get_cpubind fails, nor can we be bound - the
                 * environment does not support it
                 */
                hwloc_bitmap_free(cpus);
                OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                     "%s Binding not supported",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                goto MOVEON;
            }
            /* we are bound if the two cpusets are not equal,
             * or if there is only ONE cpu available to us
             */
            if (0 != hwloc_bitmap_compare(cpus, nodeset) ||
                opal_hwloc_base_single_cpu(nodeset) ||
                opal_hwloc_base_single_cpu(cpus)) {
                /* someone external set it - indicate it is set
                 * so that we know
                 */
                orte_proc_is_bound = true;
                hwloc_bitmap_free(cpus);
                OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                     "%s Process was externally bound",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            } else if (support->cpubind->set_thisproc_cpubind &&
                       OPAL_BINDING_POLICY_IS_SET(opal_hwloc_binding_policy) &&
                       OPAL_BIND_TO_NONE != OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                /* the system is capable of doing processor affinity, but it
                 * has not yet been set - see if a slot_list was given
                 */
                hwloc_bitmap_zero(cpus);
                if (OPAL_BIND_TO_CPUSET == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                    if (OPAL_SUCCESS != (ret = opal_hwloc_base_slot_list_parse(opal_hwloc_base_slot_list,
                                                                               opal_hwloc_topology, cpus))) {
                        error = "Setting processor affinity failed";
                        hwloc_bitmap_free(cpus);
                        goto error;
                    }
                    if (0 > hwloc_set_cpubind(opal_hwloc_topology, cpus, 0)) {
                        error = "Setting processor affinity failed";
                        hwloc_bitmap_free(cpus);
                        goto error;
                    }
                    /* try to find a level and index for this location */
                    opal_hwloc_base_get_level_and_index(cpus, &orte_process_info.bind_level, &orte_process_info.bind_idx);
                    /* cleanup */
                    hwloc_bitmap_free(cpus);
                    orte_proc_is_bound = true;
                    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                         "%s Process bound according to slot_list",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                } else {
                    /* cleanup */
                    hwloc_bitmap_free(cpus);
                    /* get the node rank */
                    if (ORTE_NODE_RANK_INVALID == orte_process_info.my_node_rank) {
                        /* this is not an error - could be due to being
                         * direct launched - so just ignore and leave
                         * us unbound
                         */
                        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                             "%s Process not bound - no node rank available",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                        goto MOVEON;
                    }
                    /* if the binding policy is hwthread, then we bind to the nrank-th
                     * hwthread on this node
                     */
                    if (OPAL_BIND_TO_HWTHREAD == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                        if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_PU,
                                                                           0, orte_process_info.my_node_rank, OPAL_HWLOC_LOGICAL))) {
                            ret = ORTE_ERR_NOT_FOUND;
                            error = "Getting hwthread object";
                            goto error;
                        }
                        cpus = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, obj);
                        if (0 > hwloc_set_cpubind(opal_hwloc_topology, cpus, 0)) {
                            ret = ORTE_ERROR;
                            error = "Setting processor affinity failed";
                            goto error;
                        }
                        orte_process_info.bind_level = OPAL_HWLOC_HWTHREAD_LEVEL;
                        orte_process_info.bind_idx = orte_process_info.my_node_rank;
                        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                             "%s Process bound to hwthread",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    } else if (OPAL_BIND_TO_CORE == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                        /* if the binding policy is core, then we bind to the nrank-th
                         * core on this node
                         */
                        if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE,
                                                                           0, orte_process_info.my_node_rank, OPAL_HWLOC_LOGICAL))) {
                            ret = ORTE_ERR_NOT_FOUND;
                            error = "Getting core object";
                            goto error;
                        }
                        cpus = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, obj);
                        if (0 > hwloc_set_cpubind(opal_hwloc_topology, cpus, 0)) {
                            error = "Setting processor affinity failed";
                            ret = ORTE_ERROR;
                            goto error;
                        }
                        orte_process_info.bind_level = OPAL_HWLOC_CORE_LEVEL;
                        orte_process_info.bind_idx = orte_process_info.my_node_rank;
                        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                             "%s Process bound to core",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    } else {
                        /* for all higher binding policies, we bind to the specified
                         * object that the nrank-th core belongs to
                         */
                        if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE,
                                                                           0, orte_process_info.my_node_rank, OPAL_HWLOC_LOGICAL))) {
                            ret = ORTE_ERR_NOT_FOUND;
                            error = "Getting core object";
                            goto error;
                        }
                        if (OPAL_BIND_TO_L1CACHE == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                            target = HWLOC_OBJ_CACHE;
                            cache_level = 1;
                            orte_process_info.bind_level = OPAL_HWLOC_L1CACHE_LEVEL;
                        } else if (OPAL_BIND_TO_L2CACHE == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                            target = HWLOC_OBJ_CACHE;
                            cache_level = 2;
                            orte_process_info.bind_level = OPAL_HWLOC_L2CACHE_LEVEL;
                        } else if (OPAL_BIND_TO_L3CACHE == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                            target = HWLOC_OBJ_CACHE;
                            cache_level = 3;
                            orte_process_info.bind_level = OPAL_HWLOC_L3CACHE_LEVEL;
                        } else if (OPAL_BIND_TO_SOCKET == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                            target = HWLOC_OBJ_SOCKET;
                            orte_process_info.bind_level = OPAL_HWLOC_SOCKET_LEVEL;
                        } else if (OPAL_BIND_TO_NUMA == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                            target = HWLOC_OBJ_NODE;
                            orte_process_info.bind_level = OPAL_HWLOC_NUMA_LEVEL;
                        } else {
                            ret = ORTE_ERR_NOT_FOUND;
                            error = "Binding policy not known";
                            goto error;
                        }
                        for (obj = obj->parent; NULL != obj; obj = obj->parent) {
                            if (target == obj->type) {
                                if (HWLOC_OBJ_CACHE == target && cache_level != obj->attr->cache.depth) {
                                    continue;
                                }
                                /* this is the place! */
                                cpus = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, obj);
                                if (0 > hwloc_set_cpubind(opal_hwloc_topology, cpus, 0)) {
                                    ret = ORTE_ERROR;
                                    error = "Setting processor affinity failed";
                                    goto error;
                                }
                                orte_process_info.bind_idx = opal_hwloc_base_get_obj_idx(opal_hwloc_topology,
                                                                                         obj, OPAL_HWLOC_LOGICAL);
                                orte_proc_is_bound = true;
                                OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                                     "%s Process bound to %s",
                                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                     opal_hwloc_base_print_level(orte_process_info.bind_level)));
                                break;
                            }
                        }
                        if (!orte_proc_is_bound) {
                            ret = ORTE_ERROR;
                            error = "Setting processor affinity failed";
                            goto error;
                        }
                    }
                }
            }
        }
    } else {
        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                             "%s Process bound at launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    }

 MOVEON:
    /* get or update our local cpuset - it will get used multiple
     * times, so it's more efficient to keep a global copy
     */
    opal_hwloc_base_get_local_cpuset();
    /* report bindings, if requested */
    if (opal_hwloc_report_bindings) {
        char tmp1[1024], tmp2[1024];
        hwloc_cpuset_t mycpus;
        /* get the cpus we are bound to */
        mycpus = hwloc_bitmap_alloc();
        if (hwloc_get_cpubind(opal_hwloc_topology, 
                              mycpus, 
                              HWLOC_CPUBIND_PROCESS) < 0) {
            opal_output(0, "MCW rank %d is not bound",
                        ORTE_PROC_MY_NAME->vpid);
        } else {
            opal_hwloc_base_cset2str(tmp1, sizeof(tmp1), mycpus);
            opal_hwloc_base_cset2mapstr(tmp2, sizeof(tmp2), mycpus);
            opal_output(0, "MCW rank %d bound to %s: %s",
                        ORTE_PROC_MY_NAME->vpid, tmp1, tmp2);
        }
        hwloc_bitmap_free(mycpus);
    }

    return ORTE_SUCCESS;

 error:
    if (ORTE_ERR_SILENT != ret) {
        orte_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       true, error, ORTE_ERROR_NAME(ret), ret);
    }

    return ORTE_ERR_SILENT;

#else
    return ORTE_SUCCESS;
#endif
}
