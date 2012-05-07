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

static orte_proc_t* find_proc(orte_process_name_t *proc)  /* used by daemons */
{
    orte_job_t *jdata;
    
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }

    return (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);
}

opal_hwloc_locality_t orte_ess_base_proc_get_locality(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;

    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                             "%s LOOKING FOR PROC %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return OPAL_PROC_NON_LOCAL;
    }
    
    return pmap->locality;   
}

orte_vpid_t orte_ess_base_proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    orte_proc_t *pdata;
    orte_vpid_t vpid;

    if (NULL == proc) {
        return ORTE_VPID_INVALID;
    }

    if (ORTE_JOBID_IS_DAEMON(proc->jobid)) {
        return proc->vpid;
    }

    if (ORTE_PROC_IS_APP) {
        if (NULL == (nid = orte_util_lookup_nid(proc))) {
            return ORTE_VPID_INVALID;
        }
        vpid = nid->daemon;
    } else {
        /* get the job data */
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_VPID_INVALID;
        }
        
        if (NULL == pdata->node || NULL == pdata->node->daemon) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_VPID_INVALID;
        }
        vpid = pdata->node->daemon->name.vpid;
    }

    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(vpid)));
    
    return vpid;
}

char* orte_ess_base_proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    orte_proc_t *pdata;
    char *hostname;

    if (NULL == proc) {
        return NULL;
    }

    if (ORTE_PROC_IS_APP) {
        if (NULL == (nid = orte_util_lookup_nid(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                                 "%s LOOKING FOR PROC %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(proc)));
            return NULL;
        }
        hostname = nid->name;
    } else {
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return NULL;
        }
        hostname = pdata->node->name;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         hostname));
    
    return hostname;
}

orte_local_rank_t orte_ess_base_proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    orte_proc_t *pdata;
    orte_local_rank_t lrank;

    if (NULL == proc) {
        return ORTE_LOCAL_RANK_INVALID;
    }

    if (ORTE_PROC_IS_APP) {
        if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_LOCAL_RANK_INVALID;
        }
        lrank = pmap->local_rank;
    } else {
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_LOCAL_RANK_INVALID;
        }
        lrank = pdata->local_rank;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)lrank));
    
    return lrank;
}

orte_node_rank_t orte_ess_base_proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    orte_proc_t *pdata;
    orte_node_rank_t nrank;

    if (NULL == proc) {
        return ORTE_NODE_RANK_INVALID;
    }

    if (ORTE_PROC_IS_APP) {
        /* is this me? */
        if (proc->jobid == ORTE_PROC_MY_NAME->jobid &&
            proc->vpid == ORTE_PROC_MY_NAME->vpid) {
            /* yes it is - reply with my rank. This is necessary
             * because the pidmap will not have arrived when I
             * am starting up, and if we use static ports, then
             * I need to know my node rank during init
             */
            return orte_process_info.my_node_rank;
        }
        if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
            return ORTE_NODE_RANK_INVALID;
        }
        nrank = pmap->node_rank;
    } else {
        if (NULL == (pdata = find_proc(proc))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_NODE_RANK_INVALID;
        }
        nrank = pdata->node_rank;
    }
    
    OPAL_OUTPUT_VERBOSE((5, orte_ess_base_output,
                         "%s ess:base: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)nrank));
    
    return nrank;
}

int orte_ess_base_update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:base: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_PROC_IS_APP) {
        if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
            ORTE_ERROR_LOG(ret);
        }
    } else {
        if (ORTE_SUCCESS != (ret = orte_util_decode_daemon_pidmap(bo))) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    return ret;
}

int orte_ess_base_update_nidmap(opal_byte_object_t *bo)
{
    int rc;

    /* decode the nidmap - the util will know what to do */
    if (ORTE_PROC_IS_APP) {
        if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
            ORTE_ERROR_LOG(rc);
        }
    } else {
        if (ORTE_SUCCESS != (rc = orte_util_decode_daemon_nodemap(bo))) {
            ORTE_ERROR_LOG(rc);
        }
    }

    return rc;
}

int orte_ess_base_proc_binding(void)
{
#if OPAL_HAVE_HWLOC
    hwloc_obj_t node, obj;
    hwloc_cpuset_t cpus, nodeset;
    orte_node_rank_t nrank;
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
                } else {
                    /* cleanup */
                    hwloc_bitmap_free(cpus);
                    /* get the node rank */
                    if (ORTE_NODE_RANK_INVALID == (nrank = orte_ess.get_node_rank(ORTE_PROC_MY_NAME))) {
                        /* this is not an error - could be due to being
                         * direct launched - so just ignore and leave
                         * us unbound
                         */
                        goto MOVEON;
                    }
                    /* if the binding policy is hwthread, then we bind to the nrank-th
                     * hwthread on this node
                     */
                    if (OPAL_BIND_TO_HWTHREAD == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                        if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_PU,
                                                                           0, nrank, OPAL_HWLOC_LOGICAL))) {
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
                        orte_process_info.bind_level = OPAL_HWLOC_L1CACHE_LEVEL;
                        orte_process_info.bind_idx = nrank;
                    } else if (OPAL_BIND_TO_CORE == OPAL_GET_BINDING_POLICY(opal_hwloc_binding_policy)) {
                        /* if the binding policy is core, then we bind to the nrank-th
                         * core on this node
                         */
                        if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE,
                                                                           0, nrank, OPAL_HWLOC_LOGICAL))) {
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
                        orte_process_info.bind_idx = nrank;
                    } else {
                        /* for all higher binding policies, we bind to the specified
                         * object that the nrank-th core belongs to
                         */
                        if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology, HWLOC_OBJ_CORE,
                                                                           0, nrank, OPAL_HWLOC_LOGICAL))) {
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
    }

 MOVEON:
    /* get or update our local cpuset - it will get used multiple
     * times, so it's more efficient to keep a global copy
     */
    opal_hwloc_base_get_local_cpuset();
    /* report bindings, if requested */
    if (opal_hwloc_report_bindings) {
        char bindings[64];
        hwloc_obj_t root;
        hwloc_cpuset_t cpus;
        /* get the root object for this node */
        root = hwloc_get_root_obj(opal_hwloc_topology);
        cpus = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, root);
        /* we are not bound if this equals our cpuset */
        if (0 == hwloc_bitmap_compare(cpus, opal_hwloc_my_cpuset)) {
            opal_output(0, "%s is not bound",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        } else {
            hwloc_bitmap_list_snprintf(bindings, 64, opal_hwloc_my_cpuset);
            opal_output(0, "%s is bound to cpus %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        bindings);
        }
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
