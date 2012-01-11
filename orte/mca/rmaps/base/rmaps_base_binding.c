/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <string.h>

#include "opal/util/if.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/threads/tsd.h"

#include "orte/types.h"
#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/hostfile/hostfile.h"
#include "orte/util/dash_host/dash_host.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"

static bool membind_warned=false;

static int bind_upwards(orte_job_t *jdata,
                        hwloc_obj_type_t target,
                        unsigned cache_level)
{
    /* traverse the hwloc topology tree on each node upwards
     * until we find an object of type target - and then bind
     * the process to that target
     */
    int i, j;
    orte_job_map_t *map;
    orte_node_t *node;
    orte_proc_t *proc;
    hwloc_obj_t obj;
    hwloc_cpuset_t cpus;
    unsigned int idx, ncpus, nobjs, nsave = 0, *nbound=NULL;
    struct hwloc_topology_support *support;

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps: bind upwards for job %s with bindings %s",
                        ORTE_JOBID_PRINT(jdata->jobid),
                        opal_hwloc_base_print_binding(jdata->map->binding));
    /* initialize */
    map = jdata->map;

    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        support = (struct hwloc_topology_support*)hwloc_topology_get_support(node->topology);
        /* check if topology supports cpubind - have to be careful here
         * as Linux doesn't currently support thread-level binding. This
         * may change in the future, though, and it isn't clear how hwloc
         * interprets the current behavior. So check both flags to be sure.
         */
        if (!support->cpubind->set_thisproc_cpubind &&
            !support->cpubind->set_thisthread_cpubind) {
            if (!OPAL_BINDING_REQUIRED(opal_hwloc_binding_policy)) {
                /* we are not required to bind, so ignore this */
                continue;
            }
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:cpubind-not-supported", true, node->name);
            if (NULL != nbound) {
                free(nbound);
            }
            return ORTE_ERR_SILENT;
        }
        /* check if topology supports membind - have to be careful here
         * as hwloc treats this differently than I (at least) would have
         * expected. Per hwloc, Linux memory binding is at the thread,
         * and not process, level. Thus, hwloc sets the "thisproc" flag
         * to "false" on all Linux systems, and uses the "thisthread" flag
         * to indicate binding capability
         */
        if (!support->membind->set_thisproc_membind &&
            !support->membind->set_thisthread_membind) {
            if (OPAL_HWLOC_BASE_MBFA_WARN == opal_hwloc_base_mbfa && !membind_warned) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:membind-not-supported", true, node->name);
                membind_warned = true;
            } else if (OPAL_HWLOC_BASE_MBFA_ERROR == opal_hwloc_base_mbfa) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:membind-not-supported-fatal", true, node->name);
                if (NULL != nbound) {
                    free(nbound);
                }
                return ORTE_ERR_SILENT;
            }
        }

        /* get the number of objects of this type on this node */
        nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology, target,
                                                   cache_level, OPAL_HWLOC_AVAILABLE);
        if (0 == nobjs) {
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-bindable-objects", true,
                           node->name, hwloc_obj_type_string(target));
            return ORTE_ERR_SILENT;
        }
        /* setup the array */
        if (NULL == nbound) {
            nbound = (unsigned int*)malloc(nobjs * sizeof(int));
            nsave = nobjs;
        } else if (nsave < nobjs) {
            nbound = (unsigned int*)realloc(nbound, nobjs * sizeof(int));
        }
        memset(nbound, 0, nobjs * sizeof(int));

        /* cycle thru the procs */
        for (j=0; j < node->procs->size; j++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                continue;
            }
            /* ignore procs from other jobs */
            if (proc->name.jobid != jdata->jobid) {
                continue;
            }
            /* ignore procs that have already been bound - should
             * never happen, but safer
             */
            if (NULL != proc->cpu_bitmap) {
                continue;
            }
            /* bozo check */
            if (NULL == proc->locale) {
                opal_output(0, "BIND UPWARDS: LOCALE FOR PROC %s IS NULL", ORTE_NAME_PRINT(&proc->name));
                return ORTE_ERR_SILENT;
            }
            /* starting at the locale, move up thru the parents
             * to find the target object type
             */
            for (obj = proc->locale->parent; NULL != obj; obj = obj->parent) {
                opal_output(0, "%s bind:upward target %s type %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            hwloc_obj_type_string(target),
                            hwloc_obj_type_string(obj->type));
                if (target == obj->type) {
                    if (HWLOC_OBJ_CACHE == target && cache_level != obj->attr->cache.depth) {
                        continue;
                    }
                    /* get its index */
                    if (UINT_MAX == (idx = opal_hwloc_base_get_obj_idx(node->topology, obj, OPAL_HWLOC_AVAILABLE))) {
                        free(nbound);
                        return ORTE_ERR_SILENT;
                    }
                    /* track the number bound */
                    ++nbound[idx];
                    /* get the number of cpus under this location */
                    if (0 == (ncpus = opal_hwloc_base_get_npus(node->topology, obj))) {
                        orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-available-cpus", true, node->name);
                        free(nbound);
                        return ORTE_ERR_SILENT;
                    }
                    /* error out if adding a proc would cause overload and that wasn't allowed */
                    if (ncpus < nbound[idx] &&
                        !OPAL_BIND_OVERLOAD_ALLOWED(jdata->map->binding)) {
                        orte_show_help("help-orte-rmaps-base.txt", "rmaps:binding-overload", true,
                                       opal_hwloc_base_print_binding(map->binding), node->name,
                                       nbound[idx], ncpus);
                        free(nbound);
                        return ORTE_ERR_SILENT;
                    }
                    /* bind it here */
                    proc->bind_idx = idx;
                    cpus = opal_hwloc_base_get_available_cpus(node->topology, obj);
                    hwloc_bitmap_list_asprintf(&proc->cpu_bitmap, cpus);
                    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                        "%s BOUND PROC %s TO %s[%s:%u] on node %s",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        ORTE_NAME_PRINT(&proc->name),
                                        proc->cpu_bitmap,
                                        hwloc_obj_type_string(target),
                                        idx, node->name);
                    break;
                }
            }
            if (NULL == proc->cpu_bitmap && OPAL_BINDING_REQUIRED(jdata->map->binding)) {
                /* didn't find anyone to bind to - this is an error
                 * unless the user specified if-supported
                 */
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:binding-target-not-found", true,
                               opal_hwloc_base_print_binding(map->binding), node->name);
                free(nbound);
                return ORTE_ERR_SILENT;
            }
        }
    }

    if (NULL != nbound) {
        free(nbound);
    }

    return ORTE_SUCCESS;
}

static int bind_downwards(orte_job_t *jdata,
                          hwloc_obj_type_t target,
                          unsigned cache_level)
{
    int i, j;
    orte_job_map_t *map;
    orte_node_t *node;
    orte_proc_t *proc;
    hwloc_obj_t obj;
    hwloc_cpuset_t cpus;
    unsigned int n, idx, minval, ncpus, nobjs, nsave = 0, *nbound=NULL;
    struct hwloc_topology_support *support;

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps: bind downward for job %s with bindings %s",
                        ORTE_JOBID_PRINT(jdata->jobid),
                        opal_hwloc_base_print_binding(jdata->map->binding));
    /* initialize */
    map = jdata->map;

    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        support = (struct hwloc_topology_support*)hwloc_topology_get_support(node->topology);
        /* check if topology supports cpubind - have to be careful here
         * as Linux doesn't currently support thread-level binding. This
         * may change in the future, though, and it isn't clear how hwloc
         * interprets the current behavior. So check both flags to be sure.
         */
        if (!support->cpubind->set_thisproc_cpubind &&
            !support->cpubind->set_thisthread_cpubind) {
            if (!OPAL_BINDING_REQUIRED(opal_hwloc_binding_policy)) {
                /* we are not required to bind, so ignore this */
                continue;
            }
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:cpubind-not-supported", true, node->name);
            if (NULL != nbound) {
                free(nbound);
            }
            return ORTE_ERR_SILENT;
        }
        /* check if topology supports membind - have to be careful here
         * as hwloc treats this differently than I (at least) would have
         * expected. Per hwloc, Linux memory binding is at the thread,
         * and not process, level. Thus, hwloc sets the "thisproc" flag
         * to "false" on all Linux systems, and uses the "thisthread" flag
         * to indicate binding capability
         */
        if (!support->membind->set_thisproc_membind &&
            !support->membind->set_thisthread_membind) {
            if (OPAL_HWLOC_BASE_MBFA_WARN == opal_hwloc_base_mbfa && !membind_warned) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:membind-not-supported", true, node->name);
                membind_warned = true;
            } else if (OPAL_HWLOC_BASE_MBFA_ERROR == opal_hwloc_base_mbfa) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:membind-not-supported-fatal", true, node->name);
                if (NULL != nbound) {
                    free(nbound);
                }
                return ORTE_ERR_SILENT;
            }
        }

        /* get the number of objects of this type on this node */
        nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology, target,
                                                   cache_level, OPAL_HWLOC_AVAILABLE);
        if (0 == nobjs) {
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-bindable-objects", true,
                           node->name, hwloc_obj_type_string(target));
            return ORTE_ERR_SILENT;
        }
        /* setup the array */
        if (NULL == nbound) {
            nbound = (unsigned int*)malloc(nobjs * sizeof(int));
            nsave = nobjs;
        } else if (nsave < nobjs) {
            nbound = (unsigned int*)realloc(nbound, nobjs * sizeof(int));
        }
        memset(nbound, 0, nobjs * sizeof(int));

        /* cycle thru the procs */
        for (j=0; j < node->procs->size; j++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                continue;
            }
            /* ignore procs from other jobs */
            if (proc->name.jobid != jdata->jobid) {
                continue;
            }
            /* ignore procs that have already been bound - should
             * never happen, but safer
             */
            if (NULL != proc->cpu_bitmap) {
                continue;
            }
            /* cycle across the target objects and select the one with
             * minimum usage
             */
            minval = UINT_MAX;
            idx = 0;
            for (n=0; n < nobjs; n++) {
                if (nbound[n] < minval) {
                    minval = nbound[n];
                    idx = n;
                }
            }
            /* track the number bound */
            ++nbound[idx];
            /* get the number of cpus under this location */
            obj = opal_hwloc_base_get_obj_by_type(node->topology, target, cache_level,
                                                  idx, OPAL_HWLOC_AVAILABLE);
            if (0 == (ncpus = opal_hwloc_base_get_npus(node->topology, obj))) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-available-cpus", true, node->name);
                free(nbound);
                return ORTE_ERR_SILENT;
            }
            /* error out if adding a proc would cause overload and that wasn't allowed */
            if (ncpus < nbound[idx] &&
                !OPAL_BIND_OVERLOAD_ALLOWED(jdata->map->binding)) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:binding-overload", true,
                               opal_hwloc_base_print_binding(map->binding), node->name,
                               nbound[idx], ncpus);
                free(nbound);
                return ORTE_ERR_SILENT;
            }
            /* bind the proc here */
            proc->bind_idx = idx;
            cpus = opal_hwloc_base_get_available_cpus(node->topology, obj);
            hwloc_bitmap_list_asprintf(&proc->cpu_bitmap, cpus);
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "%s BOUND PROC %s TO %s[%s:%u] on node %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&proc->name),
                                proc->cpu_bitmap, hwloc_obj_type_string(obj->type),
                                idx, node->name);
        }
    }

    if (NULL != nbound) {
        free(nbound);
    }

    return ORTE_SUCCESS;
}

static int bind_in_place(orte_job_t *jdata,
                         hwloc_obj_type_t target,
                         unsigned cache_level)
{
    /* traverse the hwloc topology tree on each node downwards
     * until we find an unused object of type target - and then bind
     * the process to that target
     */
    int i, j;
    orte_job_map_t *map;
    orte_node_t *node;
    orte_proc_t *proc;
    hwloc_cpuset_t cpus;
    unsigned int idx, ncpus, nobjs, nsave = 0, *nbound=NULL;
    struct hwloc_topology_support *support;

    opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                        "mca:rmaps: bind in place for job %s with bindings %s",
                        ORTE_JOBID_PRINT(jdata->jobid),
                        opal_hwloc_base_print_binding(jdata->map->binding));
    /* initialize */
    map = jdata->map;

    for (i=0; i < map->nodes->size; i++) {
        if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, i))) {
            continue;
        }
        support = (struct hwloc_topology_support*)hwloc_topology_get_support(node->topology);
        /* check if topology supports cpubind - have to be careful here
         * as Linux doesn't currently support thread-level binding. This
         * may change in the future, though, and it isn't clear how hwloc
         * interprets the current behavior. So check both flags to be sure.
         */
        if (!support->cpubind->set_thisproc_cpubind &&
            !support->cpubind->set_thisthread_cpubind) {
            if (!OPAL_BINDING_REQUIRED(opal_hwloc_binding_policy)) {
                /* we are not required to bind, so ignore this */
                continue;
            }
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:cpubind-not-supported", true, node->name);
            if (NULL != nbound) {
                free(nbound);
            }
            return ORTE_ERR_SILENT;
        }
        /* check if topology supports membind - have to be careful here
         * as hwloc treats this differently than I (at least) would have
         * expected. Per hwloc, Linux memory binding is at the thread,
         * and not process, level. Thus, hwloc sets the "thisproc" flag
         * to "false" on all Linux systems, and uses the "thisthread" flag
         * to indicate binding capability
         */
        if (!support->membind->set_thisproc_membind &&
            !support->membind->set_thisthread_membind) {
            if (OPAL_HWLOC_BASE_MBFA_WARN == opal_hwloc_base_mbfa && !membind_warned) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:membind-not-supported", true, node->name);
                membind_warned = true;
            } else if (OPAL_HWLOC_BASE_MBFA_ERROR == opal_hwloc_base_mbfa) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:membind-not-supported-fatal", true, node->name);
                if (NULL != nbound) {
                    free(nbound);
                }
                return ORTE_ERR_SILENT;
            }
        }

        /* get the number of objects of this type on this node */
        nobjs = opal_hwloc_base_get_nbobjs_by_type(node->topology, target,
                                                   cache_level, OPAL_HWLOC_AVAILABLE);
        if (0 == nobjs) {
            orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-bindable-objects", true,
                           node->name, hwloc_obj_type_string(target));
            return ORTE_ERR_SILENT;
        }
        /* setup the array */
        if (NULL == nbound) {
            nbound = (unsigned int*)malloc(nobjs * sizeof(int));
            nsave = nobjs;
        } else if (nsave < nobjs) {
            nbound = (unsigned int*)realloc(nbound, nobjs * sizeof(int));
        }
        memset(nbound, 0, nobjs * sizeof(int));
        /* cycle thru the procs */
        for (j=0; j < node->procs->size; j++) {
            if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, j))) {
                continue;
            }
            /* ignore procs from other jobs */
            if (proc->name.jobid != jdata->jobid) {
                continue;
            }
            /* ignore procs that have already been bound - should
             * never happen, but safer
             */
            if (NULL != proc->cpu_bitmap) {
                continue;
            }
            /* get the index of this location */
            if (UINT_MAX == (idx = opal_hwloc_base_get_obj_idx(node->topology, proc->locale, OPAL_HWLOC_AVAILABLE))) {
                free(nbound);
                return ORTE_ERR_SILENT;
            }
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "BINDING PROC %s TO %s NUMBER %u",
                                ORTE_NAME_PRINT(&proc->name),
                                hwloc_obj_type_string(proc->locale->type), idx);
            /* get the number of cpus under this location */
            if (0 == (ncpus = opal_hwloc_base_get_npus(node->topology, proc->locale))) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:no-available-cpus", true, node->name);
                free(nbound);
                return ORTE_ERR_SILENT;
            }
            /* track number bound */
            ++nbound[idx];
            /* error out if adding a proc would cause overload and that wasn't allowed */
            if (ncpus < nbound[idx] &&
                !OPAL_BIND_OVERLOAD_ALLOWED(jdata->map->binding)) {
                orte_show_help("help-orte-rmaps-base.txt", "rmaps:binding-overload", true,
                               opal_hwloc_base_print_binding(map->binding), node->name,
                               nbound[idx], ncpus);
                free(nbound);
                return ORTE_ERR_SILENT;
            }
            /* bind the proc here */
            proc->bind_idx = idx;
            cpus = opal_hwloc_base_get_available_cpus(node->topology, proc->locale);
            hwloc_bitmap_list_asprintf(&proc->cpu_bitmap, cpus);
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "%s BOUND PROC %s TO %s[%s:%u] on node %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&proc->name),
                                proc->cpu_bitmap,
                                hwloc_obj_type_string(proc->locale->type),
                                idx, node->name);
        }
    }

    if (NULL != nbound) {
        free(nbound);
    }
    return ORTE_SUCCESS;
}

int orte_rmaps_base_compute_bindings(orte_job_t *jdata)
{
    if (!OPAL_BINDING_POLICY_IS_SET(jdata->map->binding) ||
        OPAL_BIND_TO_NONE == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        /* no binding requested */
        return ORTE_SUCCESS;
    }

    if (OPAL_BIND_TO_BOARD == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        /* doesn't do anything at this time */
        return ORTE_SUCCESS;
    }

    /* binding requested */
    /* if the job was mapped by the corresponding target, then
     * there is nothing more to do - the launch message creator
     * will see that the binding object is NULL and will simply
     * use the locale as the place to bind the proc
     *
     * otherwise, we have to bind either up or down the hwloc
     * tree. If we are binding upwards (e.g., mapped to hwthread
     * but binding to core), then we just climb the tree to find
     * the first matching object.
     *
     * if we are binding downwards (e.g., mapped to node and bind
     * to core), then we have to do a round-robin assigment of
     * procs to the resources below.
     */
    if (OPAL_BIND_TO_HWTHREAD == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_HWTHREAD_LEVEL;
        if (ORTE_MAPPING_BYHWTHREAD == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - hwthread to hwthread",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_PU, 0))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* HW threads are at the bottom, so all other bindings are upwards */
        if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_PU, 0))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    } else if (OPAL_BIND_TO_CORE == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_CORE_LEVEL;
        if (ORTE_MAPPING_BYCORE == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - core to core",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_CORE, 0))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* if the mapping policy used is less than bycore, then it is a
         * downward binding - i.e., the locale is above the binding location.
         * for example, if we map-to-socket and bind-to-core, then we compare
         * the mapping value of ORTE_MAPPING_BYCORE to ORTE_MAPPING_BYSOCKET.
         * In this case, BYCORE > BYSOCKET, so we know that the locale is
         * above the desired binding level (sockets are at a higher level than
         * the desired core binding level), and we will have to bind downwards
         */
        if (ORTE_MAPPING_BYCORE > ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            if (ORTE_SUCCESS != (rc = bind_downwards(jdata, HWLOC_OBJ_CORE, 0))) {
                ORTE_ERROR_LOG(rc);
            }
        } else {
            if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_CORE, 0))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    } else if (OPAL_BIND_TO_L1CACHE == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_L1CACHE_LEVEL;
        if (ORTE_MAPPING_BYL1CACHE == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - L1cache to L1cache",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_CACHE, 1))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* if the mapping policy is less than l1cache, then it is a
         * downward binding
         */
        if (ORTE_MAPPING_BYL1CACHE > ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            if (ORTE_SUCCESS != (rc = bind_downwards(jdata, HWLOC_OBJ_CACHE, 1))) {
                ORTE_ERROR_LOG(rc);
            }
        } else {
            if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_CACHE, 1))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    } else if (OPAL_BIND_TO_L2CACHE == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_L2CACHE_LEVEL;
        if (ORTE_MAPPING_BYL2CACHE == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - L2cache to L2cache",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_CACHE, 2))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* if the mapping policy is less than l2cache, then it is a
         * downward binding
         */
        if (ORTE_MAPPING_BYL2CACHE > ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            if (ORTE_SUCCESS != (rc = bind_downwards(jdata, HWLOC_OBJ_CACHE, 2))) {
                ORTE_ERROR_LOG(rc);
            }
        } else {
            if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_CACHE, 2))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    } else if (OPAL_BIND_TO_L3CACHE == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_L3CACHE_LEVEL;
        if (ORTE_MAPPING_BYL3CACHE == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - L3cache to L3cache",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_CACHE, 3))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* if the mapping policy is less than l3cache, then it is a
         * downward binding
         */
        if (ORTE_MAPPING_BYL3CACHE > ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            if (ORTE_SUCCESS != (rc = bind_downwards(jdata, HWLOC_OBJ_CACHE, 3))) {
                ORTE_ERROR_LOG(rc);
            }
        } else {
            if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_CACHE, 3))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    } else if (OPAL_BIND_TO_SOCKET == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_SOCKET_LEVEL;
        if (ORTE_MAPPING_BYSOCKET == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - socket to socket",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_SOCKET, 0))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* if the mapping policy is less than bysocket, then it is a
         * downward binding
         */
        if (ORTE_MAPPING_BYSOCKET > ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            if (ORTE_SUCCESS != (rc = bind_downwards(jdata, HWLOC_OBJ_SOCKET, 0))) {
                ORTE_ERROR_LOG(rc);
            }
        } else {
            if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_SOCKET, 0))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    } else if (OPAL_BIND_TO_NUMA == OPAL_GET_BINDING_POLICY(jdata->map->binding)) {
        int rc;
        /* record the level for locality purposes */
        jdata->map->bind_level = OPAL_HWLOC_NUMA_LEVEL;
        if (ORTE_MAPPING_BYNUMA == ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            opal_output_verbose(5, orte_rmaps_base.rmaps_output,
                                "mca:rmaps: bindings for job %s - numa to numa",
                                ORTE_JOBID_PRINT(jdata->jobid));
            if (ORTE_SUCCESS != (rc = bind_in_place(jdata, HWLOC_OBJ_NODE, 0))) {
                ORTE_ERROR_LOG(rc);
            }
            return rc;
        }
        /* if the mapping policy is less than numa, then it is a
         * downward binding
         */
        if (ORTE_MAPPING_BYNUMA > ORTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
            if (ORTE_SUCCESS != (rc = bind_downwards(jdata, HWLOC_OBJ_NODE, 0))) {
                ORTE_ERROR_LOG(rc);
            }
        } else {
            if (ORTE_SUCCESS != (rc = bind_upwards(jdata, HWLOC_OBJ_NODE, 0))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        return rc;
    } else {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_SUPPORTED);
        return ORTE_ERR_NOT_SUPPORTED;
    }

    return ORTE_SUCCESS;
}
