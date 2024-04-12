/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018      Inria.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/threads/pmix_tsd.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/runtime/prte_globals.h"
#include "src/util/dash_host/dash_host.h"
#include "src/util/hostfile/hostfile.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"
#include "types.h"

#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"

static int bind_generic(prte_job_t *jdata, prte_proc_t *proc,
                        prte_node_t *node, hwloc_obj_t obj,
                        prte_rmaps_options_t *options)
{
    hwloc_obj_t trg_obj, tmp_obj;
    unsigned ncpus;
    hwloc_obj_type_t type;
    hwloc_obj_t target;
    hwloc_cpuset_t tgtcpus, tmpcpus;
    int nobjs, n;

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps: bind %s with policy %s",
                        PRTE_NAME_PRINT(&proc->name),
                        prte_hwloc_base_print_binding(jdata->map->binding));
    /* initialize */
    if (NULL == obj) {
        target = hwloc_get_root_obj(node->topology->topo);
    } else {
        target = obj;
    }
    if (NULL == options->target) {
        return PRTE_ERROR;
    }
#if HWLOC_API_VERSION < 0x20000
    tgtcpus = target->allowed_cpuset;
#else
    tgtcpus = target->cpuset;
#endif
    hwloc_bitmap_and(prte_rmaps_base.baseset, options->target, tgtcpus);

    nobjs = hwloc_get_nbobjs_by_type(node->topology->topo, options->hwb);

    for (n=0; n < nobjs; n++) {
        tmp_obj = hwloc_get_obj_by_type(node->topology->topo, options->hwb, n);
#if HWLOC_API_VERSION < 0x20000
        tmpcpus = tmp_obj->allowed_cpuset;
#else
        tmpcpus = tmp_obj->cpuset;
#endif
        hwloc_bitmap_and(prte_rmaps_base.available, node->available, tmpcpus);
        hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.available, prte_rmaps_base.baseset);

        if (options->use_hwthreads) {
            ncpus = hwloc_bitmap_weight(prte_rmaps_base.available);
        } else {
            /* if we are treating cores as cpus, then we really
             * want to know how many cores are in this object.
             * hwloc sets a bit for each "pu", so we can't just
             * count bits in this case as there may be more than
             * one hwthread/core. Instead, find the number of cores
             * under the object
             */
            ncpus = hwloc_get_nbobjs_inside_cpuset_by_type(node->topology->topo,
                                                           prte_rmaps_base.available,
                                                           HWLOC_OBJ_CORE);
        }
        if (0 < ncpus) {
            trg_obj = tmp_obj;
            break;
        }
    }
    if (NULL == trg_obj) {
        /* there aren't any appropriate targets under this object */
        if (PRTE_BINDING_REQUIRED(jdata->map->binding)) {
            pmix_show_help("help-prte-rmaps-base.txt", "rmaps:no-available-cpus", true, node->name);
            return PRTE_ERR_SILENT;
        } else {
            return PRTE_SUCCESS;
        }
    }

#if HWLOC_API_VERSION < 0x20000
    tgtcpus = trg_obj->allowed_cpuset;
#else
    tgtcpus = trg_obj->cpuset;
#endif
    hwloc_bitmap_list_asprintf(&proc->cpuset, tgtcpus); // bind to the entire target object
    if (4 < pmix_output_get_verbosity(prte_rmaps_base_framework.framework_output)) {
        char *tmp1;
        tmp1 = prte_hwloc_base_cset2str(trg_obj->cpuset, options->use_hwthreads, node->topology->topo);
        pmix_output(prte_rmaps_base_framework.framework_output, "%s BOUND PROC %s[%s] TO %s",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                    node->name, tmp1);
        free(tmp1);
    }

    /* mark the assigned cpus as having been consumed. If we are binding to
     * an object that has multiple CPUs, then we only mark one here to indicate
     * that a process was bound to that object. This provides an accounting
     * mechanism that lets us know when we become overloaded - i.e., more procs
     * are bound to the object than there are CPUs. We must also mark the bits
     * in the options->target cpuset so that the next proc we assign doesn't
     * attempt to take the same location. */
    if (options->use_hwthreads) {
        type = HWLOC_OBJ_PU;
    } else {
        type = HWLOC_OBJ_CORE;
    }
    tmp_obj = hwloc_get_obj_inside_cpuset_by_type(node->topology->topo,
                                                  prte_rmaps_base.available,
                                                  type, 0);
#if HWLOC_API_VERSION < 0x20000
    hwloc_bitmap_andnot(node->available, node->available, tmp_obj->allowed_cpuset);
    if (hwloc_bitmap_iszero(node->available) && options->overload) {
        /* reset the availability */
        hwloc_bitmap_copy(node->available, node->jobcache);
    }
#else
    hwloc_bitmap_andnot(node->available, node->available, tmp_obj->cpuset);
    if (hwloc_bitmap_iszero(node->available) && options->overload) {
        /* reset the availability */
        hwloc_bitmap_copy(node->available, node->jobcache);
    }
#endif
    return PRTE_SUCCESS;
}

static int bind_to_cpuset(prte_job_t *jdata,
                          prte_proc_t *proc,
                          prte_node_t *node,
                          prte_rmaps_options_t *options)
{
    /* bind each process to prte_hwloc_base_cpu_list */
    unsigned idx;
    hwloc_bitmap_t tset;
    hwloc_obj_t obj, root, pkg;
    char **cpus;
    hwloc_obj_type_t type;
    unsigned n, npkgs;
    int rc;
    bool included;

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps: bind job %s to cpus %s %s",
                        PRTE_JOBID_PRINT(jdata->nspace),
                        options->cpuset,
                        options->ordered ? "ordered" : "not-ordered");

    if (NULL == options->cpuset) {
        /* not enough cpus were specified */
        return PRTE_ERR_OUT_OF_RESOURCE;
    }
    cpus = PMIX_ARGV_SPLIT_COMPAT(options->cpuset, ',');
    /* take the first one */
    idx = strtoul(cpus[0], NULL, 10);
    if (options->use_hwthreads) {
        type = HWLOC_OBJ_PU;
    } else {
        type = HWLOC_OBJ_CORE;
    }

    /* the CPU numbers would have been given to us based on the total
     * available CPUs on the machine. Thus, we cannot use the node->available
     * CPU set as we are removing CPUs for accounting purposes there.
     * Instead, refer back to the root-level information */
    root = hwloc_get_root_obj(node->topology->topo);

    if (options->ordered) {
        /* assign each proc to the next available
         * cpu in the list. Since we are assigning
         * procs as they are mapped, this ensures they
         * will be assigned in order */
#if HWLOC_API_VERSION < 0x20000
        tset = root->allowed_cpuset;
#else
        tset = root->cpuset;
#endif
        obj = hwloc_get_obj_inside_cpuset_by_type(node->topology->topo, tset, type, idx);
        if (NULL == obj) {
            PMIX_ARGV_FREE_COMPAT(cpus);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
#if HWLOC_API_VERSION < 0x20000
        tset = obj->allowed_cpuset;
#else
        tset = obj->cpuset;
#endif
    } else {
        /* bind the proc to all assigned cpus */
        tset = options->target;
    }
    /* sanity check - are all the target cpus in a single
     * package, or do they span packages?
     */
    npkgs = hwloc_get_nbobjs_by_type(node->topology->topo, HWLOC_OBJ_PACKAGE);
    included = false;
    for (n=0; n < npkgs; n++) {
        pkg = hwloc_get_obj_by_type(node->topology->topo, HWLOC_OBJ_PACKAGE, n);
#if HWLOC_API_VERSION < 0x20000
        rc = hwloc_bitmap_isincluded(tset, pkg->allowed_cpuset);
#else
        rc = hwloc_bitmap_isincluded(tset, pkg->cpuset);
#endif
        if (1 == rc) {
            included = true;
            break;
        }
    }
    if (!included) {
        pmix_show_help("help-prte-rmaps-base.txt", "span-packages-cpuset", true,
                       prte_rmaps_base_print_mapping(jdata->map->mapping),
                       prte_hwloc_base_print_binding(jdata->map->binding),
                       options->cpuset);
        PMIX_ARGV_FREE_COMPAT(cpus);
        return PRTE_ERR_SILENT;
    }
    /* bind to the specified cpuset */
    hwloc_bitmap_list_asprintf(&proc->cpuset, tset);

    /* remove one of the CPUs from the cpuset to indicate that
     * we assigned a proc to this range */
    free(options->cpuset);
    if (NULL == cpus[1]) {
        options->cpuset = NULL;
    } else {
        options->cpuset = PMIX_ARGV_JOIN_COMPAT(&cpus[1], ',');
    }
    PMIX_ARGV_FREE_COMPAT(cpus);

    /* mark that we used ONE of these cpus - we do this each time we
     * the cpuset is assigned to a proc. When all the cpus in the
     * set have been removed, we know that the set will be overloaded
     * if any more procs are assigned to it. */
#if HWLOC_API_VERSION < 0x20000
    tset = root->allowed_cpuset;
#else
    tset = root->cpuset;
#endif
    obj = hwloc_get_obj_inside_cpuset_by_type(node->topology->topo, tset, type, idx);
    if (NULL == obj) {
    } else {
#if HWLOC_API_VERSION < 0x20000
        hwloc_bitmap_andnot(node->available, node->available, obj->allowed_cpuset);
#else
        hwloc_bitmap_andnot(node->available, node->available, obj->cpuset);
#endif
    }
    return PRTE_SUCCESS;
}

static int bind_multiple(prte_job_t *jdata, prte_proc_t *proc,
                         prte_node_t *node, hwloc_obj_t obj,
                         prte_rmaps_options_t *options)
{
    hwloc_obj_type_t type;
    hwloc_cpuset_t result, tgtcpus;
    hwloc_obj_t target, tmp_obj, pkg;
    uint16_t n;
    unsigned npkgs, ncpus;
    bool moveon = false;
    PRTE_HIDE_UNUSED_PARAMS(jdata);

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps: bind proc %s to %d cpus",
                        PRTE_NAME_PRINT(&proc->name),
                        options->cpus_per_rank);

    /* initialize */
    result = hwloc_bitmap_alloc();
    hwloc_bitmap_zero(result);
    if (NULL == obj) {
        target = hwloc_get_root_obj(node->topology->topo);
    } else {
        target = obj;
    }
#if HWLOC_API_VERSION < 0x20000
    tgtcpus = target->allowed_cpuset;
#else
    tgtcpus = target->cpuset;
#endif
    hwloc_bitmap_and(prte_rmaps_base.baseset, options->target, tgtcpus);
    if (options->use_hwthreads) {
        type = HWLOC_OBJ_PU;
    } else {
        type = HWLOC_OBJ_CORE;
    }
    if (NULL == obj) {
        /* we do not want the cpu envelope to split across
         * packages, so we need to ensure we set the
         * available processors to cover whichever package
         * has enough CPUs to fill the request */
        npkgs = hwloc_get_nbobjs_by_type(node->topology->topo, HWLOC_OBJ_PACKAGE);
        for (n=0; n < npkgs; n++) {
            pkg = hwloc_get_obj_by_type(node->topology->topo, HWLOC_OBJ_PACKAGE, n);
#if HWLOC_API_VERSION < 0x20000
            hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.baseset, pkg->allowed_cpuset);
#else
            hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.baseset, pkg->cpuset);
#endif
            hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.available, node->available);
            ncpus = hwloc_get_nbobjs_inside_cpuset_by_type(node->topology->topo, prte_rmaps_base.available, type);
            if (ncpus >= options->cpus_per_rank) {
                /* this is a good spot */
                moveon = true;
                break;
            }
        }
        if (!moveon) {
            /* if we get here, then there are no packages that can completely
             * cover the request - so return an error */
            hwloc_bitmap_free(result);
            pmix_show_help("help-prte-rmaps-base.txt", "span-packages-multiple", true,
                           prte_rmaps_base_print_mapping(jdata->map->mapping),
                           prte_hwloc_base_print_binding(jdata->map->binding),
                           options->cpus_per_rank);
            return PRTE_ERR_SILENT;
        }
    } else {
        hwloc_bitmap_and(prte_rmaps_base.available, prte_rmaps_base.baseset, node->available);
    }
    /* we bind-to-cpu for the number of cpus that was specified,
     * restricting ourselves to the available cpus in the object */
    for (n=0; n < options->cpus_per_rank; n++) {
        tmp_obj = hwloc_get_obj_inside_cpuset_by_type(node->topology->topo, prte_rmaps_base.available, type, n);
        if (NULL != tmp_obj) {
#if HWLOC_API_VERSION < 0x20000
            hwloc_bitmap_or(result, result, tmp_obj->allowed_cpuset);
            hwloc_bitmap_andnot(node->available, node->available, tmp_obj->allowed_cpuset);
            hwloc_bitmap_andnot(options->target, options->target, tmp_obj->allowed_cpuset);
#else
            hwloc_bitmap_or(result, result, tmp_obj->cpuset);
            hwloc_bitmap_andnot(node->available, node->available, tmp_obj->cpuset);
            hwloc_bitmap_andnot(options->target, options->target, tmp_obj->cpuset);
#endif
        }
    }
    hwloc_bitmap_list_asprintf(&proc->cpuset, result);
    hwloc_bitmap_free(result);
    return PRTE_SUCCESS;
}

int prte_rmaps_base_bind_proc(prte_job_t *jdata,
                              prte_proc_t *proc,
                              prte_node_t *node,
                              hwloc_obj_t obj,
                              prte_rmaps_options_t *options)
{
    int rc;

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps: compute bindings for job %s with policy %s[%x]",
                        PRTE_JOBID_PRINT(jdata->nspace),
                        prte_hwloc_base_print_binding(jdata->map->binding), jdata->map->binding);

    if (PRTE_MAPPING_BYUSER == options->map) {
        /* user specified binding by rankfile - nothing for us to do */
        return PRTE_SUCCESS;
    }

    if (PRTE_BIND_TO_NONE == options->bind) {
        rc = PRTE_SUCCESS;
        if (NULL != options->cpuset &&
            !(PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_TOOL))) {
            /* "soft" cgroup was given but no other
             * binding directive was provided, so bind
             * to those specific cpus */
            if (PRTE_SUCCESS != (rc = bind_to_cpuset(jdata, proc, node, options))) {
                PRTE_ERROR_LOG(rc);
            }
        }
        return rc;
    }

    /* some systems do not report cores, and so we can get a situation where our
     * default binding policy will fail for no necessary reason. So if we are
     * computing a binding due to our default policy, and no cores are found
     * on this node, just silently skip it - we will not bind
     */
    if (!PRTE_BINDING_POLICY_IS_SET(jdata->map->binding) &&
        HWLOC_TYPE_DEPTH_UNKNOWN == hwloc_get_type_depth(node->topology->topo, HWLOC_OBJ_CORE)) {
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "Unable to bind-to core by default on node %s as no cores detected",
                            node->name);
        return PRTE_SUCCESS;
    }

    if (PRTE_MAPPING_PELIST == options->map) {
        rc = bind_to_cpuset(jdata, proc, node, options);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
        }
        return rc;
    }

    if (1 < options->cpus_per_rank) {
        rc = bind_multiple(jdata, proc, node, obj, options);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
        }
        return rc;
    }

    rc = bind_generic(jdata, proc, node, obj, options);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }

    return rc;
}
