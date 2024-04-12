/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2018 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2018 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <string.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_output.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/runtime/prte_globals.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_show_help.h"

#include "rmaps_rr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"

int prte_rmaps_rr_byslot(prte_job_t *jdata,
                         prte_app_context_t *app,
                         pmix_list_t *node_list,
                         int32_t num_slots,
                         pmix_rank_t num_procs,
                         prte_rmaps_options_t *options)
{
    int i, rc, nprocs_mapped, ncpus;
    prte_node_t *node, *nd;
    int extra_procs_to_assign = 0, nxtra_nodes = 0;
    float balance;
    prte_proc_t *proc;
    bool second_pass = false;
    prte_binding_policy_t savebind = options->bind;

    pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rr: mapping by slot for job %s slots %d num_procs %lu",
                        PRTE_JOBID_PRINT(jdata->nspace), (int) num_slots,
                        (unsigned long) num_procs);

    /* check to see if we can map all the procs */
    if (num_slots < (int) app->num_procs) {
        if (!options->oversubscribe) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error", true,
                           app->num_procs, app->app, prte_process_info.nodename);
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
            return PRTE_ERR_SILENT;
        } else {
            if (!PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
                jdata->map->binding = PRTE_BIND_TO_NONE;
                options->bind = PRTE_BIND_TO_NONE;
                savebind = options->bind;
            }
        }
    }

    nprocs_mapped = 0;

pass:
    PMIX_LIST_FOREACH_SAFE(node, nd, node_list, prte_node_t)
    {
        pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rr:slot working node %s", node->name);

        prte_rmaps_base_get_cpuset(jdata, node, options);

        /* compute the number of procs to go on this node */
        if (second_pass) {
            options->nprocs = extra_procs_to_assign;
            if (0 < nxtra_nodes) {
                --nxtra_nodes;
                if (0 == nxtra_nodes) {
                    --extra_procs_to_assign;
                }
            }
        } else {
            if (!options->donotlaunch) {
                rc = prte_rmaps_base_check_support(jdata, node, options);
                if (PRTE_SUCCESS != rc) {
                    return rc;
                }
            }
            /* assign a number of procs equal to the number of available slots */
            if (!PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL)) {
                options->nprocs = node->slots_available;
            } else {
                options->nprocs = node->slots;
            }
        }

        if (!options->oversubscribe) {
            /* since oversubscribe is not allowed, cap our usage
             * at the number of available slots. */
            if (node->slots_available < options->nprocs) {
                options->nprocs = node->slots_available;
            }
        }

        /* if the number of procs is greater than the number of CPUs
         * on this node, but less or equal to the number of slots,
         * then we are not oversubscribed but we are overloaded. If
         * the user didn't specify a required binding, then we set
         * the binding policy to do-not-bind for this node */
        ncpus = prte_rmaps_base_get_ncpus(node, NULL, options);
        if (options->nprocs > ncpus &&
            options->nprocs <= node->slots_available &&
            !PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
            options->bind = PRTE_BIND_TO_NONE;
            jdata->map->binding = PRTE_BIND_TO_NONE;
        }

        if (!prte_rmaps_base_check_avail(jdata, app, node, node_list, NULL, options)) {
            rc = PRTE_ERR_OUT_OF_RESOURCE;
            options->bind = savebind;
            continue;
        }

        pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rr:slot assigning %d procs to node %s",
                            (int) options->nprocs, node->name);

        for (i = 0; i < options->nprocs && nprocs_mapped < app->num_procs; i++) {
            proc = prte_rmaps_base_setup_proc(jdata, app->idx, node, NULL, options);
            if (NULL == proc) {
                /* move on to the next node */
                rc = PRTE_ERR_SILENT;
                break;
            }
            nprocs_mapped++;
            rc = prte_rmaps_base_check_oversubscribed(jdata, app, node, options);
            if (PRTE_ERR_TAKE_NEXT_OPTION == rc) {
                /* move to next node */
                PMIX_RELEASE(proc);
                break;
            } else if (PRTE_SUCCESS != rc) {
                /* got an error */
                PMIX_RELEASE(proc);
                goto errout;
            }
            PMIX_RELEASE(proc);
        }

        if (nprocs_mapped == app->num_procs) {
            return PRTE_SUCCESS;
        }
        options->bind = savebind;
        if(NULL != options->target)
        {
            hwloc_bitmap_free(options->target);
            options->target = NULL;
        }
    }

    if (second_pass) {
    errout:
        if (PRTE_ERR_SILENT != rc) {
            pmix_show_help("help-prte-rmaps-base.txt",
                           "failed-map", true,
                           PRTE_ERROR_NAME(rc),
                           (NULL == app) ? "N/A" : app->app,
                           (NULL == app) ? -1 : app->num_procs,
                           prte_rmaps_base_print_mapping(options->map),
                           prte_hwloc_base_print_binding(options->bind));
        }
        return PRTE_ERR_SILENT;
    }

    pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rr:slot job %s is oversubscribed - performing second pass",
                        PRTE_JOBID_PRINT(jdata->nspace));

    /* second pass: if we haven't mapped everyone yet, it is
     * because we are oversubscribed. All of the nodes that are
     * at max_slots have been removed from the list as that specifies
     * a hard boundary, so the nodes remaining are available for
     * handling the oversubscription. Figure out how many procs
     * to add to each of them.
     */
    balance = (float) ((int) app->num_procs - nprocs_mapped)
              / (float) pmix_list_get_size(node_list);
    extra_procs_to_assign = (int) balance;
    if (0 < (balance - (float) extra_procs_to_assign)) {
        /* compute how many nodes need an extra proc */
        nxtra_nodes = app->num_procs - nprocs_mapped
                      - (extra_procs_to_assign * pmix_list_get_size(node_list));
        /* add one so that we add an extra proc to the first nodes
         * until all procs are mapped
         */
        extra_procs_to_assign++;
    }
    // Rescan the nodes
    second_pass = true;
    goto pass;
}

int prte_rmaps_rr_bynode(prte_job_t *jdata,
                         prte_app_context_t *app,
                         pmix_list_t *node_list,
                         int32_t num_slots,
                         pmix_rank_t num_procs,
                         prte_rmaps_options_t *options)
{
    int rc, j, nprocs_mapped, ncpus;
    prte_node_t *node, *nd;
    bool second_pass = false;
    prte_proc_t *proc;
    prte_binding_policy_t savebind = options->bind;

    pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rr: mapping by node for job %s app %d slots %d num_procs %lu",
                        PRTE_JOBID_PRINT(jdata->nspace), (int) app->idx, (int) num_slots,
                        (unsigned long) num_procs);

    /* quick check to see if we can map all the procs */
    if (num_slots < (int) app->num_procs) {
        if (!options->oversubscribe) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error", true,
                           app->num_procs, app->app, prte_process_info.nodename);
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
            return PRTE_ERR_SILENT;
        } else {
            if (!PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
                jdata->map->binding = PRTE_BIND_TO_NONE;
                options->bind = PRTE_BIND_TO_NONE;
                savebind = PRTE_BIND_TO_NONE;
            }
        }
    }

    nprocs_mapped = 0;

pass:
    /* divide the procs evenly across all nodes - this is the
     * average we have to maintain as we go, but we adjust
     * the number on each node to reflect its available slots.
     * Obviously, if all nodes have the same number of slots,
     * then the avg is what we get on each node - this is
     * the most common situation.
     */
    options->nprocs = (app->num_procs - nprocs_mapped) / pmix_list_get_size(node_list);
    if (0 == options->nprocs) {
        /* if there are less procs than nodes, we have to
         * place at least one/node
         */
        options->nprocs = 1;
    }

    PMIX_LIST_FOREACH_SAFE(node, nd, node_list, prte_node_t)
    {
        prte_rmaps_base_get_cpuset(jdata, node, options);

        if (!options->oversubscribe) {
            /* since oversubscribe is not allowed, cap our usage
             * at the number of available slots. */
            if (node->slots_available < options->nprocs) {
                options->nprocs = node->slots_available;
            }
        }

        /* if the number of procs is greater than the number of CPUs
         * on this node, but less or equal to the number of slots,
         * then we are not oversubscribed but we are overloaded. If
         * the user didn't specify a required binding, then we set
         * the binding policy to do-not-bind for this node */
        ncpus = prte_rmaps_base_get_ncpus(node, NULL, options);
        if (options->nprocs > ncpus &&
            options->nprocs <= node->slots_available &&
            !PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
            options->bind = PRTE_BIND_TO_NONE;
            jdata->map->binding = PRTE_BIND_TO_NONE;
        }

        if (!prte_rmaps_base_check_avail(jdata, app, node, node_list, NULL, options)) {
            rc = PRTE_ERR_OUT_OF_RESOURCE;
            options->bind = savebind;
            continue;
        }

        PMIX_OUTPUT_VERBOSE((10, prte_rmaps_base_framework.framework_output,
                             "%s NODE %s ASSIGNING %d PROCS",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             node->name, options->nprocs));

        for (j=0; j < options->nprocs && nprocs_mapped < app->num_procs; j++) {
            proc = prte_rmaps_base_setup_proc(jdata, app->idx, node, NULL, options);
            if (NULL == proc) {
                /* move to next node */
                rc = PRTE_ERR_SILENT;
                break;
            }
            nprocs_mapped++;
            rc = prte_rmaps_base_check_oversubscribed(jdata, app, node, options);
            if (PRTE_ERR_TAKE_NEXT_OPTION == rc) {
                /* move to next node */
                PMIX_RELEASE(proc);
                break;
            } else if (PRTE_SUCCESS != rc) {
                /* got an error */
                PMIX_RELEASE(proc);
                goto errout;
            }
            PMIX_RELEASE(proc);
        }
        if (nprocs_mapped == app->num_procs) {
            return PRTE_SUCCESS;
        }
        options->bind = savebind;
        if(NULL != options->target)
        {
            hwloc_bitmap_free(options->target);
            options->target = NULL;
        }
    }

    if (second_pass) {
    errout:
        /* unable to do it */
        if (PRTE_ERR_SILENT != rc) {
            pmix_show_help("help-prte-rmaps-base.txt",
                           "failed-map", true,
                           PRTE_ERROR_NAME(rc),
                           (NULL == app) ? "N/A" : app->app,
                           (NULL == app) ? -1 : app->num_procs,
                           prte_rmaps_base_print_mapping(options->map),
                           prte_hwloc_base_print_binding(options->bind));
        }
        return PRTE_ERR_SILENT;
    }
    pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rr:node job %s is oversubscribed - performing second pass",
                        PRTE_JOBID_PRINT(jdata->nspace));

    /* second pass: if we haven't mapped everyone yet, it is
     * because we are oversubscribed. All of the nodes that are
     * at max_slots have been removed from the list as that specifies
     * a hard boundary, so the nodes remaining are available for
     * handling the oversubscription.
     */
    second_pass = true;
    goto pass;
}

/* mapping by cpu */
int prte_rmaps_rr_bycpu(prte_job_t *jdata, prte_app_context_t *app,
                        pmix_list_t *node_list, int32_t num_slots,
                        pmix_rank_t num_procs, prte_rmaps_options_t *options)
{
    int i, rc, nprocs_mapped, ncpus;
    prte_node_t *node, *nd;
    prte_proc_t *proc;
    char **tmp;
    int ntomap;
    bool second_pass = false;
    int extra_procs_to_assign = 0, nxtra_nodes = 0;
    float balance;
    char *savecpuset = NULL;
    prte_binding_policy_t savebind = options->bind;
    PRTE_HIDE_UNUSED_PARAMS(num_procs);

    pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rr: mapping by cpu for job %s slots %d num_procs %lu",
                        PRTE_JOBID_PRINT(jdata->nspace), (int) num_slots,
                        (unsigned long)app->num_procs);

    /* check to see if we can map all the procs */
    if (num_slots < (int) app->num_procs) {
        if (!options->oversubscribe) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error", true,
                           app->num_procs, app->app, prte_process_info.nodename);
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
            return PRTE_ERR_SILENT;
        } else {
            if (!PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
                jdata->map->binding = PRTE_BIND_TO_NONE;
                options->bind = PRTE_BIND_TO_NONE;
                savebind = PRTE_BIND_TO_NONE;
            }
        }
    }

    nprocs_mapped = 0;
    tmp = PMIX_ARGV_SPLIT_COMPAT(options->cpuset, ',');
    ntomap = PMIX_ARGV_COUNT_COMPAT(tmp);
    PMIX_ARGV_FREE_COMPAT(tmp);
    savecpuset = strdup(options->cpuset);

pass:
    PMIX_LIST_FOREACH_SAFE(node, nd, node_list, prte_node_t)
    {
        pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rr:cpu working node %s", node->name);

        prte_rmaps_base_get_cpuset(jdata, node, options);

        if (second_pass) {
            options->nprocs = extra_procs_to_assign;
            if (0 < nxtra_nodes) {
                --nxtra_nodes;
                if (0 == nxtra_nodes) {
                    --extra_procs_to_assign;
                }
            }
        } else  if (options->ordered || !options->overload) {
            options->nprocs = ntomap;
        } else {
            /* assign a number of procs equal to the number of available slots */
            if (!PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL)) {
                options->nprocs = node->slots_available;
            } else {
                options->nprocs = node->slots;
            }
        }

        if (!options->oversubscribe) {
            /* oversubscribe is not allowed, so cap our usage
             * at the number of available slots. */
            if (node->slots_available < options->nprocs) {
                options->nprocs = node->slots_available;
            }
        }

        /* if the number of procs is greater than the number of CPUs
         * on this node, but less or equal to the number of slots,
         * then we are not oversubscribed but we are overloaded. If
         * the user didn't specify a required binding, then we set
         * the binding policy to do-not-bind for this node */
        ncpus = prte_rmaps_base_get_ncpus(node, NULL, options);
        if (options->nprocs > ncpus &&
            options->nprocs <= node->slots_available &&
            !PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
            options->bind = PRTE_BIND_TO_NONE;
            jdata->map->binding = PRTE_BIND_TO_NONE;
        }

        if (!prte_rmaps_base_check_avail(jdata, app, node, node_list, NULL, options)) {
            rc = PRTE_ERR_OUT_OF_RESOURCE;
            options->bind = savebind;
            continue;
        }

        pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rr:cpu assigning %d procs to node %s",
                            (int) options->nprocs, node->name);

        for (i = 0; i < options->nprocs && nprocs_mapped < app->num_procs; i++) {
            proc = prte_rmaps_base_setup_proc(jdata, app->idx, node, NULL, options);
            if (NULL == proc) {
                rc = PRTE_ERR_SILENT;
                goto errout;
            }
            nprocs_mapped++;
            rc = prte_rmaps_base_check_oversubscribed(jdata, app, node, options);
            if (PRTE_ERR_TAKE_NEXT_OPTION == rc) {
                /* move to next node */
                PMIX_RELEASE(proc);
                break;
            } else if (PRTE_SUCCESS != rc) {
                /* got an error */
                PMIX_RELEASE(proc);
                goto errout;
            }
            PMIX_RELEASE(proc);
        }
        if (nprocs_mapped == app->num_procs) {
            if (NULL != options->target) {
                hwloc_bitmap_free(options->target);
                options->target = NULL;
            }
            if (NULL != options->job_cpuset) {
                hwloc_bitmap_free(options->job_cpuset);
                options->job_cpuset = NULL;
            }
            if (NULL != savecpuset) {
                free(savecpuset);
            }
            return PRTE_SUCCESS;
        }
        if (NULL != options->target) {
            hwloc_bitmap_free(options->target);
            options->target = NULL;
        }
        if (NULL != options->job_cpuset) {
            hwloc_bitmap_free(options->job_cpuset);
            options->job_cpuset = NULL;
        }
        if (NULL != options->cpuset) {
            free(options->cpuset);
        }
        options->cpuset = strdup(savecpuset);
    } // next node

    /* second pass: if we haven't mapped everyone yet, it is
     * because we are oversubscribed. All of the nodes that are
     * at max_slots have been removed from the list as that specifies
     * a hard boundary, so the nodes remaining are available for
     * handling the oversubscription. Figure out how many procs
     * to add to each of them.
     */
    if (options->oversubscribe && !second_pass) {
        balance = (float) ((int) app->num_procs - nprocs_mapped)
        / (float) pmix_list_get_size(node_list);
        extra_procs_to_assign = (int) balance;
        if (0 < (balance - (float) extra_procs_to_assign)) {
            /* compute how many nodes need an extra proc */
            nxtra_nodes = app->num_procs - nprocs_mapped
            - (extra_procs_to_assign * pmix_list_get_size(node_list));
            /* add one so that we add an extra proc to the first nodes
             * until all procs are mapped
             */
            extra_procs_to_assign++;
        }
        /* restore the cpuset */
        if (NULL != options->cpuset) {
            free(options->cpuset);
        }
        options->cpuset = strdup(savecpuset);
        // Rescan the nodes
        second_pass = true;
        goto pass;
    }

errout:
    /* if we get here, then we were unable to map all the procs */
    if (PRTE_ERR_SILENT != rc) {
        pmix_show_help("help-prte-rmaps-rr.txt",
                       "prte-rmaps-rr:not-enough-cpus", true,
                       (NULL == app) ? "N/A" : app->app,
                       (NULL == app) ? -1 : app->num_procs,
                       savecpuset);
    }
    if (NULL != savecpuset) {
        free(savecpuset);
    }
    return PRTE_ERR_SILENT;
}

/* mapping by hwloc object looks a lot like mapping by node,
 * but has the added complication of possibly having different
 * numbers of objects on each node
 */
int prte_rmaps_rr_byobj(prte_job_t *jdata, prte_app_context_t *app,
                        pmix_list_t *node_list, int32_t num_slots,
                        pmix_rank_t num_procs,
                        prte_rmaps_options_t *options)
{
    int i, k, rc, nprocs_mapped, nprocs;
    prte_node_t *node, *nnext;
    int ncpus;
    float balance;
    prte_proc_t *proc;
    bool second_pass = false;
    bool nodefull, allfull, outofcpus;
    hwloc_obj_t obj = NULL;
    unsigned j, total_nobjs, nobjs;
    prte_binding_policy_t savebind = options->bind;

    pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rr:byobj mapping by %s for job %s slots %d num_procs %lu",
                        hwloc_obj_type_string(options->maptype),
                        PRTE_JOBID_PRINT(jdata->nspace),
                        (int) num_slots, (unsigned long) num_procs);

    /* quick check to see if we can map all the procs */
    if (num_slots < app->num_procs) {
        if (!options->oversubscribe) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error", true,
                           app->num_procs, app->app, prte_process_info.nodename);
            PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
            return PRTE_ERR_SILENT;
        } else {
            if (!PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
                jdata->map->binding = PRTE_BIND_TO_NONE;
                options->bind = PRTE_BIND_TO_NONE;
                savebind = PRTE_BIND_TO_NONE;
            }
        }
    }

    /* there are two modes for mapping by object: span and not-span. The
     * span mode essentially operates as if there was just a single
     * "super-node" in the system - i.e., it balances the load across
     * all objects of the indicated type regardless of their location.
     * In essence, it acts as if we placed one proc on each object, cycling
     * across all objects on all nodes, and then wrapped around to place
     * another proc on each object, doing so until all procs were placed.
     *
     * In contrast, the non-span mode operates similar to byslot mapping.
     * All slots on each node are filled, assigning each proc to an object
     * on that node in a balanced fashion, and then the mapper moves on
     * to the next node. Thus, procs tend to be "front loaded" onto the
     * list of nodes, as opposed to being "load balanced" in the span mode
     */
    allfull = true;
    nprocs_mapped = 0;
    do {
        allfull = true;
        PMIX_LIST_FOREACH_SAFE(node, nnext, node_list, prte_node_t)
        {
            outofcpus = false;
            prte_rmaps_base_get_cpuset(jdata, node, options);
            if (!options->donotlaunch) {
                rc = prte_rmaps_base_check_support(jdata, node, options);
                if (PRTE_SUCCESS != rc) {
                    PRTE_ERROR_LOG(rc);
                    goto errout;
                }
            }

            options->nobjs = 0;
            /* have to delay checking for availability until we have the object */

            /* get the number of objects of this type on this node */
            nobjs = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                       options->maptype, options->cmaplvl);
            if (0 == nobjs) {
                /* this node doesn't have any objects of this type, so
                 * we might as well drop it from consideration */
                pmix_list_remove_item(node_list, &node->super);
                PMIX_RELEASE(node);
                continue;
            }
            pmix_output_verbose(2, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:rr: found %u %s objects on node %s",
                                nobjs, hwloc_obj_type_string(options->maptype),
                                node->name);

            nodefull = false;
        redo:
            for (j=0; j < nobjs && nprocs_mapped < app->num_procs && !nodefull; j++) {
                pmix_output_verbose(10, prte_rmaps_base_framework.framework_output,
                                    "mca:rmaps:rr: assigning proc to object %d", j);
                /* get the hwloc object */
                obj = prte_hwloc_base_get_obj_by_type(node->topology->topo,
                                                      options->maptype, options->cmaplvl, j);
                if (NULL == obj) {
                    /* out of objects on this node */
                    break;
                }
                /* does this object have enough available cpus to
                 * support the requested cpus_per_rank? */
                ncpus = prte_rmaps_base_get_ncpus(node, obj, options);
                if (ncpus < options->cpus_per_rank && !options->overload) {
                    outofcpus = true;
                    continue;
                }
                options->nprocs = 1;

                if (!prte_rmaps_base_check_avail(jdata, app, node, node_list, obj, options)) {
                    rc = PRTE_ERR_OUT_OF_RESOURCE;
                    PRTE_ERROR_LOG(rc);
                    continue;
                }

                proc = prte_rmaps_base_setup_proc(jdata, app->idx, node, obj, options);
                if (NULL == proc) {
                    rc = PRTE_ERR_OUT_OF_RESOURCE;
                    goto errout;
                }
                nprocs_mapped++;
                rc = prte_rmaps_base_check_oversubscribed(jdata, app, node, options);
                if (PRTE_ERR_TAKE_NEXT_OPTION == rc) {
                    /* move to next node */
                    pmix_list_remove_item(node_list, &node->super);
                    PMIX_RELEASE(node);
                    nodefull = true;
                    PMIX_RELEASE(proc);
                    break;
                } else if (PRTE_SUCCESS != rc) {
                    /* got an error */
                    PMIX_RELEASE(proc);
                    goto errout;
                }
                PMIX_RELEASE(proc);
                allfull = false;
            }
            if (nprocs_mapped < app->num_procs && !allfull &&
                !nodefull && !outofcpus && !options->mapspan) {
                // keep working these objects until full
                goto redo;
            }
            // move to the next node
            if (NULL != options->target) {
                hwloc_bitmap_free(options->target);
                options->target = NULL;
            }
        }
    } while (nprocs_mapped < app->num_procs && !allfull);

    if (nprocs_mapped == app->num_procs) {
        return PRTE_SUCCESS;
    }

errout:
    if (outofcpus) {
        /* ran out of cpus */
        pmix_show_help("help-prte-rmaps-base.txt",
                       "allocation-overload", true,
                       (NULL == app) ? "N/A" : app->app,
                       (NULL == app) ? -1 : app->num_procs,
                       prte_rmaps_base_print_mapping(options->map),
                       prte_hwloc_base_print_binding(options->bind));
        return PRTE_ERR_SILENT;
    }
    pmix_show_help("help-prte-rmaps-base.txt",
                   "failed-map", true,
                   PRTE_ERROR_NAME(rc),
                   (NULL == app) ? "N/A" : app->app,
                   (NULL == app) ? -1 : app->num_procs,
                   prte_rmaps_base_print_mapping(options->map),
                   prte_hwloc_base_print_binding(options->bind));
    return PRTE_ERR_SILENT;
}
