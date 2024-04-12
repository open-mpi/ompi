/*
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include <errno.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_argv.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/util/pmix_show_help.h"

#include "rmaps_ppr.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"

static int ppr_mapper(prte_job_t *jdata,
                      prte_rmaps_options_t *options);

prte_rmaps_base_module_t prte_rmaps_ppr_module = {
    .map_job = ppr_mapper
};

static int ppr_mapper(prte_job_t *jdata,
                      prte_rmaps_options_t *options)
{
    int rc = PRTE_SUCCESS, j, idx, ncpus;
    prte_proc_t *proc;
    pmix_mca_base_component_t *c = &prte_mca_rmaps_ppr_component;
    prte_node_t *node, *nd;
    prte_app_context_t *app;
    pmix_rank_t nprocs_mapped;
    prte_mapping_policy_t mapping = 0;
    prte_ranking_policy_t ranking;
    hwloc_obj_t obj;
    unsigned int nobjs, i;
    pmix_list_t node_list;
    int32_t num_slots;
    char **ck, *jobppr = NULL;
    size_t len;
    bool initial_map = true;
    prte_binding_policy_t savebind = options->bind;

    /* only handle initial launch of loadbalanced
     * or NPERxxx jobs - allow restarting of failed apps
     */
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:ppr: job %s being restarted - ppr cannot map",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }
    if (NULL != jdata->map->req_mapper
        && 0 != strcasecmp(jdata->map->req_mapper, c->pmix_mca_component_name)) {
        /* a mapper has been specified, and it isn't me */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:ppr: job %s not using ppr mapper",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }

    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_PPR, (void **) &jobppr, PMIX_STRING) ||
        NULL == jobppr || PRTE_MAPPING_PPR != PRTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
        /* not for us */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:ppr: job %s not using ppr mapper PPR %s policy %s",
                            PRTE_JOBID_PRINT(jdata->nspace), (NULL == jobppr) ? "NULL" : jobppr,
                            (PRTE_MAPPING_PPR == PRTE_GET_MAPPING_POLICY(jdata->map->mapping))
                                ? "PPRSET"
                                : "PPR NOTSET");
        if (NULL != jobppr) {
            free(jobppr);
        }
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:ppr: mapping job %s with ppr %s",
                        PRTE_JOBID_PRINT(jdata->nspace), jobppr);

    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->pmix_mca_component_name);

    ranking = PRTE_RANK_BY_SLOT;
    if (HWLOC_OBJ_MACHINE == options->maptype) {
        mapping = PRTE_MAPPING_BYNODE;
        ranking = PRTE_RANK_BY_NODE;
    } else if (HWLOC_OBJ_PACKAGE == options->maptype) {
            mapping = PRTE_MAPPING_BYPACKAGE;
    } else if (HWLOC_OBJ_NUMANODE== options->maptype) {
            mapping = PRTE_MAPPING_BYNUMA;
#if HWLOC_API_VERSION < 0x20000
    } else if (HWLOC_OBJ_CACHE == options->maptype) {
        if (1 == options->cmaplvl) {
            mapping = PRTE_MAPPING_BYL1CACHE;
        } else if (2 == options->cmaplvl) {
            mapping = PRTE_MAPPING_BYL2CACHE;
        } else if (3 == options->cmaplvl) {
            mapping = PRTE_MAPPING_BYL3CACHE;
        }
#else
    } else if (HWLOC_OBJ_L1CACHE == options->maptype) {
        mapping = PRTE_MAPPING_BYL1CACHE;
    } else if (HWLOC_OBJ_L2CACHE == options->maptype) {
        mapping = PRTE_MAPPING_BYL2CACHE;
    } else if (HWLOC_OBJ_L3CACHE == options->maptype) {
        mapping = PRTE_MAPPING_BYL3CACHE;
#endif
    } else if (HWLOC_OBJ_CORE == options->maptype) {
        mapping = PRTE_MAPPING_BYCORE;
    } else if (HWLOC_OBJ_PU == options->maptype) {
        mapping = PRTE_MAPPING_BYHWTHREAD;
    }

    /* record the results */
    PRTE_SET_MAPPING_POLICY(jdata->map->mapping, mapping);
    if (!PRTE_RANKING_POLICY_IS_SET(jdata->map->ranking)) {
        PRTE_SET_RANKING_POLICY(jdata->map->ranking, ranking);
    }
    options->map = PRTE_GET_MAPPING_POLICY(jdata->map->mapping);
    options->rank = PRTE_GET_RANKING_POLICY(jdata->map->ranking);
    if (PRTE_RANK_BY_SPAN == options->rank ||
        PRTE_RANK_BY_FILL == options->rank) {
        if (options->map < PRTE_MAPPING_BYNUMA ||
            options->map > PRTE_MAPPING_BYHWTHREAD) {
            pmix_show_help("help-prte-rmaps-base.txt", "must-map-by-obj",
                           true, prte_rmaps_base_print_mapping(options->map),
                           prte_rmaps_base_print_ranking(options->rank));
            free(jobppr);
            return PRTE_ERR_SILENT;
        }
    }

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:ppr: job %s assigned policy %s:%s",
                        PRTE_JOBID_PRINT(jdata->nspace),
                        prte_rmaps_base_print_mapping(options->map),
                        prte_rmaps_base_print_ranking(options->rank));

    /* cycle thru the apps */
    for (idx = 0; idx < jdata->apps->size; idx++) {
        app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, idx);
        if (NULL == app) {
            continue;
        }
        options->total_nobjs = 0;

        /* get the available nodes */
        PMIX_CONSTRUCT(&node_list, pmix_list_t);
        rc = prte_rmaps_base_get_target_nodes(&node_list, &num_slots, jdata, app,
                                              jdata->map->mapping, initial_map, false);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            goto error;
        }
        /* flag that all subsequent requests should not reset the node->mapped flag */
        initial_map = false;
        /* check to see if we can map all the procs */
        if (!PRTE_FLAG_TEST(app, PRTE_APP_FLAG_TOOL) &&
            num_slots < (int) app->num_procs) {
            if (!options->oversubscribe) {
                pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:alloc-error", true,
                               app->num_procs, app->app, prte_process_info.nodename);
                rc = PRTE_ERR_SILENT;
                goto error;
            } else {
                if (!PRTE_BINDING_POLICY_IS_SET(jdata->map->binding)) {
                    jdata->map->binding = PRTE_BIND_TO_NONE;
                    options->bind = PRTE_BIND_TO_NONE;
                }
            }
        }

        /* cycle across the nodes */
        nprocs_mapped = 0;
        PMIX_LIST_FOREACH_SAFE(node, nd, &node_list, prte_node_t) {
            options->nobjs = 0;
            prte_rmaps_base_get_cpuset(jdata, node, options);

            if (!options->donotlaunch) {
                rc = prte_rmaps_base_check_support(jdata, node, options);
                if (PRTE_SUCCESS != rc) {
                    goto error;
                }
            }

            if (HWLOC_OBJ_MACHINE == options->maptype) {
                options->nprocs = options->pprn;
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
                /* check availability and set the target cpuset - this
                 * also computes the nprocs to be assigned capped by
                 * the number of available binding targets */
                if (!prte_rmaps_base_check_avail(jdata, app, node, &node_list, NULL, options)) {
                    options->bind = savebind;
                    continue;
                }
                for (j = 0; j < options->pprn && nprocs_mapped < app->num_procs; j++) {
                    proc = prte_rmaps_base_setup_proc(jdata, idx, node, NULL, options);
                    if (NULL == proc) {
                        rc = PRTE_ERR_OUT_OF_RESOURCE;
                        goto error;
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
                        return rc;
                    }
                    PMIX_RELEASE(proc);
                }
            } else {
                /* get the number of resources on this node */
                nobjs = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                           options->maptype, options->cmaplvl);
                if (0 == nobjs) {
                    continue;
                }
                options->nprocs = options->pprn * nobjs;
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
                /* map the specified number of procs to each such resource on this node */
                for (j = 0; j < nobjs && nprocs_mapped < app->num_procs; j++) {
                    obj = prte_hwloc_base_get_obj_by_type(node->topology->topo,
                                                          options->maptype, options->cmaplvl, j);
                    if (!prte_rmaps_base_check_avail(jdata, app, node, &node_list, obj, options)) {
                        continue;
                    }
                    for (i=0; i < options->pprn && app->num_procs; i++) {
                        proc = prte_rmaps_base_setup_proc(jdata, idx, node, obj, options);
                        if (NULL == proc) {
                            rc = PRTE_ERR_OUT_OF_RESOURCE;
                            goto error;
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
                            return rc;
                        }
                        PMIX_RELEASE(proc);
                    }
                }
            }
            options->bind = savebind;

            /* if we haven't mapped all the procs, continue on to the
             * next node
             */
            if (nprocs_mapped == app->num_procs) {
                break;
            }
        }
        if (0 == app->num_procs) {
            app->num_procs = nprocs_mapped;
        }
        if (nprocs_mapped < app->num_procs) {
            /* couldn't map them all */
            pmix_show_help("help-prte-rmaps-ppr.txt", "ppr-too-many-procs", true, app->app,
                           app->num_procs, nprocs_mapped, options->nprocs, jobppr);
            rc = PRTE_ERR_SILENT;
            goto error;
        }

        jdata->num_procs += app->num_procs;

        PMIX_LIST_DESTRUCT(&node_list);
    }
    free(jobppr);
    /* calculate the ranks for this app */
    rc = prte_rmaps_base_compute_vpids(jdata, options);
    return rc;

error:
    PMIX_LIST_DESTRUCT(&node_list);
    free(jobppr);
    return rc;
}
