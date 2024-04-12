/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2008      Voltaire. All rights reserved
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2022 IBM Corporation.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
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
#include <sys/stat.h>
#include <sys/types.h>

#include "src/class/pmix_pointer_array.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_string_copy.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"
#include "src/mca/rmaps/rank_file/rmaps_rank_file.h"
#include "src/mca/rmaps/rank_file/rmaps_rank_file_lex.h"
#include "src/runtime/prte_globals.h"
#include "src/util/pmix_show_help.h"

static int prte_rmaps_rf_map(prte_job_t *jdata,
                             prte_rmaps_options_t *options);

prte_rmaps_base_module_t prte_rmaps_rank_file_module = {
    .map_job = prte_rmaps_rf_map
};

static int prte_rmaps_rank_file_parse(const char *);
static char *prte_rmaps_rank_file_parse_string_or_int(void);

static int prte_rmaps_rf_lsf_convert_affinity_to_rankfile(char *affinity_file, char **aff_rankfile);
static int prte_rmaps_rf_process_lsf_affinity_hostfile(prte_job_t *jdata, prte_rmaps_options_t *options, char *affinity_file);

char *prte_rmaps_rank_file_slot_list = NULL;

#if PMIX_NUMERIC_VERSION < 0x00040205
static char *pmix_getline(FILE *fp)
{
    char *ret, *buff;
    char input[1024];

    ret = fgets(input, 1024, fp);
    if (NULL != ret) {
        input[strlen(input) - 1] = '\0'; /* remove newline */
        buff = strdup(input);
        return buff;
    }

    return NULL;
}
#endif

/*
 * Local variable
 */
static pmix_pointer_array_t rankmap;
static int num_ranks = 0;

/*
 * Create a rank_file  mapping for the job.
 */
static int prte_rmaps_rf_map(prte_job_t *jdata,
                             prte_rmaps_options_t *options)
{
    prte_job_map_t *map;
    prte_app_context_t *app = NULL;
    int32_t i, k;
    pmix_list_t node_list;
    prte_node_t *node, *nd, *root_node;
    pmix_rank_t rank, vpid_start;
    int32_t num_slots;
    prte_rmaps_rank_file_map_t *rfmap;
    int32_t relative_index, tmp_cnt;
    int rc;
    prte_proc_t *proc;
    pmix_mca_base_component_t *c = &prte_mca_rmaps_rank_file_component.super;
    char *slots = NULL;
    bool initial_map = true;
    char *rankfile = NULL;
    hwloc_obj_t obj = NULL;
    char *affinity_file = NULL;
    hwloc_cpuset_t proc_bitmap, bitmap;
    char *cpu_bitmap;
    char *avail_bitmap = NULL;
    char *overlap_bitmap = NULL;

    /* only handle initial launch of rf job */
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: job %s being restarted - rank_file cannot map",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }
    /* check to see if any mapping or binding directives were given */
    if (!(PRTE_MAPPING_GIVEN & PRTE_GET_MAPPING_DIRECTIVE(jdata->map->mapping)) &&
        !(PRTE_MAPPING_GIVEN & PRTE_GET_MAPPING_DIRECTIVE(prte_rmaps_base.mapping))) {
        if (NULL != (affinity_file = getenv("LSB_AFFINITY_HOSTFILE"))) {
            /* Process the affinity hostfile (if valid) and update jdata
             * structure approprately.
             */
            prte_rmaps_rf_process_lsf_affinity_hostfile(jdata, options, affinity_file);
        }
    }
    if (NULL != jdata->map->req_mapper
        && 0 != strcasecmp(jdata->map->req_mapper, c->pmix_mca_component_name)) {
        /* a mapper has been specified, and it isn't me */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: job %s not using rank_file mapper",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }
    if (PRTE_MAPPING_BYUSER != PRTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
        /* NOT FOR US */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: job %s not using rankfile policy",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }
    if (options->ordered) {
        /* NOT FOR US */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: job %s binding order requested - rank_file cannot map",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_FILE, (void **) &rankfile, PMIX_STRING)
        || NULL == rankfile) {
        /* we cannot do it */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: job %s no rankfile specified",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_BAD_PARAM;
    }

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rank_file: mapping job %s", PRTE_JOBID_PRINT(jdata->nspace));

    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->pmix_mca_component_name);

    /* convenience def */
    map = jdata->map;

    /* setup the node list */
    PMIX_CONSTRUCT(&node_list, pmix_list_t);

    /* pickup the first app - there must be at least one */
    app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, 0);
    if (NULL == app) {
        rc = PRTE_ERR_SILENT;
        goto error;
    }

    /* SANITY CHECKS */

    /* if the number of processes wasn't specified, then we know there can be only
     * one app_context allowed in the launch, and that we are to launch it across
     * all available slots.
     */
    if (0 == app->num_procs && 1 < jdata->num_apps) {
        pmix_show_help("help-rmaps_rank_file.txt", "prte-rmaps-rf:multi-apps-and-zero-np", true,
                       jdata->num_apps, NULL);
        rc = PRTE_ERR_SILENT;
        goto error;
    }

    /* END SANITY CHECKS */

    /* start at the beginning... */
    vpid_start = 0;
    jdata->num_procs = 0;
    PMIX_CONSTRUCT(&rankmap, pmix_pointer_array_t);

    /* parse the rankfile, storing its results in the rankmap */
    if (PRTE_SUCCESS != (rc = prte_rmaps_rank_file_parse(rankfile))) {
        rc = PRTE_ERR_SILENT;
        goto error;
    }

    /* cycle through the app_contexts, mapping them sequentially */
    for (i = 0; i < jdata->apps->size; i++) {
        app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i);
        if (NULL == app) {
            continue;
        }

        /* for each app_context, we have to get the list of nodes that it can
         * use since that can now be modified with a hostfile and/or -host
         * option
         */
        rc = prte_rmaps_base_get_target_nodes(&node_list, &num_slots, jdata, app,
                                              options->map, initial_map, false);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            goto error;
        }
        /* flag that all subsequent requests should not reset the node->mapped flag */
        initial_map = false;

        /* we already checked for sanity, so it's okay to just do here */
        if (0 == app->num_procs) {
            /* set the number of procs to the number of entries in that rankfile */
            app->num_procs = num_ranks;
        }
        if (0 == app->num_procs) {
            pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
            rc = PRTE_ERR_SILENT;
            goto error;
        }
        for (k = 0; k < app->num_procs; k++) {
            rank = vpid_start + k;
            /* get the rankfile entry for this rank */
            rfmap = (prte_rmaps_rank_file_map_t *) pmix_pointer_array_get_item(&rankmap, rank);
            if (NULL == rfmap) {
                /* if this job was given a slot-list, then use it */
                if (NULL != options->cpuset) {
                    slots = options->cpuset;
                } else if (NULL != prte_hwloc_default_cpu_list) {
                    /* if we were give a default slot-list, then use it */
                    slots = prte_hwloc_default_cpu_list;
                } else {
                    /* all ranks must be specified */
                    pmix_show_help("help-rmaps_rank_file.txt", "missing-rank", true, rank,
                                   rankfile);
                    rc = PRTE_ERR_SILENT;
                    goto error;
                }
                /* take the next node off of the available list */
                node = NULL;
                PMIX_LIST_FOREACH(nd, &node_list, prte_node_t)
                {
                    /* if adding one to this node would oversubscribe it, then try
                     * the next one */
                    if (nd->slots <= (int) nd->num_procs) {
                        continue;
                    }
                    /* take this one */
                    node = nd;
                    break;
                }
                if (NULL == node) {
                    /* all would be oversubscribed, so take the least loaded one */
                    k = (int32_t) UINT32_MAX;
                    PMIX_LIST_FOREACH(nd, &node_list, prte_node_t)
                    {
                        if (nd->num_procs < (pmix_rank_t) k) {
                            k = nd->num_procs;
                            node = nd;
                        }
                    }
                }
                /* if we still have nothing, then something is very wrong */
                if (NULL == node) {
                    PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
                    rc = PRTE_ERR_OUT_OF_RESOURCE;
                    goto error;
                }
            } else {
                if (0 == strlen(rfmap->slot_list)) {
                    /* rank was specified but no slot list given - that's an error */
                    pmix_show_help("help-rmaps_rank_file.txt", "no-slot-list", true, rank,
                                   rfmap->node_name);
                    rc = PRTE_ERR_SILENT;
                    goto error;
                }
                slots = rfmap->slot_list;
                /* find the node where this proc was assigned */
                node = NULL;
                PMIX_LIST_FOREACH(nd, &node_list, prte_node_t)
                {
                    if (NULL != rfmap->node_name && 0 == strcmp(nd->name, rfmap->node_name)) {
                        node = nd;
                        break;
                    } else if (NULL != rfmap->node_name
                               && (('+' == rfmap->node_name[0])
                                   && (('n' == rfmap->node_name[1])
                                       || ('N' == rfmap->node_name[1])))) {

                        relative_index = atoi(strtok(rfmap->node_name, "+n"));
                        if (relative_index >= (int) pmix_list_get_size(&node_list)
                            || (0 > relative_index)) {
                            pmix_show_help("help-rmaps_rank_file.txt", "bad-index", true,
                                           rfmap->node_name);
                            PRTE_ERROR_LOG(PRTE_ERR_BAD_PARAM);
                            return PRTE_ERR_BAD_PARAM;
                        }
                        root_node = (prte_node_t *) pmix_list_get_first(&node_list);
                        for (tmp_cnt = 0; tmp_cnt < relative_index; tmp_cnt++) {
                            root_node = (prte_node_t *) pmix_list_get_next(root_node);
                        }
                        node = root_node;
                        break;
                    }
                }
            }
            if (NULL == node) {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-host", true, rfmap->node_name);
                rc = PRTE_ERR_SILENT;
                goto error;
            }
            if (!options->donotlaunch) {
                rc = prte_rmaps_base_check_support(jdata, node, options);
                if (PRTE_SUCCESS != rc) {
                    return rc;
                }
            }
            prte_rmaps_base_get_cpuset(jdata, node, options);
            if (!prte_rmaps_base_check_avail(jdata, app, node, &node_list, NULL, options)) {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-host", true, rfmap->node_name);
                rc = PRTE_ERR_SILENT;
                goto error;
            }
            /* check if we are oversubscribed */
            rc = prte_rmaps_base_check_oversubscribed(jdata, app, node, options);
            if (PRTE_SUCCESS != rc) {
                PMIX_RELEASE(proc);
                goto error;
            }
            options->map = PRTE_MAPPING_BYUSER;
            proc = prte_rmaps_base_setup_proc(jdata, app->idx, node, NULL, options);
            if (NULL == proc) {
                PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
                rc = PRTE_ERR_OUT_OF_RESOURCE;
                goto error;
            }
            /* set the vpid */
            proc->name.rank = rank;
            /* Assign this process to these slots
             * Allow for overload in cases where different ranks are assigned to
             * the same PU, but it must be requested by the user.
             */
            if (NULL != slots &&
                (PRTE_BIND_TO_NONE != PRTE_GET_BINDING_POLICY(jdata->map->binding) || options->overload) ) {
                if (NULL == node->topology || NULL == node->topology->topo) {
                    // Not allowed - for rank-file, we must have the topology
                    pmix_show_help("help-prte-rmaps-base.txt", "rmaps:no-topology", true,
                                   node->name);
                    rc = PRTE_ERR_SILENT;
                    goto error;
                }
                proc_bitmap = hwloc_bitmap_alloc();

                /* parse the slot_list to find the package and core */
                rc = prte_hwloc_base_cpu_list_parse(slots, node->topology->topo, options->use_hwthreads, proc_bitmap);
                if (PRTE_ERR_NOT_FOUND == rc) {
                    char *tmp = prte_hwloc_base_cset2str(hwloc_topology_get_allowed_cpuset(node->topology->topo),
                                                         false, node->topology->topo);
                    pmix_show_help("help-rmaps_rank_file.txt", "missing-cpu", true,
                                   prte_tool_basename, slots, tmp);
                    free(tmp);
                    rc = PRTE_ERR_SILENT;
                    hwloc_bitmap_free(proc_bitmap);
                    goto error;
                } else if (PRTE_ERROR == rc) {
                    pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = PRTE_ERR_SILENT;
                    hwloc_bitmap_free(proc_bitmap);
                    goto error;
                } else if (PRTE_SUCCESS != rc) {
                    PRTE_ERROR_LOG(rc);
                    hwloc_bitmap_free(proc_bitmap);
                    goto error;
                }
                /* note that we cannot set the proc locale to any specific object
                 * as the slot list may have assigned it to more than one - so
                 * leave that field NULL
                 */

                /* set the proc to the specified map */
                hwloc_bitmap_list_asprintf(&cpu_bitmap, proc_bitmap);
                proc->cpuset = strdup(cpu_bitmap);

                pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                    "mca:rmaps:rank_file: convert slots from <%s> to <%s>",
                                    slots, cpu_bitmap);

                /* Check to see if these slots are available on this node */
                if (!hwloc_bitmap_isincluded(proc_bitmap, node->available) && !options->overload) {
                    bitmap = hwloc_bitmap_alloc();
                    hwloc_bitmap_list_asprintf(&avail_bitmap, node->available);

                    hwloc_bitmap_andnot(bitmap, proc_bitmap, node->available);
                    hwloc_bitmap_list_asprintf(&overlap_bitmap, bitmap);

                    pmix_show_help("help-rmaps_rank_file.txt", "rmaps:proc-slots-overloaded", true,
                                   PRTE_NAME_PRINT(&proc->name),
                                   node->name,
                                   proc->cpuset,
                                   avail_bitmap,
                                   overlap_bitmap);

                    hwloc_bitmap_free(bitmap);
                    hwloc_bitmap_free(proc_bitmap);
                    rc = PRTE_ERR_OUT_OF_RESOURCE;
                    goto error;
                }

                /* Mark these slots as taken on this node */
#if HWLOC_API_VERSION < 0x20000
                hwloc_bitmap_andnot(node->available, node->available, proc_bitmap);
#else
                hwloc_bitmap_andnot(node->available, node->available, proc_bitmap);
#endif

                /* cleanup */
                free(cpu_bitmap);
                hwloc_bitmap_free(proc_bitmap);
            }
            /* insert the proc into the proper place */
            PMIX_RETAIN(proc);
            rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
            if (PRTE_SUCCESS != rc) {
                PRTE_ERROR_LOG(rc);
                PMIX_RELEASE(proc);
                goto error;
            }
            jdata->num_procs++;
            PMIX_RELEASE(proc);
        }
        /* update the starting point */
        vpid_start += app->num_procs;
        /* cleanup the node list - it can differ from one app_context
         * to another, so we have to get it every time
         */
        PMIX_LIST_DESTRUCT(&node_list);
        PMIX_CONSTRUCT(&node_list, pmix_list_t);
    }
    PMIX_LIST_DESTRUCT(&node_list);

    /* cleanup the rankmap */
    for (i = 0; i < rankmap.size; i++) {
        if (NULL != (rfmap = pmix_pointer_array_get_item(&rankmap, i))) {
            PMIX_RELEASE(rfmap);
        }
    }
    PMIX_DESTRUCT(&rankmap);
    if (NULL != rankfile) {
        free(rankfile);
    }
    /* compute local/app ranks */
    rc = prte_rmaps_base_compute_vpids(jdata, options);
    return rc;

error:
    PMIX_LIST_DESTRUCT(&node_list);
    if (NULL != rankfile) {
        free(rankfile);
    }

    return rc;
}

static int prte_rmaps_rank_file_parse(const char *rankfile)
{
    int token;
    int rc = PRTE_SUCCESS;
    int cnt;
    char *node_name = NULL;
    char **argv;
    char buff[RMAPS_RANK_FILE_MAX_SLOTS];
    char *value;
    int rank = -1;
    int i;
    prte_node_t *hnp_node;
    prte_rmaps_rank_file_map_t *rfmap = NULL;
    pmix_pointer_array_t *assigned_ranks_array;
    char tmp_rank_assignment[RMAPS_RANK_FILE_MAX_SLOTS];

    /* keep track of rank assignments */
    assigned_ranks_array = PMIX_NEW(pmix_pointer_array_t);

    /* get the hnp node's info */
    hnp_node = (prte_node_t *) (prte_node_pool->addr[0]);

    prte_rmaps_rank_file_done = false;
    prte_rmaps_rank_file_in = fopen(rankfile, "r");

    if (NULL == prte_rmaps_rank_file_in) {
        pmix_show_help("help-rmaps_rank_file.txt", "no-rankfile", true,
                       prte_tool_basename, rankfile, prte_tool_basename);
        rc = PRTE_ERR_NOT_FOUND;
        goto unlock;
    }

    while (!prte_rmaps_rank_file_done) {
        token = prte_rmaps_rank_file_lex();

        switch (token) {
        case PRTE_RANKFILE_ERROR:
            pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
            rc = PRTE_ERR_BAD_PARAM;
            PRTE_ERROR_LOG(rc);
            goto unlock;
        case PRTE_RANKFILE_QUOTED_STRING:
            pmix_show_help("help-rmaps_rank_file.txt", "not-supported-rankfile", true,
                           "QUOTED_STRING", rankfile);
            rc = PRTE_ERR_BAD_PARAM;
            PRTE_ERROR_LOG(rc);
            goto unlock;
        case PRTE_RANKFILE_NEWLINE:
            rank = -1;
            if (NULL != node_name) {
                free(node_name);
            }
            node_name = NULL;
            rfmap = NULL;
            break;
        case PRTE_RANKFILE_RANK:
            token = prte_rmaps_rank_file_lex();
            if (PRTE_RANKFILE_INT == token) {
                rank = prte_rmaps_rank_file_value.ival;
                rfmap = PMIX_NEW(prte_rmaps_rank_file_map_t);
                pmix_pointer_array_set_item(&rankmap, rank, rfmap);
                num_ranks++; // keep track of number of provided ranks
            } else {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                rc = PRTE_ERR_BAD_PARAM;
                PRTE_ERROR_LOG(rc);
                goto unlock;
            }
            break;
        case PRTE_RANKFILE_USERNAME:
            pmix_show_help("help-rmaps_rank_file.txt", "not-supported-rankfile", true, "USERNAME",
                           rankfile);
            rc = PRTE_ERR_BAD_PARAM;
            PRTE_ERROR_LOG(rc);
            goto unlock;
        case PRTE_RANKFILE_EQUAL:
            if (rank < 0) {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                rc = PRTE_ERR_BAD_PARAM;
                PRTE_ERROR_LOG(rc);
                goto unlock;
            }
            token = prte_rmaps_rank_file_lex();
            switch (token) {
            case PRTE_RANKFILE_HOSTNAME:
            case PRTE_RANKFILE_IPV4:
            case PRTE_RANKFILE_IPV6:
            case PRTE_RANKFILE_STRING:
            case PRTE_RANKFILE_INT:
            case PRTE_RANKFILE_RELATIVE:
                if (PRTE_RANKFILE_INT == token) {
                    sprintf(buff, "%d", prte_rmaps_rank_file_value.ival);
                    value = buff;
                } else {
                    value = prte_rmaps_rank_file_value.sval;
                }
                argv = PMIX_ARGV_SPLIT_COMPAT(value, '@');
                cnt = PMIX_ARGV_COUNT_COMPAT(argv);
                if (NULL != node_name) {
                    free(node_name);
                }
                if (1 == cnt) {
                    node_name = strdup(argv[0]);
                } else if (2 == cnt) {
                    node_name = strdup(argv[1]);
                } else {
                    pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = PRTE_ERR_BAD_PARAM;
                    PRTE_ERROR_LOG(rc);
                    PMIX_ARGV_FREE_COMPAT(argv);
                    node_name = NULL;
                    goto unlock;
                }
                PMIX_ARGV_FREE_COMPAT(argv);

                // Strip off the FQDN if present, ignore IP addresses
                if (!prte_keep_fqdn_hostnames && !pmix_net_isaddr(node_name)) {
                    char *ptr;
                    if (NULL != (ptr = strchr(node_name, '.'))) {
                        *ptr = '\0';
                    }
                }

                /* check the rank item */
                if (NULL == rfmap) {
                    pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                    rc = PRTE_ERR_BAD_PARAM;
                    PRTE_ERROR_LOG(rc);
                    goto unlock;
                }
                /* check if this is the local node */
                if (prte_check_host_is_local(node_name)) {
                    rfmap->node_name = strdup(hnp_node->name);
                } else {
                    rfmap->node_name = strdup(node_name);
                }
            }
            break;
        case PRTE_RANKFILE_SLOT:
            if (NULL == node_name || rank < 0
                || NULL == (value = prte_rmaps_rank_file_parse_string_or_int())) {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                rc = PRTE_ERR_BAD_PARAM;
                PRTE_ERROR_LOG(rc);
                goto unlock;
            }

            /* check for a duplicate rank assignment */
            if (NULL != pmix_pointer_array_get_item(assigned_ranks_array, rank)) {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-assign", true, rank,
                               pmix_pointer_array_get_item(assigned_ranks_array, rank), rankfile);
                rc = PRTE_ERR_BAD_PARAM;
                free(value);
                goto unlock;
            } else {
                /* prepare rank assignment string for the help message in case of a bad-assign */
                sprintf(tmp_rank_assignment, "%s slot=%s", node_name, value);
                pmix_pointer_array_set_item(assigned_ranks_array, 0, tmp_rank_assignment);
            }

            /* check the rank item */
            if (NULL == rfmap) {
                pmix_show_help("help-rmaps_rank_file.txt", "bad-syntax", true, rankfile);
                rc = PRTE_ERR_BAD_PARAM;
                PRTE_ERROR_LOG(rc);
                free(value);
                goto unlock;
            }
            for (i = 0; i < RMAPS_RANK_FILE_MAX_SLOTS && '\0' != value[i]; i++) {
                rfmap->slot_list[i] = value[i];
            }
            free(value);
            break;
        }
    }
    fclose(prte_rmaps_rank_file_in);
    prte_rmaps_rank_file_lex_destroy();

unlock:
    if (NULL != node_name) {
        free(node_name);
    }
    PMIX_RELEASE(assigned_ranks_array);
    return rc;
}

static char *prte_rmaps_rank_file_parse_string_or_int(void)
{
    int rc;
    char tmp_str[RMAPS_RANK_FILE_MAX_SLOTS];

    if (PRTE_RANKFILE_EQUAL != prte_rmaps_rank_file_lex()) {
        return NULL;
    }

    rc = prte_rmaps_rank_file_lex();
    switch (rc) {
    case PRTE_RANKFILE_STRING:
        return strdup(prte_rmaps_rank_file_value.sval);
    case PRTE_RANKFILE_INT:
        sprintf(tmp_str, "%d", prte_rmaps_rank_file_value.ival);
        return strdup(tmp_str);
    default:
        return NULL;
    }
}

static int prte_rmaps_rf_process_lsf_affinity_hostfile(prte_job_t *jdata,
                                                       prte_rmaps_options_t *options,
                                                       char *affinity_file)
{
    char *aff_rankfile = NULL;
    struct stat buf;
    int rc;

    /* check to see if the file is empty - if it is,
     * then affinity wasn't actually set for this job */
    if (0 != stat(affinity_file, &buf)) {
        pmix_show_help("help-rmaps_rank_file.txt", "lsf-affinity-file-not-found", true, affinity_file);
        return PRTE_ERR_SILENT;
    }
    if (0 == buf.st_size) {
        /* no affinity, so just return */
        return PRTE_SUCCESS;
    }

    /* the affinity file sequentially lists rank locations, with
     * cpusets given as physical cpu-ids. Setup the job object
     * so it knows to process this accordingly */
    if (NULL == jdata->map) {
        jdata->map = PMIX_NEW(prte_job_map_t);
    }

    /* We need to use the rank file mapper since each line in the affinity
     * file is the specification for a single rank.
     */
    PRTE_SET_MAPPING_POLICY(jdata->map->mapping, PRTE_MAPPING_BYUSER);
    jdata->map->req_mapper = strdup("rank_file");

    /*
     * Ranking is also determined by the rank file that we generate
     */
    options->userranked = true;
    PRTE_SET_MAPPING_POLICY(jdata->map->ranking, PRTE_RANKING_BYUSER);

    /* Setup a temporary hostfile with logical cpu-ids converted from the the physical
     * cpu-ids provided by LSF. Further convert the format to match the rankfile
     *  - https://github.com/openpmix/prrte/pull/580 removed support for Physical CPU IDs
     *
     * LSF Provides a "LSF_BINDIR/openmpi_rankfile.sh" script to convert the
     * format of the LSB_AFFINITY_HOSTFILE to a rankfile. However, it does not
     * adjust the CPU IDs. So we need a custom function to do this.
     */
    rc = prte_rmaps_rf_lsf_convert_affinity_to_rankfile(affinity_file, &aff_rankfile);
    if (PRTE_SUCCESS != rc ) {
        pmix_show_help("help-rmaps_rank_file.txt", "lsf-affinity-file-failed-convert", true, affinity_file);
        return PRTE_ERR_SILENT;
    }
    pmix_output_verbose(10, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:rf: (lsf) Converted LSB_AFFINITY_HOSTFILE to rankfile %s",
                        aff_rankfile);
    prte_set_attribute(&jdata->attributes, PRTE_JOB_FILE, PRTE_ATTR_GLOBAL, aff_rankfile, PMIX_STRING);

    /* LSF provides its info as hwthreads, so set the hwthread-as-cpus flag */
    prte_set_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, PRTE_ATTR_GLOBAL, NULL, PMIX_BOOL);
    options->use_hwthreads = true;
    /* don't override something provided by the user, but default to bind-to hwthread */
    if (!PRTE_BINDING_POLICY_IS_SET(prte_hwloc_default_binding_policy)) {
        PRTE_SET_BINDING_POLICY(prte_hwloc_default_binding_policy, PRTE_BIND_TO_HWTHREAD);
    }

    return PRTE_SUCCESS;
}

static int prte_rmaps_rf_lsf_convert_affinity_to_rankfile(char *affinity_file, char **aff_rankfile)
{
    FILE *fp;
    int fp_rank, cur_rank = 0;
    char *hstname = NULL;
    char *sep, *eptr, *membind_opt;
    char *tmp_str = NULL, *tmp_rid = NULL;
    size_t len;
    char **cpus;
    int i;
    hwloc_obj_t obj;
    prte_topology_t *my_topo = NULL;

    if( NULL != *aff_rankfile) {
        free(*aff_rankfile);
    }

    // session dir + / (1) + lsf_rf. (7) + XXXXXX (6) + \0 (1)
    len = strlen(prte_process_info.top_session_dir) + 1 + 7 + 6 + 1;
    (*aff_rankfile) = (char*) malloc(sizeof(char) * len);
    sprintf(*aff_rankfile, "%s/lsf_rf.XXXXXX", prte_process_info.top_session_dir);

    /* open the file */
    fp = fopen(affinity_file, "r");
    if (NULL == fp) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }
    fp_rank = mkstemp((*aff_rankfile));
    if (-1 == fp_rank) {
        fclose(fp);
        free((*aff_rankfile));
        (*aff_rankfile) = NULL;
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    while (NULL != (hstname = pmix_getline(fp))) {
        if (0 == strlen(hstname)) {
            free(hstname);
            /* blank line - ignore */
            continue;
        }
        if ('#' == hstname[0]) {
            free(hstname);
            /* Comment line - ignore */
            continue;
        }
        if (NULL != (sep = strchr(hstname, ' '))) {
            *sep = '\0';
            sep++;
            /* remove any trailing space */
            eptr = sep + strlen(sep) - 1;
            while (eptr > sep && isspace(*eptr)) {
                eptr--;
            }
            *(eptr + 1) = 0;
            /*
             * If the submitted LSF job has memory binding related resource requirement, after
             * the cpu id list there are memory binding options.
             *
             * The following is the format of LSB_AFFINITY_HOSTFILE file:
             *
             * Host1 0,1,2,3 0 2
             * Host1 4,5,6,7 1 2
             *
             * Each line includes: host_name, cpu_id_list, NUMA_node_id_list, and memory_policy.
             * In this fix we will drop the last two sections (NUMA_node_id_list and memory_policy)
             * of each line and keep them in 'membind_opt' for future use.
             */
            if (NULL != (membind_opt = strchr(sep, ' '))) {
                *membind_opt = '\0';
                membind_opt++;
            }
        }

        // Convert the Physical CPU set from LSF to a Hwloc logical CPU set
        pmix_output_verbose(20, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: (lsf) Convert Physical CPUSET from <%s>", sep);
        my_topo = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, 0);
        cpus = PMIX_ARGV_SPLIT_COMPAT(sep, ',');
        for(i = 0; NULL != cpus[i]; ++i) {
            // assume HNP has the same topology as other nodes
            obj = hwloc_get_pu_obj_by_os_index(my_topo->topo, strtol(cpus[i], NULL, 10)) ;

            free(cpus[i]);
            // 10 max number of digits in an int
            cpus[i] = (char*)malloc(sizeof(char) * 10);
            sprintf(cpus[i], "%d", obj->logical_index);
        }
        sep = PMIX_ARGV_JOIN_COMPAT(cpus, ',');
        PMIX_ARGV_FREE_COMPAT(cpus);
        pmix_output_verbose(20, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:rf: (lsf) Convert Physical CPUSET to   <%s>", sep);

        // Format: rank 1=host1 slot=0,1,2,3,4
        // "rank " (5) + id (max 10) + = (1) + host (?) + " slot=" (6) + ids (?) + '\0' (1)
        len = 5 + 10 + 1 + strlen(hstname) + 6 + strlen(sep) + 1;
        tmp_str = (char *)malloc(sizeof(char) * len);
        sprintf(tmp_str, "rank %d=%s slot=%s\n", cur_rank, hstname, sep);
        write(fp_rank, tmp_str, strlen(tmp_str));
        free(tmp_str);
        ++cur_rank;
    }
    fclose(fp);
    close(fp_rank);

    return PRTE_SUCCESS;
}
