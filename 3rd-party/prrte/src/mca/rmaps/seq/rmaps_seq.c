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
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
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
#include <ctype.h>
#include <string.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_if.h"
#include "src/util/pmix_net.h"
#include "src/util/pmix_string_copy.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/runtime/prte_globals.h"
#include "src/util/dash_host/dash_host.h"
#include "src/util/hostfile/hostfile.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"

#include "rmaps_seq.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/base/rmaps_private.h"

static int prte_rmaps_seq_map(prte_job_t *jdata,
                              prte_rmaps_options_t *options);

/* define the module */
prte_rmaps_base_module_t prte_rmaps_seq_module = {
    .map_job = prte_rmaps_seq_map
};

/* local object for tracking rank locations */
typedef struct {
    pmix_list_item_t super;
    char *hostname;
    char *cpuset;
} seq_node_t;
static void sn_con(seq_node_t *p)
{
    p->hostname = NULL;
    p->cpuset = NULL;
}
static void sn_des(seq_node_t *p)
{
    if (NULL != p->hostname) {
        free(p->hostname);
        p->hostname = NULL;
    }
    if (NULL != p->cpuset) {
        free(p->cpuset);
        p->cpuset = NULL;
    }
}
PMIX_CLASS_INSTANCE(seq_node_t, pmix_list_item_t, sn_con, sn_des);

static int process_file(char *path, pmix_list_t *list);

static bool quickmatch(prte_node_t *nd, char *name)
{
    int n;

    if (0 == strcmp(nd->name, name)) {
        return true;
    }
    if (0 == strcmp(nd->name, prte_process_info.nodename) &&
        (0 == strcmp(name, "localhost") ||
         0 == strcmp(name, "127.0.0.1"))) {
        return true;
    }
    if (NULL != nd->aliases) {
        for (n=0; NULL != nd->aliases[n]; n++) {
            if (0 == strcmp(nd->aliases[n], name)) {
                return true;
            }
        }
    }
    return false;
}

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
 * Sequentially map the ranks according to the placement in the
 * specified hostfile
 */
static int prte_rmaps_seq_map(prte_job_t *jdata,
                              prte_rmaps_options_t *options)
{
    prte_job_map_t *map;
    prte_app_context_t *app;
    int i, n;
    int32_t j;
    pmix_list_item_t *item;
    prte_node_t *node, *nd;
    seq_node_t *sq, *save = NULL, *seq;
    pmix_rank_t vpid, apprank;
    int32_t num_nodes;
    int rc;
    pmix_list_t default_seq_list;
    pmix_list_t node_list, *seq_list, sq_list;
    prte_proc_t *proc;
    pmix_mca_base_component_t *c = &prte_mca_rmaps_seq_component;
    char *hosts = NULL;
    bool match;

    PMIX_OUTPUT_VERBOSE((1, prte_rmaps_base_framework.framework_output,
                         "%s rmaps:seq called on job %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_JOBID_PRINT(jdata->nspace)));

    /* this mapper can only handle initial launch
     * when seq mapping is desired - allow
     * restarting of failed apps
     */
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_RESTART)) {
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:seq: job %s is being restarted - seq cannot map",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }
    if (NULL != jdata->map->req_mapper) {
        if (0 != strcasecmp(jdata->map->req_mapper, c->pmix_mca_component_name)) {
            /* a mapper has been specified, and it isn't me */
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: job %s not using sequential mapper",
                                PRTE_JOBID_PRINT(jdata->nspace));
            return PRTE_ERR_TAKE_NEXT_OPTION;
        }
    }
    if (PRTE_MAPPING_SEQ != PRTE_GET_MAPPING_POLICY(jdata->map->mapping)) {
        /* I don't know how to do these - defer */
        pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                            "mca:rmaps:seq: job %s not using seq mapper",
                            PRTE_JOBID_PRINT(jdata->nspace));
        return PRTE_ERR_TAKE_NEXT_OPTION;
    }

    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                        "mca:rmaps:seq: mapping job %s",
                        PRTE_JOBID_PRINT(jdata->nspace));

    /* flag that I did the mapping */
    if (NULL != jdata->map->last_mapper) {
        free(jdata->map->last_mapper);
    }
    jdata->map->last_mapper = strdup(c->pmix_mca_component_name);

    /* convenience def */
    map = jdata->map;

    /* if there is a default hostfile, go and get its ordered list of nodes */
    PMIX_CONSTRUCT(&default_seq_list, pmix_list_t);
    if (NULL != prte_default_hostfile) {
        rc = process_file(prte_default_hostfile, &default_seq_list);
        if (PRTE_SUCCESS != rc) {
            PMIX_LIST_DESTRUCT(&default_seq_list);
            return rc;
        }
    }

    /* start at the beginning... */
    vpid = 0;
    jdata->num_procs = 0;
    if (0 < pmix_list_get_size(&default_seq_list)) {
        save = (seq_node_t *) pmix_list_get_first(&default_seq_list);
    }

    /* cycle through the app_contexts, mapping them sequentially */
    for (i = 0; i < jdata->apps->size; i++) {
        app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i);
        if (NULL == app) {
            continue;
        }
        apprank = 0;

        /* specified seq file trumps all */
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_FILE, (void **) &hosts, PMIX_STRING)) {
            if (NULL == hosts) {
                rc = PRTE_ERR_NOT_FOUND;
                goto error;
            }
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: using hostfile %s nodes on app %s", hosts,
                                app->app);
            PMIX_CONSTRUCT(&sq_list, pmix_list_t);
            rc = process_file(hosts, &sq_list);
            if (PRTE_SUCCESS != rc) {
                PMIX_LIST_DESTRUCT(&sq_list);
                goto error;
            }
            seq_list = &sq_list;
        } else if (prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &hosts,
                                      PMIX_STRING)) {
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: using dash-host nodes on app %s", app->app);
            PMIX_CONSTRUCT(&node_list, pmix_list_t);
            /* dash host entries cannot specify cpusets, so used the std function to retrieve the
             * list */
            if (PRTE_SUCCESS != (rc = prte_util_get_ordered_dash_host_list(&node_list, hosts))) {
                PRTE_ERROR_LOG(rc);
                goto error;
            }
            /* transfer the list to a seq_node_t list */
            PMIX_CONSTRUCT(&sq_list, pmix_list_t);
            while (NULL != (nd = (prte_node_t *) pmix_list_remove_first(&node_list))) {
                sq = PMIX_NEW(seq_node_t);
                sq->hostname = strdup(nd->name);
                pmix_list_append(&sq_list, &sq->super);
                PMIX_RELEASE(nd);
            }
            PMIX_DESTRUCT(&node_list);
            seq_list = &sq_list;
        } else if (prte_get_attribute(&app->attributes, PRTE_APP_HOSTFILE, (void **) &hosts,
                                      PMIX_STRING)) {
            if (NULL == hosts) {
                rc = PRTE_ERR_NOT_FOUND;
                goto error;
            }
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: using hostfile %s nodes on app %s", hosts,
                                app->app);
            PMIX_CONSTRUCT(&sq_list, pmix_list_t);
            rc = process_file(hosts, &sq_list);
            if (PRTE_SUCCESS != rc) {
                PMIX_LIST_DESTRUCT(&sq_list);
                goto error;
            }
            seq_list = &sq_list;
        } else if (0 < pmix_list_get_size(&default_seq_list)) {
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: using default hostfile nodes on app %s", app->app);
            seq_list = &default_seq_list;
            hosts = strdup(prte_default_hostfile);
        } else {
            /* can't do anything - no nodes available! */
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:no-available-resources",
                           true);
            rc = PRTE_ERR_SILENT;
            goto error;
        }

        /* check for nolocal and remove the head node, if required */
        if (PRTE_GET_MAPPING_DIRECTIVE(map->mapping) & PRTE_MAPPING_NO_USE_LOCAL) {
            for (item = pmix_list_get_first(seq_list); item != pmix_list_get_end(seq_list);
                 item = pmix_list_get_next(item)) {
                seq = (seq_node_t *) item;
                /* need to check ifislocal because the name in the
                 * hostfile may not have been FQDN, while name returned
                 * by gethostname may have been (or vice versa)
                 */
                if (prte_check_host_is_local(seq->hostname)) {
                    pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                        "mca:rmaps:seq: removing head node %s", seq->hostname);
                    pmix_list_remove_item(seq_list, item);
                    PMIX_RELEASE(item); /* "un-retain" it */
                }
            }
        }

        if (NULL == seq_list || 0 == (num_nodes = (int32_t) pmix_list_get_size(seq_list))) {
            pmix_show_help("help-prte-rmaps-base.txt", "prte-rmaps-base:no-available-resources",
                           true);
            rc = PRTE_ERR_SILENT;
            goto error;
        }

        /* set #procs to the number of entries */
        if (0 == app->num_procs) {
            app->num_procs = num_nodes;
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: setting num procs to %s for app %s",
                                PRTE_VPID_PRINT(app->num_procs), app->app);
        } else if (num_nodes < app->num_procs) {
            pmix_show_help("help-prte-rmaps-seq.txt", "seq:not-enough-resources", true,
                           app->num_procs, num_nodes);
            rc = PRTE_ERR_SILENT;
            goto error;
        }

        if (seq_list == &default_seq_list) {
            sq = save;
        } else {
            sq = (seq_node_t *) pmix_list_get_first(seq_list);
        }
        for (n = 0; n < app->num_procs; n++) {
            /* find this node on the global array - this is necessary so
             * that our mapping gets saved on that array as the objects
             * returned by the hostfile function are -not- on the array
             */
            match = false;
            for (j = 0; j < prte_node_pool->size; j++) {
                node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, j);
                if (NULL == node) {
                    continue;
                }
                if (quickmatch(node, sq->hostname)) {
                    match = true;
                    break;
                }
            }
            if (!match) {
                /* wasn't found - that is an error */
                pmix_show_help("help-prte-rmaps-seq.txt", "prte-rmaps-seq:resource-not-found", true,
                               sq->hostname);
                rc = PRTE_ERR_SILENT;
                goto error;
            }
            /* check availability */
            prte_rmaps_base_get_cpuset(jdata, node, options);
            if (!prte_rmaps_base_check_avail(jdata, app, node, seq_list, NULL, options)) {
                continue;
            }

            /* map the proc */
            proc = prte_rmaps_base_setup_proc(jdata, i, node, NULL, options);
            if (NULL == proc) {
                pmix_show_help("help-prte-rmaps-seq.txt", "proc-failed-to-map", true,
                               sq->hostname, app->app);
                rc = PRTE_ERR_SILENT;
                goto error;
            }
            proc->name.rank = vpid;
            vpid++;
            proc->app_rank = apprank;
            apprank++;
            PMIX_RETAIN(proc);
            rc = pmix_pointer_array_set_item(jdata->procs, proc->name.rank, proc);
            if (PMIX_SUCCESS != rc) {
                PMIX_RELEASE(proc);
                goto error;
            }
            rc = prte_rmaps_base_check_oversubscribed(jdata, app, node, options);
            if (PRTE_SUCCESS != rc &&
                PRTE_ERR_TAKE_NEXT_OPTION != rc) {
                PMIX_RELEASE(proc);
                goto error;
            }
            pmix_output_verbose(5, prte_rmaps_base_framework.framework_output,
                                "mca:rmaps:seq: assigned proc %s to node %s for app %s",
                                PRTE_VPID_PRINT(proc->name.rank), sq->hostname, app->app);

            /* move to next node */
            sq = (seq_node_t *) pmix_list_get_next(&sq->super);
            PMIX_RELEASE(proc);
        }

        /** track the total number of processes we mapped */
        jdata->num_procs += app->num_procs;

        /* cleanup the node list if it came from this app_context */
        if (seq_list != &default_seq_list) {
            PMIX_LIST_DESTRUCT(seq_list);
        } else {
            save = sq;
        }
        if (NULL != hosts) {
            free(hosts);
        }
    }
    /* compute local/app ranks */
    rc = prte_rmaps_base_compute_vpids(jdata, options);
    return rc;

error:
    PMIX_LIST_DESTRUCT(&default_seq_list);
    if (NULL != hosts) {
        free(hosts);
    }
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

static int process_file(char *path, pmix_list_t *list)
{
    char *hstname = NULL;
    FILE *fp;
    seq_node_t *sq;
    char *sep, *eptr, *membind_opt;

    /* open the file */
    fp = fopen(path, "r");
    if (NULL == fp) {
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
        sq = PMIX_NEW(seq_node_t);
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
            sq->cpuset = strdup(sep);
        }

        sq->hostname = hstname;
        pmix_list_append(list, &sq->super);
    }
    fclose(fp);
    return PRTE_SUCCESS;
}
