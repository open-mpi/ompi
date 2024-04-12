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
 * Copyright (c) 2010      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "types.h"

#include <sys/types.h>

#include "src/hwloc/hwloc-internal.h"
#include "src/util/pmix_argv.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/runtime/prte_globals.h"
#include "src/util/error_strings.h"
#include "src/util/name_fns.h"



/* This function is a modified version of the one found in src/mca/ras/base/ras_base_allocate.c*/
static void display_cpus(prte_topology_t *t,
                         prte_job_t *jdata,
                         char *node, char**output)
{
    char tmp[2048];
    unsigned pkg, npkgs;
    bool bits_as_cores = false, use_hwthread_cpus = prte_hwloc_default_use_hwthread_cpus;
    unsigned npus, ncores;
    hwloc_obj_t obj;
    hwloc_cpuset_t avail = NULL;
    hwloc_cpuset_t allowed;
    hwloc_cpuset_t coreset = NULL;

    char *tmp1, *tmp2;

    npus = hwloc_get_nbobjs_by_type(t->topo, HWLOC_OBJ_PU);
    ncores = hwloc_get_nbobjs_by_type(t->topo, HWLOC_OBJ_CORE);
    if (npus == ncores && !use_hwthread_cpus) {
        /* the bits in this bitmap represent cores */
        bits_as_cores = true;
    }
    use_hwthread_cpus = prte_get_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, NULL, PMIX_BOOL);
    if (!use_hwthread_cpus && !bits_as_cores) {
        coreset = hwloc_bitmap_alloc();
    }
    avail = hwloc_bitmap_alloc();
    pmix_asprintf(&tmp1, "        <processors>\n");
    npkgs = hwloc_get_nbobjs_by_type(t->topo, HWLOC_OBJ_PACKAGE);
    allowed = (hwloc_cpuset_t)hwloc_topology_get_allowed_cpuset(t->topo);
    for (pkg = 0; pkg < npkgs; pkg++) {
        obj = hwloc_get_obj_by_type(t->topo, HWLOC_OBJ_PACKAGE, pkg);
        hwloc_bitmap_and(avail, obj->cpuset, allowed);
        if (hwloc_bitmap_iszero(avail)) {
            pmix_asprintf(&tmp2, "%s            <package id=\"%d\" cpus=\"%s\"/>\n", tmp1, pkg, "NONE");
            continue;
        }
        if (bits_as_cores) {
            /* can just use the hwloc fn directly */
            hwloc_bitmap_list_snprintf(tmp, 2048, avail);
            pmix_asprintf(&tmp2, "%s            <package id=\"%d\" cpus=\"%s\"/>\n", tmp1, pkg, tmp);
        } else if (use_hwthread_cpus) {
            /* can just use the hwloc fn directly */
            hwloc_bitmap_list_snprintf(tmp, 2048, avail);
            pmix_asprintf(&tmp2, "%s            <package id=\"%d\" cpus=\"%s\"/>\n", tmp1, pkg, tmp);
        } else {
            prte_hwloc_build_map(t->topo, avail, use_hwthread_cpus | bits_as_cores, coreset);
            /* now print out the string */
            hwloc_bitmap_list_snprintf(tmp, 2048, coreset);
            pmix_asprintf(&tmp2, "%s            <package id=\"%d\" cpus=\"%s\"/>\n", tmp1, pkg, tmp);
        }
        free(tmp1);
        tmp1 = tmp2;
        tmp2 = NULL;
    }
    hwloc_bitmap_free(avail);
    if (NULL != coreset) {
        hwloc_bitmap_free(coreset);
    }
    
    pmix_asprintf(output, "%s        </processors>\n", tmp1);
    free(tmp1);
    return;
}


/*
 * JOB
 */
void prte_job_print(char **output, prte_job_t *src)
{
    char *tmp, *tmp2, *tmp3;
    int32_t i;
    prte_app_context_t *app;
    prte_proc_t *proc;

    /* set default result */
    *output = NULL;

    tmp2 = PMIX_ARGV_JOIN_COMPAT(src->personality, ',');
    pmix_asprintf(&tmp,
                  "\nData for job: %s\tPersonality: %s\tRecovery: %s\n\tNum apps: %ld\tStdin "
                  "target: %s\tState: %s\tAbort: %s",
                  PRTE_JOBID_PRINT(src->nspace), tmp2,
                  (prte_get_attribute(&src->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL)) ? "ENABLED" : "DISABLED",
                  (long) src->num_apps, PRTE_VPID_PRINT(src->stdin_target),
                  prte_job_state_to_str(src->state),
                  (PRTE_FLAG_TEST(src, PRTE_JOB_FLAG_ABORTED)) ? "True" : "False");
    free(tmp2);

    for (i = 0; i < src->apps->size; i++) {
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(src->apps, i))) {
            continue;
        }
        prte_app_print(&tmp2, src, app);
        pmix_asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    if (NULL != src->map) {
        prte_map_print(&tmp2, src);
        pmix_asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    } else {
        pmix_asprintf(&tmp2, "%s\nNo Map", tmp);
        free(tmp);
        tmp = tmp2;
    }

    pmix_asprintf(&tmp2, "%s\nNum procs: %ld\tOffset: %ld", tmp, (long) src->num_procs,
                  (long) src->offset);
    free(tmp);
    tmp = tmp2;

    for (i = 0; i < src->procs->size; i++) {
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(src->procs, i))) {
            continue;
        }
        prte_proc_print(&tmp2, src, proc);
        pmix_asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    pmix_asprintf(&tmp2, "%s\n\tNum launched: %ld\tNum reported: %ld\tNum terminated: %ld", tmp,
                  (long) src->num_launched, (long) src->num_reported, (long) src->num_terminated);
    free(tmp);
    tmp = tmp2;

    /* set the return */
    *output = tmp;
    return;
}

/*
 * NODE
 */
void prte_node_print(char **output, prte_job_t *jdata, prte_node_t *src)
{
    char *tmp, *tmp1, *tmp2, *tmp3;
    int32_t i,j;
    prte_proc_t *proc;
    prte_topology_t *t;

    /* set default result */
    *output = NULL;

    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_PARSEABLE_OUTPUT, NULL, PMIX_BOOL)) {
        pmix_asprintf(&tmp, "    <host name=\"%s\" slots=\"%d\" max_slots=\"%d\">\n",
                      (NULL == src->name) ? "UNKNOWN" : src->name, (int) src->slots,
                      (int) src->slots_max);

        pmix_asprintf(&tmp2,""); 
        for (j=0; j < prte_node_topologies->size; j++) {
            t = (prte_topology_t*)pmix_pointer_array_get_item(prte_node_topologies, j);
            if (NULL != t) {
                display_cpus(t, jdata, "N/A", &tmp1);
                pmix_asprintf(&tmp3, "%s%s",tmp2, tmp1);
                free(tmp1);
                tmp1 = NULL;
                free(tmp2);
                tmp2 = NULL;
                tmp2 = tmp3;
            }
        }

        pmix_asprintf(&tmp3, "%s%s", tmp,tmp2);
        free(tmp2);
        tmp2 = NULL;
        free(tmp1);
        tmp1 = NULL;
        free(tmp);
        tmp = tmp3; 

        /* loop through procs and print their rank */
        for (j = 0; j < src->procs->size; j++) {
            if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(src->procs, j))) {
                continue;
            }
            if (!PMIX_CHECK_NSPACE(proc->name.nspace, jdata->nspace)) {
                continue;
            }
            prte_proc_print(&tmp2, jdata, proc);
            pmix_asprintf(&tmp3, "%s%s", tmp, tmp2);
            free(tmp2);
            tmp2 = NULL;
            free(tmp);
            tmp = tmp3;
        }
        pmix_asprintf(&tmp3, "%s    </host>\n", tmp);
        free(tmp);
        *output = tmp3;
        return;
    }

    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_DEVEL_MAP, NULL, PMIX_BOOL)) {
        /* just provide a simple output for users */
        pmix_asprintf(&tmp, "\nData for node: %s\tNum slots: %ld\tMax slots: %ld\tNum procs: %ld",
                      (NULL == src->name) ? "UNKNOWN" : src->name, (long) src->slots,
                      (long) src->slots_max, (long) src->num_procs);
        if (0 == src->num_procs) {
            *output = tmp;
            return;
        }
        goto PRINT_PROCS;
    }

    tmp3 = prte_ras_base_flag_string(src);
    pmix_asprintf(&tmp, "\nData for node: %s\tState: %0x\t%s",
                  (NULL == src->name) ? "UNKNOWN" : src->name, src->state, tmp3);
    free(tmp3);
    /* does this node have any aliases? */
    tmp3 = NULL;
    if (NULL != src->aliases) {
        for (i = 0; NULL != src->aliases[i]; i++) {
            pmix_asprintf(&tmp2, "%s\n                resolved from %s", tmp, src->aliases[i]);
            free(tmp);
            tmp = tmp2;
        }
    }
    if (NULL != tmp3) {
        free(tmp3);
    }

    pmix_asprintf(&tmp2, "%s\n        Daemon: %s\tDaemon launched: %s", tmp,
                  (NULL == src->daemon) ? "Not defined" : PRTE_NAME_PRINT(&(src->daemon->name)),
                  PRTE_FLAG_TEST(src, PRTE_NODE_FLAG_DAEMON_LAUNCHED) ? "True" : "False");
    free(tmp);
    tmp = tmp2;

    pmix_asprintf(&tmp2, "%s\n            Num slots: %ld\tSlots in use: %ld\tOversubscribed: %s",
                  tmp, (long) src->slots, (long) src->slots_inuse,
                  PRTE_FLAG_TEST(src, PRTE_NODE_FLAG_OVERSUBSCRIBED) ? "TRUE" : "FALSE");
    free(tmp);
    tmp = tmp2;

    pmix_asprintf(&tmp2, "%s\n            Num slots allocated: %ld\tMax slots: %ld\tNum procs: %ld", tmp,
                  (long) src->slots, (long) src->slots_max, (long) src->num_procs);
    free(tmp);
    tmp = tmp2;

    tmp3 = NULL;
    if (prte_get_attribute(&src->attributes, PRTE_NODE_USERNAME, (void **) &tmp3, PMIX_STRING)) {
        pmix_asprintf(&tmp2, "%s\n            Username on node: %s", tmp, tmp3);
        free(tmp3);
        free(tmp);
        tmp = tmp2;
    }

PRINT_PROCS:
    /* we want to print these procs in their job-rank'd order, but they
     * will be in the node array based on the order in which they were
     * mapped - which doesn't match job-rank'd order in many cases */
    for (i = 0; i < jdata->procs->size; i++) {
        if (NULL == (proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        if (proc->node != src) {
            continue;
        }
        prte_proc_print(&tmp2, jdata, proc);
        pmix_asprintf(&tmp3, "%s%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    /* set the return */
    *output = tmp;

    return;
}

/*
 * PROC
 */
void prte_proc_print(char **output, prte_job_t *jdata, prte_proc_t *src)
{
    char *tmp, *tmp3, *tmp4, *pfx2 = "        ";
    char *tmp2;
    hwloc_cpuset_t mycpus;
    char *str;
    bool use_hwthread_cpus;
    int pkgnum;
    int npus;
    char *cores = NULL;
    char xmlsp = ' ';

    /* set default result */
    *output = NULL;

    /* check for type of cpu being used */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, NULL, PMIX_BOOL)) {
        use_hwthread_cpus = true;
    } else {
        use_hwthread_cpus = false;
    }

    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_PARSEABLE_OUTPUT, NULL, PMIX_BOOL)) {
        if (NULL != src->cpuset && NULL != src->node->topology &&
            NULL != src->node->topology->topo) {
            mycpus = hwloc_bitmap_alloc();
            hwloc_bitmap_list_sscanf(mycpus, src->cpuset);

            npus = hwloc_get_nbobjs_by_type(src->node->topology->topo, HWLOC_OBJ_PU);
            /* assuming each "core" xml element will take 20 characters. There could be at most npus such elements */
            int sz = sizeof(char) * npus * 20;
            cores = (char*)malloc(sz);
            if (NULL == cores) {
                pmix_asprintf(&tmp, "\n%*c<MemoryError/>\n", 8, xmlsp);
                *output = tmp;
                return;
            }
            prte_hwloc_get_binding_info(mycpus, use_hwthread_cpus,
                                        src->node->topology->topo, &pkgnum, cores, sz);

            hwloc_bitmap_free(mycpus);

            pmix_asprintf(&tmp, "\n%*c<rank id=\"%s\" appid=\"%ld\">\n%*c<binding>\n"
                          "%*c<package id=\"%d\">\n%s\n%*c</package>\n%*c</binding>\n%*c</rank>\n",
                          8, xmlsp, PRTE_VPID_PRINT(src->name.rank), (long) src->app_idx, 12, xmlsp,
                          16, xmlsp, pkgnum, cores, 16, xmlsp, 12, xmlsp, 8, xmlsp);
            
            free (cores);
        } else {
            pmix_asprintf(&tmp, "\n%*c<rank id=\"%s\">\n%*c<binding></binding>\n%*c</rank>\n", 
                          8, xmlsp, PRTE_VPID_PRINT(src->name.rank), 12, xmlsp, 8, xmlsp);        
        }

        /* set the return */
        *output = tmp;
        return;
    }

    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_DEVEL_MAP, NULL, PMIX_BOOL)) {
        if (NULL != src->cpuset && NULL != src->node->topology
            && NULL != src->node->topology->topo) {
            mycpus = hwloc_bitmap_alloc();
            hwloc_bitmap_list_sscanf(mycpus, src->cpuset);
            str = prte_hwloc_base_cset2str(mycpus, use_hwthread_cpus,
                                           src->node->topology->topo);
            if (NULL == str) {
                str = strdup("UNBOUND");
            }
            hwloc_bitmap_free(mycpus);
            pmix_asprintf(&tmp, "\n%sProcess jobid: %s App: %ld Process rank: %s Bound: %s", pfx2,
                          PRTE_JOBID_PRINT(src->name.nspace), (long) src->app_idx,
                          PRTE_VPID_PRINT(src->name.rank), str);
            free(str);
        } else {
            /* just print a very simple output for users */
            pmix_asprintf(&tmp, "\n%sProcess jobid: %s App: %ld Process rank: %s Bound: N/A", pfx2,
                          PRTE_JOBID_PRINT(src->name.nspace), (long) src->app_idx,
                          PRTE_VPID_PRINT(src->name.rank));
        }

        /* set the return */
        *output = tmp;
        return;
    }

    pmix_asprintf(&tmp, "\n%sData for proc: %s", pfx2, PRTE_NAME_PRINT(&src->name));

    pmix_asprintf(&tmp3, "%s\n%s        Pid: %ld\tLocal rank: %lu\tNode rank: %lu\tApp rank: %d",
                  tmp, pfx2, (long) src->pid, (unsigned long) src->local_rank,
                  (unsigned long) src->node_rank, src->app_rank);
    free(tmp);
    tmp = tmp3;

    if (NULL != src->cpuset) {
        mycpus = hwloc_bitmap_alloc();
        hwloc_bitmap_list_sscanf(mycpus, src->cpuset);
        tmp2 = prte_hwloc_base_cset2str(mycpus, use_hwthread_cpus, src->node->topology->topo);
        hwloc_bitmap_free(mycpus);
    } else {
        tmp2 = strdup("UNBOUND");
    }
    pmix_asprintf(&tmp4,
                  "%s\n%s        State: %s\tApp_context: %ld\n%s\tBinding: %s",
                  tmp, pfx2, prte_proc_state_to_str(src->state), (long) src->app_idx, pfx2, tmp2);
    free(tmp);
    free(tmp2);

    /* set the return */
    *output = tmp4;

    return;
}

/*
 * APP CONTEXT
 */
void prte_app_print(char **output, prte_job_t *jdata, prte_app_context_t *src)
{
    char *tmp, *tmp2, *tmp3;
    int i, count;
    PRTE_HIDE_UNUSED_PARAMS(jdata);

    /* set default result */
    *output = NULL;

    pmix_asprintf(&tmp,
                  "\nData for app_context: index %lu\tapp: %s\n\tNum procs: %lu\tFirstRank: %s",
                  (unsigned long) src->idx, (NULL == src->app) ? "NULL" : src->app,
                  (unsigned long) src->num_procs, PRTE_VPID_PRINT(src->first_rank));

    count = PMIX_ARGV_COUNT_COMPAT(src->argv);
    for (i = 0; i < count; i++) {
        pmix_asprintf(&tmp2, "%s\n\tArgv[%d]: %s", tmp, i, src->argv[i]);
        free(tmp);
        tmp = tmp2;
    }

    count = PMIX_ARGV_COUNT_COMPAT(src->env);
    for (i = 0; i < count; i++) {
        pmix_asprintf(&tmp2, "%s\n\tEnv[%lu]: %s", tmp, (unsigned long) i, src->env[i]);
        free(tmp);
        tmp = tmp2;
    }

    tmp3 = NULL;
    prte_get_attribute(&src->attributes, PRTE_APP_PREFIX_DIR, (void **) &tmp3, PMIX_STRING);
    pmix_asprintf(&tmp2, "%s\n\tWorking dir: %s\n\tPrefix: %s\n\tUsed on node: %s", tmp,
                  (NULL == src->cwd) ? "NULL" : src->cwd, (NULL == tmp3) ? "NULL" : tmp3,
                  PRTE_FLAG_TEST(src, PRTE_APP_FLAG_USED_ON_NODE) ? "TRUE" : "FALSE");
    free(tmp);
    tmp = tmp2;

    /* set the return */
    *output = tmp;

    return;
}


/*
 * JOB_MAP
 */
void prte_map_print(char **output, prte_job_t *jdata)
{
    char *tmp = NULL, *tmp2 = NULL, *tmp3 = NULL, *tmp4 = NULL;
    char *tmp_node = NULL;
    int32_t i, j;
    prte_node_t *node;
    prte_proc_t *proc;
    prte_job_map_t *src = jdata->map;
    uint16_t u16, *u16ptr = &u16;
    char *ppr, *cpus_per_rank, *cpu_type, *cpuset = NULL;
    prte_topology_t *t;

    /* set default result */
    *output = NULL;


    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_PARSEABLE_OUTPUT, NULL, PMIX_BOOL)) {
        /* creating the output in an XML format */
        pmix_asprintf(&tmp4, "<?xml version=\"1.0\" ?>\n<map>\n");
        pmix_asprintf(&tmp,""); 

        /* loop through nodes */
        for (i = 0; i < src->nodes->size; i++) {
            if (NULL == (node = (prte_node_t*)pmix_pointer_array_get_item(src->nodes, i))) {
                continue;
            }
            prte_node_print(&tmp_node, jdata, node);
            pmix_asprintf(&tmp3, "%s%s",tmp,tmp_node);
            free(tmp_node);
            tmp_node = NULL;
            free(tmp);
            tmp = tmp3;
        }

        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
            pmix_asprintf(&tmp2, "%s<!-- \n"
                "\tWarning: This map has been generated with the DONOTLAUNCH option;\n"
                "\tThe compute node architecture has not been probed, and the displayed\n"
                "\tmap reflects the HEADNODE ARCHITECTURE. On systems with a different\n"
                "\tarchitecture between headnode and compute nodes, the map can be\n"
                "\tdisplayed using prterun's display `map /bin/true`, which will launch\n"
                "\tenough of the DVM to probe the compute node architecture.\n"
                " -->\n", tmp);
            free(tmp);
            tmp = tmp2;
        }

        /* end of the xml "map" tag */  
        pmix_asprintf(&tmp2, "%s%s</map>\n", tmp4,tmp);
        *output = tmp2;
        free(tmp);
        free(tmp4);
        return;
    }

    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_PPR, (void **) &ppr, PMIX_STRING)) {
        ppr = strdup("N/A");
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_PES_PER_PROC, (void **) &u16ptr,
                           PMIX_UINT16)) {
        pmix_asprintf(&cpus_per_rank, "%d", (int) u16);
    } else {
        cpus_per_rank = strdup("N/A");
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_HWT_CPUS, NULL, PMIX_BOOL)) {
        cpu_type = "HWT";
    } else {
        cpu_type = "CORE";
    }
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_CPUSET, (void **) &cpuset, PMIX_STRING)) {
        if (NULL == prte_hwloc_default_cpu_list) {
            cpuset = strdup("N/A");
        } else {
            cpuset = strdup(prte_hwloc_default_cpu_list);
        }
    }

    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DISPLAY_DEVEL_MAP, NULL, PMIX_BOOL)) {
        pmix_asprintf(
            &tmp,
            "\n=================================   JOB MAP   =================================\n"
            "Data for JOB %s offset %s Total slots allocated %lu\n"
            "Mapper requested: %s  Last mapper: %s  Mapping policy: %s  Ranking policy: %s\n"
            "Binding policy: %s  Cpu set: %s  PPR: %s  Cpus-per-rank: %s  Cpu Type: %s",
            PRTE_JOBID_PRINT(jdata->nspace), PRTE_VPID_PRINT(jdata->offset),
            (long unsigned) jdata->total_slots_alloc,
            (NULL == src->req_mapper) ? "NULL" : src->req_mapper,
            (NULL == src->last_mapper) ? "NULL" : src->last_mapper,
            prte_rmaps_base_print_mapping(src->mapping),
            prte_rmaps_base_print_ranking(src->ranking),
            prte_hwloc_base_print_binding(src->binding), cpuset, ppr, cpus_per_rank, cpu_type);

        if (PMIX_RANK_INVALID == src->daemon_vpid_start) {
            pmix_asprintf(
                &tmp2, "%s\nNum new daemons: %ld\tNew daemon starting vpid INVALID\nNum nodes: %ld",
                tmp, (long) src->num_new_daemons, (long) src->num_nodes);
        } else {
            pmix_asprintf(&tmp2,
                          "%s\nNum new daemons: %ld\tNew daemon starting vpid %ld\nNum nodes: %ld",
                          tmp, (long) src->num_new_daemons, (long) src->daemon_vpid_start,
                          (long) src->num_nodes);
        }
        free(tmp);
        tmp = tmp2;
    } else {
        /* this is being printed for a user, so let's make it easier to see */
        pmix_asprintf(&tmp,
                      "\n========================   JOB MAP   ========================\n"
                      "Data for JOB %s offset %s Total slots allocated %lu\n"
                      "    Mapping policy: %s  Ranking policy: %s Binding policy: %s\n"
                      "    Cpu set: %s  PPR: %s  Cpus-per-rank: %s  Cpu Type: %s\n",
                      PRTE_JOBID_PRINT(jdata->nspace), PRTE_VPID_PRINT(jdata->offset),
                      (long unsigned) jdata->total_slots_alloc,
                      prte_rmaps_base_print_mapping(src->mapping),
                      prte_rmaps_base_print_ranking(src->ranking),
                      prte_hwloc_base_print_binding(src->binding), cpuset, ppr, cpus_per_rank,
                      cpu_type);
    }
    free(ppr);
    free(cpus_per_rank);
    free(cpuset);

    for (i = 0; i < src->nodes->size; i++) {
        if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(src->nodes, i))) {
            continue;
        }
        prte_node_print(&tmp2, jdata, node);
        pmix_asprintf(&tmp3, "%s\n%s", tmp, tmp2);
        free(tmp);
        free(tmp2);
        tmp = tmp3;
    }

    /* put some warning out for the donotlaunch case */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        pmix_asprintf(&tmp2, "%s\n\nWarning: This map has been generated with the DONOTLAUNCH option;\n"
                             "\tThe compute node architecture has not been probed, and the displayed\n"
                             "\tmap reflects the HEADNODE ARCHITECTURE. On systems with a different\n"
                             "\tarchitecture between headnode and compute nodes, the map can be\n"
                             "\tdisplayed using `prte --display map /bin/true`, which will launch\n"
                             "\tenough of the DVM to probe the compute node architecture.", tmp);
        free(tmp);
        tmp = tmp2;
    }

    /* let's make it easier to see */
    pmix_asprintf(&tmp2, "%s\n\n=============================================================\n",
                  tmp);
    free(tmp);
    tmp = tmp2;

    /* set the return */
    *output = tmp;

    return;
}
