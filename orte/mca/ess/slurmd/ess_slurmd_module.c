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
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */
#include <ctype.h>
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_IFADDRS_H
#include <ifaddrs.h>
#endif
#if WANT_SLURM_PMI_SUPPORT
#include <slurm/pmi.h>
#endif

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/opal_sos.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/paffinity/paffinity.h"
#include "opal/util/printf.h"

#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/nidmap.h"
#include "orte/util/pre_condition_transports.h"
#include "orte/util/regex.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/slurmd/ess_slurmd.h"

static int rte_init(void);
static int rte_finalize(void);
static void rte_abort(int error_code, bool report) __opal_attribute_noreturn__;
static uint8_t proc_get_locality(orte_process_name_t *proc);
static orte_vpid_t proc_get_daemon(orte_process_name_t *proc);
static char* proc_get_hostname(orte_process_name_t *proc);
static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc);
static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc);
static int update_pidmap(opal_byte_object_t *bo);
static int update_nidmap(opal_byte_object_t *bo);

orte_ess_base_module_t orte_ess_slurmd_module = {
    rte_init,
    rte_finalize,
    rte_abort,
    proc_get_locality,
    proc_get_daemon,
    proc_get_hostname,
    proc_get_local_rank,
    proc_get_node_rank,
    orte_ess_base_proc_get_epoch,  /* proc_get_epoch */
    update_pidmap,
    update_nidmap,
    orte_ess_base_query_sys_info,
    NULL /* ft_event */
};

/* Local globals */
static bool app_init_complete;
static bool slurm20;

/* Local functions */
static int discover_nodes(char *regexp, char*** nodelist);
static int parse_ranges(char *base, char *ranges, char ***names);
static int parse_range(char *base, char *range, char ***names);

/****    MODULE FUNCTIONS    ****/

static int rte_init(void)
{
    int ret;
    char *error = NULL;
    int32_t jobfam, stepid;
    char **nodes = NULL;
    char *envar;
    int i, j;
    orte_nid_t *node;
    orte_jmap_t *jmap;
    orte_pmap_t *pmap;
    orte_vpid_t vpid;
    int local_rank;
    int nodeid;
    int num_nodes;
    int cpus_per_task;
    char *regexp, *tasks_per_node;
    int *ppn;
    bool block=false, cyclic=false;
    uint64_t unique_key[2];
    char *cs_env, *string_key;

    /* init flag */
    app_init_complete = false;
    slurm20 = false;
    
    /* run the prolog */
    if (ORTE_SUCCESS != (ret = orte_ess_base_std_prolog())) {
        error = "orte_ess_base_std_prolog";
        goto error;
    }
    
    
    /* Only application procs can use this module. Since we
     * were directly launched by srun, we need to bootstrap
     * our own global info so we can startup. Srun will have
     * provided that info in our environment, so get it from there
     */
    
    /* declare ourselves to be standalone - i.e., not launched by orted */
    orte_standalone_operation = true;
    
    /* get the slurm jobid - this will be our job family */
    envar = getenv("SLURM_JOBID");
    /* don't need to check this for NULL - if it was, we would
     * never have been selected anyway
     */
    jobfam = strtol(envar, NULL, 10);
    /* get the slurm stepid - this will be our local jobid */
    if (NULL == (envar = getenv("SLURM_STEPID"))) {
        error = "could not get SLURM_STEPID";
        goto error;
    }
    /* because the stepid could be zero, and we want the local
     * jobid to be unique, increment it by one so the system
     * doesn't think that we are a bunch of daemons!
     */
    stepid = strtol(envar, NULL, 10) + 1;
    /* now build the jobid */
    ORTE_PROC_MY_NAME->jobid = ORTE_CONSTRUCT_LOCAL_JOBID(jobfam << 16, stepid);
    
    /* setup transport keys in case the MPI layer needs them -
     * we can use the SLURM jobid and stepid as unique keys
     * because they are unique values assigned by the RM
     */
    unique_key[0] = (uint64_t)jobfam;
    unique_key[1] = (uint64_t)stepid;
    if (NULL == (string_key = orte_pre_condition_transports_print(unique_key))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (NULL == (cs_env = mca_base_param_environ_variable("orte_precondition_transports",NULL,NULL))) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    asprintf(&envar, "%s=%s", cs_env, string_key);
    putenv(envar);
    /* cannot free the envar as that messes of our environ */
    free(cs_env);
    free(string_key);

#if WANT_SLURM_PMI_SUPPORT
    /* get our rank from PMI */
    if (PMI_SUCCESS != PMI_Get_rank(&i)) {
        error = "PMI_Get_rank failed";
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = i;
#else
    /* get the slurm procid - this will be our vpid */
    if (NULL == (envar = getenv("SLURM_PROCID"))) {
        error = "could not get SLURM_PROCID";
        goto error;
    }
    ORTE_PROC_MY_NAME->vpid = strtol(envar, NULL, 10);
#endif
    ORTE_PROC_MY_NAME->epoch = ORTE_EPOCH_MIN;
    /* get our local rank */
    if (NULL == (envar = getenv("SLURM_LOCALID"))) {
        error = "could not get SLURM_LOCALID";
        goto error;
    }
    local_rank = strtol(envar, NULL, 10);

    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "%s local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         local_rank));
#if WANT_SLURM_PMI_SUPPORT
    if (PMI_SUCCESS != PMI_Get_universe_size(&i)) {
        error = "PMI_Get_universe_size failed";
        goto error;
    }
    orte_process_info.num_procs = i;
#else
    /* get the number of procs in this job */
    if (NULL == (envar = getenv("SLURM_STEP_NUM_TASKS"))) {
        error = "could not get SLURM_STEP_NUM_TASKS";
        goto error;
    }
    orte_process_info.num_procs = strtol(envar, NULL, 10);
#endif

    if (orte_process_info.max_procs < orte_process_info.num_procs) {
        orte_process_info.max_procs = orte_process_info.num_procs;
    }
#if WANT_SLURM_PMI_SUPPORT
    if (PMI_SUCCESS != PMI_Get_appnum(&i)) {
        error = "PMI_Get_appnum failed";
        goto error;
    }
    
    orte_process_info.app_num = i;
#else
    /* set the app_num so that MPI attributes get set correctly */
    orte_process_info.app_num = 1;
#endif

    /* if this is SLURM 2.0 or above, get our port
     * assignments for use in the OOB
     */
    if (NULL != (envar = getenv("SLURM_STEP_RESV_PORTS"))) {
        /* convert this to an MCA param that will be
         * picked up by the OOB
         */
        orte_oob_static_ports = strdup(envar);
        slurm20 = true;
        OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                             "%s using SLURM-reserved ports %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             envar));
    }
    
    /* get my local nodeid */
    if (NULL == (envar = getenv("SLURM_NODEID"))) {
        error = "could not get SLURM_NODEID";
        goto error;
    }
    nodeid = strtol(envar, NULL, 10);
    ORTE_PROC_MY_DAEMON->jobid = 0;
    ORTE_PROC_MY_DAEMON->vpid = nodeid;
    ORTE_PROC_MY_DAEMON->epoch = ORTE_PROC_MY_NAME->epoch;
    
    /* get the number of ppn */
    if (NULL == (tasks_per_node = getenv("SLURM_STEP_TASKS_PER_NODE"))) {
        error = "could not get SLURM_STEP_TASKS_PER_NODE";
        goto error;
    }
    
    /* get the number of CPUs per task that the user provided to slurm */
    if (NULL != (envar = getenv("SLURM_CPUS_PER_TASK"))) {
        cpus_per_task = strtol(envar, NULL, 10);
        if(0 >= cpus_per_task) {
            error = "got bad value from SLURM_CPUS_PER_TASK";
            goto error;
        }
    } else {
        cpus_per_task = 1;
    }
    
    /* get the node list */
    if (NULL == (regexp = getenv("SLURM_STEP_NODELIST"))) {
        error = "could not get SLURM_STEP_NODELIST";
        goto error;
    }
    /* break that down into a list of nodes */
    if (ORTE_SUCCESS != (ret = discover_nodes(regexp, &nodes))) {
        error = "could not parse node list";
        goto error;
    }
    num_nodes = opal_argv_count(nodes);
    orte_process_info.num_nodes = num_nodes;
    
    /* compute the ppn */
    if (ORTE_SUCCESS != (ret = orte_regex_extract_ppn(num_nodes, tasks_per_node, &ppn))) {
        error = "could not determine #procs on each node";
        goto error;
    }
    /* for slurm, we have to normalize the ppn by the cpus_per_task */
    for (i=0; i < num_nodes; i++) {
        ppn[i] /= cpus_per_task;
    }
    
    /* get the distribution (i.e., mapping) mode */
    if (NULL == (envar = getenv("SLURM_DISTRIBUTION")) ||
        0 == strcmp(envar, "block")) {
        /* assume byslot mapping */
        block = true;
    } else if (0 == strcmp(envar, "cyclic")) {
        /* bynode mapping */
        cyclic = true;
    } else {
        /* cannot currently support other mapping modes */
        error = "distribution/mapping mode not supported";
        goto error;
    }
#if 0
    SLURM_DIST_PLANESIZE=0
    SLURM_DIST_LLLP=
#endif
    
    /* setup the nidmap arrays */
    if (ORTE_SUCCESS != (ret = orte_util_nidmap_init(NULL))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_util_nidmap_init";
        goto error;
    }
    
    /* set the size of the nidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_size(&orte_nidmap, orte_process_info.num_nodes))) {
        error = "could not set pointer array size for nidmap";
        goto error;
    }
    
    /* construct the nidmap */
    for (i=0; i < num_nodes; i++) {
        node = OBJ_NEW(orte_nid_t);
        node->name = strdup(nodes[i]);
        node->daemon = i;
        node->index = i;
        opal_pointer_array_set_item(&orte_nidmap, i, node);
    }
    opal_argv_free(nodes);
    
    /* create a job map for this job */
    jmap = OBJ_NEW(orte_jmap_t);
    jmap->job = ORTE_PROC_MY_NAME->jobid;
    opal_pointer_array_add(&orte_jobmap, jmap);
    /* update the num procs */
    jmap->num_procs = orte_process_info.num_procs;

    /* set the size of the pidmap storage so we minimize realloc's */
    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_size(&jmap->pmap, jmap->num_procs))) {
        ORTE_ERROR_LOG(ret);
        error = "could not set value array size for pidmap";
        goto error;
    }
        
    /* construct the pidmap */
    if (block) {
        /* for each node, cycle through the ppn */
        vpid = 0;
        for (i=0; i < orte_nidmap.size; i++) {
            if (NULL == (node = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                continue;
            }
            /* compute the vpid for each proc on this node
             * and add a pmap entry for it
             */
            for (j=0; j < ppn[i]; j++) {
                pmap = OBJ_NEW(orte_pmap_t);
                pmap->node = node->index;
                pmap->local_rank = j;
                pmap->node_rank = j;
                if (ORTE_SUCCESS != (ret = opal_pointer_array_set_item(&jmap->pmap, vpid, pmap))) {
                    ORTE_ERROR_LOG(ret);
                    error = "could not set pmap values";
                    goto error;
                }
                OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                                     "%s node %d name %s rank %d",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     (int) node->index, node->name, (int)vpid));
                vpid++;
            }
        }
    } else if (cyclic) {
        /* cycle across the nodes */
        vpid = 0;
        while (vpid < orte_process_info.num_procs) {
            for (i=0; i < num_nodes && vpid < orte_process_info.num_procs; i++) {
                if (0 < ppn[i]) {
                    if (NULL == (node = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                        /* this is an error */
                        error = "error initializing process map";
                        goto error;
                    }
                    pmap = OBJ_NEW(orte_pmap_t);
                    pmap->node = node->index;
                    pmap->local_rank = ppn[i]-1;
                    pmap->node_rank = ppn[i]-1;
                    if (ORTE_SUCCESS != (ret = opal_pointer_array_set_item(&jmap->pmap, vpid, pmap))) {
                        ORTE_ERROR_LOG(ret);
                        error = "could not set pmap values";
                        goto error;
                    }
                    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                                         "%s node %d name %s rank %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         (int) node->index, node->name, (int)vpid));
                    vpid++;
                    --ppn[i];
                }
            }
        }
    }
    free(ppn);

    /* ensure we pick the correct critical components */
#if WANT_SLURM_PMI_SUPPORT
    putenv("OMPI_MCA_grpcomm=pmi");
#else
    putenv("OMPI_MCA_grpcomm=hier");
#endif
    putenv("OMPI_MCA_routed=direct");
    
    /* now use the default procedure to finish my setup */
    if (ORTE_SUCCESS != (ret = orte_ess_base_app_setup())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_app_setup";
        goto error;
    }
    
    /* flag that we completed init */
    app_init_complete = true;
    
    return ORTE_SUCCESS;
    
error:
    orte_show_help("help-orte-runtime.txt",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);
    return ret;
}

static int rte_finalize(void)
{
    int ret = ORTE_SUCCESS;
   
    if (app_init_complete) {
        /* use the default procedure to finish */
        if (ORTE_SUCCESS != (ret = orte_ess_base_app_finalize())) {
            ORTE_ERROR_LOG(ret);
        }
    }
    
    /* remove the envars that we pushed into environ
     * so we leave that structure intact
     */
    unsetenv("OMPI_MCA_grpcomm");
    unsetenv("OMPI_MCA_routed");
    unsetenv("OMPI_MCA_orte_precondition_transports");

    /* deconstruct my nidmap and jobmap arrays - this
     * function protects itself from being called
     * before things were initialized
     */
    orte_util_nidmap_finalize();
    
    return ret;    
}

static void rte_abort(int error_code, bool report)
{
    if (ORTE_ERR_SOCKET_NOT_AVAILABLE == OPAL_SOS_GET_ERROR_CODE(error_code) &&
        slurm20) {
        /* exit silently with a special error code for slurm 2.0 */
        orte_ess_base_app_abort(108, false);
    } else {
        orte_ess_base_app_abort(error_code, report);
    }
}

static uint8_t proc_get_locality(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return OPAL_PROC_NON_LOCAL;
    }
    
    if (nid->daemon == ORTE_PROC_MY_DAEMON->vpid) {
        OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                             "%s ess:slurmd: proc %s is LOCAL",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        return (OPAL_PROC_ON_NODE | OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s is REMOTE",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    return OPAL_PROC_NON_LOCAL;
    
}

static orte_vpid_t proc_get_daemon(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if( ORTE_JOBID_IS_DAEMON(proc->jobid) ) {
        return proc->vpid;
    }

    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        return ORTE_VPID_INVALID;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s is hosted by daemon %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         ORTE_VPID_PRINT(nid->daemon)));
    
    return nid->daemon;
}

static char* proc_get_hostname(orte_process_name_t *proc)
{
    orte_nid_t *nid;
    
    if (NULL == (nid = orte_util_lookup_nid(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return NULL;
    }
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s is on host %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         nid->name));
    
    return nid->name;
}

static orte_local_rank_t proc_get_local_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_LOCAL_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s has local rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->local_rank));
    
    return pmap->local_rank;
}

static orte_node_rank_t proc_get_node_rank(orte_process_name_t *proc)
{
    orte_pmap_t *pmap;
    
    if (NULL == (pmap = orte_util_lookup_pmap(proc))) {
        return ORTE_NODE_RANK_INVALID;
    }    
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: proc %s has node rank %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         (int)pmap->node_rank));
    
    return pmap->node_rank;
}

static int update_pidmap(opal_byte_object_t *bo)
{
    int ret;
    
    OPAL_OUTPUT_VERBOSE((2, orte_ess_base_output,
                         "%s ess:slurmd: updating pidmap",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* build the pmap */
    if (ORTE_SUCCESS != (ret = orte_util_decode_pidmap(bo))) {
        ORTE_ERROR_LOG(ret);
    }
    
    return ret;
}

static int update_nidmap(opal_byte_object_t *bo)
{
    int rc;
    /* decode the nidmap - the util will know what to do */
    if (ORTE_SUCCESS != (rc = orte_util_decode_nodemap(bo))) {
        ORTE_ERROR_LOG(rc);
    }    
    return rc;
}


/**
 * Discover the available resources.
 * 
 * In order to fully support slurm, we need to be able to handle 
 * node regexp/task_per_node strings such as:
 * foo,bar    5,3
 * foo        5
 * foo[2-10,12,99-105],bar,foobar[3-11] 2(x10),5,100(x16)
 *
 * @param *regexp A node regular expression from SLURM (i.e. SLURM_NODELIST)
 * @param **nodelist argv array to return the found nodes in
 */
static int discover_nodes(char *regexp, char*** names)
{
    int i, j, len, ret;
    char *base;
    char *orig;
    bool found_range = false;
    bool more_to_come = false;
    
    orig = base = strdup(regexp);
    if (NULL == base) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                         "%s ess:slurmd:discover: checking nodelist: %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         regexp));
    
    do {
        /* Find the base */
        len = strlen(base);
        for (i = 0; i <= len; ++i) {
            if (base[i] == '[') {
                /* we found a range. this gets dealt with below */
                base[i] = '\0';
                found_range = true;
                break;
            }
            if (base[i] == ',') {
                /* we found a singleton node, and there are more to come */
                base[i] = '\0';
                found_range = false;
                more_to_come = true;
                break;
            }
            if (base[i] == '\0') {
                /* we found a singleton node */
                found_range = false;
                more_to_come = false;
                break;
            }
        }
        if(i == 0) {
            /* we found a special character at the beginning of the string */
            orte_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1, regexp);
            ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
            free(orig);
            return ORTE_ERR_BAD_PARAM;
        }
        
        if (found_range) {
            /* If we found a range, now find the end of the range */
            for (j = i; j < len; ++j) {
                if (base[j] == ']') {
                    base[j] = '\0';
                    break;
                }
            }
            if (j >= len) {
                /* we didn't find the end of the range */
                orte_show_help("help-ess-slurdm.txt", "slurm-env-var-bad-value", 1, regexp);
                ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
                free(orig);
                return ORTE_ERR_BAD_PARAM;
            }
            
            ret = parse_ranges(base, base + i + 1, names);
            if(ORTE_SUCCESS != ret) {
                orte_show_help("help-ras-slurm.txt", "slurm-env-var-bad-value", 1, regexp);
                ORTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }    
            if(base[j + 1] == ',') {
                more_to_come = true;
                base = &base[j + 2];
            } else {
                more_to_come = false;
            }
        } else {
            /* If we didn't find a range, just add the node */
            
            OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                                 "%s ess:slurmd:discover: found node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 base));
            
            if(ORTE_SUCCESS != (ret = opal_argv_append_nosize(names, base))) {
                ORTE_ERROR_LOG(ret);
                free(orig);
                return ret;
            }
            /* set base equal to the (possible) next base to look at */
            base = &base[i + 1];
        }
    } while(more_to_come);
   
    free(orig);
    
    /* All done */
    return ret;
}


/*
 * Parse one or more ranges in a set
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a range. This can contain multiple ranges
 *                 (i.e. "1-3,10" or "5" or "9,0100-0130,250") 
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int parse_ranges(char *base, char *ranges, char ***names)
{
    int i, len, ret;
    char *start, *orig;
    
    /* Look for commas, the separator between ranges */

    len = strlen(ranges);
    for (orig = start = ranges, i = 0; i < len; ++i) {
        if (',' == ranges[i]) {
            ranges[i] = '\0';
            ret = parse_range(base, start, names);
            if (ORTE_SUCCESS != ret) {
                ORTE_ERROR_LOG(ret);
                return ret;
            }
            start = ranges + i + 1;
        }
    }

    /* Pick up the last range, if it exists */

    if (start < orig + len) {
        
        OPAL_OUTPUT_VERBOSE((1, orte_ess_base_output,
                             "%s ess:slurmd:discover: parse range %s (2)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             start));
        
        ret = parse_range(base, start, names);
        if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    /* All done */
    return ORTE_SUCCESS;
}


/*
 * Parse a single range in a set and add the full names of the nodes
 * found to the names argv
 *
 * @param base     The base text of the node name
 * @param *ranges  A pointer to a single range. (i.e. "1-3" or "5") 
 * @param ***names An argv array to add the newly discovered nodes to
 */
static int parse_range(char *base, char *range, char ***names)
{
    char *str, temp1[BUFSIZ];
    size_t i, j, start, end;
    size_t base_len, len, num_len;
    size_t str_start, str_end;
    size_t num_str_len;
    bool found;
    int ret;
    
    len = strlen(range);
    base_len = strlen(base);
    /* Silence compiler warnings; start and end are always assigned
       properly, below */
    start = end = 0;
    
    /* Look for the beginning of the first number */
    
    for (found = false, i = 0; i < len; ++i) {
        if (isdigit((int) range[i])) {
            if (!found) {
                str_start = i;
                start = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Look for the end of the first number */
    
    for (found = false, num_str_len = 0; i < len; ++i, ++num_str_len) {
        if (!isdigit((int) range[i])) {
            break;
        }
    }
    
    /* Was there no range, just a single number? */
    
    if (i >= len) {
        str_end = len;
        end = start;
        found = true;
    }
    
    /* Nope, there was a range.  Look for the beginning of the second
       number */
    
    else {
        str_end = i - 1;
        for (; i < len; ++i) {
            if (isdigit((int) range[i])) {
                end = atoi(range + i);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }
    
    /* Make strings for all values in the range */
    
    len = base_len + num_str_len + 32;
    str = malloc(len);
    if (NULL == str) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    strcpy(str, base);
    for (i = start; i <= end; ++i) {
        str[base_len] = '\0';
        snprintf(temp1, BUFSIZ - 1, "%lu", (long) i);
        
        /* Do we need zero pading? */
        
        if ((num_len = strlen(temp1)) < num_str_len) {
            for (j = base_len; j < base_len + (num_str_len - num_len); ++j) {
                str[j] = '0';
            }
            str[j] = '\0';
        }
        strcat(str, temp1);
        ret = opal_argv_append_nosize(names, str);
        if(ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            free(str);
            return ret;
        }
    }
    free(str);
    
    /* All done */
    return ORTE_SUCCESS;
}
