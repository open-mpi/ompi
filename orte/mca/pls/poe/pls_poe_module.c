/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <fcntl.h>
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/smr/smr.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"

/* remove for ORTE 2.0 */
#include "orte/mca/sds/base/base.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/poe/pls_poe.h"

/*
 * Local functions
 */
static int pls_poe_launch_job(orte_jobid_t jobid);
static int pls_poe_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_poe_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs);
static int pls_poe_terminate_proc(const orte_process_name_t *name);
static int pls_poe_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs);
static int pls_poe_signal_proc(const orte_process_name_t *name, int32_t signal);
static int pls_poe_finalize(void);
static int pls_poe_cancel_operation(void);

orte_pls_base_module_t orte_pls_poe_module = {
    pls_poe_launch_job,
    pls_poe_terminate_job,
    pls_poe_terminate_orteds,
    pls_poe_terminate_proc,
    pls_poe_signal_job,
    pls_poe_signal_proc,
    pls_poe_cancel_operation,
    pls_poe_finalize
};

/**
poe_set_handler_default - set signal handler to default
@param sig signal [IN]
*/
static void poe_set_handler_default(int sig)
{
    struct sigaction act;

    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(sig, &act, (struct sigaction *)0);
}

/**
poe_argv_append_int - append integer variable to argument variable
@param argc argument count [OUT]
@param argv argument variable [OUT]
@param varname variable name [IN]
@param min minimum value [IN]
@param argname argument name [IN]
*/
static inline int poe_argv_append_int(int *argc, char ***argv, int varname, int min, char *argname)
{
    char *tmp_string;
    if(varname >= min) {
        opal_argv_append(argc, argv, argname);
        asprintf(&tmp_string, "%d", varname);
        opal_argv_append(argc, argv, tmp_string);
        free(tmp_string);
    } else {
        return ORTE_ERR_BAD_PARAM;
    }
    return ORTE_SUCCESS;
}

/**
@warning - THIS FUNCTION IS NOT USED. IT WILL BE USED WHEN FAULT-TOLERANCE FEATURE IS NEEDED
*/

#ifdef __FOR_LATER

int pls_poe_launch_interactive_orted(orte_jobid_t jobid)
{
    opal_list_t nodes, mapping_list;
    opal_list_item_t* item;
    orte_std_cntr_t num_nodes;
    orte_vpid_t vpid;
    int node_name_index1;
    int node_name_index2;
    int proc_name_index;
    char *tmp_string;
    char *uri, *param;
    char* name_string;
    char** argv;
    int argc;
    int pid;
    int rc;
    int i;
    int status;
    FILE *hfp, *cfp;

    /* Query the list of nodes allocated and mapped to this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     * All other mapping responsibilities fall to orted in the fork PLS
     */

    if((mca_pls_poe_component.hostfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((mca_pls_poe_component.cmdfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((hfp=fopen(mca_pls_poe_component.hostfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cfp=fopen(mca_pls_poe_component.cmdfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;

    OBJ_CONSTRUCT(&nodes, opal_list_t);
    OBJ_CONSTRUCT(&mapping_list, opal_list_t);
    rc = orte_rmaps_base_mapped_node_query(&mapping_list, &nodes, jobid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /*
     * Allocate a range of vpids for the daemons.
     */

    num_nodes = opal_list_get_size(&nodes);
    if(num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* application */
    argv = opal_argv_copy(opal_argv_split(mca_pls_poe_component.orted, ' '));
    argc = opal_argv_count(argv);
    if (mca_pls_poe_component.debug) {
         opal_argv_append(&argc, &argv, "--debug");
    }
    opal_argv_append(&argc, &argv, "--debug-daemons");

    opal_argv_append(&argc, &argv, "--no-daemonize");
    opal_argv_append(&argc, &argv, "--bootproxy");
    /* need integer value for command line parameter - NOT hex */
    asprintf(&tmp_string, "%lu", (unsigned long)jobid);
    opal_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);
    opal_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    opal_argv_append(&argc, &argv, "");
    opal_argv_append(&argc, &argv, "--nodename");
    node_name_index2 = argc;
    opal_argv_append(&argc, &argv, "");

    /* pass along the universe name and location info */
    opal_argv_append(&argc, &argv, "--universe");
    asprintf(&tmp_string, "%s@%s:%s", orte_universe_info.uid,
    orte_universe_info.host, orte_universe_info.name);
    opal_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);


    /* setup ns contact info */
    opal_argv_append(&argc, &argv, "--nsreplica");
    if(NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);

    /* setup gpr contact info */
    opal_argv_append(&argc, &argv, "--gprreplica");
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    opal_argv_append(&argc, &argv, param);
    free(uri);

    /*
     * Iterate through each of the nodes and spin
     * up a daemon.
     */

    for(item =  opal_list_get_first(&nodes);
        item != opal_list_get_end(&nodes);
        item =  opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;
        orte_process_name_t* name;
        pid_t pid;

        /* setup node name */
        argv[node_name_index2] = node->node_name;

        fprintf(hfp,"%s\n",node->node_name);

        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* setup process name */
        rc = orte_ns.get_proc_name_string(&name_string, name);
        if(ORTE_SUCCESS != rc) {
            opal_output(0, "orte_pls_poe: unable to create process name");
            return rc;
        }
        argv[proc_name_index] = name_string;
        for(i=0;i<argc;i++) {
           fprintf(cfp,"%s ",argv[i]);
        }
        fprintf(cfp,"\n");

        if (mca_pls_poe_component.verbose) {
           opal_output(0, "%s:cmdfile %s\n", __FUNCTION__, opal_argv_join(argv, ' '));
        }
        vpid++;
        free(name);
    }

    fclose(cfp);
    fclose(hfp);

    argv = opal_argv_copy(mca_pls_poe_component.argv);
    argc = mca_pls_poe_component.argc;
    opal_argv_append(&argc, &argv, "-hostfile");
    opal_argv_append(&argc, &argv, mca_pls_poe_component.hostfile);
    opal_argv_append(&argc, &argv, "-cmdfile");
    opal_argv_append(&argc, &argv, mca_pls_poe_component.cmdfile);
    opal_argv_append(&argc, &argv, "-procs");
    asprintf(&tmp_string, "%d", num_nodes);
    opal_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);
    opal_argv_append(&argc, &argv, "-pgmmodel");
    opal_argv_append(&argc, &argv, "mpmd");
    opal_argv_append(&argc, &argv, "-resd");
    opal_argv_append(&argc, &argv, "no");
    opal_argv_append(&argc, &argv, "-labelio");
    opal_argv_append(&argc, &argv, "yes");
    opal_argv_append(&argc, &argv, "-infolevel");
    opal_argv_append(&argc, &argv, "6");
    opal_argv_append(&argc, &argv, "-stdoutmode");
    opal_argv_append(&argc, &argv, "ordered");
    rc=poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    if (mca_pls_poe_component.verbose) {
       opal_output(0, "%s:cmdline %s\n", __FUNCTION__, opal_argv_join(argv, ' '));
    }

    pid = fork();
    if(pid < 0) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* child */
    if(pid == 0) {
       execv(mca_pls_poe_component.path, argv);
       opal_output(0, "orte_pls_poe: execv failed with errno=%d\n", errno);
       exit(-1);
    }

cleanup:
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    while(NULL != (item = opal_list_remove_first(&mapping_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&mapping_list);

    if (mca_pls_poe_component.verbose > 10) opal_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
    return rc;
}

#endif

/**
poe_wait_job - call back when POE finish
@param pid pid
@param status status
@param cbdata call back data
@return error number
*/
static void poe_wait_job(pid_t pid, int status, void* cbdata)
{
    orte_job_map_t *map;
    opal_list_item_t *item, *item2;
    int rc;

    /* query allocation for the job */
    rc = orte_rmaps.get_job_map(&map, mca_pls_poe_component.jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

    for(item =  opal_list_get_first(&map->nodes);
        item != opal_list_get_end(&map->nodes);
        item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*) item;

        for (item2 = opal_list_get_first(&node->procs);
             item2 != opal_list_get_end(&node->procs);
             item2 = opal_list_get_next(item2)) {
            orte_mapped_proc_t* proc = (orte_mapped_proc_t*)item2;
            
            orte_session_dir_finalize(&(proc->name));
            rc = orte_smr.set_proc_state(&(proc->name),
                                        ORTE_PROC_STATE_ABORTED, status);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
            }
        }
    }
}

/**
poe_create_cmd_file - create POE command file
@param cfp command file pointer [IN]
@param context context [IN]
@param proc proc [IN]
@param vpid_start vpid start [IN]
@param vpid_range vpid range [IN]
@return error number
*/
static int poe_create_cmd_file(
    FILE *cfp,
    orte_app_context_t* context,
    orte_mapped_proc_t* proc,
    orte_vpid_t vpid_start,
    orte_vpid_t vpid_range)
{
    int i;

    char* param;
    char* uri;
    char **environ_copy;

    /* setup base environment */
    environ_copy = NULL;
    param = mca_base_param_environ_variable("rmgr","bootproxy","jobid");
    opal_unsetenv(param, &environ_copy);

    /* setup universe info */
    if(NULL != orte_universe_info.name) {
        param = mca_base_param_environ_variable("universe", NULL, NULL);
        asprintf(&uri, "%s@%s:%s", orte_universe_info.uid,
                                  orte_universe_info.host,
                                  orte_universe_info.name);
        opal_setenv(param, uri, true, &environ_copy);
        free(param);
        free(uri);
    }

    /* setup ns contact info */
    if(NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    param = mca_base_param_environ_variable("ns","replica","uri");
    opal_setenv(param, uri, true, &environ_copy);
    free(param);
    free(uri);

    /* setup gpr contact info */
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    param = mca_base_param_environ_variable("gpr","replica","uri");
    opal_setenv(param, uri, true, &environ_copy);
    free(param);
    free(uri);

    /* push name into environment */
    orte_ns_nds_env_put(&proc->name, vpid_start, vpid_range, &environ_copy);

    if (context->argv == NULL) {
        context->argv = malloc(sizeof(char*)*2);
        context->argv[0] = strdup(context->app);
        context->argv[1] = NULL;
    }

    i=0;
    fprintf(cfp,"%s",mca_pls_poe_component.env);
    while(environ_copy[i]!=NULL) {
        fprintf(cfp," %s",environ_copy[i++]);
    }
    opal_argv_free(environ_copy);
    fprintf(cfp," %s",context->app);
    i=1;
    while(context->argv[i]!=NULL) {
        fprintf(cfp," %s",context->argv[i++]);
    }

    /* POE will upset if the file doesn't contain end of line. */
    fprintf(cfp,"\n");

    return ORTE_SUCCESS;
}

/**
poe_launch_interactive - launch an interactive job
@param jobid JOB Identifier [IN]
@return error number
*/
static inline int poe_launch_interactive_job(orte_jobid_t jobid)
{
    orte_job_map_t *map;
    opal_list_item_t *item, *item2;
    orte_vpid_t vpid_start, vpid_range;
    orte_std_cntr_t num_nodes, num_procs;
    FILE *hfp, *cfp;
    char** argv;
    int argc;
    int rc, pid;
    sigset_t sigs;

    if( (NULL==(mca_pls_poe_component.cmdfile=tempnam(NULL,NULL))) ||
        (NULL==(cfp=fopen(mca_pls_poe_component.cmdfile,"w"))) ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    mca_pls_poe_component.jobid = jobid;

    /* get the map for this job */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    
    num_nodes = opal_list_get_size(&map->nodes);

    if(!strncmp(mca_pls_poe_component.resource_allocation,"hostfile",8)) {

        /* Create a temporary hostlist file if user specify */

        if( (NULL==(mca_pls_poe_component.hostfile=tempnam(NULL,NULL))) ||
            (NULL==(hfp=fopen(mca_pls_poe_component.hostfile,"w"))) ) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for(item =  opal_list_get_first(&map->nodes);
            item != opal_list_get_end(&map->nodes);
            item =  opal_list_get_next(item)) {
            orte_mapped_node_t* node = (orte_mapped_node_t*)item;
            fprintf(hfp,"%s\n",node->nodename);
        }
        fclose(hfp);
    }

    rc = orte_rmgr.get_vpid_range(jobid, &vpid_start, &vpid_range);
    if (ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    /* Create a temporary POE command file */

    num_procs = 0;
    for(item =  opal_list_get_first(&map->nodes);
        item != opal_list_get_end(&map->nodes);
        item =  opal_list_get_next(item)) {
        orte_mapped_node_t* node = (orte_mapped_node_t*)item;

        for (item2 = opal_list_get_first(&node->procs);
             item2 != opal_list_get_end(&node->procs);
             item2 = opal_list_get_next(item2)) {
            orte_mapped_proc_t* proc = (orte_mapped_proc_t*)item2;
            rc = poe_create_cmd_file(cfp, map->apps[proc->app_idx], proc, vpid_start, vpid_range);
            if(ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
            num_procs++;
        }
    }
    fclose(cfp);

    /* Generate POE command line */

    argv = opal_argv_copy(mca_pls_poe_component.argv);
    argc = mca_pls_poe_component.argc;

    if(!strncmp(mca_pls_poe_component.resource_allocation,"hostfile",8)) {
       opal_argv_append(&argc, &argv, "-hostfile");
       opal_argv_append(&argc, &argv, mca_pls_poe_component.hostfile);
       opal_argv_append(&argc, &argv, "-resd");
       opal_argv_append(&argc, &argv, "no");
       rc=poe_argv_append_int(&argc, &argv, num_nodes, 1, "-nodes");
       if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    }

    opal_argv_append(&argc, &argv, "-pgmmodel");
    opal_argv_append(&argc, &argv, "mpmd");
    opal_argv_append(&argc, &argv, "-cmdfile");
    opal_argv_append(&argc, &argv, mca_pls_poe_component.cmdfile);
    opal_argv_append(&argc, &argv, "-labelio");
    opal_argv_append(&argc, &argv, mca_pls_poe_component.mp_labelio);
    opal_argv_append(&argc, &argv, "-stdoutmode");
    opal_argv_append(&argc, &argv, mca_pls_poe_component.mp_stdoutmode);

    rc=poe_argv_append_int(&argc, &argv, num_procs, 1, "-procs");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_infolevel, 0, "-infolevel");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    if(mca_pls_poe_component.verbose>10) {
        opal_output(0, "POE cmdline %s\n", opal_argv_join(argv, ' '));
    }

    /* Start job with POE */

    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if(pid == 0) {
        poe_set_handler_default(SIGTERM);
        poe_set_handler_default(SIGINT);
        poe_set_handler_default(SIGHUP);
        poe_set_handler_default(SIGCHLD);
        poe_set_handler_default(SIGPIPE);
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);
        execv(mca_pls_poe_component.path, argv);
        opal_output(0, "orte_pls_poe: execv failed with errno=%d\n", errno);
        exit(-1);
    } else {
        orte_wait_cb(pid, poe_wait_job, NULL);
    }


cleanup:
    OBJ_RELEASE(map);
    
    return rc;
}

/**
pls_poe_launch - launch a POE job
@warning current support interactive class only!.
@param jobid JOB Identifier [IN]
@return error number
*/
static int pls_poe_launch_job(orte_jobid_t jobid)
{
    if(0 == strncmp(mca_pls_poe_component.class,"interactive",11)) {
        return poe_launch_interactive_job(jobid);
    }
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int pls_poe_terminate_job(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int pls_poe_terminate_proc(const orte_process_name_t *name)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int pls_poe_terminate_orteds(orte_jobid_t jobid, struct timeval *timeout, opal_list_t *attrs)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int pls_poe_signal_job(orte_jobid_t jobid, int32_t signal, opal_list_t *attrs)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int pls_poe_signal_proc(const orte_process_name_t *name, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/**
pls_poe_finalize - clean up temporary files
@return error number
*/
static int pls_poe_finalize(void)
{
    unlink(mca_pls_poe_component.cmdfile);
    unlink(mca_pls_poe_component.hostfile);
    return ORTE_SUCCESS;
}
