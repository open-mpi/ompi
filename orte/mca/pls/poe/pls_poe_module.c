/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
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

#include "ompi_config.h"

#include <fcntl.h>
#include <errno.h>
#include "include/orte_constants.h"
#include "mca/pls/pls.h"
#include "mca/gpr/gpr.h"
#include "mca/rmaps/base/base.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "mca/base/mca_base_param.h"
#include "mca/ns/ns.h"
#include "mca/rml/rml.h"
#include "mca/errmgr/errmgr.h"
#include "mca/soh/soh.h"
#include "util/univ_info.h"
#include "opal/util/argv.h"
#include "opal/util/opal_environ.h"
#include "pls_poe.h"

extern char **environ;

/*
 * Local functions
 */
static int pls_poe_launch(orte_jobid_t jobid);
static int pls_poe_terminate_job(orte_jobid_t jobid);
static int pls_poe_terminate_proc(const orte_process_name_t *name);
static int pls_poe_finalize(void);

orte_pls_base_module_1_0_0_t orte_pls_poe_module = {
    pls_poe_launch,
    pls_poe_terminate_job,
    pls_poe_terminate_proc,
    pls_poe_finalize
};

/**
__poe_set_handler_default - set signal handler to default
@param sig signal [IN]
*/
static void __poe_set_handler_default(int sig)
{
    struct sigaction act;
                                                                                                      
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(sig, &act, (struct sigaction *)0);
}

/**
__poe_argv_append_int - append integer variable to argument variable
@param argc argument count [OUT]
@param argv argument variable [OUT]
@param varname variable name [IN]
@param min minimum value [IN]
@param argname argument name [IN]
*/
static inline int __poe_argv_append_int(int *argc, char ***argv, int varname, int min, char *argname) 
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
int pls_poe_launch_interactive_orted(orte_jobid_t jobid)
{
    opal_list_t nodes;
    opal_list_item_t* item;
    size_t num_nodes;
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
                                                                                                       
    /* query the list of nodes allocated to the job - don't need the entire
     * mapping - as the daemon/proxy is responsibe for determining the apps
     * to launch on each node.
     */
    if (mca_pls_poe_component.verbose > 10) opal_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);

    if((mca_pls_poe_component.hostfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((mca_pls_poe_component.cmdfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((hfp=fopen(mca_pls_poe_component.hostfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cfp=fopen(mca_pls_poe_component.cmdfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;

    OBJ_CONSTRUCT(&nodes, opal_list_t);
    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
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
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
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
            exit(-1);
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
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retrycount, 0, "-retrycount");
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
    } else {
    }
     
cleanup:
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    if (mca_pls_poe_component.verbose > 10) opal_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
    return rc;
}

/**
__poe_wait_job - call back when POE finish
@param pid pid
@param status status
@param cbdata call back data
@return error number
*/
int __poe_wait_job(pid_t pid, int status, void* cbdata)
{
    opal_list_t map;
    opal_list_item_t* item;
    int rc;

    if(mca_pls_poe_component.verbose > 10) {
       opal_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);
    }

    /* query allocation for the job */
    OBJ_CONSTRUCT(&map, opal_list_t);
    rc = orte_rmaps_base_get_map(mca_pls_poe_component.jobid,&map);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    for(item =  opal_list_get_first(&map);
        item != opal_list_get_end(&map);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*) item;
        size_t i;
                
        for(i = 0 ; i < map->num_procs ; ++i) {
            orte_session_dir_finalize(&(map->procs[i])->proc_name);
            rc = orte_soh.set_proc_soh(&(map->procs[i]->proc_name),
                                        ORTE_PROC_STATE_ABORTED, status);
        }
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
        }
    }
    OBJ_DESTRUCT(&map);
cleanup:
    if(mca_pls_poe_component.verbose>10) {
        opal_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
    }
    return rc;
}

/**
__poe_create_cmd_file - create POE command file
@param cfp command file pointer [IN]
@param context context [IN]
@param proc proc [IN]
@param vpid_start vpid start [IN]
@param vpid_range vpid range [IN]
@return error number
*/
static int __poe_create_cmd_file(
    FILE *cfp,
    orte_app_context_t* context,
    orte_rmaps_base_proc_t* proc,
    orte_vpid_t vpid_start,
    orte_vpid_t vpid_range)
{
    pid_t pid;
    int rc;
    int i;

    char* param;
    char* uri;
    char **environ_copy;

    if(mca_pls_poe_component.verbose > 10) {
        opal_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);
    }

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
    orte_ns_nds_env_put(&proc->proc_name, vpid_start, vpid_range, &environ_copy);

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

    if(mca_pls_poe_component.verbose>10) {
        opal_output(0, "%s: --- END ---\n", __FUNCTION__);
    } 

    return ORTE_SUCCESS;
}

/**
__poe_launch_interactive - launch an interactive job
@param jobid JOB Identifier [IN]
@return error number
*/
static inline int __poe_launch_interactive(orte_jobid_t jobid)
{
    opal_list_t map, nodes;
    opal_list_item_t* item;
    orte_vpid_t vpid_start, vpid_range;
    size_t num_nodes, num_procs;
    FILE *hfp, *cfp;
    char** argv;
    int argc;
    int rc, status, pid;
    sigset_t sigs;

    if(mca_pls_poe_component.verbose > 10) {
        opal_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);
    }

    if( (NULL==(mca_pls_poe_component.cmdfile=tempnam(NULL,NULL))) || 
        (NULL==(cfp=fopen(mca_pls_poe_component.cmdfile,"w"))) ) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    mca_pls_poe_component.jobid = jobid;

    OBJ_CONSTRUCT(&nodes, opal_list_t);
    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
    if(ORTE_SUCCESS != rc) { goto cleanup; }
     
    num_nodes = opal_list_get_size(&nodes);

    if(num_nodes > 0) { 

        /* Create a tempolary hostlist file if user specify */

        if( (NULL==(mca_pls_poe_component.hostfile=tempnam(NULL,NULL))) ||
            (NULL==(hfp=fopen(mca_pls_poe_component.hostfile,"w"))) ) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for(item =  opal_list_get_first(&nodes);
            item != opal_list_get_end(&nodes);
            item =  opal_list_get_next(item)) {
            orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
            fprintf(hfp,"%s\n",node->node_name); 
        }
        fclose(hfp);
    }

    rc = orte_rmgr_base_get_job_slots(jobid, &num_procs);
    if(ORTE_SUCCESS != rc) { return rc; }
    
    OBJ_CONSTRUCT(&map, opal_list_t);
    rc = orte_rmaps_base_get_map(jobid,&map);
    if (ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    rc = orte_rmaps_base_get_vpid_range(jobid, &vpid_start, &vpid_range);
    if (ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    /* Create a tempolary POE command file */

    for(item =  opal_list_get_first(&map);
        item != opal_list_get_end(&map);
        item =  opal_list_get_next(item)) {
        orte_rmaps_base_map_t* map2 = (orte_rmaps_base_map_t*)item;
        size_t i;
        for(i=0; i<map2->num_procs; i++) {
            rc = __poe_create_cmd_file(cfp, map2->app, map2->procs[i], vpid_start, vpid_range);
            if(ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
        }
    }
    fclose(cfp);

    /* Generate POE command line */

    argv = opal_argv_copy(mca_pls_poe_component.argv);
    argc = mca_pls_poe_component.argc;

    if(num_nodes > 0) {
       opal_argv_append(&argc, &argv, "-hostfile");
       opal_argv_append(&argc, &argv, mca_pls_poe_component.hostfile);
       opal_argv_append(&argc, &argv, "-resd");
       opal_argv_append(&argc, &argv, "no");
       rc=__poe_argv_append_int(&argc, &argv, num_nodes, 1, "-nodes");
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

    rc=__poe_argv_append_int(&argc, &argv, num_procs, 1, "-procs");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.mp_infolevel, 0, "-infolevel");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    if(mca_pls_poe_component.verbose>10) {
        opal_output(0, "%s:POE cmdline %s\n", __FUNCTION__, opal_argv_join(argv, ' '));
    }  

    /* Start job with POE */

    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if(pid == 0) {
        __poe_set_handler_default(SIGTERM);
        __poe_set_handler_default(SIGINT);
        __poe_set_handler_default(SIGHUP);
        __poe_set_handler_default(SIGCHLD);
        __poe_set_handler_default(SIGPIPE);
        sigprocmask(0, 0, &sigs);
        sigprocmask(SIG_UNBLOCK, &sigs, 0);
        execv(mca_pls_poe_component.path, argv);
        opal_output(0, "orte_pls_poe: execv failed with errno=%d\n", errno);
        exit(-1);
    } else {
        orte_wait_cb(pid, __poe_wait_job, NULL);
    }
   
cleanup:
    while(NULL != (item = opal_list_remove_first(&map))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&map);
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    if(mca_pls_poe_component.verbose>10) {
        opal_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
    } 
    return rc;
}

/**
pls_poe_launch - launch a POE job
@warning current support interactive class only!.
@param jobid JOB Identifier [IN]
@return error number
*/
static int pls_poe_launch(orte_jobid_t jobid)
{
    if(!strncmp(mca_pls_poe_component.class,"interactive",11)) {
        return __poe_launch_interactive(jobid);
    }
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int pls_poe_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int pls_poe_terminate_proc(const orte_process_name_t *name)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

/**
pls_poe_finalize - clean up tempolary files
@return error number
*/
static int pls_poe_finalize(void)
{
    if (mca_pls_poe_component.verbose > 10) {
       opal_output(0, "%s: --- BEGIN ---\n", __FUNCTION__);
    }

    unlink(mca_pls_poe_component.cmdfile);
    unlink(mca_pls_poe_component.hostfile);

    if (mca_pls_poe_component.verbose > 10) {
       opal_output(0, "%s: --- END ---\n", __FUNCTION__);
    }
    return ORTE_SUCCESS;
}
