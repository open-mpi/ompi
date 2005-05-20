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

#include "util/argv.h"
#include "util/ompi_environ.h"
#include "pls_poe.h"


extern char **environ;
orte_jobid_t global_jobid;


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

int __poe_argv_append_int(int *argc, char ***argv, int varname, int min, char *argname) 
{
    char *tmp_string;
    if(varname >= min) {
        ompi_argv_append(argc, argv, argname);
        asprintf(&tmp_string, "%d", varname);
        ompi_argv_append(argc, argv, tmp_string);
        free(tmp_string);
    } else {
        return ORTE_ERR_BAD_PARAM;
    } 
    return ORTE_SUCCESS;
}

int pls_poe_launch_interactive_orted(orte_jobid_t jobid)
{
    ompi_list_t nodes;
    ompi_list_item_t* item;
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
    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);

    if((mca_pls_poe_component.hostfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((mca_pls_poe_component.cmdfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((hfp=fopen(mca_pls_poe_component.hostfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cfp=fopen(mca_pls_poe_component.cmdfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;

    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }
                                                                                                       
    /*
     * Allocate a range of vpids for the daemons.
     */
     
    num_nodes = ompi_list_get_size(&nodes);
    if(num_nodes == 0) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }
     
    /* application */
    argv = ompi_argv_copy(ompi_argv_split(mca_pls_poe_component.orted, ' '));
    argc = ompi_argv_count(argv);
    if (mca_pls_poe_component.debug) {
         ompi_argv_append(&argc, &argv, "--debug");
    }
    ompi_argv_append(&argc, &argv, "--debug-daemons");
     
    ompi_argv_append(&argc, &argv, "--no-daemonize");
    ompi_argv_append(&argc, &argv, "--bootproxy");
    /* need integer value for command line parameter - NOT hex */
    asprintf(&tmp_string, "%lu", (unsigned long)jobid);
    ompi_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);
    ompi_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    ompi_argv_append(&argc, &argv, "");
    ompi_argv_append(&argc, &argv, "--nodename");
    node_name_index2 = argc;
    ompi_argv_append(&argc, &argv, "");

    /* pass along the universe name and location info */
    ompi_argv_append(&argc, &argv, "--universe");
    asprintf(&tmp_string, "%s@%s:%s", orte_universe_info.uid,
    orte_universe_info.host, orte_universe_info.name);
    ompi_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);

                                        
    /* setup ns contact info */
    ompi_argv_append(&argc, &argv, "--nsreplica");
    if(NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, param);
    free(uri);
                                           
    /* setup gpr contact info */
    ompi_argv_append(&argc, &argv, "--gprreplica");
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    ompi_argv_append(&argc, &argv, param);
    free(uri);
     
    /*
     * Iterate through each of the nodes and spin
     * up a daemon.
     */
     
    for(item =  ompi_list_get_first(&nodes);
        item != ompi_list_get_end(&nodes);
        item =  ompi_list_get_next(item)) {
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
            ompi_output(0, "orte_pls_poe: unable to create process name");
            exit(-1);
        }
        argv[proc_name_index] = name_string;
        for(i=0;i<argc;i++) {
           fprintf(cfp,"%s ",argv[i]); 
        }
        fprintf(cfp,"\n");

        if (mca_pls_poe_component.verbose) {
           ompi_output(0, "%s:cmdfile %s\n", __FUNCTION__, ompi_argv_join(argv, ' '));
        }  
        vpid++;
        free(name);
    }
   
    fclose(cfp);
    fclose(hfp);

    argv = ompi_argv_copy(mca_pls_poe_component.argv);
    argc = mca_pls_poe_component.argc;
    ompi_argv_append(&argc, &argv, "-hostfile");
    ompi_argv_append(&argc, &argv, mca_pls_poe_component.hostfile);
    ompi_argv_append(&argc, &argv, "-cmdfile");
    ompi_argv_append(&argc, &argv, mca_pls_poe_component.cmdfile);
    ompi_argv_append(&argc, &argv, "-procs");
    asprintf(&tmp_string, "%d", num_nodes);
    ompi_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);
    ompi_argv_append(&argc, &argv, "-pgmmodel");
    ompi_argv_append(&argc, &argv, "mpmd");
    ompi_argv_append(&argc, &argv, "-resd");
    ompi_argv_append(&argc, &argv, "no");
    ompi_argv_append(&argc, &argv, "-labelio");
    ompi_argv_append(&argc, &argv, "yes");
    ompi_argv_append(&argc, &argv, "-infolevel");
    ompi_argv_append(&argc, &argv, "6");
    ompi_argv_append(&argc, &argv, "-stdoutmode");
    ompi_argv_append(&argc, &argv, "ordered");
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    if (mca_pls_poe_component.verbose) {
       ompi_output(0, "%s:cmdline %s\n", __FUNCTION__, ompi_argv_join(argv, ' '));
    }  

    pid = fork();
    if(pid < 0) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
     
    /* child */
    if(pid == 0) {
       execv(mca_pls_poe_component.path, argv);
       ompi_output(0, "orte_pls_poe: execv failed with errno=%d\n", errno);
       exit(-1);
    } else {
    }
     
cleanup:
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
    return rc;
}




int orte_pls_poe_wait_proc(pid_t pid, int status, void* cbdata)
{
    orte_proc_state_t state;
    orte_gpr_value_t *value;
    int rc;
    size_t i;

    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);

    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    value->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND;
    if(WIFEXITED(status)) {
       state=ORTE_PROC_STATE_TERMINATED;
    } else {
       state=ORTE_PROC_STATE_ABORTED;
    }

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), global_jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    value->tokens = NULL; /* set everyone on job segment to this status */

    value->cnt = 2;
    value->keyvals = (orte_gpr_keyval_t**)malloc(2 * sizeof(orte_gpr_keyval_t*));
    if (NULL == value->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (value->keyvals[0])->key = strdup(ORTE_PROC_STATE_KEY);
    (value->keyvals[0])->type = ORTE_PROC_STATE;
    (value->keyvals[0])->value.proc_state = state;
                                
    value->keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (value->keyvals[1])->key = strdup(ORTE_PROC_EXIT_CODE_KEY);
    (value->keyvals[1])->type = ORTE_EXIT_CODE;
    (value->keyvals[1])->value.exit_code = status;
                                
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(value);
                       
    /* check to see if we need to increment orte-standard counters */
    /* first, cleanup value so it can be used for that purpose */
                                
    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->addr_mode = ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_AND;
                                
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), global_jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(NULL == (value->tokens = (char**)malloc(sizeof(char*)))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    value->num_tokens = 1;

    /* If we're setting ABORTED, we're also setting TERMINATED, so we
       need 2 keyvals.  Everything else only needs 1 keyval. */
                                
    value->cnt = 1;
    if (ORTE_PROC_STATE_ABORTED == state) {
        ++value->cnt;
    }
    value->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*) *
                                                 value->cnt);
    if (NULL == value->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i = 0; i < value->cnt; ++i) {
        value->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value->keyvals[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_RELEASE(value);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        (value->keyvals[i])->type = ORTE_NULL;
    }
     
    /* see which state we are in - let that determine the counter, if any */
    switch (state) {
        case ORTE_PROC_STATE_TERMINATED:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_TERMINATED);
            break;
     
        case ORTE_PROC_STATE_ABORTED:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_ABORTED);
            (value->keyvals[1])->key = strdup(ORTE_PROC_NUM_TERMINATED);
            break;
    }
    if (NULL != (value->keyvals[0])->key) { /* need to increment a counter */
        if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return rc;
        }
    }

    OBJ_RELEASE(value);
    if (mca_pls_poe_component.verbose>10) ompi_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);

    return rc;
}


static int orte_pls_poe_launch_create_cmd_file(
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

    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);

    /* setup base environment */
    environ_copy = NULL;
    param = mca_base_param_environ_variable("rmgr","bootproxy","jobid");
    ompi_unsetenv(param, &environ_copy);

    /* setup universe info */
    if (NULL != orte_universe_info.name) {
        param = mca_base_param_environ_variable("universe", NULL, NULL);
        asprintf(&uri, "%s@%s:%s", orte_universe_info.uid,
                                  orte_universe_info.host,
                                  orte_universe_info.name);
        ompi_setenv(param, uri, true, &environ_copy);
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
    ompi_setenv(param, uri, true, &environ_copy);
    free(param);
    free(uri);

    /* setup gpr contact info */
    if(NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    param = mca_base_param_environ_variable("gpr","replica","uri");
    ompi_setenv(param, uri, true, &environ_copy);
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
    ompi_argv_free(environ_copy);
    fprintf(cfp," %s",context->app);
    i=1;
    while(context->argv[i]!=NULL) {
       fprintf(cfp," %s",context->argv[i++]);
    } 
    fprintf(cfp,"\n"); /* POE will upset if the file doesn't contain end of line. */

    if (mca_pls_poe_component.verbose>10) ompi_output(0, "%s: --- END ---\n", __FUNCTION__);

    return ORTE_SUCCESS;
}

int orte_pls_poe_launch_interactive(orte_jobid_t jobid)
{
    ompi_list_t map;
    ompi_list_item_t* item;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;
    size_t num_nodes, num_procs;
    ompi_list_t nodes;
    char *tmp_string;
    int rc;
    FILE *hfp, *cfp;
    char** argv;
    int status;
    int argc;
    int pid;

    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s:--- BEGIN ---\n", __FUNCTION__);

    if((mca_pls_poe_component.cmdfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cfp=fopen(mca_pls_poe_component.cmdfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;

    global_jobid = jobid;

    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    rc = orte_ras_base_node_query_alloc(&nodes, jobid);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }
     
    num_nodes = ompi_list_get_size(&nodes);
    if(0 < num_nodes) { 
         /* If user specify hosts */
        if((mca_pls_poe_component.hostfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
        if((hfp=fopen(mca_pls_poe_component.hostfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
        for(item =  ompi_list_get_first(&nodes);
           item != ompi_list_get_end(&nodes);
           item =  ompi_list_get_next(item)) {
           orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
           fprintf(hfp,"%s\n",node->node_name); 
        }
        fclose(hfp);
    }

    rc = orte_rmgr_base_get_job_slots(jobid, &num_procs);
    if(ORTE_SUCCESS != rc) {
        return rc;
    }
    
    /* query allocation for the job */
    OBJ_CONSTRUCT(&map, ompi_list_t);
    rc = orte_rmaps_base_get_map(jobid,&map);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    rc = orte_rmaps_base_get_vpid_range(jobid, &vpid_start, &vpid_range);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* setup a POE command file */
    for(item =  ompi_list_get_first(&map);
        item != ompi_list_get_end(&map);
        item =  ompi_list_get_next(item)) {
        orte_rmaps_base_map_t* map2 = (orte_rmaps_base_map_t*)item;
        size_t i;
        for(i=0; i<map2->num_procs; i++) {
            rc = orte_pls_poe_launch_create_cmd_file(cfp,map2->app, map2->procs[i], vpid_start, vpid_range);
            if(ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
        }
    }
    fclose(cfp);

    /********************/
    /* POE Command line */
    /********************/
    argv = ompi_argv_copy(mca_pls_poe_component.argv);
    argc = mca_pls_poe_component.argc;

    if(num_nodes > 0) {
       ompi_argv_append(&argc, &argv, "-hostfile");
       ompi_argv_append(&argc, &argv, mca_pls_poe_component.hostfile);
       ompi_argv_append(&argc, &argv, "-nodes");
       asprintf(&tmp_string, "%d", num_nodes);
       ompi_argv_append(&argc, &argv, tmp_string);
       free(tmp_string);
       ompi_argv_append(&argc, &argv, "-resd");
       ompi_argv_append(&argc, &argv, "no");
    }

    ompi_argv_append(&argc, &argv, "-cmdfile");
    ompi_argv_append(&argc, &argv, mca_pls_poe_component.cmdfile);
    ompi_argv_append(&argc, &argv, "-procs");
    asprintf(&tmp_string, "%d", num_procs);
    ompi_argv_append(&argc, &argv, tmp_string);
    free(tmp_string);
    ompi_argv_append(&argc, &argv, "-pgmmodel");
    ompi_argv_append(&argc, &argv, "mpmd");
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    /* FIXME: Debugging only! */
    ompi_argv_append(&argc, &argv, "-labelio");
    ompi_argv_append(&argc, &argv, "yes");
    ompi_argv_append(&argc, &argv, "-infolevel");
    ompi_argv_append(&argc, &argv, "6");
    ompi_argv_append(&argc, &argv, "-stdoutmode");
    ompi_argv_append(&argc, &argv, "ordered");

    if (mca_pls_poe_component.verbose) {
       ompi_output(0, "%s:cmdline %s\n", __FUNCTION__, ompi_argv_join(argv, ' '));
    }  

    pid = fork();
    if(pid < 0) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if(pid == 0) {
       execv(mca_pls_poe_component.path, argv);
       ompi_output(0, "orte_pls_poe: execv failed with errno=%d\n", errno);
       exit(-1);
    } else {
       orte_wait_cb(pid, orte_pls_poe_wait_proc, NULL);
    }
   
cleanup:
    while(NULL != (item = ompi_list_remove_first(&map))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&map);
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    if (mca_pls_poe_component.verbose>10) ompi_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
    return rc;
}





static int pls_poe_launch(orte_jobid_t jobid)
{
    if(!strncmp(mca_pls_poe_component.class,"interactive",11)) {
        return orte_pls_poe_launch_interactive(jobid);
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


static int pls_poe_finalize(void)
{
    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s: --- BEGIN ---\n", __FUNCTION__);
    unlink(mca_pls_poe_component.cmdfile);
    unlink(mca_pls_poe_component.hostfile);
    if (mca_pls_poe_component.verbose > 10) ompi_output(0, "%s: --- END ---\n", __FUNCTION__);
    return ORTE_SUCCESS;
}
