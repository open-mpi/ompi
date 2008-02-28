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
#include "orte/constants.h"

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
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/univ_info.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

/* remove for ORTE 2.0 */
#include "orte/mca/sds/base/base.h"

#include "orte/mca/plm/plm.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/poe/plm_poe.h"

/*
 * Local functions
 */
static int plm_poe_init(void);
static int plm_poe_launch_job(orte_job_t *jdata);
static int plm_poe_terminate_job(orte_jobid_t jobid);
static int plm_poe_terminate_orteds(void);
static int plm_poe_signal_job(orte_jobid_t jobid, int32_t signal);
static int plm_poe_finalize(void);

orte_plm_base_module_t orte_plm_poe_module = {
    plm_poe_init,
    orte_plm_base_set_hnp_name,
    plm_poe_launch_job,
    plm_poe_terminate_job,
    plm_poe_terminate_orteds,
    plm_poe_signal_job,
    plm_poe_finalize
};

/**
 * Init the module
 */
int plm_poe_init(void)
{
    return ORTE_SUCCESS;
}
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

int plm_poe_launch_interactive_orted(orte_job_t *jdata)
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
     * All other mapping responsibilities fall to orted in the fork PLM
     */

    if((mca_plm_poe_component.hostfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((mca_plm_poe_component.cmdfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((hfp=fopen(mca_plm_poe_component.hostfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cfp=fopen(mca_plm_poe_component.cmdfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;

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
    argv = opal_argv_copy(opal_argv_split(mca_plm_poe_component.orted, ' '));
    argc = opal_argv_count(argv);
    if (mca_plm_poe_component.debug) {
         opal_argv_append(&argc, &argv, "--debug");
    }
    opal_argv_append(&argc, &argv, "--debug-daemons");

    /* need integer value for command line parameter - NOT hex */
    asprintf(&tmp_string, "%lu", (unsigned long)jobid);

    /* Add basic orted command line options */
    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          &proc_name_index,
                                          &node_name_index2,
                                          tmp_string,
                                          num_nodes
                                          );
    free(tmp_string);

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
        rc = orte_ns.create_process_name(&name, 0, vpid);
        if(ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        /* setup process name */
        rc = orte_ns.get_proc_name_string(&name_string, name);
        if(ORTE_SUCCESS != rc) {
            opal_output(0, "orte_plm_poe: unable to create process name");
            return rc;
        }
        argv[proc_name_index] = name_string;
        for(i=0;i<argc;i++) {
           fprintf(cfp,"%s ",argv[i]);
        }
        fprintf(cfp,"\n");

        if (mca_plm_poe_component.verbose) {
           opal_output(0, "%s:cmdfile %s\n", __FUNCTION__, opal_argv_join(argv, ' '));
        }
        vpid++;
        free(name);
    }

    fclose(cfp);
    fclose(hfp);

    argv = opal_argv_copy(mca_plm_poe_component.argv);
    argc = mca_plm_poe_component.argc;
    opal_argv_append(&argc, &argv, "-hostfile");
    opal_argv_append(&argc, &argv, mca_plm_poe_component.hostfile);
    opal_argv_append(&argc, &argv, "-cmdfile");
    opal_argv_append(&argc, &argv, mca_plm_poe_component.cmdfile);
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
    rc=poe_argv_append_int(&argc, &argv, mca_plm_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_plm_poe_component.mp_retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    if (mca_plm_poe_component.verbose) {
       opal_output(0, "%s:cmdline %s\n", __FUNCTION__, opal_argv_join(argv, ' '));
    }

    pid = fork();
    if(pid < 0) {
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }

    /* child */
    if(pid == 0) {
       execv(mca_plm_poe_component.path, argv);
       opal_output(0, "orte_plm_poe: execv failed with errno=%d\n", errno);
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

    if (mca_plm_poe_component.verbose > 10) opal_output(0, "%s: --- END rc(%d) ---\n", __FUNCTION__, rc);
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
    orte_std_cntr_t nnode, nproc;
    orte_node_t **nodes;
    orte_proc_t **procs;
    int rc;

    /* query allocation for the job */
    if (NULL == (map = orte_rmaps.get_job_map(mca_plm_poe_component.jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
    }
    nodes = (orte_node_t**)map->nodes->addr;

    for(nnode=0; nnode < map->num_nodes; nnode++) {
        orte_node_t* node = nodes[nnode];

        procs = (orte_proc_t**)node->procs->addr;
        for (nproc=0; nproc < node->num_procs; nproc++) {
            orte_proc_t* proc = procs[nproc];
            
            orte_session_dir_finalize(&(proc->name));
            proc->state = ORTE_PROC_STATE_ABORTED,;
        }
    }
}

/**
poe_create_cmd_file - create POE command file
@param cfp command file pointer [IN]
@param context context [IN]
@param proc proc [IN]
@param vpid_range vpid range [IN]
@return error number
*/
static int poe_create_cmd_file(
    FILE *cfp,
    orte_app_context_t* context,
    orte_proc_t* proc,
    orte_vpid_t vpid_range)
{
    int i;

    char* param;
    char* param2;
    char **environ_copy;

    /* setup base environment */
    environ_copy = NULL;

    /* setup hnp contact info */
    param2 = orte_rml.get_contact_info();
    param = mca_base_param_environ_variable("orte","hnp","uri");
    opal_setenv(param, param2, true, &environ_copy);
    free(param);
    free(param2);

    /* push data into environment */
    orte_sds_env_put(vpid_range, ORTE_VPID_INVALID, &environ_copy);

    /* pass the jobid */
    orte_util_convert_jobid_to_string(&param2, proc->name.jobid);
    param = mca_base_param_environ_variable("orte","sds","jobid");
    opal_setenv(param, uri, true, &environ_copy);
    free(param);
    free(param2);
    
    /* pass the vpid */
    orte_util_convert_vpid_to_string(&param2, proc->name.vpid);
    param = mca_base_param_environ_variable("orte","sds","vpid");
    opal_setenv(param, uri, true, &environ_copy);
    free(param);
    free(param2);

    if (context->argv == NULL) {
        context->argv = malloc(sizeof(char*)*2);
        context->argv[0] = strdup(context->app);
        context->argv[1] = NULL;
    }

    i=0;
    fprintf(cfp,"%s",mca_plm_poe_component.env);
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
static inline int poe_launch_interactive_job(orte_job_t *jdata)
{
    orte_job_map_t *map;
    orte_std_cntr_t nnode, nproc;
    FILE *hfp, *cfp;
    char** argv;
    int argc;
    int rc, pid;
    sigset_t sigs;
    orte_node_t **nodes;
    orte_proc_t **procs;
    orte_app_context_t **apps;
    
    if( (NULL==(mca_plm_poe_component.cmdfile=tempnam(NULL,NULL))) ||
        (NULL==(cfp=fopen(mca_plm_poe_component.cmdfile,"w"))) ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    mca_plm_poe_component.jobid = jdata->jobid;

    /* get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(active_job))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }
    nodes = (orte_node_t**)map->nodes->addr;
    apps = (orte_app_context_t**)jdata->apps->addr;
    
    if(!strncmp(mca_plm_poe_component.resource_allocation,"hostfile",8)) {

        /* Create a temporary hostlist file if user specify */

        if( (NULL==(mca_plm_poe_component.hostfile=tempnam(NULL,NULL))) ||
            (NULL==(hfp=fopen(mca_plm_poe_component.hostfile,"w"))) ) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        for(nnode=0; nnode < map->num_nodes; nnode++) {
            fprintf(hfp,"%s\n",nodes[nnode]->name);
        }
        fclose(hfp);
    }

    /* Create a temporary POE command file */

    for(nnode=0; nnode < map->num_nodes; nnode++) {
        orte_node_t* node = nodes[nnode];
        procs = (orte_proc_t**)node->procs->addr;
        for (nproc=0; nproc < node->num_procs; nproc++) {
            rc = poe_create_cmd_file(cfp, apps[procs[nproc]->app_idx], procs[nproc], jdata->num_procs);
            if(ORTE_SUCCESS != rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
        }
    }
    fclose(cfp);

    /* Generate POE command line */

    argv = opal_argv_copy(mca_plm_poe_component.argv);
    argc = mca_plm_poe_component.argc;

    if(!strncmp(mca_plm_poe_component.resource_allocation,"hostfile",8)) {
       opal_argv_append(&argc, &argv, "-hostfile");
       opal_argv_append(&argc, &argv, mca_plm_poe_component.hostfile);
       opal_argv_append(&argc, &argv, "-resd");
       opal_argv_append(&argc, &argv, "no");
       rc=poe_argv_append_int(&argc, &argv, map->num_nodes, 1, "-nodes");
       if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    }

    opal_argv_append(&argc, &argv, "-pgmmodel");
    opal_argv_append(&argc, &argv, "mpmd");
    opal_argv_append(&argc, &argv, "-cmdfile");
    opal_argv_append(&argc, &argv, mca_plm_poe_component.cmdfile);
    opal_argv_append(&argc, &argv, "-labelio");
    opal_argv_append(&argc, &argv, mca_plm_poe_component.mp_labelio);
    opal_argv_append(&argc, &argv, "-stdoutmode");
    opal_argv_append(&argc, &argv, mca_plm_poe_component.mp_stdoutmode);

    rc=poe_argv_append_int(&argc, &argv, jdata->num_procs, 1, "-procs");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_plm_poe_component.mp_retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_plm_poe_component.mp_retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=poe_argv_append_int(&argc, &argv, mca_plm_poe_component.mp_infolevel, 0, "-infolevel");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }

    if(mca_plm_poe_component.verbose>10) {
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
        execv(mca_plm_poe_component.path, argv);
        opal_output(0, "orte_plm_poe: execv failed with errno=%d\n", errno);
        exit(-1);
    } else {
        orte_wait_cb(pid, poe_wait_job, NULL);
    }


cleanup:
    OBJ_RELEASE(map);
    
    return rc;
}

/**
plm_poe_launch - launch a POE job
@warning current support interactive class only!.
@param jobid JOB Identifier [IN]
@return error number
*/
static int plm_poe_launch_job(orte_job_t *jdata)
{
    if(0 == strncmp(mca_plm_poe_component.class,"interactive",11)) {
        return poe_launch_interactive_job(jdata);
    }
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int plm_poe_terminate_job(orte_jobid_t jobid)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


static int plm_poe_terminate_orteds(void)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int plm_poe_signal_job(orte_jobid_t jobid, int32_t signal)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}


/**
plm_poe_finalize - clean up temporary files
@return error number
*/
static int plm_poe_finalize(void)
{
    unlink(mca_plm_poe_component.cmdfile);
    unlink(mca_plm_poe_component.hostfile);
    return ORTE_SUCCESS;
}
