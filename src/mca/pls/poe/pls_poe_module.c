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

#include "util/argv.h"
#include "util/ompi_environ.h"
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

int pls_poe_launch_interactive(orte_jobid_t jobid)
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
    char *hostfile, *cmdfile;
    FILE *hfp, *cfp;
                                                                                                       
    /* query the list of nodes allocated to the job - don't need the entire
     * mapping - as the daemon/proxy is responsibe for determining the apps
     * to launch on each node.
     */

    if((hostfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cmdfile=tempnam(NULL,NULL))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((hfp=fopen(hostfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;
    if((cfp=fopen(cmdfile,"w"))==NULL) return ORTE_ERR_OUT_OF_RESOURCE;

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
     
    /* need integer value for command line parameter - NOT hex */
    asprintf(&tmp_string, "%lu", (unsigned long)jobid);
     
    /* application */
    argv = ompi_argv_copy(ompi_argv_split(mca_pls_poe_component.orted, ' '));
    argc = ompi_argv_count(argv);
    if (mca_pls_poe_component.debug) {
         ompi_argv_append(&argc, &argv, "--debug");
    }
     
   
    ompi_argv_append(&argc, &argv, "--bootproxy");
    ompi_argv_append(&argc, &argv, tmp_string);
    ompi_argv_append(&argc, &argv, "--name");
    proc_name_index = argc;
    ompi_argv_append(&argc, &argv, "");
    ompi_argv_append(&argc, &argv, "--nodename");
    node_name_index2 = argc;
    ompi_argv_append(&argc, &argv, "");
                                        
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
           printf("%s ",argv[i]);
           fprintf(cfp,"%s ",argv[i]); 
        }
        printf("\n"); fflush(stdout); 
        fprintf(cfp,"\n");
        vpid++;
        free(name);
    }
   
    fclose(cfp);
    fclose(hfp);

    argv = ompi_argv_copy(mca_pls_poe_component.argv);
    argc = mca_pls_poe_component.argc;
    ompi_argv_append(&argc, &argv, "-hostfile");
    ompi_argv_append(&argc, &argv, hostfile);
    ompi_argv_append(&argc, &argv, "-cmdfile");
    ompi_argv_append(&argc, &argv, cmdfile);
    ompi_argv_append(&argc, &argv, "-procs");
    asprintf(&tmp_string, "%d", num_nodes);
    ompi_argv_append(&argc, &argv, tmp_string);
    ompi_argv_append(&argc, &argv, "-pgmmodel");
    ompi_argv_append(&argc, &argv, "mpmd");
    ompi_argv_append(&argc, &argv, "-resd");
    ompi_argv_append(&argc, &argv, "no");
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.retry, 0, "-retry");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    rc=__poe_argv_append_int(&argc, &argv, mca_pls_poe_component.retrycount, 0, "-retrycount");
    if(ORTE_SUCCESS!=rc) { ORTE_ERROR_LOG(rc); goto cleanup; }
    ompi_output(0, "%s", ompi_argv_join(argv, ' '));

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
    }
     
             
     
cleanup:
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    return rc;
}





static int pls_poe_launch(orte_jobid_t jobid)
{
    if(!strncmp(mca_pls_poe_component.class,"interactive",11)) {
        return pls_poe_launch_interactive(jobid);
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
    return ORTE_ERR_NOT_IMPLEMENTED;
}
