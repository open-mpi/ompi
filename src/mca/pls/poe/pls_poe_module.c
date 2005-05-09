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


static int pls_poe_launch(orte_jobid_t jobid)
{
    char **argv;
    int argc;
    int rc;
    int pid;
    char *tmp_string;
    ompi_list_t map;
    char* param;
    char* uri;
    char **new_env, **environ_copy;
    orte_vpid_t vpid_start;
    orte_vpid_t vpid_range;
    ompi_list_item_t* item;
    int i;
    orte_rmaps_base_map_t* map2;

    OBJ_CONSTRUCT(&map, ompi_list_t);

    rc = orte_rmaps_base_get_map(jobid,&map);
    if (ORTE_SUCCESS != rc) {
        /* ORTE_ERROR_LOG(rc); */
        goto cleanup;
    }

    rc = orte_rmaps_base_get_vpid_range(jobid, &vpid_start, &vpid_range);
    if (ORTE_SUCCESS != rc) {
        /* ORTE_ERROR_LOG(rc); */
        goto cleanup;
    }

    /* attempt to launch each of the apps */
    for(item =  ompi_list_get_first(&map);
        item != ompi_list_get_end(&map);
        item =  ompi_list_get_next(item)) {
        map2 = (orte_rmaps_base_map_t*)item; 


       /* set working directory */
       if(chdir((map2->app)->cwd) != 0) {
          /* ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM); */
       }

       /* setup base environment */
       environ_copy = ompi_argv_copy(environ);
       param = mca_base_param_environ_variable("rmgr","bootproxy","jobid");
       ompi_unsetenv(param, &environ_copy);

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
       orte_ns_nds_env_put(map2->procs[0]->proc_name, vpid_start, vpid_range, &environ_copy);

       new_env = ompi_environ_merge(map2->app->env, environ_copy);
       ompi_argv_free(environ_copy);

       /* POE command */
       argv = ompi_argv_copy(mca_pls_poe_component.argv);
       argc = mca_pls_poe_component.argc;

       /* POE argument */
       ompi_argv_append(&argc, &argv, "-procs");
       asprintf(&tmp_string, "%d", map2->num_procs);
       ompi_argv_append(&argc, &argv, tmp_string);
       free(tmp_string);

       /* Application */
       ompi_argv_append(&argc, &argv, map2->app->app);
       i=0;
       while(map2->app->argv[i]!=NULL) {
          ompi_argv_append(&argc, &argv, map2->app->argv[i++]);
       }

       for(i=0;i<argc;i++) printf("%s ", argv[i]); fflush(stdout);

       pid = fork();
       if(pid < 0) {
          /* ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE); */
          return ORTE_ERR_OUT_OF_RESOURCE;
       }

       if(pid == 0) {
           execve(mca_pls_poe_component.path, argv, new_env);
           ompi_output(0, "orte_pls_poe: %s - %s\n", map2->app->app,
           ompi_argv_join(map2->app->argv, ' '));
           ompi_output(0, "orte_pls_poe: execv failed with errno=%d\n", errno);
           exit(-1);
       } 

    }

cleanup:
    while(NULL != (item = ompi_list_remove_first(&map))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&map);
    printf("EXIT FROM POE_LAUNCH\n"); fflush(stdout);
    return rc;
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
