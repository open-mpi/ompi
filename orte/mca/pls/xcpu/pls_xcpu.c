/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
 */

/* @file:
 * xcpu Lancher to launch jobs on compute nodes..
 */

#include "orte_config.h"
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif  /* HAVE_SYS_STAT_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#include <errno.h>
#include <signal.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif  /* HAVE_STRING_H */

#include "opal/event/event.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/opal_environ.h"
#include "opal/util/path.h"
#include "opal/util/show_help.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/sds/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/ras/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/rmaps/base/base.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/soh/base/base.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/runtime.h"

#include "pls_xcpu.h"

/**
 * Our current evironment
 */
extern char **environ;

/**
 * Initialization of the xcpu module with all the needed function pointers
 */
orte_pls_base_module_t orte_pls_xcpu_module = {
    orte_pls_xcpu_launch,
    orte_pls_xcpu_terminate_job,
    orte_pls_xcpu_terminate_proc,
    orte_pls_xcpu_finalize
};

/** include a prototype for the xcpu launch function */
int lrx(int argc, char **argv);

/**   LOCAL SUPPORT FUNCTIONS   **/

/** provide a local function to release the function stack
 * required by xcpu
 */
static void orte_pls_xcpu_free_stack(orte_pls_xcpu_tid_stack *s){
    if(s){
        orte_pls_xcpu_free_stack(s->next);
        free(s);
    }
}

/** provide a function to setup the environment for the remote
 * processes. We need to ensure that the remote processes know
 * their gpr and ns replicas, the universe
 * to which they belong, etc. - otherwise, they may run, but they
 * will never actually join the rest of the job. This function
 * creates the common environment for all the processes.
 *
 * @param env a pointer to the environment to setup
 */
static int orte_pls_xcpu_setup_env(char ***env)
{
    char ** merged;
    char * var;
    char * param;
    int rc;
    int num_env;

    num_env = opal_argv_count(*env);
    /** append mca parameters to our environment */
    if(ORTE_SUCCESS != (rc = mca_base_param_build_env(env, &num_env, false))) {
        ORTE_ERROR_LOG(rc);
    }

    /** ns replica contact info */
    if (NULL != orte_process_info.ns_replica) {
        param = strdup(orte_process_info.ns_replica_uri);
    } else {
        param = orte_rml.get_uri();
    }
    var = mca_base_param_environ_variable("ns","replica","uri");
    opal_setenv(var, param, true, env);
    free(var);
    var = mca_base_param_environ_variable("ns","replica","uri");
    opal_setenv(var, param, true, env);
    free(var);

    /** make sure the frontend hostname does not get pushed out to the backend */
    var = mca_base_param_environ_variable("orte", "base", "nodename");
    opal_unsetenv(var, env);
    free(var);
    opal_unsetenv("HOSTNAME", env);

    /** gpr replica contact info */
    if (NULL != orte_process_info.gpr_replica) {
        param = strdup(orte_process_info.gpr_replica_uri);
    } else {
        param = orte_rml.get_uri();
    }
    var = mca_base_param_environ_variable("gpr","replica","uri");
    opal_setenv(var, param, true, env);
    free(param);
    free(var);

    /** universe name */
    var = mca_base_param_environ_variable("universe", NULL, NULL);
    asprintf(&param, "%s@%s:%s", orte_universe_info.uid,
              orte_universe_info.host, orte_universe_info.name);
    opal_setenv(var, param, true, env);
    free(param);
    free(var);

    /** merge in environment */
    merged = opal_environ_merge(*env, environ);
    opal_argv_free(*env);
    *env = merged;

    /** make sure hostname doesn't get pushed to backend node */
    opal_unsetenv("HOSTNAME", env);

    return ORTE_SUCCESS;
}


/**   LAUNCH   **/

/* This is the main function that will launch jobs on remote compute modes
 * @param jobid the jobid of the job to launch
 * @retval ORTE_SUCCESS or error
 */
int orte_pls_xcpu_launch(orte_jobid_t jobid){
    opal_list_t mapping;
    char *param, *var;
    char *header[] = {
        "dummy",
        NULL,
        NULL};
    int argc;
    int rc;
    size_t nprocs=0, proc_id=0;
    orte_pls_xcpu_tid_stack *t_stack, *temp_stack;
    opal_list_item_t *item;
    orte_rmaps_base_map_t* map;
    orte_rmaps_base_node_t *node;
    orte_rmaps_base_proc_t *proc;

    /** first get the mapping we are going to use to launch job. The head
     * of the list is OBJ_CONSTRUCT'd since it is not dynamically allocated. The
     * get_map function, however, will dynamically allocate the items in the
     * list itself - these will be released when we OBJ_DESTRUCT the list at
     * the end
     */
    /*fprintf(stdout, "\nxcpu launch called, job id: %d\n", jobid);*/
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    /** get the mapping from the registry. This will provide a linked list, one
     * item for each mapping. Each item contains the full context of the application
     * that is to be executed upon that node. In particular, we need to obtain
     * the argv array that is included in that context as this tells us the application
     * to launch plus any "flags" to pass to it.
     */
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_map(jobid, &mapping))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /** we have to do the following so that we can use the opal_argv utilities
     * to properly insert the header into the app's argv
     */
    header[1] = strdup("dummy");

    /** Now loop through all the provided maps to launch their associated apps */
    t_stack=NULL;
    nprocs = 0;
    for(item =  opal_list_get_first(&mapping);
         item != opal_list_get_end(&mapping);
         item =  opal_list_get_next(item)) {
        map = (orte_rmaps_base_map_t*) item;

        /** xcpu requires an argv format that has a dummy filler in the
         * first location, followed by the node name, and then the standard
         * argv array we've all come to know and love (i.e., the application
         * name followed by options). We use the opal_argv utilities to
         * prepend this header info to the application's argv.
         *
         * Note: at this point, the header contains a dummy placeholder
         * for the node name - we'll fill that in later.
         */
        opal_argv_insert(&(map->app->argv), 0, header);


        /** since it is possible that each node could be executing a different application,
         * we cannot just do a mass launch - that would only be supported in the special
         * case of all the application processes being identical. Instead, we are going to
         * step our way through the list, launching each process individually.
         */
       proc_id=0;
        while (proc_id < map->num_procs){
            proc_id++;
            proc = (orte_rmaps_base_proc_t*)(map->procs[proc_id]);
            node = proc->proc_node;

            /** each proc_t entry contains the application to be executed,
             * the node upon which it is to be executed, and its OpenRTE
             * process name (plus a few other things). We use that
             * info to build the launch command by inserting them into
             * the argv array
             */

            /** start by pointing the proper location at the node name where
             * this process is to be launched
             */
            if (NULL != map->app->argv[1]) free(map->app->argv[1]);
            map->app->argv[1] = strdup(node->node->node_name);

            /** we also need to pass the proper environment to the remote
             * process so it knows its universe, gpr and ns replicas, etc. Since this
             * can be specified by the user for each app, we have to do this
             * each time.
             */
            if (ORTE_SUCCESS != (rc = orte_pls_xcpu_setup_env(&map->app->env))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

            /** now add the process name to the environment so we can
             * retrieve it on the other end
             */
            if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&param, &(proc->proc_name)))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            var = mca_base_param_environ_variable("ns", "nds", "name");
            opal_setenv(var, param, true, &(map->app->env));
            free(var);
            free(param);

            /** the launcher wants to know how long the argv array is - get that now */
            argc = opal_argv_count(map->app->argv);

            /** add this process to the stack so we can track it */
            temp_stack=(orte_pls_xcpu_tid_stack*)malloc(sizeof(orte_pls_xcpu_tid_stack));
            temp_stack->next=t_stack;
            t_stack=temp_stack;

            /** launch the process */
   /*         i=0;
            while(i<argc){
                printf("%s ", (map->app->argv)[i]);
                i++;
            }
            printf("\n");
 */
            t_stack->tid=lrx(argc, map->app->argv);
        }
    }

    /** wait for all threads that have launched processes on remote nodes */
    temp_stack=t_stack;
    while(t_stack){
        pthread_join(t_stack->tid, NULL);
        t_stack=t_stack->next;
    }
    orte_soh.begin_monitoring_job(jobid);

    /** cleanup local storage */
    orte_pls_xcpu_free_stack(temp_stack);
    OBJ_DESTRUCT(&mapping);

    /** launch complete */
    /*fprintf(stdout, "launch finished\n");*/
    return ORTE_SUCCESS;
}

int orte_pls_xcpu_terminate_job(orte_jobid_t jobid){
    return ORTE_SUCCESS;
}
int orte_pls_xcpu_terminate_proc(const orte_process_name_t* proc_name){
    return ORTE_SUCCESS;
}
int orte_pls_xcpu_finalize(void){
    return ORTE_SUCCESS;
}
