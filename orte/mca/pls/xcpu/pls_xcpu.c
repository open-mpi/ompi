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
 * their OpenRTE name, their gpr and ns replicas, the universe
 * to which they belong, etc. - otherwise, they may run, but they
 * will never actually join the rest of the job. This function
 * creates the common environment for all the processes.
 */
static int orte_pls_xcpu_setup_env(char ***env)
{
    char *uri, *param;
    int rc;
    /** the append_nosize utility is kind enough to do whatever allocation is necessary
     * to start the argv array if it doesn't already exist, so we can just start "appending"
     * information to it
     */
    if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(env, "--universe"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(env, orte_universe_info.name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /** Since we may be doing this anywhere, we always check to see if we are
     * the replica, or if we need to link this process to somewhere else
     */
    if (NULL != orte_process_info.ns_replica_uri) {
        uri = strdup(orte_process_info.ns_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    free(uri);
    if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(env, "--nsreplica"))) {
        ORTE_ERROR_LOG(rc);
        free(param);
        return rc;
    }
    if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(env, param))) {
        ORTE_ERROR_LOG(rc);
        free(param);
        return rc;
    }
    free(param);
    /** do the same for the gpr */
    if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(env, "--gprreplica"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (NULL != orte_process_info.gpr_replica_uri) {
        uri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        uri = orte_rml.get_uri();
    }
    asprintf(&param, "\"%s\"", uri);
    free(uri);
    if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(env, param))) {
        ORTE_ERROR_LOG(rc);
        free(param);
        return rc;
    }
    free(param);
    return rc;
}


/**   LAUNCH   **/

/* This is the main function that will launch jobs on remote compute modes
 * @param jobid the jobid of the job to launch
 * @retval ORTE_SUCCESS or error
 */
int orte_pls_xcpu_launch(orte_jobid_t jobid){
    opal_list_t mapping;
    const orte_process_name_t* name;
    char **base_argv, **app_argv, **base_env=NULL, *param;
    char *header[] = {
        "dummy",
        NULL,
        NULL};
    int nprocs=0, argc;
    int rc, i=0, proc_id=0;
    orte_pls_xcpu_tid_stack *t_stack, *temp_stack;
    opal_list_item_t *item;
    orte_rmaps_base_proc_t *temp;
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

    /** since it is possible that each node could be executing a different application,
     * we cannot just do a mass launch - that would only be supported in the special
     * case of all the application processes being identical. Instead, we are going to
     * step our way through the list, launching each process individually. For each node,
     * however, we need to have an argv array that fully describes the respective
     * command line options -- *including* all those required by OpenRTE and Open MPI
     * to interconnect the processes to the rest of the job.
     *
     * First, therefore, let's construct an argv that contains all the OpenRTE and
     * Open MPI required information. We will later "merge" this into the argv for
     * each application prior to launch.
     */
    if (ORTE_SUCCESS != (rc = orte_pls_xcpu_setup_env(&base_env))) {
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

        /** augment the map's argv with our base environment */
        argc = opal_argv_count(map->app->argv);
        opal_argv_insert(&(map->app->argv), argc, base_env);

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

        /** Loop through each process in the map and launch it */

        /* now here.. do we want to pass all node-names and binary as
         * arguments to xcpu_launch or do we want to launch then one
         * by one, by providing only one node-name and binary at a time?
         */
        proc_id=0;
        /*for(temp = opal_list_get_first(&map->procs[0]);
            temp != opal_list_get_end(&map->procs[0]);
            temp = opal_list_get_next(temp)){
        */
        while(proc_id<map->num_procs){
            temp=map->procs[proc_id];
            proc_id++;
            proc = (orte_rmaps_base_proc_t*)temp;
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

            /** now add the process name to the argv */
            if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(&(map->app->argv), "--name"))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            name=&(proc->proc_name);
            if (ORTE_SUCCESS != (rc = orte_ns.get_proc_name_string(&param, name))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (OPAL_SUCCESS != (rc = opal_argv_append_nosize(&(map->app->argv), param))) {
                ORTE_ERROR_LOG(rc);
                free(param);
                return rc;
            }
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

            /** cleanup the app's argv. Only the last two locations need to be deleted
             * since everything else is common to all the applications (we let the
             * node name location be handled as above)
             */
            opal_argv_delete(&argc, &(map->app->argv), argc-2, 2);
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
    opal_argv_free(base_env);
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
