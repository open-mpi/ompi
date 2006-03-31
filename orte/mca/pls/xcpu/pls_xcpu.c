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
int lrx(int argc, char **argv);
int get_argc(char **argv){
    int i=0;
    while(argv[i]){
        i++;
    }
    return i;
}

void free_stack(tid_stack *s){
    if(s){
        free_stack(s->next);
        free(s);
    }
}

/* This is the main function that will launch jobs on remote compute modes
 * @param jobid the jobid of the job to launch
 * @retval ORTE_SUCCESS or error
 */
int orte_pls_xcpu_launch(orte_jobid_t jobid){
    opal_list_t mapping;
    char **new_argv;
    int new_argc, nprocs=0;
    int rc, i=0;
    tid_stack *t_stack, *temp_stack;
    opal_list_item_t *item, *temp;
    orte_rmaps_base_map_t* map;
    /* first get the list of nodes on which we are going to launch job */
    /* OBJ_CONSTRUCT construct/initialize objects that are not dynamically allocated.
    * see file opal/class/opal_object.h for detils
    */
    /*fprintf(stdout, "\nxcpu launch called, job id: %d\n", jobid);*/
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    /* 1. get map from registry*/
    if(ORTE_SUCCESS != (rc = orte_rmaps_base_get_map(jobid, &mapping))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* 2. use the map to launch jobs*/
    map=(orte_rmaps_base_map_t*)opal_list_get_first(&mapping); 
    new_argc=get_argc(map->app->argv)+3;
    new_argv=(char**)malloc(new_argc*sizeof(char*));
    new_argv[0]=(char*)malloc(1);/*it could be anything ... doesn't matter*/
    for(i=2; i<new_argc; i++){
        new_argv[i]=map->app->argv[i-2];
        /*fprintf(stdout, "new_argv[%d]:%s\n", i, new_argv[i]);*/
    }
    new_argv[i]=NULL;
    /*printf("new_argv[%d] is nulled\n", i);*/
    t_stack=NULL;
    for(item =  opal_list_get_first(&mapping);
         item != opal_list_get_end(&mapping);
         item =  opal_list_get_next(item)) {
        map = (orte_rmaps_base_map_t*) item;
        /* now here.. do we want to pass all node-names and binary as
         * arguments to xcpu_launch or do we want to launch then one
         * by one, by providing only one node-name and binary at a time?
         */
        for(temp = opal_list_get_first(&map->nodes);
            temp != opal_list_get_end(&map->nodes);
            temp = opal_list_get_next(temp)){
            
            new_argv[1]=((orte_rmaps_base_node_t*)temp)->node->node_name;
            /*above should contain node name where process is to be launched*/
            /*fprintf(stdout, "node name: %s\n", new_argv[1]);*/
            nprocs=((orte_rmaps_base_node_t*)temp)->node_procs.opal_list_length;
            /*fprintf(stdout, "list length: %d\n", nprocs);*/
            for (i = 0; i<nprocs; ++i) { 
                temp_stack=(tid_stack*)malloc(sizeof(tid_stack));
                temp_stack->next=t_stack;
                t_stack=temp_stack;
                t_stack->tid=lrx(new_argc, new_argv); 
            }
        }
    }
    /* wait for all thrads that have launched processes on remote nodes
     * */
    temp_stack=t_stack;
    while(t_stack){
        pthread_join(t_stack->tid, NULL);
        t_stack=t_stack->next;
    }
    orte_soh.begin_monitoring_job(jobid);
    
    free_stack(temp_stack);
    free(new_argv[0]);
    /*free(new_argv[1]);*/
    free(new_argv);
    OBJ_DESTRUCT(&mapping);
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
