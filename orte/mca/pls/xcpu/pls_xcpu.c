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
 * Copyright (c) 2004-2006 The Regents of the University of California.
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
#include <regex.h>
#include <dirent.h>
/**
 * Our current evironment
 */
extern char **environ;
extern int errno;

char **g_environ;
int g_regexploc=1;
regex_t g_compiled_exp;
orte_pls_xcpu_mount_nodes *g_current_m=NULL;
orte_pls_xcpu_thread_info *g_thread_info;
orte_pls_xcpu_pthread_tindex t_info;
orte_pls_xcpu_stdio_thread_info *g_stdout_thread_info, *g_stderr_thread_info;
pthread_mutex_t mymutex = PTHREAD_MUTEX_INITIALIZER;

orte_pls_xcpu_pthread_tindex *orte_pls_xcpu_launch_procs(int, char **, char**, orte_process_name_t *);
int orte_pls_xcpu_cmd_check(int, char **);
void orte_pls_xcpu_cleanup();
void *orte_pls_xcpu_start_thread(void *);
void *orte_pls_xcpu_stdio_thread(void *);
int orte_pls_xcpu_check_exp(char *);

/**
 * Initialization of the xcpu module with all the needed function pointers
 */
orte_pls_base_module_t orte_pls_xcpu_module = {
    orte_pls_xcpu_launch,
    orte_pls_xcpu_terminate_job,
    orte_pls_xcpu_terminate_proc,
    orte_pls_xcpu_finalize
};

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

/* for handling stdout/err */
void *orte_pls_xcpu_stdio_thread(void *info){
    orte_pls_xcpu_stdio_thread_info *io_t_info;
    char buf[100];int x, rc;
    io_t_info = (orte_pls_xcpu_stdio_thread_info*)info;
    if((x=open(io_t_info->stdio_path, O_RDONLY))<0){
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
    }else{
    while(1){
        if((rc=read(x, buf, 100))>0){
            write(io_t_info->outdes, buf, rc);
        }else{
            if(rc==-1){
                ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
            }
            break;
        }
    }
    }
    x>=0?close(x):0;
    free(io_t_info->stdio_path);
    free(io_t_info);
    pthread_exit(NULL);
}
 
/* used by orte_pls_xcpu_launch_procs to start process
 * on remote compute node.
 * one thread per process for time being
 *
 * @info: contains all the information required by thread
 *        to launch process on remote compute node.
 */
void *orte_pls_xcpu_start_thread(void *info){
    orte_pls_xcpu_thread_info *t_info;
    char *session_clone, session_dir[255], *session_dir_path;
    int clone_des, rc=0, des1, des2/*, tdes*/, trc[2];
    char *env_path, *exec_path, *argv_path, *ctl_path;
    char character[8193];
    int i;
    orte_process_name_t *peers;
    pthread_t tids[2];
    trc[0]=trc[1]=0;
    t_info=(orte_pls_xcpu_thread_info*)info;
    
    session_clone=(char*)malloc(strlen(t_info->local_mounts.name)+7);
    sprintf(session_clone, "%s/clone", t_info->local_mounts.name);
    if((clone_des=open(session_clone, O_RDONLY))<0){
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
    }
    if((rc=read(clone_des, session_dir, 255))<0){
        ORTE_ERROR_LOG(ORTE_ERR_FILE_READ_FAILURE);
    }
    else{
        session_dir[rc]='\0';
        session_dir_path=(char*)malloc(strlen(t_info->local_mounts.name)+strlen(session_dir)+2);
        sprintf(session_dir_path, "%s/%s", t_info->local_mounts.name, session_dir);

        /* write environment if needed */
        env_path=(char*)malloc(strlen(session_dir_path)+5);
        sprintf(env_path, "%s/env", session_dir_path);
        if(t_info->env){
           if((des1=open(env_path, O_WRONLY))<0){
               ORTE_ERROR_LOG(ORTE_ERR_FILE_WRITE_FAILURE);
            }else{
                i=0;
                while(t_info->env[i]){
                    /*printf("from lrx: %s\n", t_info->env[i]);
                    */if(write(des1, t_info->env[i], strlen(t_info->env[i])) == -1){
                        ORTE_ERROR_LOG(ORTE_ERR_FILE_WRITE_FAILURE);
                        break;
                    }else{
                        if(t_info->env[i+1]){
                        if(write(des1, "\n", 1) == -1){
                            ORTE_ERROR_LOG(ORTE_ERR_FILE_WRITE_FAILURE);
                            break;
                        }
                        }
                    }
                    i++;
                }
                close(des1);
            }
        }
        free(env_path);
        
        /*then copy binary*/
        exec_path=(char*)malloc(strlen(session_dir_path)+6);
        sprintf(exec_path, "%s/exec", session_dir_path);
        if((des1=open(exec_path, O_WRONLY))<0){
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        }else
            if((des2=open(t_info->binary, O_RDONLY))<0){
                ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            }else{
                while(1){
                    if((rc=read(des2, character, 8192))<=0){
                            if(close(des1)!=0){ /*?????*/
                                /*no ORTE_ERR defined for FILE_CLOSE_FAILURE*/
                            }
                            if(close(des2)!=0){
                                /*no ORTE_ERR defined for FILE_CLOSE_FAILURE*/
                            }
                            break;
                    }else{
                        if(write(des1, character, rc)==-1){
                            ORTE_ERROR_LOG(ORTE_ERR_FILE_WRITE_FAILURE);
                            break;
                        }
                    }
                }
            }
        
        /* then write args*/
        argv_path=(char*)malloc(strlen(session_dir_path)+6);
        sprintf(argv_path, "%s/argv", session_dir_path);
        if((des1=open(argv_path, O_WRONLY))<0){
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        }else{
            write(des1, t_info->argv, strlen(t_info->argv));
            close(des1);
        }
        /* then write exec into ctl file to start remote execution*/
        ctl_path=(char*)malloc(strlen(session_dir_path)+5);
        sprintf(ctl_path, "%s/ctl", session_dir_path);
        /*continuation of writing ctl*/
        if((des1=open(ctl_path, O_WRONLY))<0){
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
        }else{
            if(write(des1, "exec\n", 5)==-1){
                ORTE_ERROR_LOG(ORTE_ERR_FILE_WRITE_FAILURE);
            }else
            close(des1);
        }
        
        /*then spawn threads for stderr and atdout*/
        g_stdout_thread_info=(orte_pls_xcpu_stdio_thread_info*)malloc(sizeof(orte_pls_xcpu_stdio_thread_info));
        g_stdout_thread_info->stdio_path=(char*)malloc(strlen(session_dir_path)+8);
        sprintf(g_stdout_thread_info->stdio_path, "%s/stdout", session_dir_path);
        g_stdout_thread_info->outdes=1;
        if((rc=pthread_create(&tids[0], NULL, orte_pls_xcpu_stdio_thread, (void*)g_stdout_thread_info))==0){
            trc[0]=1;
        }else ;
            /*ORTE_ERR for thread_creation_failure not defined yet*/
            /*fprintf(stderr, "\nstdout thread creation error\n");*/
        g_stderr_thread_info=(orte_pls_xcpu_stdio_thread_info*)malloc(sizeof(orte_pls_xcpu_stdio_thread_info));
        g_stderr_thread_info->stdio_path=(char*)malloc(strlen(session_dir_path)+8);
        sprintf(g_stderr_thread_info->stdio_path, "%s/stderr", session_dir_path);
        g_stderr_thread_info->outdes=2;
        if((rc=pthread_create(&tids[1], NULL, orte_pls_xcpu_stdio_thread, (void*)g_stderr_thread_info))==0){
            trc[1]=1;
        }else ;
            /*ORTE_ERR for thread_creation_failure not defined yet*/
            /*fprintf(stderr, "stderr thread creation error\n");*/
        
        free(session_dir_path);
        free(exec_path);
        free(argv_path);
        free(ctl_path);
        if(trc[0]){
            pthread_join(tids[0], NULL);
        }
        if(trc[1]){
            pthread_join(tids[1], NULL);
        }
    }
    free(session_clone);
    (clone_des>0)?close(clone_des):0;
    /* make registry update thread-safe */
    pthread_mutex_lock(&mymutex);
    /*write into registry that you are done*/
    if (ORTE_SUCCESS != (orte_soh_base_set_proc_soh(t_info->peers, ORTE_PROC_STATE_TERMINATED, 0)) ){
        ORTE_ERROR_LOG(rc);
    }
    pthread_mutex_unlock(&mymutex);
    /* free the allocated variables after you are done*/
    free(t_info->local_mounts.name);
    free(t_info->binary);
    free(t_info->argv);
    free(t_info);
    pthread_exit(NULL);
}

/* xcpu launcher function.
 * this function is called once for each process to be launched. or might
 * be called one time for multiple processes if regular expression is passed
 * to it. but for now regular expressions are not being passed.
 * 
 * @argc:  number of arguments or number of elements in argv
 * @argv:  it will be name of remote node as mounted at $XCPUBASE or /mnt/xcpu/
 * @env:   environment the needs to be setup on remote node before
 *         starting the process
 * @peers: process info, this will be passed onto the threads to help them write 
 *         process completion information in open-mpi registry.
 */
orte_pls_xcpu_pthread_tindex *orte_pls_xcpu_launch_procs(int argc, char **argv, char **env, orte_process_name_t *peers){
    char *xcpu_base, *xcpu_argv;
    struct dirent *d_entry;
    DIR *dirp;
    int temp_fd, rc=0, index=0, argvsize=0, ntids=0;
    pthread_t *tids;
    orte_pls_xcpu_mount_nodes *m_nodes, *local_mounts;
    g_current_m=NULL;
    m_nodes=NULL;
    (!(xcpu_base=getenv("XCPUBASE")))?xcpu_base="/mnt/xcpu":0;
    if(!(dirp=opendir(xcpu_base))){
        ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);/* it should be DIR_OPEN_ERROR */
        return NULL;
    }
    /* this logic should be fast than the one commented below*/
    m_nodes=(orte_pls_xcpu_mount_nodes*)malloc(sizeof(orte_pls_xcpu_mount_nodes));
    m_nodes->next=g_current_m;
    m_nodes->name=(char*)malloc(1+strlen(xcpu_base)+1+
            strlen(argv[1])+1+strlen("xcpu")+1);
    sprintf(m_nodes->name, "%s/%s/xcpu", xcpu_base, argv[1]);
    if((temp_fd=open(m_nodes->name, O_RDONLY))<0){
        fprintf(stderr, "Node %s/%s/xcpu does not exist\n",xcpu_base, argv[1]);
        free(m_nodes->name);
    }else{
        close(temp_fd);
        g_current_m=m_nodes;
    }
    /* logic ends */
            
    /*
    while((d_entry=readdir(dirp))!=NULL){
        printf("comapring %s %s\n",d_entry->d_name, argv[1]);
        if((strcmp(d_entry->d_name, ".")==0)||(strcmp(d_entry->d_name, "..")==0))
            ;else
        if(regexec(&g_compiled_exp, d_entry->d_name, 0, NULL, 0)!=REG_NOMATCH){
            printf("matched %s\n", argv[1]);
            ntids++;
            m_nodes=(orte_pls_xcpu_mount_nodes*)malloc(sizeof(orte_pls_xcpu_mount_nodes));
            m_nodes->next=g_current_m;
            m_nodes->name=(char*)malloc(1+strlen(xcpu_base)+1+
                                        strlen(d_entry->d_name)+1+strlen("xcpu")+1);
            sprintf(m_nodes->name, "%s/%s/xcpu", xcpu_base, d_entry->d_name);
            g_current_m=m_nodes;
      */      /* we can break after finding the first one 
             * or if you want to give the user an option of
             * specifying regular expressions in hostfiles
             * then don't break here
             */
            /* on a second thought we should not be going thrugh mounted node list
             * just check if xcpu_base/d_entry->d_name/xcpu exists or not
             */
        /*    break;
        }
    }*/
    if(g_current_m==NULL){ /* is that an error.... no?*/
        return NULL; 
    }
    closedir(dirp);
    /* now combine argv's so that they could be passed on */
    /* g_regexploc will have proper value only if 
     * cmd_check is already called in lrx
     * and the location of first arg after name of binary will be
     * argv[g_regexploc+2] because usage: ./o.lrx [-D xx] regexp binary args
     */
    /* number of arguments = argc - g_regexploc - 2;*/
    index=g_regexploc+2-1; /*argv[0] could be anything*/
    while(argv[index]){
        argvsize+=strlen(argv[index])+1;
        index++;
    }
    xcpu_argv=(char*)malloc(argvsize+1);
    index=g_regexploc+2-1;
    while(argv[index]){
        if(index==g_regexploc+2-1)
            strcpy(xcpu_argv, argv[index]);/* i dont know why strcpy 1st time?*/
        else
            strcat(xcpu_argv, argv[index]);
        strcat(xcpu_argv, " ");
        index++;
    }
    xcpu_argv[argvsize]='\0';
    local_mounts=g_current_m; /* this is a linked list of mounted directories
                              * where binaries need to run
                              */
    tids=(pthread_t*)malloc(ntids*sizeof(pthread_t));
    index=0;
    while(local_mounts){
        /* dont use a shared copy 
         * give every thread its own copy since we dont know
         * when all threads will exit and when to free a shared copy
         */
        g_thread_info=(orte_pls_xcpu_thread_info*)malloc(sizeof(orte_pls_xcpu_thread_info));
        /*copy name first*/
        g_thread_info->local_mounts.name=(char*)malloc(strlen(local_mounts->name)+1);
        strcpy(g_thread_info->local_mounts.name, local_mounts->name);
        /*then copy binary*/
        g_thread_info->binary=(char*)malloc(strlen(argv[g_regexploc+1])+1);
        strcpy(g_thread_info->binary,argv[g_regexploc+1]);
        /*then copy argv*/
        g_thread_info->argv=(char*)malloc(strlen(xcpu_argv)+1);
        strcpy(g_thread_info->argv, xcpu_argv);
        /* for env and peers, since we are not allocating space for these
         * and these will be freed after all the threads are completed at the
         * end of mpirun (i hope).. otherwise we might have to copy these
         * first and then pass to threads
         */
        g_thread_info->env=env; 
        g_thread_info->peers=peers;

        /*following thread will free the thread_info structure*/
        rc=pthread_create(&tids[index], NULL, orte_pls_xcpu_start_thread, (void*)g_thread_info);
        index++;
        if(rc){
            /*ORTE_ERR for thread_creation_failure not defined yet*/
            /*fprintf(stderr, "pthread_create: error while creating thread %d\n", rc);*/
            return NULL;
        }
        local_mounts=local_mounts->next;
    }
    /* use pthrad_join here if you want to wait for threads 
     * to finish execution
     *//*
    while(1){
        index--;
        pthread_join(tids[index], NULL);
        if(index==0)
            break;
    }
    free(tids);*/
    /* remember to free tids in calling function*/
    free(xcpu_argv);
    t_info.tids=tids;
    t_info.index=index;
    return &t_info;
}

/* this function is to check if argv is in correct format.
 * Some checks being done in this function (for -D) are not necessary 
 * and will be removed in future.
 */
int orte_pls_xcpu_cmd_check(int argc, char **argv){
    char *temp_exp;
    int rc=0;
    g_regexploc=1;
    if(argc>=3){
        if(argv[1][0]=='-'){
            switch(argv[1][1]){
                case 'D': /* for debugging*/
                    g_regexploc+=2;
                    if(argc<5){
                        /*fprintf(stderr, "usage: o.lrx [-D debuglevel"
                                "] nodes binary [argv0 argv1 ...]\n");
                        */rc=1;
                    }
                    break;
                default: /* unspecified option*/
                    /*fprintf(stderr, "usage: o.lrx [-D debuglevel"
                            "] nodes binary [argv0 argv1 ...]\n");
                    */return 1;
                    break;
            }
        }
    }else{
        /*fprintf(stderr, "usage: o.lrx [-D debuglevel"
                "] nodes binary [argv0 argv1 ...]\n");
        */rc=1;
    }
    if(!rc){/*check for regular expression*/
        temp_exp=(char*)malloc(strlen(argv[g_regexploc])+3);
        sprintf(temp_exp, "^%s$", argv[g_regexploc]);
        rc=orte_pls_xcpu_check_exp(temp_exp);
        free(temp_exp);
    }
    return rc;
}

void orte_pls_xcpu_free_mount(orte_pls_xcpu_mount_nodes *g_current_m){
    if(g_current_m){
        orte_pls_xcpu_free_mount(g_current_m->next);
        free(g_current_m->name);
        free(g_current_m);
    }
}

void orte_pls_xcpu_cleanup(){
    regfree(&g_compiled_exp);
    orte_pls_xcpu_free_mount(g_current_m);
}


/* Launcher can accept regular expressions as the list of nodes where
 * processes are going to be launched. This is just a helper function to check 
 * if regular expression is correct or not
 */
int orte_pls_xcpu_check_exp(char *exp){
    if(regcomp(&g_compiled_exp, exp, REG_EXTENDED|REG_NOSUB)){
        /*fprintf(stderr, "Invlid regular expression: %s\n", exp);*/
        return 1;
    }
    /*regfree(&g_compiled_exp);*/
    return 0; /* now dont forget to call regfree at the end*/
}

/* This is the main launcher function
 * It will call orte_pls_xcpu_launch_procs which will
 * start a thread for each process to be launched
 */
int lrx(int argc, char **argv, char **env, orte_process_name_t *peers){
    int rc;
    orte_pls_xcpu_pthread_tindex *t_info;
    if((rc=orte_pls_xcpu_cmd_check(argc, argv))==1){
        return 0;
    }
    if((t_info=orte_pls_xcpu_launch_procs(argc, argv, env, peers))==NULL){
        /*fprintf(stderr, "lrx: 0 processes launched\n");*/
        orte_pls_xcpu_cleanup();
        return 0;
    }
    else{
        orte_pls_xcpu_cleanup();
        t_info->index--;
        rc=t_info->tids[t_info->index];
        free(t_info->tids);
        return rc; /* no need to return thread_id
                    * thread will write its completition 
                    * itself in the registry 
                    */
    }
        /*
        while(1){
            t_info->index--;
            pthread_join(t_info->tids[t_info->index], NULL);
            if(t_info->index==0)
                break;
        }
        */
    return 0;/* can never be called*/
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

    /** merge in environment */
    merged = opal_environ_merge(*env, environ);
    opal_argv_free(*env);
    *env = merged;

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
    int i;
    size_t nprocs=0, proc_id=0;
    orte_pls_xcpu_tid_stack *t_stack, *temp_stack;
    opal_list_item_t *item;
    orte_rmaps_base_map_t* map;
    orte_rmaps_base_node_t *node;
    orte_rmaps_base_proc_t *proc;
    orte_vpid_t vpid_start, vpid_range;
    orte_process_name_t *peers;
    int peer_id, num_peers;
    /** first get the mapping we are going to use to launch job. The head
     * of the list is OBJ_CONSTRUCT'd since it is not dynamically allocated. The
     * get_map function, however, will dynamically allocate the items in the
     * list itself - these will be released when we OBJ_DESTRUCT the list at
     * the end
     */
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

    /** next, get the vpid_start and range info so we can pass it along */
    if (ORTE_SUCCESS != (rc = orte_rmaps_base_get_vpid_range(jobid, &vpid_start, &vpid_range))) {
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
    peer_id=0;
    if (ORTE_SUCCESS != (rc = orte_ns.get_job_peers(&peers, &num_peers, jobid))) {
        ORTE_ERROR_LOG(rc);
    }
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

        /** we also need to pass the proper environment to the remote
         * process so it knows its universe, gpr and ns replicas, etc. Since this
         * can be specified by the user for each app, we have to do this
         * each time.
         */
        if (ORTE_SUCCESS != (rc = orte_pls_xcpu_setup_env(&map->app->env))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }

        /** since it is possible that each node could be executing a different application,
         * we cannot just do a mass launch - that would only be supported in the special
         * case of all the application processes being identical. Instead, we are going to
         * step our way through the list, launching each process individually.
         */
        proc_id=0;
        while (proc_id < map->num_procs){
            char** env;
            proc = (orte_rmaps_base_proc_t*)(map->procs[proc_id]);
            node = proc->proc_node;
            proc_id++;

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

            /* create a copy of the environment and modify for this proc */
            env = opal_argv_copy(map->app->env);

            /** now setup the process name in the environment so we can
             * retrieve it on the other end
             */
            if (ORTE_SUCCESS != (rc = orte_ns_nds_env_put(&(proc->proc_name),
                                            vpid_start, map->num_procs,
                                            &env))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }

            /** the launcher wants to know how long the argv array is - get that now */
            argc = opal_argv_count(map->app->argv);

            /** add this process to the stack so we can track it */
            temp_stack=(orte_pls_xcpu_tid_stack*)malloc(sizeof(orte_pls_xcpu_tid_stack));
            temp_stack->next=t_stack;
            t_stack=temp_stack;

            /** launch the process */
            t_stack->tid=lrx(argc, map->app->argv, env, &peers[peer_id]);
            if(t_stack->tid==0){
                 /* first kill all the processes started on remote nodes
                  */
                i=0;
                while(i<num_peers){
                    if (ORTE_SUCCESS != (orte_soh_base_set_proc_soh(&peers[i], ORTE_PROC_STATE_TERMINATED, 0)) ){
                        ORTE_ERROR_LOG(rc);
                    }
                    i++;
                }
                break;
            }
                
            peer_id++;
        }
    }

    /** cleanup local storage */
    orte_pls_xcpu_free_stack(temp_stack);
    OBJ_DESTRUCT(&mapping);

    /** launch complete */
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
