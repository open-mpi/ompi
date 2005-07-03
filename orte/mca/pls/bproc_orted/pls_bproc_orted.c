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
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <pty.h>
#include <sys/bproc.h>
#include <dirent.h>

#include "include/orte_constants.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/univ_info.h"
#include "mca/errmgr/errmgr.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/iof_base_setup.h"
#include "mca/base/mca_base_param.h"
#include "mca/pls/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/rmaps/base/rmaps_base_map.h"
#include "pls_bproc_orted.h"
#include "mca/ns/base/base.h"
#include "util/os_create_dirpath.h"
#include "mca/oob/base/base.h"
#include "util/os_path.h"
#include "opal/runtime/opal_progress.h"
#include "pls_bproc_orted.h"

orte_pls_base_module_1_0_0_t orte_pls_bproc_orted_module = {
    orte_pls_bproc_orted_launch,
    orte_pls_bproc_orted_terminate_job,
    orte_pls_bproc_orted_terminate_proc,
    orte_pls_bproc_orted_finalize
};

static int pls_bproc_orted_make_dir(char *directory);
static char * pls_bproc_orted_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                                size_t app_context);
static int pls_bproc_orted_link_pty(int proc_rank, char * pty_path, 
                                orte_jobid_t jobid, bool connect_stdin, 
                                size_t app_context);
static int pls_bproc_orted_link_pipes(int proc_rank, orte_jobid_t jobid, int * fd,
                                      bool connect_stdin, size_t app_context);
static void pls_bproc_orted_delete_dir_tree(char * path);
static int pls_bproc_orted_remove_dir(void);

/**
 * Creates the passed directory. If the directory already exists, it and its
 * contents will be deleted then the directory will be created.
 */
static int pls_bproc_orted_make_dir(char *directory)
{
    struct stat buf;
    mode_t my_mode = S_IRWXU;  /* at the least, I need to be able to do anything */

    if (0 == stat(directory, &buf)) { /* exists - delete it and its contents */
        pls_bproc_orted_delete_dir_tree(directory);
    }
    /* try to create it with proper mode */
    return(orte_os_create_dirpath(directory, my_mode)); 
}

/**
 * returns a path of the form:
 * /tmp/openmpi-bproc-<user>/<universe>/<jobid>-<app_context>/<proc_rank>/
 * @param proc_rank   the process's rank on the node
 * @param jobid       the jobid the proc belongs to
 * @param app_context the application context number within the job
 */ 
static char * pls_bproc_orted_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                                size_t app_context) {
    char *path = NULL, *user = NULL, *job = NULL;
    int rc;

    /* ensure that system info is set */
    orte_sys_info();

    if (NULL == orte_universe_info.name) {  /* error condition */
        ORTE_ERROR_LOG(OMPI_ERROR);
        return NULL;
    }
    
    rc = orte_ns_base_convert_jobid_to_string(&job, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        free(job);
        return NULL;
    }
    
    /* get the username set by the bproc pls. We need to get it from here
     * because on many bproc systems the method we use to get the username
     * from the system on the backend fails and we only get the uid. */
    rc = mca_base_param_register_string("pls", "bproc", "username", NULL, 
                                        orte_system_info.user);
    mca_base_param_lookup_string(rc,&user);

    if (0 > asprintf(&path, "%stmp%sopenmpi-bproc-%s%s%s%s%s-%d%s%d",
                     orte_system_info.path_sep, orte_system_info.path_sep, user,
                     orte_system_info.path_sep, orte_universe_info.name,
                     orte_system_info.path_sep, job, (int) app_context,
                     orte_system_info.path_sep, proc_rank)) {
        ORTE_ERROR_LOG(OMPI_ERROR);
        path = NULL;
    }
    free(user);
    free(job);
    return path;
}

/**
 * creates symlinks to the pty in the directory 
 * /tmp/openmpi-bproc-<user>/<universe>/<jobid>-<app_context>/<proc_rank>/
 * @param proc_rank   the process's rank on the node
 * @param pty_path    the path that the pty is at
 * @param jobid       the jobid the proc belongs to
 * @param connect_stdin if true, stdin will be connected, otherwise it will be
 *                      set to /dev/null
 * @param app_context the application context number within the job
 */
static int pls_bproc_orted_link_pty(int proc_rank, char * pty_path, 
                                    orte_jobid_t jobid, bool connect_stdin,
                                    size_t app_context) {
    char *frontend = NULL, *link_path = NULL;
    int rc, i;

    frontend = pls_bproc_orted_get_base_dir_name(proc_rank, jobid, app_context);
    if(NULL == frontend) {
        rc = OMPI_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* check for existence and access, or create it */
    if (OMPI_SUCCESS != (rc = pls_bproc_orted_make_dir(frontend))) { 
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    for(i = 0; i < 3; i++)
    {
        if(0 > asprintf(&link_path, "%s%s%d", frontend, 
                        orte_system_info.path_sep, i)) {
            rc = OMPI_ERROR;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if(mca_pls_bproc_orted_component.debug) {
            ompi_output(0, "orted bproc io setup. Path: %s\n", link_path);
        }
        /* we only want to actually connect stdin if the process is rank 0 */
        if(0 != i || connect_stdin) {
            if(0 != symlink(pty_path, link_path)) {
                perror("pls_bproc_orted could not create symlink");
                rc = OMPI_ERROR;
                goto cleanup;
            }
        } else { /* otherwise connect stdin to /dev/null */
            if(0 != symlink("/dev/null", link_path)) {
                perror("pls_bproc_orted could not create symlink");
                rc = OMPI_ERROR;
                goto cleanup;
            }
        }
        free(link_path);
        link_path = NULL;
    }

 cleanup:
    if (NULL != frontend) {
       free(frontend);
    }
    if (NULL != link_path) {
        free(link_path);
    }
    return rc;
}

/**
 * creates pipes for the io in the filesystem in the directory 
 * /tmp/openmpi-bproc-<user>/<universe>/<jobid>-<app_context>/<proc_rank>/
 * and returns their file dexcriptors
 * @param proc_rank   the process's rank on the node
 * @param jobid       the jobid the proc belongs to
 * @param fd          a pointer to an array of file descriptors 3 long
 * @param connect_stdin if true, stdin will be connected, otherwise it will be
 *                      set to /dev/null
 * @param app_context the application context number within the job
 */
static int pls_bproc_orted_link_pipes(int proc_rank, orte_jobid_t jobid, int * fd,
                                      bool connect_stdin, size_t app_context) {
    char *frontend = NULL, *link_path = NULL;
    int rc, i;

    frontend = pls_bproc_orted_get_base_dir_name(proc_rank, jobid, app_context);
    if(NULL == frontend) {
        rc = OMPI_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* check for existence and access, or create it */
    if (OMPI_SUCCESS != (rc = pls_bproc_orted_make_dir(frontend))) { 
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    for(i = 0; i < 3; i++)
    {
        if(0 > asprintf(&link_path, "%s%s%d", frontend, 
                        orte_system_info.path_sep, i)) {
            rc = OMPI_ERROR;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        if(mca_pls_bproc_orted_component.debug) {
            ompi_output(0, "orted bproc pipe io setup. Path: %s\n", link_path);
        }
        /* we only want to actually connect stdin if the process is rank 0 */
        if(0 != i || connect_stdin) {
            if(0 != mkfifo(link_path, S_IRWXU)) {
                perror("pls_bproc_orted mkfifo failed");
                rc = OMPI_ERROR;
                goto cleanup;
            }
            /* for stdin, open write only */
            if(0 == i) {
                fd[i] = open(link_path, O_RDWR);
            } else {
                fd[i] = open(link_path, O_RDWR);
            }
            if(-1 == fd[i]) {
                perror("pls_bproc_orted open failed");
                rc = OMPI_ERROR;
                goto cleanup;
            }
        } else { /* otherwise connect stdin to /dev/null */
            if(0 != symlink("/dev/null", link_path)) {
                perror("pls_bproc_orted could not create symlink");
                rc = OMPI_ERROR;
                goto cleanup;
            }
        }
        free(link_path);
        link_path = NULL;
    }

 cleanup:
    if (NULL != frontend) {
       free(frontend);
    }
    if (NULL != link_path) {
        free(link_path);
    }
    return rc;
}

/**
 * deletes the passed directory tree recursively
 * @param path the path to the base directory to delete
 */
static void pls_bproc_orted_delete_dir_tree(char * path) {
    DIR *dp;
    struct dirent *ep;
    char *filenm;
    int ret;
    struct stat buf;
    dp = opendir(path);
    if (NULL == dp) {
        return;
    }

    while (NULL != (ep = readdir(dp)) ) {
        /* skip: . and ..  */
        if ((0 != strcmp(ep->d_name, ".")) && (0 != strcmp(ep->d_name, ".."))) {
            filenm = orte_os_path(false, path, ep->d_name, NULL);
            ret = stat(filenm, &buf);
            if (ret < 0 || S_ISDIR(buf.st_mode)) {
                pls_bproc_orted_delete_dir_tree(filenm);
                free(filenm);
                continue;
            }
            unlink(filenm);
            free(filenm);
        }
    }
    closedir(dp);
    rmdir(path);
}

/**
 * Removes the bproc directory /tmp/openmpi-bproc-<user>/ and all of its contents
 */
static int pls_bproc_orted_remove_dir() {
    char *frontend = NULL, *user = NULL;
    int id;

    /* get the username set by the bproc pls. We need to get it from here
     * because on many bproc systems the method we use to get the username
     * from the system on the backend fails and we only get the uid. */
    id = mca_base_param_register_string("pls", "bproc", "username", NULL, 
                                        orte_system_info.user);
    mca_base_param_lookup_string(id,&user);

    if (0 > asprintf(&frontend, "%stmp%sopenmpi-bproc-%s",
                     orte_system_info.path_sep, orte_system_info.path_sep, user)) {
        free(frontend);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return OMPI_ERROR;
    }
    /* we do our best to clean up the directory tree, but we ignore errors*/
    pls_bproc_orted_delete_dir_tree(frontend);
    free(frontend);
    return OMPI_SUCCESS;
}

/**
 * Setup io for the current node, then tell orterun we are ready for the actual
 * processes.
 */
int orte_pls_bproc_orted_launch(orte_jobid_t jobid)
{
    opal_list_t map;
    orte_rmaps_base_map_t * mapping;
    opal_list_item_t* item;
    int rc, id;
    int master[3];
    int slave;
    int num_procs = 0;
    size_t i;
    size_t app_context;
    int32_t src = 0;
    bool connect_stdin;
    bool pty_error_thrown = false;
    orte_buffer_t ack;
    char * param;
    char * pty_name = malloc(256);
    if (NULL == pty_name) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto cleanup;
    }

    OBJ_CONSTRUCT(&ack, orte_buffer_t);

    rc = bproc_currnode();
    if(0 > rc) {
        ompi_output(0, "pls_bproc_orted component running on invalid node");
    }
    if(0 > asprintf(&param, "%d", rc)) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        goto cleanup;
    }
    /* query the allocation for this node */
    OBJ_CONSTRUCT(&map, opal_list_t);
    rc = orte_rmaps_base_get_node_map(orte_process_info.my_name->cellid, jobid, 
                                      param, &map);
    free(param);
    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* look up the app context number. This is necessary since the user
     * can launch multiple apps on the same node, we have to have a way
     * to keep them seperate */
    id = mca_base_param_register_int("pls", "bproc", "app_context", NULL, 0);
    mca_base_param_lookup_int(id, (int *) &app_context);

    /* figure out what processes will be on this node and set up the io files */
    for(item =  opal_list_get_first(&map);
        item != opal_list_get_end(&map);
        item =  opal_list_get_next(item)) {
        mapping = (orte_rmaps_base_map_t *) item;
        if(mapping->app->idx != app_context) {
            continue;
        }
        for(i = 0; i < mapping->num_procs; i++) {
            if(0 < mca_pls_bproc_orted_component.debug) {
                ompi_output(0, "orte_pls_bproc_orted_launch: setting up io for "
                               "[%lu,%lu,%lu]\n",
                               ORTE_NAME_ARGS((&mapping->procs[i]->proc_name)));
            }
            /* only setup to forward stdin if it is rank 0, otherwise connect
             * to /dev/null */
            if(0 == mapping->procs[i]->proc_rank) {
                connect_stdin = true;
            } else {
                connect_stdin = false;
            }
            if(0 == openpty(&master[0], &slave, pty_name, NULL, NULL)) {
                master[2] = master[1] = master[0];
                rc = pls_bproc_orted_link_pty(num_procs, pty_name, jobid, 
                                              connect_stdin, app_context);
                if(OMPI_SUCCESS != rc) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            } else {
                if(!pty_error_thrown) {
                    ompi_output(1, "pls_bproc_orted: openpty failed, "
                                   "using pipes instead");
                    pty_error_thrown = true;
                }
                rc = pls_bproc_orted_link_pipes(num_procs, jobid, master,
                                                connect_stdin, app_context);
                if(OMPI_SUCCESS != rc) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
            }

            if(connect_stdin) {
                orte_iof.iof_publish(&(mapping->procs[i]->proc_name), 
                                     ORTE_IOF_SINK, ORTE_IOF_STDIN, master[0]);
            }
            /* set up io forwarding connections */
            orte_iof.iof_publish(&(mapping->procs[i]->proc_name), ORTE_IOF_SOURCE, 
                                 ORTE_IOF_STDOUT, master[1]);
            orte_iof.iof_publish(&(mapping->procs[i]->proc_name), ORTE_IOF_SOURCE, 
                                 ORTE_IOF_STDERR, master[2]);
                
            num_procs++;
        }
    }

    mca_pls_bproc_orted_component.num_procs = num_procs;

    /* do callback to say we are ready */
    orte_dps.pack(&ack, &src, 1, ORTE_INT32);
    rc = mca_oob_send_packed(MCA_OOB_NAME_SEED, &ack, MCA_OOB_TAG_BPROC, 0);
    if (0 > rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = OMPI_SUCCESS;

cleanup:
    while(NULL != (item = opal_list_remove_first(&map))) {
        OBJ_RELEASE(item);
    }
    if(NULL != pty_name) {
        free(pty_name);
    }
    OBJ_DESTRUCT(&map);
    OBJ_DESTRUCT(&ack);
    return rc;
}

/**
 *  Query for all processes allocated to the job and terminate
 *  those on the current node.
 */

int orte_pls_bproc_orted_terminate_job(orte_jobid_t jobid)
{
    orte_iof.iof_flush();
    return ORTE_SUCCESS;
}


int orte_pls_bproc_orted_terminate_proc(const orte_process_name_t* proc)
{
    return ORTE_SUCCESS;
}

int orte_pls_bproc_orted_finalize(void)
{
    OPAL_THREAD_LOCK(&mca_pls_bproc_orted_component.lock);
    opal_condition_wait(&mca_pls_bproc_orted_component.condition, 
                        &mca_pls_bproc_orted_component.lock);
    OPAL_THREAD_UNLOCK(&mca_pls_bproc_orted_component.lock);

    pls_bproc_orted_remove_dir();
    return ORTE_SUCCESS;
}

