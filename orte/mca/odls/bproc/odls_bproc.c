/*
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
 */
/**
 * @file:
 * Part of the bproc launcher.
 * See odls_bproc.h for an overview of how it works.
 */
#include "orte_config.h"
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>
#include <pty.h>
#include <dirent.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/runtime/opal_progress.h"
#include "opal/threads/condition.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/util/sys_info.h"
#include "orte/orte_constants.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/session_dir.h"
#include "orte/util/univ_info.h"

#include "orte/mca/odls/base/odls_private.h"
#include "odls_bproc.h"

/**
 * Initialization of the bproc_orted module with all the needed function pointers
 */
orte_odls_base_module_t orte_odls_bproc_module = {
    orte_odls_bproc_get_add_procs_data,
    orte_odls_bproc_launch_local_procs,
    orte_odls_bproc_kill_local_procs,
    orte_odls_bproc_signal_local_procs,
    orte_odls_bproc_deliver_message
};

static int odls_bproc_make_dir(char *directory);
static char * odls_bproc_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                                orte_std_cntr_t app_context);
static void odls_bproc_delete_dir_tree(char * path);
static int odls_bproc_remove_dir(void);
static void odls_bproc_send_cb(int status, orte_process_name_t * peer,
                                    orte_buffer_t* buffer, int tag, void* cbdata);
static int odls_bproc_setup_stdio(orte_process_name_t *proc_name, 
                                       int proc_rank, orte_jobid_t jobid,
                                       orte_std_cntr_t app_context, bool connect_stdin);


int orte_odls_bproc_get_add_procs_data(orte_gpr_notify_data_t **data, orte_job_map_t *map)
{
    orte_gpr_notify_data_t *ndat;
    orte_gpr_value_t **values, *value;
    orte_std_cntr_t cnt;
    opal_list_item_t *item, *m_item;
    orte_mapped_node_t *node;
    orte_mapped_proc_t *proc;
    int rc;
    
    /* set default answer */
    *data = NULL;
    
    ndat = OBJ_NEW(orte_gpr_notify_data_t);
    if (NULL == ndat) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    /* construct a fake trigger name so that the we can extract the jobid from it later */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&(ndat->target), "bogus", map->job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(ndat);
        return rc;
    }
    
    /* our required info is in the mapped_node objects, so all we
     * have to do is transfer it over
     */
    for (m_item = opal_list_get_first(&map->nodes);
         m_item != opal_list_get_end(&map->nodes);
         m_item = opal_list_get_next(m_item)) {
        node = (orte_mapped_node_t*)m_item;
        
        for (item = opal_list_get_first(&node->procs);
             item != opal_list_get_end(&node->procs);
             item = opal_list_get_next(item)) {
            proc = (orte_mapped_proc_t*)item;
            
            /* must not have any tokens so that launch_procs can process it correctly */
            if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, 0, "bogus", 5, 0))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]),
                                                             ORTE_PROC_NAME_KEY,
                                                             ORTE_NAME, &proc->name))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]),
                                                             ORTE_PROC_APP_CONTEXT_KEY,
                                                             ORTE_STD_CNTR, &proc->app_idx))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[2]),
                                                             ORTE_NODE_NAME_KEY,
                                                             ORTE_STRING, node->nodename))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[3]),
                                                             ORTE_PROC_LOCAL_RANK_KEY,
                                                             ORTE_VPID, &proc->local_rank))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[4]),
                                                             ORTE_NODE_NUM_PROCS_KEY,
                                                             ORTE_STD_CNTR, &node->num_procs))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(value);
                return rc;
            }
            
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&cnt, ndat->values, value))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(ndat);
                OBJ_RELEASE(values[0]);
                return rc;
            }
            ndat->cnt += 1;
        }
    }
    
    *data = ndat;
    return ORTE_SUCCESS;
}


/**
 * Creates the passed directory. If the directory already exists, it and its
 * contents will be deleted then the directory will be created.
 * @param directory The directory to be created.
 * @retval ORTE_SUCCESS
 * @retval error
 */
static int
odls_bproc_make_dir(char *directory)
{
    struct stat buf;
    mode_t my_mode = S_IRWXU;  /* at the least, I need to be able to do anything */

    if (0 == stat(directory, &buf)) { /* exists - delete it and its contents */
        odls_bproc_delete_dir_tree(directory);
    }
    /* try to create it with proper mode */
    return(opal_os_dirpath_create(directory, my_mode));
}


/**
 * Returns a path of the form:
 * @code
 * /tmp/openmpi-bproc-<user>/<universe>/<jobid>-<app_context>/<proc_rank>/
 * @endcode
 * which is used to put links to the pty/pipes in
 * @param proc_rank   the process's rank on the node
 * @param jobid       the jobid the proc belongs to
 * @param app_context the application context number within the job
 * @retval path
 */
static char *
 odls_bproc_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                   orte_std_cntr_t app_context)
{
    char *path = NULL, *user = NULL, *job = NULL;
    int rc;

    /* ensure that system info is set */
    orte_sys_info();

    if (NULL == orte_universe_info.name) {  /* error condition */
        ORTE_ERROR_LOG(ORTE_ERROR);
        return NULL;
    }

    rc = orte_ns.convert_jobid_to_string(&job, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }

    /* get the username set by the bproc pls. We need to get it from here
     * because on many bproc systems the method we use to get the username
     * from the system on the backend fails and we only get the uid. */
    rc = mca_base_param_register_string("pls", "bproc", "username", NULL,
                                        orte_system_info.user);
    mca_base_param_lookup_string(rc,&user);

    if (0 > asprintf(&path, OPAL_PATH_SEP"tmp"OPAL_PATH_SEP"openmpi-bproc-%s"OPAL_PATH_SEP"%s"OPAL_PATH_SEP"%s-%d"OPAL_PATH_SEP"%d",
                     user, orte_universe_info.name,
                     job, (int) app_context, proc_rank)) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        path = NULL;
    }
    if(0 < mca_odls_bproc_component.debug) {
        opal_output(0, "odls bproc io setup. Path: %s\n", path);
    }
    free(user);
    free(job);
    return path;
}


/**
 * deletes the passed directory tree recursively
 * @param path the path to the base directory to delete
 */
static void
odls_bproc_delete_dir_tree(char * path)
{
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
            filenm = opal_os_path(false, path, ep->d_name, NULL);
            ret = stat(filenm, &buf);
            if (ret < 0 || S_ISDIR(buf.st_mode)) {
                odls_bproc_delete_dir_tree(filenm);
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
 * Removes the bproc directory
 * @code /tmp/openmpi-bproc-<user>/ @endcode and all of its contents
 * @retval ORTE_SUCCESS
 * @retval error
 */
static int
odls_bproc_remove_dir()
{
    char *frontend = NULL, *user = NULL, *filename = NULL;
    int id;

    /* get the username set by the bproc pls. We need to get it from here
     * because on many bproc systems the method we use to get the username
     * from the system on the backend fails and we only get the uid. */
    id = mca_base_param_register_string("pls", "bproc", "username", NULL,
                                        orte_system_info.user);
    mca_base_param_lookup_string(id,&user);
    asprintf(&filename, "openmpi-bproc-%s", user );
    if( NULL == filename ) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERROR;
    }
    frontend = opal_os_path(false, "tmp", filename, NULL );
    free(filename);  /* Always free the filename */
    if (NULL == frontend) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERROR;
    }
    /* we do our best to clean up the directory tree, but we ignore errors*/
    odls_bproc_delete_dir_tree(frontend);
    free(frontend);
    return ORTE_SUCCESS;
}


/**
 * Callback function for when we tell mpirun we are ready
 * @param status
 * @param peer
 * @param buffer
 * @param tag
 * @param cbdata
 */
static void
odls_bproc_send_cb(int status, orte_process_name_t * peer,
                                    orte_buffer_t* buffer, int tag, void* cbdata)
{
    OBJ_RELEASE(buffer);
}


/**
 * Create Standard I/O symlinks in the filesystem for a given proc
 *
 * Create Standard I/O symlinks in the filesystem for a given proc.
 * The symlinks will be placed in:
 * @code
 * /tmp/openmpi-bproc-<user>/<universe>/<jobid>-<app_context>/<proc_rank>/
 * @endcode
 *
 * The symlinks will be to FIFOs for stdin and stderr.  stdout will either
 * be to a FIFO or pty, depending on the configuration of Open MPI.
 *
 * @param proc_rank   the process's rank on the node
 * @param jobid       the jobid the proc belongs to
 * @param app_context the application context number within the job
 * @param connect_stdin if true, stdin will be connected, otherwise it will be
 *                      set to /dev/null
 *
 * @retval ORTE_SUCCESS
 * @retval error
 */
static int
odls_bproc_setup_stdio(orte_process_name_t *proc_name, int proc_rank,
                            orte_jobid_t jobid,
                            orte_std_cntr_t app_context, bool connect_stdin)
{
    char *path_prefix, *fd_link_path = NULL;
    int rc = ORTE_SUCCESS, fd;
#if defined(HAVE_OPENPTY) && (OMPI_ENABLE_PTY_SUPPORT != 0)
    int amaster, aslave;
    char pty_name[256];
    struct termios term_attrs;
#endif
    
    path_prefix = odls_bproc_get_base_dir_name(proc_rank, jobid, (size_t)app_context);
    if (NULL == path_prefix) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* check for existence and access, or create it */
    if (ORTE_SUCCESS != (rc = odls_bproc_make_dir(path_prefix))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* setup the stdin FIFO.  Always use a fifo for the same reason we
       always use a pipe in the iof_setup code -- don't want to flush
       onto the floor during close */
    fd_link_path = opal_os_path( false, path_prefix, "0", NULL );
   if (NULL == fd_link_path) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    if (connect_stdin) {
        if (0 != mkfifo(fd_link_path, S_IRWXU)) {
             perror("odls_bproc mkfifo failed");
             rc = ORTE_ERROR;
             ORTE_ERROR_LOG(rc);
             goto cleanup;
        }

        fd = open(fd_link_path, O_RDWR);
        if (-1 == fd) {
            perror("odls_bproc open failed");
            rc = ORTE_ERROR;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        orte_iof.iof_publish(proc_name, ORTE_IOF_SINK,
                             ORTE_IOF_STDIN, fd);
    } else {
        if(0 != symlink("/dev/null", fd_link_path)) {
            perror("odls_bproc could not create symlink");
            rc = ORTE_ERROR;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    free(fd_link_path);
    fd_link_path = NULL;

    /* setup the stdout PTY / FIFO */
    fd_link_path = opal_os_path( false, path_prefix, "1", NULL );
    if (NULL == fd_link_path) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

#if defined(HAVE_OPENPTY) && (OMPI_ENABLE_PTY_SUPPORT != 0)
    if (0 != openpty(&amaster, &aslave, pty_name, NULL, NULL)) {
         opal_output(0, "odls_bproc: openpty failed, using pipes instead");
         goto stdout_fifo_setup;
    } 

    if (0 != symlink(pty_name, fd_link_path)) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    if (tcgetattr(aslave, &term_attrs) < 0) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    term_attrs.c_lflag &= ~ (ECHO | ECHOE | ECHOK |
                             ECHOCTL | ECHOKE | ECHONL);
    term_attrs.c_iflag &= ~ (ICRNL | INLCR | ISTRIP | INPCK | IXON);
    term_attrs.c_oflag &= ~ (OCRNL | ONLCR);
    if (tcsetattr(aslave, TCSANOW, &term_attrs) == -1) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
 
    orte_iof.iof_publish(proc_name, ORTE_IOF_SOURCE,
                         ORTE_IOF_STDOUT, amaster);

    goto stderr_fifo_setup;

stdout_fifo_setup:
#endif

    if (0 != mkfifo(fd_link_path, S_IRWXU)) {
         perror("odls_bproc mkfifo failed");
         rc = ORTE_ERROR;
         goto cleanup;
    }

    fd = open(fd_link_path, O_RDWR);
    if (-1 == fd) {
        perror("odls_bproc open failed");
        rc = ORTE_ERROR;
        goto cleanup;
    }

    orte_iof.iof_publish(proc_name, ORTE_IOF_SOURCE,
                         ORTE_IOF_STDOUT, fd);

#if defined(HAVE_OPENPTY) && (OMPI_ENABLE_PTY_SUPPORT != 0)
stderr_fifo_setup:
#endif

    free(fd_link_path);
    fd_link_path = NULL;

    /* setup the stderr FIFO.  Always a fifo */
    fd_link_path = opal_os_path( false, path_prefix, "2", NULL );
    if (NULL == fd_link_path) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    if (0 != mkfifo(fd_link_path, S_IRWXU)) {
         perror("odls_bproc mkfifo failed");
         rc = ORTE_ERROR;
         goto cleanup;
    }

    fd = open(fd_link_path, O_RDWR);
    if (-1 == fd) {
        perror("odls_bproc open failed");
        rc = ORTE_ERROR;
        goto cleanup;
    }

    orte_iof.iof_publish(proc_name, ORTE_IOF_SOURCE,
                         ORTE_IOF_STDERR, fd);

cleanup:
    if (NULL != path_prefix) {
       free(path_prefix);
    }
    if (NULL != fd_link_path) {
        free(fd_link_path);
    }
    return rc;
}


/**
 * Setup io for the current node, then tell orterun we are ready for the actual
 * processes.
 * @retval ORTE_SUCCESS
 * @retval error
 */
int
orte_odls_bproc_launch_local_procs(orte_gpr_notify_data_t *data,)
{
    odls_bproc_child_t *child;
    opal_list_item_t* item;
    orte_gpr_value_t *value, **values;
    orte_gpr_keyval_t *kval;
    char *node_name;
    int rc;
    orte_std_cntr_t i, j, kv, kv2, *sptr;
    int src = 0;
    orte_buffer_t *ack;
    bool connect_stdin;
    orte_jobid_t jobid;
    int cycle = 0;
    char *job_str=NULL, *vpid_str, *uri_file, *my_uri=NULL, *session_dir=NULL;
    FILE *fp;
    orte_vpid_t *vptr;
    bool node_included;

    /* first, retrieve the job number we are to launch from the
     * returned data - we can extract the jobid directly from the
     * subscription name we created
     */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&jobid, data->target))) {
        ORTE_ERROR_LOG(rc);
        src = rc;
        goto CALLHOME;
    }

    /**
     * hack for bproc4, change process group so that we do not receive signals
     * from the parent/front-end process, as bproc4 does not currently allow the
     * process to intercept the signal
    */
    setpgid(0,0);

    /* set the flag indicating this node is not included in the launch data */
    node_included = false;
    
    /* loop through the returned data to find the global info and
     * the info for processes going onto this node
     */
    values = (orte_gpr_value_t**)(data->values)->addr;
    for (j=0, i=0; i < data->cnt && j < (data->values)->size; j++) {  /* loop through all returned values */
        if (NULL != values[j]) {
            i++;
            value = values[j];
            /* this must have come from one of the process containers, so it must
            * contain data for a proc structure - see if it belongs to this node
            */
            for (kv=0; kv < value->cnt; kv++) {
                kval = value->keyvals[kv];
                if (strcmp(kval->key, ORTE_NODE_NAME_KEY) == 0) {
                    /* Most C-compilers will bark if we try to directly compare the string in the
                    * kval data area against a regular string, so we need to "get" the data
                    * so we can access it */
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&node_name, kval->value, ORTE_STRING))) {
                        ORTE_ERROR_LOG(rc);
                        src = rc;
                        goto CALLHOME;
                    }
                    /* if this is our node...must also protect against a zero-length string  */
                    if (NULL != node_name && 0 == strcmp(node_name, orte_system_info.nodename)) {
                        /* indicate that there is something for us to do */
                        node_included = true;
                        
                        /* setup and populate the child object */
                        child = OBJ_NEW(odls_bproc_child_t);
                        for (kv2 = 0; kv2 < value->cnt; kv2++) {
                            kval = value->keyvals[kv2];
                            if(strcmp(kval->key, ORTE_PROC_NAME_KEY) == 0) {
                                /* copy the name into the child object */
                                if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(child->name), kval->value->data, ORTE_NAME))) {
                                    ORTE_ERROR_LOG(rc);
                                    src = rc;
                                    goto CALLHOME;
                                }
                                continue;
                            }
                            if(strcmp(kval->key, ORTE_PROC_APP_CONTEXT_KEY) == 0) {
                                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, kval->value, ORTE_STD_CNTR))) {
                                    ORTE_ERROR_LOG(rc);
                                    src = rc;
                                    goto CALLHOME;
                                }
                                child->app_idx = *sptr;  /* save the index into the app_context objects */
                                continue;
                            }
                            if(strcmp(kval->key, ORTE_PROC_LOCAL_RANK_KEY) == 0) {
                                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, kval->value, ORTE_VPID))) {
                                    ORTE_ERROR_LOG(rc);
                                    src = rc;
                                    goto CALLHOME;
                                }
                                child->local_rank = *vptr;  /* save the local_rank */
                                continue;
                            }
                            if(strcmp(kval->key, ORTE_NODE_NUM_PROCS_KEY) == 0) {
                                if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, kval->value, ORTE_STD_CNTR))) {
                                    ORTE_ERROR_LOG(rc);
                                    src = rc;
                                    goto CALLHOME;
                                }
                                child->num_procs = *sptr;  /* save the number of procs from this job on this node */
                                continue;
                            }
                        } /* kv2 */
                        /* protect operation on the global list of children */
                        OPAL_THREAD_LOCK(&mca_odls_bproc_component.lock);
                        opal_list_append(&mca_odls_bproc_component.children, &child->super);
                        opal_condition_signal(&mca_odls_bproc_component.cond);
                        OPAL_THREAD_UNLOCK(&mca_odls_bproc_component.lock);

                    }
                }
            } /* for kv */
        } /* for j */
    }

    /* if there is nothing for us to do, we still have to report back
     * before we just return
     */
    if (!node_included) {
        rc = ORTE_SUCCESS;
        goto CALLHOME;
    }

    /* setup some values we'll need to drop my uri for each child */
    orte_ns.convert_jobid_to_string(&job_str, jobid);
    my_uri = orte_rml.get_uri();

    /* set up the io files for our children */
    for(item =  opal_list_get_first(&mca_odls_bproc_component.children);
        item != opal_list_get_end(&mca_odls_bproc_component.children);
        item =  opal_list_get_next(item)) {
        child = (odls_bproc_child_t *) item;
        if(0 < mca_odls_bproc_component.debug) {
            opal_output(0, "orte_odls_bproc_launch: setting up io for "
                            "[%ld,%ld,%ld] proc rank %ld\n",
                            ORTE_NAME_ARGS((child->name)),
                            (long)child->name->vpid);
        }
        /* only setup to forward stdin if it is rank 0, otherwise connect
            * to /dev/null */
        if(0 == child->name->vpid) {
            connect_stdin = true;
        } else {
            connect_stdin = false;
        }

        rc = odls_bproc_setup_stdio(child->name, cycle,
                                    jobid, child->app_idx,
                                    connect_stdin);
        if (ORTE_SUCCESS != rc) {
            ORTE_ERROR_LOG(rc);
            src = rc;
            goto CALLHOME;
        }

        /* record my uri in a file within the session directory so the child can contact me */
        /* get the session dir for this proc */
        orte_ns.convert_vpid_to_string(&vpid_str, child->name->vpid);
        
        if (ORTE_SUCCESS != (rc = orte_session_dir(true, NULL, NULL, NULL,
                                                   NULL, NULL, job_str, vpid_str))) {
            ORTE_ERROR_LOG(rc);
            src = rc;
            goto CALLHOME;
        }
        
        /* get the session dir name so we can put the file there */
        if (ORTE_SUCCESS != (rc = orte_session_dir_get_name(&session_dir, NULL, NULL, NULL,
                                                            NULL, NULL, NULL, job_str, vpid_str))) {
            ORTE_ERROR_LOG(rc);
            src = rc;
            goto CALLHOME;
        }
        free(vpid_str);
        
        /* create the file and put my uri, this child's local rank, and the
         * number of local procs into it */
        uri_file = opal_os_path(false, session_dir, "orted-uri.txt", NULL);
        fp = fopen(uri_file, "w");
        if (NULL == fp) {
            ORTE_ERROR_LOG(ORTE_ERR_FILE_OPEN_FAILURE);
            rc = ORTE_ERR_FILE_OPEN_FAILURE;
            src = rc;
            goto CALLHOME;
        }
        fprintf(fp, "%s\n", my_uri);
        fprintf(fp, "%ld\n", (long)child->local_rank);
        fprintf(fp, "%ld\n", (long)child->num_procs);
        fclose(fp);
        free(uri_file);

        cycle++;
    }

    /* release the jobid string and uri */
    free(job_str);
    free(my_uri);

CALLHOME:
    /* message to indicate that we are ready */
    ack = OBJ_NEW(orte_buffer_t);
    rc = orte_dss.pack(ack, &src, 1, ORTE_INT);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    rc = mca_oob_send_packed_nb(ORTE_PROC_MY_HNP, ack, ORTE_RML_TAG_BPROC, 0,
        odls_bproc_send_cb, NULL);
    if (0 > rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = ORTE_SUCCESS;

cleanup:

    return rc;
}

/**
 * Function to terminate a job. Since this component only runs on remote nodes
 * and doesn't actually launch any processes, this function is not needed
 * so is a noop.
 */
int orte_odls_bproc_kill_local_procs(orte_jobid_t job, bool set_state)
{
    orte_iof.iof_flush();
    return ORTE_SUCCESS;
}

/**
 * Function to signal a process. Since this component only runs on remote nodes
 * and doesn't actually launch any processes, this function is not needed
 * so is a noop.
 * @param proc the process's name
 * @param signal The signal to send
 * @retval ORTE_SUCCESS
 */
int orte_odls_bproc_signal_local_procs(const orte_process_name_t* proc, int32_t signal)
{
    orte_iof.iof_flush();
    return ORTE_SUCCESS;
}


int orte_odls_bproc_deliver_message(orte_jobid_t job, orte_buffer_t *buffer, orte_rml_tag_t tag)
{
    int rc;
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* protect operations involving the global list of children */
    OPAL_THREAD_LOCK(&mca_odls_bproc_component.lock);
    
    for (item = opal_list_get_first(&mca_odls_bproc_component.children);
         item != opal_list_get_end(&mca_odls_bproc_component.children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* do we have a child from the specified job. Because the
         *  job could be given as a WILDCARD value, we must use
         *  the dss.compare function to check for equality.
         */
        if (ORTE_EQUAL != orte_dss.compare(&job, &(child->name->jobid), ORTE_JOBID)) {
            continue;
        }
        
        /* if so, send the message */
        rc = orte_rml.send_buffer(child->name, buffer, tag, 0);
        if (rc < 0) {
            ORTE_ERROR_LOG(rc);
        }
    }
    
    opal_condition_signal(&mca_odls_bproc_component.cond);
    OPAL_THREAD_UNLOCK(&mca_odls_bproc_component.lock);
    return ORTE_SUCCESS;
}

/**
 * Finalizes the bproc module. Cleanup tmp directory/files
 * used for I/O forwarding.
 * @retval ORTE_SUCCESS
 */
int orte_odls_bproc_finalize(void)
{
    orte_iof.iof_flush();
    odls_bproc_remove_dir();
    orte_session_dir_finalize(orte_process_info.my_name);
    return ORTE_SUCCESS;
}

