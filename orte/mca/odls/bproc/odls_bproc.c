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
#include "orte/constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <dirent.h>

#include "opal/mca/base/mca_base_param.h"
#include "opal/util/os_dirpath.h"
#include "opal/util/os_path.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/iof/iof.h"
#include "orte/mca/iof/base/iof_base_setup.h"
#include "orte/util/session_dir.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/odls/base/odls_private.h"
#include "odls_bproc.h"

static int orte_odls_bproc_launch_local_procs(opal_buffer_t *data);
static int orte_odls_bproc_kill_local_procs(orte_jobid_t job, bool set_state);
static int orte_odls_bproc_signal_local_procs(const orte_process_name_t *proc, int32_t signal);

/**
 * Initialization of the bproc_orted module with all the needed function pointers
 */
orte_odls_base_module_t orte_odls_bproc_module = {
    orte_odls_base_default_get_add_procs_data,
    orte_odls_bproc_launch_local_procs,
    orte_odls_bproc_kill_local_procs,
    orte_odls_bproc_signal_local_procs,
    orte_odls_base_default_deliver_message,
    orte_odls_base_default_require_sync
};

static int odls_bproc_make_dir(char *directory);
static char * odls_bproc_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                           orte_std_cntr_t app_context);
static void odls_bproc_delete_dir_tree(char * path);
static int odls_bproc_remove_dir(void);
static void odls_bproc_send_cb(int status, orte_process_name_t * peer,
                               opal_buffer_t* buffer, int tag, void* cbdata);
static int odls_bproc_setup_stdio(orte_process_name_t *proc_name, 
                                  int proc_rank, orte_jobid_t jobid,
                                  orte_std_cntr_t app_context, bool connect_stdin);

/* Local globals */
static char *user = NULL;
static char *frontend = NULL;

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
    char *path = NULL, *job = NULL;
    int rc;

    rc = orte_util_convert_jobid_to_string(&job, jobid);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        return NULL;
    }

    /* get the username set by the bproc plm. We need to get it from here
     * because on many bproc systems the method we use to get the username
     * from the system on the backend fails and we only get the uid
     */
    mca_base_param_reg_string_name("orte", "plm_bproc_username",
                                   "Name of the user on the remote node",
                                   false, false, NULL, &user);

    if (0 > asprintf(&frontend, OPAL_PATH_SEP"%s"OPAL_PATH_SEP"openmpi-bproc-%s",
                     orte_process_info.tmpdir_base, user)) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        path = NULL;
    }
        
    if (0 > asprintf(&path, "%s"OPAL_PATH_SEP"%s-%d"OPAL_PATH_SEP"%d",
                     frontend, job, (int) app_context, proc_rank)) {
        ORTE_ERROR_LOG(ORTE_ERROR);
        path = NULL;
    }
    OPAL_OUTPUT_VERBOSE((0, orte_odls_globals.output,
                         "odls bproc io setup. Path: %s\n", path));
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
                                    opal_buffer_t* buffer, int tag, void* cbdata)
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
int orte_odls_bproc_launch_local_procs(opal_buffer_t *data)
{
    orte_odls_child_t *child;
    opal_list_item_t* item;
    int rc;
    int src = 0;
    opal_buffer_t *ack;
    bool connect_stdin;
    orte_jobid_t jobid;
    int cycle = 0;

    /**
     * hack for bproc4, change process group so that we do not receive signals
     * from the parent/front-end process, as bproc4 does not currently allow the
     * process to intercept the signal
    */
    setpgid(0,0);
    
    /* construct the list of children we are to launch */
    if (ORTE_SUCCESS != (rc = orte_odls_base_default_construct_child_list(data, &jobid))) {
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:bproc:launch:local failed to construct child list on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }
    
   /* set up the io files for our children */
    for(item =  opal_list_get_first(&orte_odls_globals.children);
        item != opal_list_get_end(&orte_odls_globals.children);
        item =  opal_list_get_next(item)) {
        child = (orte_odls_child_t *) item;
        OPAL_OUTPUT_VERBOSE((2, orte_odls_globals.output,
                             "%s odls:bproc:launch:local setting up io for %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(child->name)));
        /* only setup to forward stdin if it is rank 0, otherwise connect
         * to /dev/null
         */
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
            goto cleanup;
        }

        cycle++;
    }

    /* message to indicate that we are ready */
    ack = OBJ_NEW(opal_buffer_t);
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

