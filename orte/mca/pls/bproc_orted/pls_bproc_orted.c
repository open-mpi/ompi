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
 * See pls_bproc_orted.h for an overview of how it works.
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
#include "orte/mca/pls/base/base.h"
#include "orte/mca/rmaps/base/rmaps_base_map.h"
#include "orte/util/session_dir.h"
#include "orte/util/univ_info.h"

#include "pls_bproc_orted.h"

/**
 * Initialization of the bproc_orted module with all the needed function pointers
 */
orte_pls_base_module_1_0_0_t orte_pls_bproc_orted_module = {
    orte_pls_bproc_orted_launch,
    orte_pls_bproc_orted_terminate_job,
    orte_pls_bproc_orted_terminate_proc,
    orte_pls_bproc_orted_signal_job,
    orte_pls_bproc_orted_signal_proc,
    orte_pls_bproc_orted_finalize
};

static int pls_bproc_orted_make_dir(char *directory);
static char * pls_bproc_orted_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                                size_t app_context);
static void pls_bproc_orted_delete_dir_tree(char * path);
static int pls_bproc_orted_remove_dir(void);
static void pls_bproc_orted_send_cb(int status, orte_process_name_t * peer,
                                    orte_buffer_t* buffer, int tag, void* cbdata);
static int pls_bproc_orted_setup_stdio(orte_process_name_t *proc_name, 
                                       int proc_rank, orte_jobid_t jobid,
                                       size_t app_context, bool connect_stdin);


/**
 * Creates the passed directory. If the directory already exists, it and its
 * contents will be deleted then the directory will be created.
 * @param directory The directory to be created.
 * @retval ORTE_SUCCESS
 * @retval error
 */
static int
pls_bproc_orted_make_dir(char *directory)
{
    struct stat buf;
    mode_t my_mode = S_IRWXU;  /* at the least, I need to be able to do anything */

    if (0 == stat(directory, &buf)) { /* exists - delete it and its contents */
        pls_bproc_orted_delete_dir_tree(directory);
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
 pls_bproc_orted_get_base_dir_name(int proc_rank, orte_jobid_t jobid,
                                   size_t app_context)
{
    char *path = NULL, *user = NULL, *job = NULL;
    int rc;

    /* ensure that system info is set */
    orte_sys_info();

    if (NULL == orte_universe_info.name) {  /* error condition */
        ORTE_ERROR_LOG(ORTE_ERROR);
        return NULL;
    }

    rc = orte_ns_base_convert_jobid_to_string(&job, jobid);
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
    free(user);
    free(job);
    return path;
}


/**
 * deletes the passed directory tree recursively
 * @param path the path to the base directory to delete
 */
static void
pls_bproc_orted_delete_dir_tree(char * path)
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
 * Removes the bproc directory
 * @code /tmp/openmpi-bproc-<user>/ @endcode and all of its contents
 * @retval ORTE_SUCCESS
 * @retval error
 */
static int
pls_bproc_orted_remove_dir()
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
    pls_bproc_orted_delete_dir_tree(frontend);
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
pls_bproc_orted_send_cb(int status, orte_process_name_t * peer,
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
pls_bproc_orted_setup_stdio(orte_process_name_t *proc_name, int proc_rank,
                            orte_jobid_t jobid,
                            size_t app_context, bool connect_stdin)
{
    char *path_prefix, *fd_link_path = NULL;
    int rc = ORTE_SUCCESS, fd;
#if defined(HAVE_OPENPTY) && (OMPI_ENABLE_PTY_SUPPORT != 0)
    int amaster, aslave;
    char pty_name[256];
    struct termios term_attrs;
#endif
    
    path_prefix = pls_bproc_orted_get_base_dir_name(proc_rank, jobid, app_context);
    if (NULL == path_prefix) {
        rc = ORTE_ERROR;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }

    /* check for existence and access, or create it */
    if (ORTE_SUCCESS != (rc = pls_bproc_orted_make_dir(path_prefix))) {
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
             perror("pls_bproc_orted mkfifo failed");
             rc = ORTE_ERROR;
             ORTE_ERROR_LOG(rc);
             goto cleanup;
        }

        fd = open(fd_link_path, O_RDWR);
        if (-1 == fd) {
            perror("pls_bproc_orted open failed");
            rc = ORTE_ERROR;
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }

        orte_iof.iof_publish(proc_name, ORTE_IOF_SINK,
                             ORTE_IOF_STDIN, fd);
    } else {
        if(0 != symlink("/dev/null", fd_link_path)) {
            perror("pls_bproc_orted could not create symlink");
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
         opal_output(0, "pls_bproc_orted: openpty failed, using pipes instead");
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
         perror("pls_bproc_orted mkfifo failed");
         rc = ORTE_ERROR;
         goto cleanup;
    }

    fd = open(fd_link_path, O_RDWR);
    if (-1 == fd) {
        perror("pls_bproc_orted open failed");
        rc = ORTE_ERROR;
        goto cleanup;
    }

    orte_iof.iof_publish(proc_name, ORTE_IOF_SOURCE,
                         ORTE_IOF_STDOUT, fd);

stderr_fifo_setup:

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
         perror("pls_bproc_orted mkfifo failed");
         rc = ORTE_ERROR;
         goto cleanup;
    }

    fd = open(fd_link_path, O_RDWR);
    if (-1 == fd) {
        perror("pls_bproc_orted open failed");
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
 * @param jobid The jobid of the job to launch
 * @retval ORTE_SUCCESS
 * @retval error
 */
int
orte_pls_bproc_orted_launch(orte_jobid_t jobid)
{
    opal_list_t map;
    orte_rmaps_base_map_t * mapping;
    orte_rmaps_base_proc_t * proc;
    opal_list_item_t* item;
    int rc;
    int num_procs = 0;
    size_t i;
    int src = 0;
    orte_buffer_t *ack;
    char * param;
    bool connect_stdin;
    char * pty_name = NULL;

    /**
     * hack for bproc4, change process group so that we do not receive signals
     * from the parent/front-end process, as bproc4 does not currently allow the
     * process to intercept the signal
    */
    setpgid(0,0);

    /* get current node number */
    rc = bproc_currnode();
    if(0 > rc) {
        opal_output(0, "pls_bproc_orted component running on invalid node");
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

    /* figure out what processes will be on this node and set up the io files */
    for(item =  opal_list_get_first(&map);
        item != opal_list_get_end(&map);
        item =  opal_list_get_next(item)) {
        mapping = (orte_rmaps_base_map_t *) item;
        num_procs = 0;
        for(i = mapping->num_procs; i > 0; i--) {
            proc = mapping->procs[i - 1];
            if(0 < mca_pls_bproc_orted_component.debug) {
                opal_output(0, "orte_pls_bproc_orted_launch: setting up io for "
                               "[%lu,%lu,%lu] proc rank %lu\n",
                               ORTE_NAME_ARGS((&proc->proc_name)),
                               proc->proc_rank);
            }
            /* only setup to forward stdin if it is rank 0, otherwise connect
             * to /dev/null */
            if(0 == proc->proc_rank) {
                connect_stdin = true;
            } else {
                connect_stdin = false;
            }

            rc = pls_bproc_orted_setup_stdio(&proc->proc_name, num_procs, 
                                             jobid, mapping->app->idx,
                                             connect_stdin);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            num_procs++;
        }
    }

    /* message to indicate that we are ready */
    ack = OBJ_NEW(orte_buffer_t);
    rc = orte_dss.pack(ack, &src, 1, ORTE_INT);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }
    rc = mca_oob_send_packed_nb(MCA_OOB_NAME_SEED, ack, MCA_OOB_TAG_BPROC, 0,
        pls_bproc_orted_send_cb, NULL);
    if (0 > rc) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    rc = ORTE_SUCCESS;

cleanup:
    while(NULL != (item = opal_list_remove_first(&map))) {
        OBJ_RELEASE(item);
    }
    if(NULL != pty_name) {
        free(pty_name);
    }
    OBJ_DESTRUCT(&map);
    return rc;
}

/**
 * Function to terminate a job. Since this component only runs on remote nodes
 * and doesn't actually launch any processes, this function is not needed
 * so is a noop.
 * @param jobid The job to terminate
 * @retval ORTE_SUCCESS
 */
int orte_pls_bproc_orted_terminate_job(orte_jobid_t jobid)
{
    orte_iof.iof_flush();
    return ORTE_SUCCESS;
}

/**
 * Function to terminate a process. Since this component only runs on remote nodes
 * and doesn't actually launch any processes, this function is not needed
 * so is a noop.
 * @param proc the process's name
 * @retval ORTE_SUCCESS
 */
int orte_pls_bproc_orted_terminate_proc(const orte_process_name_t* proc)
{
    orte_iof.iof_flush();
    return ORTE_SUCCESS;
}

/**
 * Function to signal a job. Since this component only runs on remote nodes
 * and doesn't actually launch any processes, this function is not needed
 * so is a noop.
 * @param jobid The job to signal
 * @param signal The signal to send
 * @retval ORTE_SUCCESS
 */
int orte_pls_bproc_orted_signal_job(orte_jobid_t jobid, int32_t signal)
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
int orte_pls_bproc_orted_signal_proc(const orte_process_name_t* proc, int32_t signal)
{
    orte_iof.iof_flush();
    return ORTE_SUCCESS;
}


/**
 * Finalizes the bproc_orted module. Cleanup tmp directory/files
 * used for I/O forwarding.
 * @retval ORTE_SUCCESS
 */
int orte_pls_bproc_orted_finalize(void)
{
    orte_iof.iof_flush();
    pls_bproc_orted_remove_dir();
    orte_session_dir_finalize(orte_process_info.my_name);
    return ORTE_SUCCESS;
}

