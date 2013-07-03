/*
 * Copyright (c)      2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c)      2010-2011 Alex Brick <bricka@ccs.neu.edu>.
 *                         All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <sched.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <fcntl.h>

#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/constants.h"

#include "opal/mca/base/mca_base_var.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "opal/mca/event/event.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "crs_dmtcp.h"

#define MTCP_RESTART_COMMAND "mtcp_restart"

/*
 * DMTCP module
 */
static opal_crs_base_module_t dmtcp_module = {
    /** Initialization Function */
    opal_crs_dmtcp_module_init,
    /** Finalization Function */
    opal_crs_dmtcp_module_finalize,

    /** Checkpoint interface */
    opal_crs_dmtcp_checkpoint,

    /** Restart Command Access */
    opal_crs_dmtcp_restart,

    /** Disable checkpoints */
    opal_crs_dmtcp_disable_checkpoint,
    /** Enable checkpoints */
    opal_crs_dmtcp_enable_checkpoint,

    /** Prelaunch */
    opal_crs_dmtcp_prelaunch,

    /** Register Thread */
    opal_crs_dmtcp_reg_thread
};

/***************************
 * Snapshot Class Functions
 ***************************/
OBJ_CLASS_DECLARATION(opal_crs_dmtcp_snapshot_t);

struct opal_crs_dmtcp_snapshot_t {
    /** Base CRS snapshot type */
    opal_crs_base_snapshot_t super;
    char * context_filename;
};
typedef struct opal_crs_dmtcp_snapshot_t opal_crs_dmtcp_snapshot_t;

void opal_crs_dmtcp_construct(opal_crs_dmtcp_snapshot_t *obj);
void opal_crs_dmtcp_destruct(opal_crs_dmtcp_snapshot_t *obj);

OBJ_CLASS_INSTANCE(opal_crs_dmtcp_snapshot_t,
                   opal_crs_base_snapshot_t,
                   opal_crs_dmtcp_construct,
                   opal_crs_dmtcp_destruct);

/******************
 * Local Functions
 ******************/
static int dmtcp_cold_start(opal_crs_dmtcp_snapshot_t *snapshot);
static int dmtcp_generate_full_ckpt_path(opal_crs_dmtcp_snapshot_t *snapshot);
static void dmtcp_sleep_between_ckpt_callback(int interval);
static void dmtcp_pre_ckpt_callback(char **ckpt_filename);
static void dmtcp_post_ckpt_callback(int is_restarting,
                                     char *mtcp_restore_argv_start_addr);
static int dmtcp_should_ckpt_fd_callback(int fd);

/*************************
 * Local Global Variables
 *************************/
static char *full_ckpt_path = NULL;
static pthread_cond_t checkpoint_cond = PTHREAD_COND_INITIALIZER;
static pthread_cond_t checkpoint_done_cond = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t checkpoint_mutex = PTHREAD_MUTEX_INITIALIZER;
static int post_ckpt_state;

void opal_crs_dmtcp_construct(opal_crs_dmtcp_snapshot_t *snapshot) {
    snapshot->context_filename = NULL;
    snapshot->super.component_name =
      strdup(mca_crs_dmtcp_component.super.base_version.mca_component_name);
}

void opal_crs_dmtcp_destruct( opal_crs_dmtcp_snapshot_t *snapshot) {
    if(NULL != snapshot->context_filename) {
        free(snapshot->context_filename);
        snapshot->context_filename = NULL;
    }
}

/*****************
 * MCA Functions
 *****************/
int opal_crs_dmtcp_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: component_query()");

    *priority = mca_crs_dmtcp_component.super.priority;
    *module   = (mca_base_module_t *)&dmtcp_module;

    return OPAL_SUCCESS;
}

int opal_crs_dmtcp_module_init(void)
{
    char *temp_checkpoint_name;
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: module_init()");

    /*
     * JJH NOTE: Call any initialization routines you require
     */
    mtcp_set_callbacks(dmtcp_sleep_between_ckpt_callback, /* sleep_between_ckpt */
                       dmtcp_pre_ckpt_callback,           /* pre_ckpt */
                       dmtcp_post_ckpt_callback,          /* post_ckpt */
                       dmtcp_should_ckpt_fd_callback,     /* ckpt_fd */
                       NULL);                             /* write_ckpt_header */

    /* This serves to simply initialize MTCP.  The checkpoint file will
     * actually be set by our pre_ckpt callback (which takes it from the
     * snapshot given to the CRS checkpoint function), and the interval will be
     * ignored, substituted for a synchronization signal that is handled by our
     * sleep_between_ckpt callback.
     */

    asprintf(&temp_checkpoint_name, "checkpoint.dmtcp.%ld", syscall(SYS_getpid));
    mtcp_init(temp_checkpoint_name, 0, 1);
    mtcp_ok();

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: leaving module_init()");

    free(temp_checkpoint_name);

    return OPAL_SUCCESS;
}

int opal_crs_dmtcp_module_finalize(void)
{
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: module_finalize()");

    /*
     * JJH NOTE: Call any finalization routines you require
     */

    return OPAL_SUCCESS;
}

int opal_crs_dmtcp_prelaunch(int32_t rank,
                            char *base_snapshot_dir,
                            char **app,
                            char **cwd,
                            char ***argv,
                            char ***env)
{
    char * tmp_env_var = NULL;

    /*
     * The below should be left untouched for now
     */
    (void) mca_base_var_env_name("opal_cr_is_tool", &tmp_env_var);
    opal_setenv(tmp_env_var,
                "0", true, env);
    free(tmp_env_var);
    tmp_env_var = NULL;

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: leaving module_prelaunch()");

    return OPAL_SUCCESS;
}

int opal_crs_dmtcp_reg_thread(void)
{
    /*
     * JJH NOTE: If you require that all threads that may call into MTCP
     *           explicitly register with MTCP, then place the necessary
     *           initialization here.
     */

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: leaving module_reg_thread()");

    return OPAL_SUCCESS;
}

int opal_crs_dmtcp_checkpoint(pid_t pid,
                             opal_crs_base_snapshot_t *base_snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state)
{
    int unlock_retval, exit_status = OPAL_SUCCESS;
    char buf[BUFSIZ];
    opal_crs_dmtcp_snapshot_t *snapshot;

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: about to lock mutex for checkpoint()");

    pthread_mutex_lock(&checkpoint_mutex);
    snapshot = (opal_crs_dmtcp_snapshot_t *) base_snapshot;

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: checkpoint(%d, ---)", pid);

    /* Are we checkpointing ourselves or a peer.
     * JJH NOTE: This will only ever be called when pid == getpid()
     *           This is an old interface argument, that is no longer used.
     */

    /* bricka (2010-05-14): According to crs.h, 0 also indicates checkpointing
     * self.
     */
    if((pid != 0) && (pid != syscall(SYS_getpid)) ) {
        /* MTCP can only checkpoint a single process: we can only checkpoint
         * ourself. */
        *state = OPAL_CRS_ERROR;
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* the metadata file should always be NULL at this point */
    if ( NULL != snapshot->super.metadata) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: checkpoint(): Error: Metadata file already open");
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Update the snapshot metadata with the component name so opal-restart can
     * pick the correct CRS to restart with.
     */
    snapshot->super.component_name = strdup(mca_crs_dmtcp_component.super.base_version.mca_component_name);

    if( NULL == snapshot->super.metadata ) {
        if (NULL == (snapshot->super.metadata = fopen(snapshot->super.metadata_filename, "a")) ) {
            opal_output(mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: checkpoint(): Error: Unable to open the file (%s)",
                        snapshot->super.metadata_filename);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

    /* The filename of the checkpoint will be changed by our pre_ckpt hook
     * based on the options given to this function. */
    if(dmtcp_generate_full_ckpt_path(snapshot) == -1) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_checkpoint: unable to generate context filename.");

        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * JJH NOTE: You can write however much or little data you want to the
     *           metadata file. The metadata file is stored with the local
     *           checkpoint, and provided at restart time to help the
     *           CRS component deteremine how to restart from any files
     *           that is left in this directory during checkpoint.
     *           Use the command below to write key/value strings to the
     *           metadata file.
     *           (Just as we did above with the component name).
     */
    if ( 0 > fprintf(snapshot->super.metadata, "%s%s\n", CRS_METADATA_COMP,    snapshot->super.component_name)) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_checkpoint: unable to print component name to metadata");
    }

    if ( 0 > fprintf(snapshot->super.metadata, "%s%s\n", CRS_METADATA_CONTEXT, snapshot->context_filename)) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_checkpoint: unable to print context name to metadata");
    }

    fclose(snapshot->super.metadata );
    snapshot->super.metadata = NULL;

    /*
     * JJH NOTE: Setup and request a checkpoint of this process.
     */

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_checkpoint: will checkpoint to file: %s",
                        full_ckpt_path);

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_checkpoint: about to signal checkpoint");

     /* Now that we have set the requested filename, we simply need to start
      * the checkpoint. */
    pthread_cond_signal(&checkpoint_cond);

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_checkpoint: signalled checkpoint");

    /* We want to wait for the checkpoint to finish before we continue (in
     * particular, we need the post_ckpt hook to happen so that we know the
     * status of the checkpoint)
     */
    pthread_cond_wait(&checkpoint_done_cond, &checkpoint_mutex);

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_checkpoint: received checkpoint_done signal");

    /* We have now been checkpointed.  Note that the state of the checkpoint
     * (OPAL_CRS_CONTINUE, etc.) has been recorded by the post_ckpt hook.
     */
    *state = post_ckpt_state;
    exit_status = OPAL_SUCCESS;

    free(full_ckpt_path);

 cleanup:
    unlock_retval = pthread_mutex_unlock(&checkpoint_mutex);

    if( 0 != unlock_retval ) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_checkpoint: unable to unlock mutex at end of checkpoint: %s",
                    strerror_r(unlock_retval, buf, BUFSIZ));

        exit_status = OPAL_ERROR;
    }

    if( NULL != snapshot->super.metadata ) {
        fclose(snapshot->super.metadata );
        snapshot->super.metadata = NULL;
    }

    return exit_status;
}

int opal_crs_dmtcp_restart(opal_crs_base_snapshot_t *base_snapshot, bool spawn_child, pid_t *child_pid)
{
    int ret, exit_status = OPAL_SUCCESS;
    int exec_status;

    opal_crs_dmtcp_snapshot_t *snapshot = OBJ_NEW(opal_crs_dmtcp_snapshot_t);
    snapshot->super = *base_snapshot;

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: restart(--, %d)", spawn_child);

    /*
     * JJH NOTE: 'cold_start' indicates that this process is being restarted from
     *           opal-restart instead of from within an already running process.
     *           In the current code base, this is always set to true since it
     *           does not allow a process to request a restart of itself.
     */
    if(snapshot->super.cold_start) {
        /*
         * Read the metadata left by the checkpoint() of this process
         */
        if( OPAL_SUCCESS != (ret = dmtcp_cold_start(snapshot)) ) {
            opal_output(mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_restart: Unable to reconstruct the snapshot.");
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

    /* JJH NOTE: Nearly all of the time the 'spawn_child' argument is set to
     *           'false' indicating that the restart function is expected to
     *           call exec() directly. It is only set to 'true' if the user
     *           explicitly tells opal-restart to spawn off the child, which
     *           rarely/never happens. So I would not worry about that option.
     */
    if( spawn_child ) {
        pid_t child_pid = fork();

        if(child_pid > 0)
            goto cleanup;
        else if(child_pid < 0) {
            opal_output(mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_restart: Unable to spawn child.");
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }

    /*
     * JJH NOTE: Restart the process by replacing this process
     */

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_restart: About to invoke command: %s with argv: %s %s",
                        MTCP_RESTART_COMMAND,
                        MTCP_RESTART_COMMAND,
                        snapshot->context_filename);

    exec_status = execlp(MTCP_RESTART_COMMAND, MTCP_RESTART_COMMAND, snapshot->context_filename, NULL);

    /* If we get down here, something has broken. */

    if(exec_status < 0)
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_restart: error in replacing process: %s",
                    strerror(errno));
    else
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_restart: exec() returned!");

    exit_status = OPAL_ERROR;
    goto cleanup;

 cleanup:
    return exit_status;
}

int opal_crs_dmtcp_disable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: disable_checkpoint()");

    /*
     * JJH NOTE: Enter a critical section. This is not really used in the code
     *           at the moment.
     */
    mtcp_no();

    return OPAL_SUCCESS;
}

int opal_crs_dmtcp_enable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: enable_checkpoint()");
    /*
     * JJH NOTE: Leave a critical section. This is not really used in the code
     *           at the moment.
     */
    mtcp_ok();

    return OPAL_SUCCESS;
}

/*****************************
 * Local Function Definitions
 *****************************/
static int dmtcp_cold_start(opal_crs_dmtcp_snapshot_t *snapshot) {
    int ret, exit_status = OPAL_SUCCESS;
    char **tmp_argv = NULL;
    char * component_name = NULL;
    int prev_pid;

    /*
     * Find the snapshot directory, read the metadata file for
     * component name and previous pid
     */
    if( NULL == snapshot->super.metadata ) {
        if (NULL == (snapshot->super.metadata = fopen(snapshot->super.metadata_filename, "r")) ) {
            opal_output(mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: dmtcp_cold_start(): Error: Unable to open the file (%s)",
                        snapshot->super.metadata_filename);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }
    if( OPAL_SUCCESS != (ret = opal_crs_base_extract_expected_component(snapshot->super.metadata,
                                                                        &component_name, &prev_pid) ) ) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_cold_start: Error: Failed to extract the metadata from the local snapshot (%s). Returned %d.",
                    snapshot->super.metadata_filename, ret);
        exit_status = ret;
        goto cleanup;
    }

    snapshot->super.component_name = strdup(component_name);

    /*
     * Compare the component strings to make sure this is our snapshot before going further.
     * JJH NOTE: This will nearly always be true since opal-restart also checks this metadata.
     */
    if ( 0 != strncmp(mca_crs_dmtcp_component.super.base_version.mca_component_name,
                      component_name, strlen(component_name)) ) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_cold_start: Error: This snapshot (%s) is not intended for us (%s)\n",
                    component_name, mca_crs_dmtcp_component.super.base_version.mca_component_name);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /*
     * Read context information from the metadata file
     */
    opal_crs_base_metadata_read_token(snapshot->super.metadata, CRS_METADATA_CONTEXT, &tmp_argv);
    if( NULL == tmp_argv ) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: dmtcp_cold_start: Error: Failed to read the %s token from the local checkpoint in %s",
                    CRS_METADATA_CONTEXT, snapshot->super.snapshot_directory);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    asprintf(&(snapshot->context_filename), "%s/%s", snapshot->super.snapshot_directory, tmp_argv[0]);

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: cold_start(%s)", snapshot->context_filename);

    /*
     * Reset the cold_start flag
     */
    snapshot->super.cold_start = false;

 cleanup:
    if(NULL != tmp_argv) {
        opal_argv_free(tmp_argv);
        tmp_argv = NULL;
    }

    if( NULL != snapshot->super.metadata ) {
        fclose(snapshot->super.metadata);
        snapshot->super.metadata = NULL;
    }

    return exit_status;
}

/**
 * Given a snapshot, generate the context filename and its full path.
 *
 * @param snapshot the snapshot with request information
 */
static int dmtcp_generate_full_ckpt_path(opal_crs_dmtcp_snapshot_t *snapshot)
{
    int retval;
    retval = asprintf(&(snapshot->context_filename), "ompi_dmtcp_context.%ld", syscall(SYS_getpid));
    if(retval == -1)
        return -1;

    return asprintf(&full_ckpt_path, "%s/%s", snapshot->super.snapshot_directory, snapshot->context_filename);
}

/**
 * This is a callback function to call the actual checkpointing routine.
 * Instead of waiting for a specific interval as MTCP does, we will wait on a
 * synchronization signal that will allow us to checkpoint on demand.  The
 * argument to this function will be ignored.
 */
static void dmtcp_sleep_between_ckpt_callback(int interval)
{
    int signal_retval;
    char buf[BUFSIZ];

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: called sleep_between_ckpt callback");

    pthread_mutex_lock(&checkpoint_mutex);

    /* If the MPI checkpoint thread is waiting on the checkpoint_done_cond and
     * this thread is here, it means that a checkpoint has just completed.
     * Let's signal the MPI checkpoint thread to resume. */
    signal_retval = pthread_cond_signal(&checkpoint_done_cond);

    if( 0 != signal_retval) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: post_ckpt_callback(): Unable to signal checkpoint done: %s",
                    strerror_r(signal_retval, buf, BUFSIZ));
    }

    /* now we simply wait for the signal to checkpoint */
    pthread_cond_wait(&checkpoint_cond, &checkpoint_mutex);

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: received sync signal to checkpoint.");

    /* We have now been instructed to checkpoint, so we return. Note that the
     * mutex is still locked: the post_ckpt callback will unlock it. */
}

/**
 * This is a callback function that is invoked before the checkpoint actually
 * occurs.  It enables us to do any logging that is necessary, as well as change
 * the filename that the checkpoint will be written to.  We expect that this
 * filename will be pulled from the checkpoint options.
 *
 * @param ckpt_filename a pointer in which to store the desired checkpoint
 *      filename
 */
static void dmtcp_pre_ckpt_callback(char **ckpt_filename)
{
    *ckpt_filename = full_ckpt_path;
}

/**
 * This is a callback function that is invoked after the checkpoint has
 * finished.  It enables us to do any logging that is necessary, as well as
 * report whether this is called from a restart or a checkpoint.  We will report
 * this status, signal the CRS code to continue running, and then release the
 * mutex that we are holding.
 *
 * @param is_restarting whether or not this is being called as part of a restart
 * @param mtcp_restore_argv_start_addr unused
 */
static void dmtcp_post_ckpt_callback(int is_restarting, char *mtcp_restore_argv_start_addr)
{
    int unlock_retval;
    char buf[BUFSIZ];

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: in post_ckpt_callback, restarting: %d", is_restarting);
    if(is_restarting)
        post_ckpt_state = OPAL_CRS_RESTART;
    else
        post_ckpt_state = OPAL_CRS_CONTINUE;

    opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                        "crs:dmtcp: unlocking at end of post_ckpt_callback");

    unlock_retval = pthread_mutex_unlock(&checkpoint_mutex);

    if( 0 != unlock_retval) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: post_ckpt_callback(): Unable to unlock mutex: %s",
                    strerror_r(unlock_retval, buf, BUFSIZ));
    }
}

/**
 * This is a callback function that is invoked by DMTCP to see if it should
 * checkpoint the given file descriptor.
 *
 * If the file descriptor is a socket, named-pipe or pseudo-terminal, DMTCP
 * should skip checkpointing them.
 *
 * If we can't determine the type of fd (stat and/or readlink failed), we ask
 * DMTCP to try to checkpoint them anyways with the assumption that DMTCP would
 * warn users of any such case.
 *
 * @param fd file descriptor to checkpoint
 * @return: 1 if DMTCP should ckpt the file descriptor, 0 otherwise.
 */
static int dmtcp_should_ckpt_fd_callback(int fd)
{
    struct stat stat_buf;
    char device_name[PATH_MAX];
    char proc_filename[64];
    char buf[BUFSIZ];

    if (fstat(fd, &stat_buf) != 0) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: should_ckpt_fd_callback(): error stat()'ing %d: %s",
                    fd, strerror_r(errno, buf, BUFSIZ));
        return 1;
    /* Don't checkpoint sockets and FIFOs */
    } else if (S_ISSOCK(stat_buf.st_mode) || S_ISFIFO(stat_buf.st_mode)) {
        opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                            "crs:dmtcp: skipping checkpointing socket/fifo: %d",
                            fd);
        return 0;
    }

    memset(device_name, 0, sizeof device_name);
    sprintf(proc_filename, "/proc/self/fd/%d", fd);
    if (readlink(proc_filename, device_name, sizeof(device_name) - 1) <= 0) {
        opal_output(mca_crs_dmtcp_component.super.output_handle,
                    "crs:dmtcp: should_ckpt_fd_callback(): readlink(%d) failed: %s",
                    fd, strerror_r(errno, buf, BUFSIZ));
        return 1;
    }

    /* Don't checkpoint ptys */
    if (strstr(device_name, "/dev/pts/") == 0 ||
        strstr(device_name, "/dev/pty") == 0 ||
        strstr(device_name, "/dev/tty") == 0) {
        opal_output_verbose(10, mca_crs_dmtcp_component.super.output_handle,
                            "crs:dmtcp: skipping checkpointing %s",
                            device_name);
        return 0;
    }

    /* Checkpoint fd by default */
    return 1;
}
