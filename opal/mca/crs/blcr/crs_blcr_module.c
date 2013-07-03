/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Evergrid, Inc. All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
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
#include <fcntl.h>

#include "opal/util/show_help.h"
#include "opal/util/output.h"
#include "opal/util/argv.h"
#include "opal/constants.h"

#include "opal/mca/base/mca_base_var.h"

#include "opal/threads/threads.h"
#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "opal/mca/event/event.h"

#include "opal/mca/crs/crs.h"
#include "opal/mca/crs/base/base.h"

#include "crs_blcr.h"

/*
 * Blcr module
 */
static opal_crs_base_module_t blcr_module = {
    /** Initialization Function */
    opal_crs_blcr_module_init,
    /** Finalization Function */
    opal_crs_blcr_module_finalize,

    /** Checkpoint interface */
    opal_crs_blcr_checkpoint,

    /** Restart Command Access */
    opal_crs_blcr_restart,

    /** Disable checkpoints */
    opal_crs_blcr_disable_checkpoint,
    /** Enable checkpoints */
    opal_crs_blcr_enable_checkpoint,

    /** Prelaunch */
    opal_crs_blcr_prelaunch,

    /** Register Thread */
    opal_crs_blcr_reg_thread
};

/***************************
 * Snapshot Class Functions
 ***************************/
OBJ_CLASS_DECLARATION(opal_crs_blcr_snapshot_t);

struct opal_crs_blcr_snapshot_t {
    /** Base CRS snapshot type */
    opal_crs_base_snapshot_t super;
    char * context_filename;
};
typedef struct opal_crs_blcr_snapshot_t opal_crs_blcr_snapshot_t;

void opal_crs_blcr_construct(opal_crs_blcr_snapshot_t *obj);
void opal_crs_blcr_destruct( opal_crs_blcr_snapshot_t *obj);

OBJ_CLASS_INSTANCE(opal_crs_blcr_snapshot_t,
                   opal_crs_base_snapshot_t,
                   opal_crs_blcr_construct,
                   opal_crs_blcr_destruct);

/******************
 * Local Functions
 ******************/
static int blcr_get_checkpoint_filename(char **fname, pid_t pid);
static int opal_crs_blcr_thread_callback(void *arg);
static int opal_crs_blcr_signal_callback(void *arg);

static int opal_crs_blcr_restart_cmd(char *fname, char **cmd);

static int blcr_cold_start(opal_crs_blcr_snapshot_t *snapshot);

#if OPAL_ENABLE_CRDEBUG == 1
static void MPIR_checkpoint_debugger_crs_hook(cr_hook_event_t event);
#endif

/*************************
 * Local Global Variables
 *************************/
#if OPAL_ENABLE_CRDEBUG == 1
static opal_thread_t *checkpoint_thread_id = NULL;
static bool blcr_crdebug_refreshed_env = false;
#endif

static cr_client_id_t client_id;
static cr_callback_id_t cr_thread_callback_id;
static cr_callback_id_t cr_signal_callback_id;
static int blcr_current_state = OPAL_CRS_NONE;

static char *blcr_restart_cmd = NULL;
static char *blcr_checkpoint_cmd = NULL;

static opal_condition_t blcr_cond;
static opal_mutex_t     blcr_lock;

static pid_t my_pid = -1;

void opal_crs_blcr_construct(opal_crs_blcr_snapshot_t *snapshot) {
    snapshot->context_filename = NULL;
    snapshot->super.component_name = strdup(mca_crs_blcr_component.super.base_version.mca_component_name);
}

void opal_crs_blcr_destruct( opal_crs_blcr_snapshot_t *snapshot) {
    if(NULL != snapshot->context_filename) {
        free(snapshot->context_filename);
        snapshot->context_filename = NULL;
    }
}

/*****************
 * MCA Functions
 *****************/ 
int opal_crs_blcr_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: component_query()");

    *priority = mca_crs_blcr_component.super.priority;
    *module = (mca_base_module_t *)&blcr_module;

    return OPAL_SUCCESS;
}

int opal_crs_blcr_module_init(void)
{
    void *crs_blcr_thread_callback_arg = NULL;
    void *crs_blcr_signal_callback_arg = NULL;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: module_init()");

    blcr_restart_cmd    = strdup("cr_restart");
    blcr_checkpoint_cmd = strdup("cr_checkpoint");

    my_pid = getpid();

    if( !opal_cr_is_tool ) {
        /* We need to make the lock and condition variable before
         * starting the thread, since the thread uses these vars.
         */
        OBJ_CONSTRUCT(&blcr_lock, opal_mutex_t);
        OBJ_CONSTRUCT(&blcr_cond, opal_condition_t);

        /*
         * Initialize BLCR
         */
        client_id = cr_init();
        if (0 > client_id) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "Error: crs:blcr: module_init: cr_init failed (%d)\n", client_id);
            return OPAL_ERROR;
        }
    }

#if OPAL_ENABLE_CRDEBUG == 1
    blcr_crdebug_refreshed_env = false;
#endif

    blcr_restart_cmd    = strdup("cr_restart");
    blcr_checkpoint_cmd = strdup("cr_checkpoint");
    
    if( !opal_cr_is_tool ) {
        /*
         * Register the thread handler
         */
        cr_thread_callback_id = cr_register_callback(opal_crs_blcr_thread_callback,
                                                     crs_blcr_thread_callback_arg,
                                                     CR_THREAD_CONTEXT);
        /*
         * Register the signal handler
         *  - even though we do not use it
         */
        cr_signal_callback_id = cr_register_callback(opal_crs_blcr_signal_callback,
                                                     crs_blcr_signal_callback_arg,
                                                     CR_SIGNAL_CONTEXT);

#if OPAL_ENABLE_CRDEBUG == 1
        /*
         * Checkpoint/restart enabled debugging hooks
         *  "NO_CALLBACKS"   -> non-MPI threads
         *  "SIGNAL_CONTEXT" -> MPI threads
         *  "THREAD_CONTEXT" -> BLCR threads
         */
        cr_register_hook(CR_HOOK_CONT_NO_CALLBACKS,   MPIR_checkpoint_debugger_crs_hook);
        cr_register_hook(CR_HOOK_CONT_SIGNAL_CONTEXT, MPIR_checkpoint_debugger_crs_hook);

        cr_register_hook(CR_HOOK_RSTRT_NO_CALLBACKS,   MPIR_checkpoint_debugger_crs_hook);
        cr_register_hook(CR_HOOK_RSTRT_SIGNAL_CONTEXT, MPIR_checkpoint_debugger_crs_hook);
#endif
    }

    /*
     * Now that we are done with init, set the state to running
     */
    blcr_current_state = OPAL_CRS_RUNNING;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: module_init() --> Finished [%d]",
                        opal_cr_is_tool);
    
    return OPAL_SUCCESS;
}

int opal_crs_blcr_prelaunch(int32_t rank,
                            char *base_snapshot_dir,
                            char **app,
                            char **cwd,
                            char ***argv,
                            char ***env)
{
    char * tmp_env_var = NULL;

    (void) mca_base_var_env_name("opal_cr_is_tool", &tmp_env_var);
    opal_setenv(tmp_env_var,
                "0", true, env);
    free(tmp_env_var);
    tmp_env_var = NULL;

    return OPAL_SUCCESS;
}

int opal_crs_blcr_reg_thread(void)
{
    cr_client_id_t loc_client_id;

    /*
     * Initialize BLCR
     */
    loc_client_id = cr_init();
    if (0 > loc_client_id) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "Error: crs:blcr: reg_thread: cr_init failed (%d)\n", loc_client_id);
        return OPAL_ERROR;
    }

    return OPAL_SUCCESS;
}

int opal_crs_blcr_module_finalize(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: module_finalize()");

    /* Cleanup some memory */
    if( NULL != blcr_restart_cmd ) {
        free(blcr_restart_cmd);
        blcr_restart_cmd = NULL;
    }
    if( NULL != blcr_checkpoint_cmd ) {
        free(blcr_checkpoint_cmd);
        blcr_checkpoint_cmd = NULL;
    }

    if( !opal_cr_is_tool ) {
        OBJ_DESTRUCT(&blcr_lock);
        OBJ_DESTRUCT(&blcr_cond);

        if( OPAL_CRS_RUNNING == blcr_current_state ) {
            /* Unload the thread callback */
            cr_replace_callback(cr_thread_callback_id, NULL, NULL, CR_THREAD_CONTEXT);
            /* Unload the signal callback */
            cr_replace_callback(cr_signal_callback_id, NULL, NULL, CR_SIGNAL_CONTEXT);
        }

#if OPAL_ENABLE_CRDEBUG == 1
        /*
         * Checkpoint/restart enabled debugging hooks
         */
        cr_register_hook(CR_HOOK_CONT_NO_CALLBACKS,   NULL);
        cr_register_hook(CR_HOOK_CONT_SIGNAL_CONTEXT, NULL);

        cr_register_hook(CR_HOOK_RSTRT_NO_CALLBACKS,   NULL);
        cr_register_hook(CR_HOOK_RSTRT_SIGNAL_CONTEXT, NULL);
#endif
    }

    /* BLCR does not have a finalization routine */
    blcr_current_state = OPAL_CRS_NONE;

    return OPAL_SUCCESS;
}

int opal_crs_blcr_checkpoint(pid_t pid,
                             opal_crs_base_snapshot_t *base_snapshot,
                             opal_crs_base_ckpt_options_t *options,
                             opal_crs_state_type_t *state)
{
    int ret, exit_status = OPAL_SUCCESS;
    opal_crs_blcr_snapshot_t *snapshot = NULL;
#if CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT == 1
    cr_checkpoint_args_t cr_args;
    static cr_checkpoint_handle_t cr_handle = (cr_checkpoint_handle_t)(-1);
#endif
    int fd = 0;
    char *loc_fname = NULL;

    if( pid != my_pid ) {
        opal_output(0, "crs:blcr: checkpoint(%d, ---): Checkpointing of peers not allowed!", pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint(%d, ---)", pid);

    snapshot = (opal_crs_blcr_snapshot_t *)base_snapshot;

    /*
     * Update the snapshot metadata
     */
    snapshot->super.component_name = strdup(mca_crs_blcr_component.super.base_version.mca_component_name);
    blcr_get_checkpoint_filename(&(snapshot->context_filename), pid);

    if( NULL == snapshot->super.metadata ) {
        if (NULL == (snapshot->super.metadata = fopen(snapshot->super.metadata_filename, "a")) ) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint(): Error: Unable to open the file (%s)",
                        snapshot->super.metadata_filename);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }
    fprintf(snapshot->super.metadata, "%s%s\n", CRS_METADATA_COMP,    snapshot->super.component_name);
    fprintf(snapshot->super.metadata, "%s%s\n", CRS_METADATA_CONTEXT, snapshot->context_filename);

    fclose(snapshot->super.metadata );
    snapshot->super.metadata = NULL;

    /*
     * If we can checkpointing ourselves do so:
     * use cr_request_checkpoint() if available, and cr_request_file() if not
     */
    if( opal_crs_blcr_dev_null ) {
        loc_fname = strdup("/dev/null");
    } else {
        asprintf(&loc_fname, "%s/%s", snapshot->super.snapshot_directory, snapshot->context_filename);
    }

#if OPAL_ENABLE_CRDEBUG == 1
    /* Make sure to identify the checkpointing thread, so that it is not
     * prevented from requesting the checkpoint after the debugger detaches
     */
    opal_cr_debug_set_current_ckpt_thread_self();
    checkpoint_thread_id = opal_thread_get_self();
    blcr_crdebug_refreshed_env = false;

    /* If checkpoint/restart enabled debugging  then mark detachment place */
    if( MPIR_debug_with_checkpoint ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: checkpoint(): Detaching debugger...");
        MPIR_checkpoint_debugger_detach();
    }
#endif

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint SELF <%s>",
                        loc_fname);

#if CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT == 1 || CRS_BLCR_HAVE_CR_REQUEST == 1
#if CRS_BLCR_HAVE_CR_REQUEST_CHECKPOINT == 1
    fd = open(loc_fname,
              O_WRONLY | O_CREAT | O_TRUNC | O_LARGEFILE,
              S_IRUSR | S_IWUSR);
    if( fd < 0 ) {
        *state = OPAL_CRS_ERROR;
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: checkpoint(): Error: Unable to open checkpoint file (%s) for pid (%d)",
                    loc_fname, pid);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    cr_initialize_checkpoint_args_t(&cr_args);
    cr_args.cr_scope = CR_SCOPE_PROC;
    cr_args.cr_fd    = fd;
    if( options->stop ) {
        cr_args.cr_signal = SIGSTOP;
    }

    ret = cr_request_checkpoint(&cr_args, &cr_handle);
    if( ret < 0 ) {
        close(cr_args.cr_fd);
        *state = OPAL_CRS_ERROR;
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: checkpoint(): Error: Unable to checkpoint pid (%d) to file (%s)",
                    pid, loc_fname);
        exit_status = ret;
        goto cleanup;
    }

    /* Wait for checkpoint to finish */
    do {
        ret = cr_poll_checkpoint(&cr_handle, NULL);
        if( ret < 0 ) {
            /* Check if restarting. This is not an error. */
            if( (ret == CR_POLL_CHKPT_ERR_POST) && (errno == CR_ERESTARTED) ) {
                ret = 0;
                break;
            }
            /* If Call was interrupted by a signal, retry the call */
            else if (errno == EINTR) {
                ;
            }
            /* Otherwise this is a real error that we need to deal with */
            else {
                *state = OPAL_CRS_ERROR;
                opal_output(mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: checkpoint(): Error: Unable to checkpoint pid (%d) to file (%s) - poll failed with (%d)",
                            pid, loc_fname, ret);
                exit_status = ret;
                goto cleanup;
            }
        }
    } while( ret < 0 );

    /* Close the file */
    close(cr_args.cr_fd);
#else
    /* Request a checkpoint be taken of the current process.
     * Since we are not guaranteed to finish the checkpoint before this
     * returns, we also need to wait for it.
     */
    cr_request_file(loc_fname);
        
    /* Wait for checkpoint to finish */
    do {
        usleep(1000); /* JJH Do we really want to sleep? */
    } while(CR_STATE_IDLE != cr_status());
#endif
#endif

    *state = blcr_current_state;
    free(loc_fname);
    
 cleanup:
    if( NULL != snapshot->super.metadata ) {
        fclose(snapshot->super.metadata );
        snapshot->super.metadata = NULL;
    }

    return exit_status;
}

int opal_crs_blcr_restart(opal_crs_base_snapshot_t *base_snapshot, bool spawn_child, pid_t *child_pid)
{
    opal_crs_blcr_snapshot_t *snapshot = OBJ_NEW(opal_crs_blcr_snapshot_t);
    char **cr_argv = NULL;
    char *cr_cmd = NULL;
    char *cr_full_cmd = NULL;
    int ret;
    int exit_status = OPAL_SUCCESS;
    int status;

    snapshot->super = *base_snapshot;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: restart(--, %d)",  spawn_child);

    /*
     * If we need to reconstruct the snapshot,
     */
    if(snapshot->super.cold_start) {
        if( OPAL_SUCCESS != (ret = blcr_cold_start(snapshot)) ) {
            exit_status = OPAL_ERROR;
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: Unable to reconstruct the snapshot.");
            goto cleanup;
        }
    }
    

    /*
     * Get the restart command
     */
    if ( OPAL_SUCCESS != (ret = opal_crs_blcr_restart_cmd(snapshot->context_filename, &cr_cmd)) ) {
        exit_status = ret;
        goto cleanup;
    }
    if ( NULL == (cr_argv = opal_argv_split(cr_cmd, ' ')) ) {
        exit_status = OPAL_ERROR;
        goto cleanup;
    }

    /* Need to shutdown the event engine before this.
     * for some reason the BLCR checkpointer and our event engine don't get
     * along very well.
     */
    opal_progress_finalize();
    (void) mca_base_framework_close(&opal_event_base_framework);

    if (!spawn_child) {
        cr_full_cmd = opal_argv_join(cr_argv, ' ');
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: blcr_restart: SELF: exec :(%s, %s):", 
                            blcr_restart_cmd, cr_full_cmd);

        status = execvp(blcr_restart_cmd, cr_argv);

        if(status < 0) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: SELF: Child failed to execute :(%d):", status);
        }
        opal_show_help("help-opal-crs-blcr.txt", "blcr:restart_failed_exec", true,
                       status,
                       blcr_restart_cmd,
                       cr_full_cmd);

        exit_status = status;
        goto cleanup;
    }
    /*
     * Restart by starting a new process
     */
    else {
        *child_pid = fork();

        if( 0 == *child_pid) {
            /* Child Process */
            opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                                "crs:blcr: blcr_restart: CHILD: exec :(%s, %s):", 
                                blcr_restart_cmd,
                                opal_argv_join(cr_argv, ' '));
            
            status = execvp(blcr_restart_cmd, cr_argv);

            if(status < 0) {
                opal_output(mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: blcr_restart: CHILD: Child failed to execute :(%d):", status);
            }
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: CHILD: execvp returned %d", status);

            exit_status = status;
            goto cleanup;
        }
        else if(*child_pid > 0) {
            /* Parent is done once it is started. */
            ;
        }
        else {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: blcr_restart: CHILD: fork failed :(%d):", *child_pid);
        }
    }

 cleanup:
    if(NULL != cr_cmd)
        free(cr_cmd);
    if(NULL != cr_argv)
        opal_argv_free(cr_argv);
    
    return exit_status;
}
    
int opal_crs_blcr_disable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: disable_checkpoint()");
    /*
     * Enter the BLCR Critical Section
     */
    cr_enter_cs(client_id);

    return OPAL_SUCCESS;
}

int opal_crs_blcr_enable_checkpoint(void)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: enable_checkpoint()");
    /*
     * Leave the BLCR Critical Section
     */
    cr_leave_cs(client_id);

    return OPAL_SUCCESS;
}

/*****************************
 * Local Function Definitions
 *****************************/
static int opal_crs_blcr_thread_callback(void *arg) {
    const struct cr_checkpoint_info *ckpt_info = cr_get_checkpoint_info();
    int ret;
    
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: thread_callback()");

    OPAL_THREAD_LOCK(&blcr_lock);
    blcr_current_state = OPAL_CRS_CHECKPOINT;

    /*
     * Allow the checkpoint to be taken, if we requested it
     */
#if CRS_BLCR_HAVE_INFO_REQUESTER == 1
    if( ckpt_info->requester != my_pid ) {
        ret = cr_checkpoint(CR_CHECKPOINT_OMIT);
        blcr_current_state = OPAL_CRS_RUNNING;
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: thread_callback(); WARNING: An external agent attempted to checkpoint this process "
                            "when it did not expect to be checkpointed. Skipping this checkpoint request."
                            " [%d != %d].", ckpt_info->requester, my_pid);
        return 0;
    }
    else
#endif
    {
        if(OPAL_SUCCESS != (ret = trigger_user_inc_callback(OMPI_CR_INC_CRS_PRE_CKPT,
                                                            OMPI_CR_INC_STATE_PREPARE)) ) {
            ;
        }

        ret = cr_checkpoint(0);
    }
    
    /*
     * Restarting
     */
    if ( 0 < ret ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: thread_callback: Restarting.");
        blcr_current_state = OPAL_CRS_RESTART;
    }
    /*
     * Continuing
     */
    else {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: thread_callback: Continue.");
        blcr_current_state = OPAL_CRS_CONTINUE;
    }

    if( OPAL_SUCCESS != (ret = trigger_user_inc_callback(OMPI_CR_INC_CRS_POST_CKPT,
                                                         (blcr_current_state == OPAL_CRS_CONTINUE ?
                                                          OMPI_CR_INC_STATE_CONTINUE :
                                                          OMPI_CR_INC_STATE_RESTART))) ) {
        ;
    }

    OPAL_THREAD_UNLOCK(&blcr_lock);
    opal_condition_signal(&blcr_cond);

    return 0;
}

static int opal_crs_blcr_signal_callback(void *arg) {
    const struct cr_checkpoint_info *ckpt_info = cr_get_checkpoint_info();
    int ret;

    /*
     * Allow the checkpoint to be taken, if we requested it
     */
#if CRS_BLCR_HAVE_INFO_REQUESTER == 1
    if( ckpt_info->requester != my_pid ) {
        ret = cr_checkpoint(CR_CHECKPOINT_OMIT);
        return 0;
    }
    else
#endif
    {
        ret = cr_checkpoint(0);
    }

    return 0;
}

static int opal_crs_blcr_restart_cmd(char *fname, char **cmd)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: restart_cmd(%s, ---)", fname);

    if (NULL == fname) {
        opal_output_verbose(10, opal_crs_base_framework.framework_output, 
                            "crs:blcr: restart_cmd: Error: filename is NULL!");
        return OPAL_CRS_ERROR;
    }

    asprintf(cmd, "%s %s", blcr_restart_cmd, fname);

    return OPAL_SUCCESS;
}

static int blcr_get_checkpoint_filename(char **fname, pid_t pid)
{
    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: get_checkpoint_filename(--, %d)", pid);

    asprintf(fname, "ompi_blcr_context.%d", pid);
    
    return OPAL_SUCCESS;
}

static int blcr_cold_start(opal_crs_blcr_snapshot_t *snapshot) {
    int ret, exit_status = OPAL_SUCCESS;
    char **tmp_argv = NULL;
    char * component_name = NULL;
    int prev_pid;

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: cold_start()");

    /*
     * Find the snapshot directory, read the metadata file
     */
    if( NULL == snapshot->super.metadata ) {
        if (NULL == (snapshot->super.metadata = fopen(snapshot->super.metadata_filename, "r")) ) {
            opal_output(mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: checkpoint(): Error: Unable to open the file (%s)",
                        snapshot->super.metadata_filename);
            exit_status = OPAL_ERROR;
            goto cleanup;
        }
    }
    if( OPAL_SUCCESS != (ret = opal_crs_base_extract_expected_component(snapshot->super.metadata,
                                                                        &component_name, &prev_pid) ) ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_cold_start: Error: Failed to extract the metadata from the local snapshot (%s). Returned %d.",
                    snapshot->super.metadata_filename, ret);
        exit_status = ret;
        goto cleanup;
    }

    snapshot->super.component_name = strdup(component_name);

    /* Compare the component strings to make sure this is our snapshot before going further */
    if ( 0 != strncmp(mca_crs_blcr_component.super.base_version.mca_component_name,
                      component_name, strlen(component_name)) ) {
        exit_status = OPAL_ERROR;
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_cold_start: Error: This snapshot (%s) is not intended for us (%s)\n", 
                    component_name, mca_crs_blcr_component.super.base_version.mca_component_name);
        goto cleanup;
    }

    /*
     * Context Filename
     */
    opal_crs_base_metadata_read_token(snapshot->super.metadata, CRS_METADATA_CONTEXT, &tmp_argv);
    if( NULL == tmp_argv ) {
        opal_output(mca_crs_blcr_component.super.output_handle,
                    "crs:blcr: blcr_cold_start: Error: Failed to read the %s token from the local checkpoint in %s",
                    CRS_METADATA_CONTEXT, snapshot->super.snapshot_directory);
        exit_status = OPAL_ERROR;
        goto cleanup;
    }
    asprintf(&snapshot->context_filename, "%s/%s", snapshot->super.snapshot_directory, tmp_argv[0]);

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

#if OPAL_ENABLE_CRDEBUG == 1
static void MPIR_checkpoint_debugger_crs_hook(cr_hook_event_t event) {
    opal_thread_t *my_thread_id = NULL;
    my_thread_id = opal_thread_get_self();

    /* Non-MPI threads */
    if(event == CR_HOOK_RSTRT_NO_CALLBACKS ) {
        /* wait for the MPI thread to refresh the environment for us */
        while(!blcr_crdebug_refreshed_env) {
            sched_yield();
        }
    }
    /* MPI threads */
    else if(event == CR_HOOK_RSTRT_SIGNAL_CONTEXT ) {
        if( opal_thread_self_compare(checkpoint_thread_id) ) {
            opal_cr_refresh_environ(my_pid);
            blcr_crdebug_refreshed_env = true;
        } else {
            while(!blcr_crdebug_refreshed_env) {
                sched_yield();
            }
        }
    }

    /*
     * Some debugging output
     */
    /* Non-MPI threads */
    if( event == CR_HOOK_CONT_NO_CALLBACKS ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: MPIR_checkpoint_debugger_crs_hook: Waiting in Continue (Non-MPI). (%d)",
                            (int)my_thread_id->t_handle);
    }
    else if(event == CR_HOOK_RSTRT_NO_CALLBACKS ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: MPIR_checkpoint_debugger_crs_hook: Waiting in Restart (Non-MPI). (%d)",
                            (int)my_thread_id->t_handle);
    }
    /* MPI Threads */
    else if( event == CR_HOOK_CONT_SIGNAL_CONTEXT ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: MPIR_checkpoint_debugger_crs_hook: Waiting in Continue (MPI).");
    }
    else if(event == CR_HOOK_RSTRT_SIGNAL_CONTEXT ) {
        opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                            "crs:blcr: MPIR_checkpoint_debugger_crs_hook: Waiting in Restart (MPI).");
    }

    /*
     * Enter the breakpoint function.
     * If no debugger intends on attaching, then this function is expected to
     * return immediately.
     *
     * If this is an MPI thread then odds are that this is the checkpointing
     * thread, in which case this function will return immediately allowing
     * it to prepare the MPI library before signaling to the debugger that
     * it is safe to attach, if necessary.
     */
    MPIR_checkpoint_debugger_waitpoint();

    opal_output_verbose(10, mca_crs_blcr_component.super.output_handle,
                        "crs:blcr: MPIR_checkpoint_debugger_crs_hook: Finished...");
 }
#endif
