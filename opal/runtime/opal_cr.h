/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
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
 * @file
 *
 * Checkpoint functionality for Open MPI
 */

#include "opal/mca/crs/crs.h"
#include "opal/threads/mutex.h"
#include "opal/threads/threads.h"
#include "opal/threads/condition.h"
#include "opal/event/event.h"
#include "opal/runtime/opal_progress.h"
#include "opal/util/output.h"
#include "opal/prefetch.h"

#ifndef OPAL_CR_H
#define OPAL_CR_H


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * Some defines shared with opal-[checkpoint|restart] commands
 */
#define OPAL_CR_DONE       ((char) 0)
#define OPAL_CR_ACK        ((char) 1)
#define OPAL_CR_CHECKPOINT ((char) 2)
#define OPAL_CR_NAMED_PROG_R  ("opal_cr_prog_read")
#define OPAL_CR_NAMED_PROG_W  ("opal_cr_prog_write")
#define OPAL_CR_BASE_ENV_NAME ("opal_cr_restart-env")

/*
 * Possible responses to a checkpoint request from opal-checkpoint
 */
enum opal_cr_ckpt_cmd_state_t {
    OPAL_CHECKPOINT_CMD_START,       /* Checkpoint is starting on this request */
    OPAL_CHECKPOINT_CMD_IN_PROGRESS, /* Checkpoint is currently running */
    OPAL_CHECKPOINT_CMD_NULL,        /* Checkpoint cannot be started because it is not supported */
    OPAL_CHECKPOINT_CMD_ERROR,       /* An error occurred such that the checkpoint cannot be completed */
    /* State of the checkpoint operation */
    OPAL_CR_STATUS_NONE,       /* No checkpoint in progress */
    OPAL_CR_STATUS_REQUESTED,  /* Checkpoint has been requested */
    OPAL_CR_STATUS_RUNNING,    /* Checkpoint is currently running */
    OPAL_CR_STATUS_TERM        /* Checkpoint is running and will terminate process upon completion */
};
typedef enum opal_cr_ckpt_cmd_state_t opal_cr_ckpt_cmd_state_t;

    /* Directory containing the named pipes for communication
     * with the opal-checkpoint tool  */
    OPAL_DECLSPEC extern char * opal_cr_pipe_dir;
    /* Signal that opal-checkpoint uses to contact the 
     * application process */
    OPAL_DECLSPEC extern int    opal_cr_entry_point_signal;
    /* If Checkpointing is enabled in this application */
    OPAL_DECLSPEC extern bool   opal_cr_is_enabled;
    /* If the application running is a tool
     * (e.g., opal-checkpoint, orted, ...) */
    OPAL_DECLSPEC extern bool   opal_cr_is_tool;
    /* An output handle to be used by the cr runtime 
     * functionality as an argument to opal_output() */
    OPAL_DECLSPEC extern int    opal_cr_output;
    /* If a checkpoint has been requested */
    OPAL_DECLSPEC extern int opal_cr_checkpoint_request;
    /* The current state of a checkpoint operation */
    OPAL_DECLSPEC extern int opal_cr_checkpointing;

    /*
     * If this is an application that doesn't want to have
     * a notification callback installed, set this to false.
     * To see the effect, this must be called before opal_cr_init().
     * Default: Enabled
     */
    OPAL_DECLSPEC int opal_cr_set_enabled(bool);

    /**
     * Initialize the notification and coordination
     *  elements.
     */
    OPAL_DECLSPEC int opal_cr_init(void);

    /**
     * Finalize the notification and coordination
     *  elements.
     */
    OPAL_DECLSPEC int opal_cr_finalize(void);
    
    /*************************************************
     * Check to see if a checkpoint has been requested
     *
     * When the checkpoint thread is disabled:
     *   This will be checked whenever the MPI Library
     *   is entered by the application. It will stop
     *   the application for the duration of the entire
     *   checkpoint.
     * When the checkpoint thread is enabled:
     *   The request is handled in the thread parallel
     *   with the execution of the program regardless
     *   of where the program is in exection.
     *   The problem with this method is that it
     *   requires the support of progress threads
     *   which is currently not working properly :/
     *
     *************************************************/
    OPAL_DECLSPEC void opal_cr_test_if_checkpoint_ready(void);

    /* If the checkpoint operation should be stalled to
     * wait for another sevice to complete before 
     * continuing with the checkpoint */
    OPAL_DECLSPEC extern bool opal_cr_stall_check;
    OPAL_DECLSPEC extern bool opal_cr_currently_stalled;

    /* If not using FT or using the thead, disable the process checks */
#if OPAL_ENABLE_FT == 1 && OPAL_ENABLE_FT_THREAD == 0
#define OPAL_CR_TEST_CHECKPOINT_READY()      \
  {                                          \
    if(OPAL_UNLIKELY(opal_cr_is_enabled) ) { \
      opal_cr_test_if_checkpoint_ready();    \
    }                                        \
  }
#else
#define OPAL_CR_TEST_CHECKPOINT_READY() ;
#endif

#if OPAL_ENABLE_FT == 1 && OPAL_ENABLE_FT_THREAD == 0
#define OPAL_CR_TEST_CHECKPOINT_READY_STALL()        \
  {                                                  \
    if(OPAL_UNLIKELY(opal_cr_is_enabled && !opal_cr_stall_check)) { \
      opal_cr_test_if_checkpoint_ready();            \
    }                                                \
  }
#else
#define OPAL_CR_TEST_CHECKPOINT_READY_STALL() ;
#endif

    /*******************************
     * Notification Routines
     *******************************/
    /*******************************
     * Notification Routines
     *******************************/
    /*
     * Init OPAL entry point functionality
     */
    OPAL_DECLSPEC int opal_cr_entry_point_init(void);

    /*
     * Finalize OPAL entry point functionality
     */
    OPAL_DECLSPEC int opal_cr_entry_point_finalize(void);

    /**
     * A function to respond to the async checkpoint request
     * this is useful when figuring out who should respond
     * when stalling.
     */
    typedef int (*opal_cr_notify_callback_fn_t) (opal_cr_ckpt_cmd_state_t);

    OPAL_DECLSPEC int opal_cr_reg_notify_callback
    (opal_cr_notify_callback_fn_t new_func,
     opal_cr_notify_callback_fn_t *prev_func);

    /**
     * Function to go through the INC
     * - Call Registered INC_Coord(CHECKPOINT)
     * - Call the CRS.checkpoint()
     * - Call Registered INC_Coord(state)
     */
    OPAL_DECLSPEC int opal_cr_inc_core(pid_t pid, 
                                       opal_crs_base_snapshot_t *snapshot, 
                                       bool term, int *state);
    
    /*******************************
     * Coordination Routines
     *******************************/
    /** 
     * Coordination callback routine signature
     */
    typedef int (*opal_cr_coord_callback_fn_t) (int);

    /**
     * Register a checkpoint coodination routine
     * for a higher level.
     */
     OPAL_DECLSPEC int opal_cr_reg_coord_callback
     (opal_cr_coord_callback_fn_t  new_func,
      opal_cr_coord_callback_fn_t *prev_func);

    /**
     * OPAL Checkpoint Coordination Routine
     */
    OPAL_DECLSPEC int opal_cr_coord(int state);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OPAL_CR_H */

