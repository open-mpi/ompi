/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC. All rights
 *                         reserved
 * Copyright (c) 2019      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file
 *
 * Remote File Management (FileM) Interface
 *
 */

#ifndef MCA_FILEM_H
#define MCA_FILEM_H

#include "prte_config.h"
#include "constants.h"
#include "types.h"

#include "src/pmix/pmix-internal.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"

#include "src/class/pmix_object.h"

#include "src/runtime/prte_globals.h"
BEGIN_C_DECLS

/**
 * A set of flags that determine the type of the file
 * in question
 */
#define PRTE_FILEM_TYPE_FILE    0
#define PRTE_FILEM_TYPE_DIR     1
#define PRTE_FILEM_TYPE_UNKNOWN 2
#define PRTE_FILEM_TYPE_TAR     3
#define PRTE_FILEM_TYPE_BZIP    4
#define PRTE_FILEM_TYPE_GZIP    5
#define PRTE_FILEM_TYPE_EXE     6

/**
 * Type of movement
 */
#define PRTE_FILEM_MOVE_TYPE_PUT     0
#define PRTE_FILEM_MOVE_TYPE_GET     1
#define PRTE_FILEM_MOVE_TYPE_RM      2
#define PRTE_FILEM_MOVE_TYPE_UNKNOWN 3

/**
 * Hints that describe the local or remote file target for
 * optimization purposes.
 */
#define PRTE_FILEM_HINT_NONE   0
#define PRTE_FILEM_HINT_SHARED 1

/**
 * Define a Process Set
 *
 * Source: A single source of the operation.
 * Sink: Desitination of the operation.
 */
struct prte_filem_base_process_set_1_0_0_t {
    /** This is an object, so must have a super */
    pmix_list_item_t super;

    /** Source Process */
    pmix_proc_t source;

    /** Sink Process */
    pmix_proc_t sink;
};
typedef struct prte_filem_base_process_set_1_0_0_t prte_filem_base_process_set_1_0_0_t;
typedef struct prte_filem_base_process_set_1_0_0_t prte_filem_base_process_set_t;

PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_filem_base_process_set_t);

/**
 * Define a File Pair
 *
 * Local: Local file reference
 * Remove: Remote file reference
 *
 * Note: If multiple process sinks are used it is assumed that the
 * file reference is the same for each of the sinks. If this is not
 * true then more than one filem request needs to be created.
 */
struct prte_filem_base_file_set_1_0_0_t {
    /** This is an object, so must have a super */
    pmix_list_item_t super;

    /* the app_index this pertains to, if applicable */
    prte_app_idx_t app_idx;

    /* Local file reference */
    char *local_target;

    /* Local file reference hints */
    int local_hint;

    /* Remove file reference */
    char *remote_target;

    /* Remote file reference hints */
    int remote_hint;

    /* Type of file to move */
    int target_flag;
};
typedef struct prte_filem_base_file_set_1_0_0_t prte_filem_base_file_set_1_0_0_t;
typedef struct prte_filem_base_file_set_1_0_0_t prte_filem_base_file_set_t;

PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_filem_base_file_set_t);

/**
 * Definition of a file movement request
 * This will allow:
 *  - The movement of one or more files
 *  - to/from one or more processes
 * in a single call of the API function. Allowing the implementation
 * to optimize the sending/receiving of data.
 * Used for the following:
 *
 */
struct prte_filem_base_request_1_0_0_t {
    /** This is an object, so must have a super */
    pmix_list_item_t super;

    /*
     * A list of process sets - use WILDCARD to
     * indicate all procs of a given vpid/jobid,
     * INVALID to indicate not-applicable. For
     * example, if you need to move a file at time
     * of job start to each node that has a proc
     * on it, then the process set would have a
     * source proc with vpid=INVALID and a sink proc
     * with vpid=WILDCARD, and a remote hint of "shared"
     * in the file sets so we don't copy them over
     * multiple times
     */
    pmix_list_t process_sets;

    /*
     * A list of file pairings
     */
    pmix_list_t file_sets;

    /*
     * Internal use:
     * Number of movements
     */
    int num_mv;

    /*
     * Internal use:
     * Boolean to indianate if transfer is complete
     */
    bool *is_done;

    /*
     * Internal use:
     * Boolean to indianate if transfer is active
     */
    bool *is_active;

    /*
     * Internal use:
     * Exit status of the copy command
     */
    int32_t *exit_status;

    /*
     * Internal use:
     * Movement type
     */
    int movement_type;
};
typedef struct prte_filem_base_request_1_0_0_t prte_filem_base_request_1_0_0_t;
typedef struct prte_filem_base_request_1_0_0_t prte_filem_base_request_t;

PRTE_EXPORT PMIX_CLASS_DECLARATION(prte_filem_base_request_t);

/**
 * Module initialization function.
 * Returns PRTE_SUCCESS
 */
typedef int (*prte_filem_base_module_init_fn_t)(void);

/**
 * Module finalization function.
 * Returns PRTE_SUCCESS
 */
typedef int (*prte_filem_base_module_finalize_fn_t)(void);

/**
 * Put a file or directory on the remote machine
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the files/directories to send,
 *        the remote files/directories to use, and the processes to see the change.
 *
 * @return PRTE_SUCCESS on successful file transer
 * @return PRTE_ERROR on failed file transfer
 */
typedef int (*prte_filem_base_put_fn_t)(prte_filem_base_request_t *request);

/**
 * Put a file or directory on the remote machine (Async)
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the files/directories to send,
 *        the remote files/directories to use, and the processes to see the change.
 *
 * @return PRTE_SUCCESS on successful file transer
 * @return PRTE_ERROR on failed file transfer
 */
typedef int (*prte_filem_base_put_nb_fn_t)(prte_filem_base_request_t *request);

/**
 * Get a file from the remote machine
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the files/directories to receive,
 *        the remote files/directories to use, and the processes to see the change.
 *
 * @return PRTE_SUCCESS on successful file transer
 * @return PRTE_ERROR on failed file transfer
 */
typedef int (*prte_filem_base_get_fn_t)(prte_filem_base_request_t *request);

/**
 * Get a file from the remote machine (Async)
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the files/directories to receive,
 *        the remote files/directories to use, and the processes to see the change.
 *
 * @return PRTE_SUCCESS on successful file transer
 * @return PRTE_ERROR on failed file transfer
 */
typedef int (*prte_filem_base_get_nb_fn_t)(prte_filem_base_request_t *request);

/**
 * Remove a file from the remote machine
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the remote files/directories to remove,
 *        the processes to see the change.
 *
 * @return PRTE_SUCCESS on success
 * @return PRTE_ERROR on fail
 */
typedef int (*prte_filem_base_rm_fn_t)(prte_filem_base_request_t *request);

/**
 * Remove a file from the remote machine (Async)
 *
 * Note: By using a relative path for the remote file/directory, the filem
 *       component will negotiate the correct absolute path for that file/directory
 *       for the remote machine.
 *
 * @param request FileM request describing the remote files/directories to remove,
 *        the processes to see the change.
 *
 * @return PRTE_SUCCESS on success
 * @return PRTE_ERROR on fail
 */
typedef int (*prte_filem_base_rm_nb_fn_t)(prte_filem_base_request_t *request);

/**
 * Wait for a single file movement request to finish
 *
 * @param request FileM request describing the remote files/directories.
 *
 * The request must have been passed through one of the non-blocking functions
 * before calling wait or wait_all otherwise PRTE_ERROR will be returned.
 *
 * @return PRTE_SUCCESS on success
 * @return PRTE_ERROR on fail
 */
typedef int (*prte_filem_base_wait_fn_t)(prte_filem_base_request_t *request);

/**
 * Wait for a multiple file movement requests to finish
 *
 * @param request_list pmix_list_t of FileM requests describing the remote files/directories.
 *
 * The request must have been passed through one of the non-blocking functions
 * before calling wait or wait_all otherwise PRTE_ERROR will be returned.
 *
 * @return PRTE_SUCCESS on success
 * @return PRTE_ERROR on fail
 */
typedef int (*prte_filem_base_wait_all_fn_t)(pmix_list_t *request_list);

typedef void (*prte_filem_completion_cbfunc_t)(int status, void *cbdata);

/* Pre-position files
 */
typedef int (*prte_filem_base_preposition_files_fn_t)(prte_job_t *jdata,
                                                      prte_filem_completion_cbfunc_t cbfunc,
                                                      void *cbdata);

/* link local files */
typedef int (*prte_filem_base_link_local_files_fn_t)(prte_job_t *jdata, prte_app_context_t *app);

/**
 * Structure for FILEM components.
 */
typedef pmix_mca_base_component_t prte_filem_base_component_t;

/**
 * Structure for FILEM  modules
 */
struct prte_filem_base_module_1_0_0_t {
    /** Initialization Function */
    prte_filem_base_module_init_fn_t filem_init;
    /** Finalization Function */
    prte_filem_base_module_finalize_fn_t filem_finalize;

    /** Put a file on the remote machine */
    prte_filem_base_put_fn_t put;
    prte_filem_base_put_nb_fn_t put_nb;
    /** Get a file from the remote machine */
    prte_filem_base_get_fn_t get;
    prte_filem_base_get_nb_fn_t get_nb;

    /** Remove a file on the remote machine */
    prte_filem_base_rm_fn_t rm;
    prte_filem_base_rm_nb_fn_t rm_nb;

    /** Test functions for the non-blocking versions */
    prte_filem_base_wait_fn_t wait;
    prte_filem_base_wait_all_fn_t wait_all;

    /* pre-position files to every node */
    prte_filem_base_preposition_files_fn_t preposition_files;
    /* create local links for all shared files */
    prte_filem_base_link_local_files_fn_t link_local_files;
};
typedef struct prte_filem_base_module_1_0_0_t prte_filem_base_module_1_0_0_t;
typedef struct prte_filem_base_module_1_0_0_t prte_filem_base_module_t;

PRTE_EXPORT extern prte_filem_base_module_t prte_filem;

/**
 * Macro for use in components that are of type FILEM
 */
#define PRTE_FILEM_BASE_VERSION_2_0_0 PRTE_MCA_BASE_VERSION_3_0_0("filem", 2, 0, 0)

END_C_DECLS

#endif /* PRTE_FILEM_H */
