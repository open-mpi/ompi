/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2018 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2008-2009 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2018-2021 Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file **/

#ifndef OMPI_ERRHANDLER_H
#define OMPI_ERRHANDLER_H

#include "ompi_config.h"

#include "mpi.h"

#include "opal/prefetch.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/pmix/pmix-internal.h"

#include "ompi/runtime/mpiruntime.h"
#include "ompi/runtime/params.h"
#include "ompi/errhandler/errhandler_predefined.h"
#include "ompi/errhandler/errcode-internal.h"

BEGIN_C_DECLS

/*
 * These must correspond to the fortran handle indices
 */
enum {
  OMPI_ERRHANDLER_NULL_FORTRAN = 0,
  OMPI_ERRORS_ARE_FATAL_FORTRAN,
  OMPI_ERRORS_RETURN_FORTRAN,
  OMPI_ERRORS_ABORT_FORTRAN,
};


/**
 * Typedef for all fortran errhandler functions
 */
typedef void (ompi_errhandler_fortran_handler_fn_t)(MPI_Fint *,
                                                    MPI_Fint *, ...);

/**
 * Typedef for generic errhandler function
 */
typedef void (ompi_errhandler_generic_handler_fn_t)(void *, int *, ...);

/**
 * Enum to denote what language the error handler was created from
 */
enum ompi_errhandler_lang_t {
    OMPI_ERRHANDLER_LANG_C,
    OMPI_ERRHANDLER_LANG_FORTRAN
};
typedef enum ompi_errhandler_lang_t ompi_errhandler_lang_t;


/**
 * Enum used to describe what kind MPI object an error handler is used for
 */
enum ompi_errhandler_type_t {
    OMPI_ERRHANDLER_TYPE_PREDEFINED,
    OMPI_ERRHANDLER_TYPE_COMM,
    OMPI_ERRHANDLER_TYPE_WIN,
    OMPI_ERRHANDLER_TYPE_FILE,
    OMPI_ERRHANDLER_TYPE_INSTANCE,
};
typedef enum ompi_errhandler_type_t ompi_errhandler_type_t;


/**
 * Back-end type for MPI_Errorhandler.
 */
struct ompi_errhandler_t {
    opal_object_t super;

    char eh_name[MPI_MAX_OBJECT_NAME];
    /* Type of MPI object that this handler is for */

    ompi_errhandler_type_t eh_mpi_object_type;

    /* What language was the error handler created in */
    ompi_errhandler_lang_t eh_lang;

    /* Function pointers.  Note that we *have* to have all 4 types
       (vs., for example, a union) because the predefined errhandlers
       can be invoked on any MPI object type, so we need callbacks for
       all of three. */
    MPI_Comm_errhandler_function *eh_comm_fn;
    ompi_file_errhandler_function *eh_file_fn;
    MPI_Win_errhandler_function *eh_win_fn;
    MPI_Session_errhandler_function *eh_instance_fn;
    ompi_errhandler_fortran_handler_fn_t *eh_fort_fn;

    /* index in Fortran <-> C translation array */
    int eh_f_to_c_index;
};
typedef struct ompi_errhandler_t ompi_errhandler_t;

/**
 * Padded struct to maintain back compatibility.
 * See ompi/communicator/communicator.h comments with struct ompi_communicator_t
 * for full explanation why we chose the following padding construct for predefines.
 */
#define PREDEFINED_ERRHANDLER_PAD 1024

struct ompi_predefined_errhandler_t {
    struct ompi_errhandler_t eh;
    char padding[PREDEFINED_ERRHANDLER_PAD - sizeof(ompi_errhandler_t)];
};

typedef struct ompi_predefined_errhandler_t ompi_predefined_errhandler_t;


/**
 * Global variable for MPI_ERRHANDLER_NULL (_addr flavor is for F03 bindings)
 */
OMPI_DECLSPEC extern ompi_predefined_errhandler_t ompi_mpi_errhandler_null;
OMPI_DECLSPEC extern ompi_predefined_errhandler_t *ompi_mpi_errhandler_null_addr;

/**
 * Global variable for MPI_ERRORS_ARE_FATAL (_addr flavor is for F03 bindings)
 */
OMPI_DECLSPEC extern ompi_predefined_errhandler_t ompi_mpi_errors_are_fatal;
OMPI_DECLSPEC extern ompi_predefined_errhandler_t *ompi_mpi_errors_are_fatal_addr;

/**
 * Global variable for MPI_ERRORS_RETURN (_addr flavor is for F03 bindings)
 */
OMPI_DECLSPEC extern ompi_predefined_errhandler_t ompi_mpi_errors_return;
OMPI_DECLSPEC extern ompi_predefined_errhandler_t *ompi_mpi_errors_return_addr;

/*
 * Global variable for MPI_ERRORS_ABORT (_addr flavor is for F03 bindings)
 */
OMPI_DECLSPEC extern ompi_predefined_errhandler_t ompi_mpi_errors_abort;
OMPI_DECLSPEC extern ompi_predefined_errhandler_t *ompi_mpi_errors_abort_addr;

/**
 * Table for Fortran <-> C errhandler handle conversion
 */
OMPI_DECLSPEC extern opal_pointer_array_t ompi_errhandler_f_to_c_table;

/**
 * This function selects the initial error handler.
 * It may be called during MPI_INIT, or during the first MPI call
 * that raises an error. This function does not allocate memory,
 * and will only populate the ompi_initial_error_handler_eh and
 * ompi_initial_error_handler pointers with predefined error handler
 * and error handler functions aliases.
 */
OMPI_DECLSPEC int ompi_initial_errhandler_init(void);
/**
 * The initial error handler pointer. Will be set to alias one of the
 * predefined error handlers through launch keys during the first MPI call,
 * and will then be attached to predefined communicators.
 */
OMPI_DECLSPEC extern ompi_errhandler_t* ompi_initial_error_handler_eh;
/**
 * The initial error handler function pointer. Will be called when an error
 * is raised before MPI_INIT or after MPI_FINALIZE.
 */
OMPI_DECLSPEC extern void (*ompi_initial_error_handler)(struct ompi_communicator_t **comm, int *error_code, ...);

/**
 * Forward declaration so that we don't have to include
 * request/request.h here.
 */
struct ompi_request_t;


/* declared here because we can't include instance.h from this header
 * because it would create a circular dependency */
extern opal_atomic_int32_t ompi_instance_count;

/**
 * This is the macro to check the state of MPI and determine whether
 * it was properly initialized and not yet finalized.
 *
 * This macro directly invokes the ompi_mpi_errors_are_fatal_handler()
 * when an error occurs because MPI_COMM_WORLD does not exist (because
 * we're before MPI_Init() or after MPI_Finalize()).
 *
 * NOTE: The ompi_mpi_state variable is a volatile that is set
 * atomically in ompi_mpi_init() and ompi_mpi_finalize().  The
 * appropriate memory barriers are done in those 2 functions such that
 * we do not need to do a read memory barrier here (in
 * potentially-performance-critical code paths) before reading the
 * variable.
 */
#define OMPI_ERR_INIT_FINALIZE(name)                                       \
    {                                                                      \
        if (OPAL_UNLIKELY(0 == ompi_instance_count)) {                     \
            ompi_errhandler_invoke(NULL, NULL, -1,                         \
                                   ompi_errcode_get_mpi_code(MPI_ERR_ARG), \
                                   name);                                  \
        }                                                                  \
    }

/**
 * This is the macro to invoke to directly invoke an MPI error
 * handler.
 *
 * @param mpi_object The MPI object to invoke the errhandler on (a
 *    comm, win, or win)
 * @param err_code The error code
 * @param message Any additional message; typically the name of the
 *    MPI function that is invoking the error.
 *
 * This macro is used when you want to directly invoke the error
 * handler.  It is exactly equivalent to calling
 * ompi_errhandler_invoke() directly, but is provided to have a
 * parallel invocation to OMPI_ERRHANDLER_CHECK() and OMPI_ERRHANDLER_RETURN().
 */
#define OMPI_ERRHANDLER_INVOKE(mpi_object, err_code, message) \
  ompi_errhandler_invoke((mpi_object)->error_handler, \
                         (mpi_object), \
                         (int)(mpi_object)->errhandler_type, \
                         ompi_errcode_get_mpi_code(err_code), \
                         (message));

/**
 * This is the macro to route errors to the 'default' communicator
 * for non-handle attached errors (e.g., a datatype operation error).
 */
#define OMPI_ERRHANDLER_NOHANDLE_INVOKE(err_code, message) \
    ompi_errhandler_invoke(NULL, NULL, -1, \
                           ompi_errcode_get_mpi_code(err_code), \
                           (message));

/**
 * Conditionally invoke an MPI error handler.
 *
 * @param rc The return code to check
 * @param mpi_object The MPI object to invoke the errhandler on (a
 *    comm, win, or win)
 * @param err_code The error code
 * @param message Any additional message; typically the name of the
 *    MPI function that is invoking the error.
 *
 * This macro will invoke the error handler if the return code is not
 * OMPI_SUCCESS.
 */
#define OMPI_ERRHANDLER_CHECK(rc, mpi_object, err_code, message) \
  if( OPAL_UNLIKELY(rc != OMPI_SUCCESS) ) { \
    int __mpi_err_code = ompi_errcode_get_mpi_code(err_code);         \
    ompi_errhandler_invoke((mpi_object)->error_handler, \
                           (mpi_object), \
                           (int) (mpi_object)->errhandler_type, \
                           (__mpi_err_code), \
                           (message)); \
    return (__mpi_err_code); \
  }

/* Same as OMPI_ERRHANDLER_CHECK for non-handle attached errors */
#define OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, err_code, message) \
  if( OPAL_UNLIKELY(rc != OMPI_SUCCESS) ) { \
    int __mpi_err_code = ompi_errcode_get_mpi_code(err_code);         \
    ompi_errhandler_invoke(NULL, \
                           NULL, \
                           -1, \
                           (__mpi_err_code), \
                           (message)); \
    return (__mpi_err_code); \
  }

/**
 * Conditionally invoke an MPI error handler; if there is no error,
 * return MPI_SUCCESS.
 *
 * @param rc The return code to check
 * @param mpi_object The MPI object to invoke the errhandler on (a
 *    comm, win, or win)
 * @param err_code The error code
 * @param message Any additional message; typically the name of the
 *    MPI function that is invoking the error.
 *
 * This macro will invoke the error handler if the return code is not
 * OMPI_SUCCESS.  If the return code is OMPI_SUCCESS, then return
 * MPI_SUCCESS.
 */
#define OMPI_ERRHANDLER_RETURN(rc, mpi_object, err_code, message) \
  if ( OPAL_UNLIKELY(OMPI_SUCCESS != rc) ) { \
    int __mpi_err_code = ompi_errcode_get_mpi_code(err_code);         \
    ompi_errhandler_invoke((mpi_object)->error_handler, \
                           (mpi_object), \
                           (int)(mpi_object)->errhandler_type, \
                           (__mpi_err_code), \
                           (message)); \
    return (__mpi_err_code); \
  } else { \
    return MPI_SUCCESS; \
  }

/* Same as OMPI_ERRHANDLER_RETURN for non-handle attached errors */
#define OMPI_ERRHANDLER_NOHANDLE_RETURN(rc, err_code, message) {\
  OMPI_ERRHANDLER_NOHANDLE_CHECK(rc, err_code, message) \
  return MPI_SUCCESS; \
}

  /**
   * Initialize the error handler interface.
   *
   * @returns OMPI_SUCCESS Upon success
   * @returns OMPI_ERROR Otherwise
   *
   * Invoked from ompi_mpi_init(); sets up the error handler interface,
   * creates the predefined MPI errorhandlers, and creates the
   * corresopnding F2C translation table.
   */
  int ompi_errhandler_init(void);

  /**
   * \internal
   *
   * This function should not be invoked directly; it should only be
   * invoked by OMPI_ERRHANDLER_INVOKE(), OMPI_ERRHANDLER_CHECK(), or
   * OMPI_ERRHANDLER_RETURN().
   *
   * @param errhandler The MPI_Errhandler to invoke
   * @param mpi_object The MPI object to invoke the errhandler on (a
   *    comm, win, or win)
   * @param type       The type of the MPI object. Necessary, since
   *                   you can not assign a single type to the predefined
   *                   error handlers. This information is therefore
   *                   stored on the MPI object itself.
   * @param err_code The error code
   * @param message Any additional message; typically the name of the
   *    MPI function that is invoking the error.
   *
   * @returns err_code The same value as the parameter
   *
   * This function invokes the MPI error function on the error
   * handler.  If the errhandler was created from fortran, the error
   * handler will be invoked with fortran linkage.  Otherwise, it is
   * invoked with C linkage.
   *
   * If this function returns, it returns the err_code.  Note that it
   * may not return (e.g., for MPI_ERRORS_ARE_FATAL).
   */
  OMPI_DECLSPEC int ompi_errhandler_invoke(ompi_errhandler_t *errhandler, void *mpi_object,
                                           int type, int err_code, const char *message);


  /**
   * Invoke an MPI error on the first request found in the array
   * that has a non-MPI_SUCCESS value for MPI_ERROR in its status.  It
   * is safe to invoke this function if none of the requests have an
   * outstanding error; MPI_SUCCESS will be returned.
   */
  int ompi_errhandler_request_invoke(int count,
                                     struct ompi_request_t **requests,
                                     const char *message);

  /**
   * Create a ompi_errhandler_t
   *
   * @param[in]  object_type    Enum of the type of MPI object
   * @param[in]  func           Function pointer of the error handler
   * @param[in]  language       Calling language
   *
   * @returns errhandler Pointer to the ompi_errorhandler_t that will be
   *   created and returned
   *
   * This function is called as the back-end of all the
   * MPI_*_CREATE_ERRHANDLER functions.  It creates a new
   * ompi_errhandler_t object, initializes it to the correct object
   * type, and sets the callback function on it.
   *
   * The type of the function pointer is (arbitrarily) the fortran
   * function handler type.  Since this function has to accept 4
   * different function pointer types (lest we have 4 different
   * functions to create errhandlers), the fortran one was picked
   * arbitrarily.  Note that (void*) is not sufficient because at
   * least theoretically, a sizeof(void*) may not necessarily be the
   * same as sizeof(void(*)).
   */
  OMPI_DECLSPEC ompi_errhandler_t *ompi_errhandler_create(ompi_errhandler_type_t object_type,
                                            ompi_errhandler_generic_handler_fn_t *func,
                                            ompi_errhandler_lang_t language);

  OMPI_DECLSPEC void ompi_errhandler_free (ompi_errhandler_t *errhandler);

/**
 * Callback function to alert the MPI layer of an error or notification
 * from the internal RTE and/or the resource manager.
 *
 * This function is used to alert the MPI layer to a specific fault detected by the
 * runtime layer or host RM. This could be a process failure, a lost connection, or the inability
 * to send an OOB message. The MPI layer has the option to perform whatever actions it
 * needs to stabilize itself and continue running, abort, etc.
 */
typedef struct {
    volatile bool active;
    int status;
} ompi_errhandler_errtrk_t;

OMPI_DECLSPEC void ompi_errhandler_callback(size_t refid, pmix_status_t status,
                                            const pmix_proc_t *source,
                                            pmix_info_t *info, size_t ninfo,
                                            pmix_info_t *results, size_t nresults,
                                            pmix_event_notification_cbfunc_fn_t cbfunc,
                                            void *cbdata);

OMPI_DECLSPEC void ompi_errhandler_registration_callback(int status,
                                                         size_t errhandler_ref,
                                                         void *cbdata);
/**
 * Check to see if an errhandler is intrinsic.
 *
 * @param errhandler The errhandler to check
 *
 * @returns true If the errhandler is intrinsic
 * @returns false If the errhandler is not intrinsic
 *
 * Self-explanitory.  This is needed in a few top-level MPI functions;
 * this function is provided to hide the internal structure field
 * names.
 */
static inline bool ompi_errhandler_is_intrinsic(ompi_errhandler_t *errhandler)
{
    if ( OMPI_ERRHANDLER_TYPE_PREDEFINED == errhandler->eh_mpi_object_type )
	return true;

    return false;
}

#if OPAL_ENABLE_FT_MPI
struct ompi_proc_t;

OMPI_DECLSPEC int ompi_errhandler_proc_failed_internal(struct ompi_proc_t *ompi_proc, int status, bool forward);
static inline int ompi_errhandler_proc_failed(struct ompi_proc_t* ompi_proc) {
    return ompi_errhandler_proc_failed_internal(ompi_proc, PMIX_ERR_PROC_TERM_WO_SYNC, true);
}
#endif /* OPAL_ENABLE_FT_MPI */

END_C_DECLS

#endif /* OMPI_ERRHANDLER_H */
