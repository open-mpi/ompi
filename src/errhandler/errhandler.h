/*
 * $HEADER$
 */
/** @file **/

#ifndef LAM_ERRHANDLER_H
#define LAM_ERRHANDLER_H

#include "lam_config.h"

#include "mpi.h"
#include "lfc/lam_object.h"
#include "lfc/lam_pointer_array.h"

/* This must correspond to the fortran MPI_ERRHANDLER_NULL index */
#define LAM_ERRHANDLER_NULL_FORTRAN 0

/* This must correspond to the fortran MPI_ERRORS_ARE_FATAL index */
#define LAM_ERRORS_ARE_FATAL_FORTRAN 1

/* This must correspond to the fortran MPI_ERRORS_RETURN index */
#define LAM_ERRORS_RETURN_FORTRAN 2


/**
 * Typedef for all fortran errhandler functions
 */
typedef void (lam_errhandler_fortran_handler_fn_t)(int *, int *, ...);


/**
 * Enum used to describe what kind MPI object an error handler is used for
 */
enum lam_errhandler_type_t {
    LAM_ERRHANDLER_TYPE_COMM,
    LAM_ERRHANDLER_TYPE_WIN,
    LAM_ERRHANDLER_TYPE_FILE
};
typedef enum lam_errhandler_type_t lam_errhandler_type_t;


/**
 * Back-end type for MPI_Errorhandler.
 */
struct lam_errhandler_t {
  lam_object_t super;

  char eh_name[MPI_MAX_OBJECT_NAME];

  /* Type of MPI object that this handler is for */

  lam_errhandler_type_t eh_mpi_object_type;

  /* Flags about the error handler */

  bool eh_is_intrinsic;
  bool eh_fortran_function;

  /* Function pointers */

  union {
    MPI_Comm_errhandler_fn *c_comm_fn;
    MPI_File_errhandler_fn *c_file_fn;
    MPI_Win_errhandler_fn *c_win_fn;

    lam_errhandler_fortran_handler_fn_t *fort_fn;
  } eh_func;

  /* index in Fortran <-> C translation array */

  int eh_f_to_c_index;
};
typedef struct lam_errhandler_t lam_errhandler_t;


/**
 * Global variable for MPI_ERRHANDLER_NULL
 */
extern lam_errhandler_t *lam_mpi_errhandler_null;

/**
 * Global variable for MPI_ERRORS_ARE_FATAL
 */
extern lam_errhandler_t *lam_mpi_errors_are_fatal;

/**
 * Global variable for MPI_ERRORS_RETURN
 */
extern lam_errhandler_t *lam_mpi_errors_return;

/**
 * Table for Fortran <-> C errhandler handle conversion
 */
extern lam_pointer_array_t *lam_errhandler_f_to_c_table;


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
 * lam_errhandler_invoke() directly, but is provided to have a
 * parallel invocation to LAM_ERRHANDLER_CHECK() and LAM_ERRHANDLER_RETURN().
 */
#define LAM_ERRHANDLER_INVOKE(mpi_object, err_code, message) \
  lam_errhandler_invoke((mpi_object) != NULL ? (mpi_object)->error_handler : NULL, (mpi_object), \
                        (err_code), (message));

/**
 * Conditionally invoke an MPI error handler.
 *
 * @param rc The return code to check
 * @param errhandler The MPI_Errhandler to invoke
 * @param mpi_object The MPI object to invoke the errhandler on (a
 *    comm, win, or win)
 * @param err_code The error code
 * @param message Any additional message; typically the name of the
 *    MPI function that is invoking the error.
 *
 * This macro will invoke the error handler if the return code is not
 * LAM_SUCCESS.
 */
#define LAM_ERRHANDLER_CHECK(rc, mpi_object, err_code, message) \
  if (rc != LAM_SUCCESS) { \
    lam_errhandler_invoke((mpi_object) != NULL ? (mpi_object)->error_handler : NULL, (mpi_object), \
                          (err_code), (message)); \
    return (err_code); \
  }

/**
 * Conditionally invoke an MPI error handler; if there is no error,
 * return MPI_SUCCESS.
 *
 * @param rc The return code to check
 * @param errhandler The MPI_Errhandler to invoke
 * @param mpi_object The MPI object to invoke the errhandler on (a
 *    comm, win, or win)
 * @param err_code The error code
 * @param message Any additional message; typically the name of the
 *    MPI function that is invoking the error.
 *
 * This macro will invoke the error handler if the return code is not
 * LAM_SUCCESS.  If the return code is LAM_SUCCESS, then return
 * MPI_SUCCESS.
 */
#define LAM_ERRHANDLER_RETURN(rc, mpi_object, err_code, message) \
  if (rc != LAM_SUCCESS) { \
    lam_errhandler_invoke((mpi_object != NULL) ? (mpi_object)->error_handler : NULL, (mpi_object), \
                          (err_code), (message)); \
    return (err_code); \
  } else { \
    return MPI_SUCCESS; \
  }


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

  /**
   * Initialize the error handler interface.
   *
   * @returns LAM_SUCCESS Upon success
   * @returns LAM_ERROR Otherwise
   *
   * Invoked from lam_mpi_init(); sets up the error handler interface,
   * creates the predefined MPI errorhandlers, and creates the
   * corresopnding F2C translation table.
   */
  int lam_errhandler_init(void);

  /**
   * Finalize the error handler interface.
   *
   * @returns LAM_SUCCESS Always
   *
   * Invokes from lam_mpi_finalize(); tears down the error handler
   * interface, and destroys the F2C translation table.
   */
  int lam_errhandler_finalize(void);

  /**
   * \internal
   *
   * This function should not be invoked directly; it should only be
   * invoked by LAM_ERRHANDLER_INVOKE(), LAM_ERRHANDLER_CHECK(), or
   * LAM_ERRHANDLER_RETURN().
   *
   * @param errhandler The MPI_Errhandler to invoke
   * @param mpi_object The MPI object to invoke the errhandler on (a
   *    comm, win, or win)
   * @param err_code The error code
   * @param message Any additional message; typically the name of the
   *    MPI function that is invoking the error.
   *
   * @returns err_code The same value as the parameter
   *
   * This function invokes the MPI exception function on the error
   * handler.  If the errhandler was created from fortran, the error
   * handler will be invoked with fortran linkage.  Otherwise, it is
   * invoked with C linkage.
   *
   * If this function returns, it returns the err_code.  Note that it
   * may not return (e.g., for MPI_ERRORS_ARE_FATAL).
   */
  int lam_errhandler_invoke(lam_errhandler_t *errhandler, void *mpi_object, 
                            int err_code, char *message);


  /**
   * Create a lam_errhandler_t
   *
   * @param object_type Enum of the type of MPI object
   * @param func Function pointer of the error handler
   *
   * @returns errhandler Pointer to the lam_errorhandler_t that will be
   *   created and returned
   *
   * This function is called as the back-end of all the
   * MPI_*_CREATE_ERRHANDLER functions.  It creates a new
   * lam_errhandler_t object, initializes it to the correct object
   * type, and sets the callback function on it.  
   *
   * The type of the function pointer is (arbitrarily) the fortran
   * function handler type.  Since this function has to accept 4
   * different function pointer types (lest we have 4 different
   * functions to create errhandlers), the fortran one was picked
   * arbitrarily.  Note that (void*) is not sufficient because at
   * least theoretically, a sizeof(void*) may not necessarily be the
   * same as sizeof(void(*)).
   *
   * NOTE: It *always* sets the "fortran" flag to false.  Fortran
   * wrappers for MPI_*_CREATE_ERRHANDLER are expected to reset this
   * flag to false manually.
   */
  lam_errhandler_t *lam_errhandler_create(lam_errhandler_type_t object_type,
                                          lam_errhandler_fortran_handler_fn_t *func);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif


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
static inline bool lam_errhandler_is_intrinsic(lam_errhandler_t *errhandler)
{
  return errhandler->eh_is_intrinsic;
}

#endif /* LAM_ERRHANDLER_H */
