/*
 * $HEADER$
 */

#ifndef LAM_ERRHANDLER_H
#define LAM_ERRHANDLER_H

#include "lam_config.h"

#include "mpi.h"
#include "lfc/lam_object.h"

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

  /* Is this a fortran function? */

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
extern lam_errhandler_t lam_mpi_errhandler_null;

/**
 * Global variable for MPI_ERRORS_ARE_FATAL
 */
extern lam_errhandler_t lam_mpi_errors_are_fatal;

/**
 * Global variable for MPI_ERRORS_RETURN
 */
extern lam_errhandler_t lam_mpi_errors_return;



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
  lam_errhandler_invoke((mpi_object)->error_handler, (mpi_object), \
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
    lam_errhandler_invoke((mpi_object)->error_handler, (mpi_object), \
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
    lam_errhandler_invoke((mpi_object)->error_handler, (mpi_object), \
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
   * Invoked from lam_mpi_init(); sets up the error handler interface,
   * creates the predefined MPI errorhandlers, and creates the
   * corresopnding F2C translation table.
   */
  int lam_errhandler_init(void);

  /*
   * Finalize the error handler interface.
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
   * This function invokes the MPI exception function on the error
   * handler.  If the errhandler was created from fortran, the error
   * handler will be invoked with fortran linkage.  Otherwise, it is
   * invoked with C linkage.
   */
  int lam_errhandler_invoke(lam_errhandler_t *errhandler, void *mpi_object, 
                            int err_code, char *message);


  /**
   * Create a lam_errhandler_t
   *
   * @param mpi_object The object that the errhandler should be cached on
   * @param object_type Enum of the type of MPI object
   * @param func Function pointer of the error handler
   * @param errhandler Pointer to the lam_errorhandler_t that will be
   *   created and returned
   *
   * This function will.... JMS continue here
   */
  lam_errhandler_t *lam_errhandler_create(lam_errhandler_type_t object_type,
                                          lam_errhandler_fortran_handler_fn_t *func);
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* LAM_ERRHANDLER_H */
