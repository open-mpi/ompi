/*
 * $HEADER$
 */

#ifndef LAM_ERRHANDLER_H
#define LAM_ERRHANDLER_H

#include "lam_config.h"

#include "mpi.h"
#include "lfc/lam_object.h"

typedef void (fortran_handler_fn_t)(int *, int *, ...);


struct lam_errhandler_t {
  lam_object_t super;

  char eh_name[MPI_MAX_OBJECT_NAME];

  bool eh_fortran_function;
  enum {
    LAM_ERRHANDLER_COMM,
    LAM_ERRHANDLER_WIN,
    LAM_ERRHANDLER_FILE
  } eh_mpi_object_type;

  union {
    MPI_Comm_errhandler_fn *c_comm_fn;
    MPI_File_errhandler_fn *c_file_fn;
    MPI_Win_errhandler_fn *c_win_fn;

    fortran_handler_fn_t *fort_fn;
  } eh_func;
};
typedef struct lam_errhandler_t lam_errhandler_t;


/**
 * \internal
 *
 * This function should not be invoked directly; it should only be
 * invoked by LAM_ERRHDL_INVOKE(), LAM_ERRHDL_CHECK(), or
 * LAM_ERRHDL_RETURN().
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
 * This is the macro to invoke to directly invoke an MPI error
 * handler.
 *
 * @param errhandler The MPI_Errhandler to invoke
 * @param mpi_object The MPI object to invoke the errhandler on (a
 *    comm, win, or win)
 * @param err_code The error code
 * @param message Any additional message; typically the name of the
 *    MPI function that is invoking the error.
 *
 * This macro is used when you want to directly invoke the error
 * handler.  It is exactly equivalent to calling
 * lam_errhandler_invoke() directly, but is provided to have a
 * parallel invocation to LAM_ERRHDL_CHECK() and LAM_ERRHDL_RETURN().
 */
#define LAM_ERRHDL_INVOKE(errhandler, mpi_object, err_code, message) \
  lam_errhandler_invoke((errhandler), (mpi_object), (err_code), (message);

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
#define LAM_ERRHDL_CHECK(rc, errhandler, mpi_object, err_code, message) \
  if (rc != LAM_SUCCESS) { \
    lam_errhandler_invoke((errhandler), (mpi_object), (err_code), (message)); \
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
#define LAM_ERRHDL_RETURN(rc, errhandler, mpi_object, err_code, message) \
  if (rc != LAM_SUCCESS) { \
    lam_errhandler_invoke((errhandler), (mpi_object), (err_code), (message)); \
  } else { \
    return MPI_SUCCESS; \
  }


#endif /* LAM_ERRHANDLER_H */
