/*
 * $HEADER$
 */

#include "lam_config.h"

#include "communicator/communicator.h"
#include "win/win.h"
#include "file/file.h"
#include "errhandler/errhandler.h"
#include "errhandler/errhandler_predefined.h"
#include "lfc/lam_pointer_array.h"


/*
 * Table for Fortran <-> C errhandler handle conversion
 */
lam_pointer_array_t *lam_errhandler_f_to_c_table;


/*
 * Class information
 */
static void lam_errhandler_construct(lam_errhandler_t *eh);
static void lam_errhandler_destruct(lam_errhandler_t *eh);


/*
 * Class instance
 */
OBJ_CLASS_INSTANCE(lam_errhandler_t, lam_object_t, lam_errhandler_construct,
                   lam_errhandler_destruct);


/*
 * MPI_ERRHANDLER_NULL
 */
lam_errhandler_t lam_mpi_errhandler_null = {
    { NULL, 0 },

    "MPI_ERRHANDLER_NULL",
    true,
    false,
    LAM_ERRHANDLER_TYPE_COMM,
    { NULL }
};


/*
 * MPI_ERRORS_ARE_FATAL
 */
lam_errhandler_t lam_mpi_errors_are_fatal = {
    { NULL, 0 },

    "MPI_ERRORS_ARE_FATAL",
    true,
    false,
    LAM_ERRHANDLER_TYPE_COMM,
    { lam_mpi_errors_are_fatal_handler },
    -1
};


/*
 * MPI_ERRORS_RETURN
 */
lam_errhandler_t lam_mpi_errors_return = {
    { NULL, 0 },

    "MPI_ERRORS_ARE_RETURN",
    true,
    false,
    LAM_ERRHANDLER_TYPE_COMM,
    { lam_mpi_errors_return_handler },
    -1
};


/*
 * Initialize LAM errhandler infrastructure
 */
int lam_errhandler_init(void)
{
  int ret_val;
  
  /* initialize lam_errhandler_f_to_c_table */

  lam_errhandler_f_to_c_table = OBJ_NEW(lam_pointer_array_t);
  if (NULL == lam_errhandler_f_to_c_table){
    return LAM_ERROR;
  }

  /* Initialize the predefined error handlers */
  OBJ_CONSTRUCT( &lam_mpi_errhandler_null, lam_errhandler_t );
  if( lam_mpi_errhandler_null.eh_f_to_c_index != LAM_ERRHANDLER_NULL_FORTRAN )
      return LAM_ERROR;

  OBJ_CONSTRUCT( &lam_mpi_errors_are_fatal, lam_errhandler_t );
  if( lam_mpi_errors_are_fatal.eh_f_to_c_index != LAM_ERRORS_ARE_FATAL_FORTRAN )
      return LAM_ERROR;
  
  OBJ_CONSTRUCT( &lam_mpi_errors_return, lam_errhandler_t );
  if( lam_mpi_errors_return.eh_f_to_c_index != LAM_ERRORS_RETURN_FORTRAN )
      return LAM_ERROR;

  /* All done */

  return LAM_SUCCESS;
}


/*
 * Clean up the errorhandler resources
 */
int lam_errhandler_finalize(void)
{
  /* Remove errhandler F2C table */
  
  OBJ_RELEASE(lam_errhandler_f_to_c_table);
  
  /* All done */

  return LAM_SUCCESS;
}


lam_errhandler_t *lam_errhandler_create(lam_errhandler_type_t object_type,
                                        lam_errhandler_fortran_handler_fn_t *func)
{
  lam_errhandler_t *new_errhandler;

  /* Create a new object and ensure that it's valid */

  new_errhandler = OBJ_NEW(lam_errhandler_t);
  if (NULL == new_errhandler) {
    if (LAM_ERROR == new_errhandler->eh_f_to_c_index) {
      OBJ_RELEASE(new_errhandler);
      new_errhandler = NULL;
    } else {

      /* The new object is valid -- initialize it.  If this is being
         created from fortran, the fortran MPI API wrapper function
         will override the eh_fortran_field directly.  We cast the
         function pointer type to the fortran type arbitrarily -- it
         only has to be a function pointer in order to store properly,
         it doesn't matter what type it is (we'll cast it to the Right
         type when we *use* it). */

      new_errhandler->eh_mpi_object_type = object_type;
      new_errhandler->eh_is_intrinsic = false;
      new_errhandler->eh_fortran_function = false;
      new_errhandler->eh_func.fort_fn = func;
    }
  }


  /* All done */
  return LAM_SUCCESS;
}


/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

/**
 * Errhandler constructor
 */
static void lam_errhandler_construct(lam_errhandler_t *new_errhandler)
{
  int ret_val;

  /* assign entry in fortran <-> c translation array */

  ret_val = lam_pointer_array_add(lam_errhandler_f_to_c_table, 
                                  new_errhandler);
  new_errhandler->eh_f_to_c_index = ret_val;
}


/**
 * Errhandler destructor
 */
static void lam_errhandler_destruct(lam_errhandler_t *errhandler)
{
  /* reset the lam_errhandler_f_to_c_table entry - make sure that the
     entry is in the table */

  if (NULL!= lam_pointer_array_get_item(lam_errhandler_f_to_c_table,
                                        errhandler->eh_f_to_c_index)) {
    lam_pointer_array_set_item(lam_errhandler_f_to_c_table,
                               errhandler->eh_f_to_c_index, NULL);
  }
}


