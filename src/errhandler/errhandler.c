/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "communicator/communicator.h"
#include "win/win.h"
#include "file/file.h"
#include "errhandler/errhandler.h"
#include "errhandler/errhandler_predefined.h"
#include "class/ompi_pointer_array.h"


/*
 * Table for Fortran <-> C errhandler handle conversion
 */
ompi_pointer_array_t *ompi_errhandler_f_to_c_table;


/*
 * Class information
 */
static void ompi_errhandler_construct(ompi_errhandler_t *eh);
static void ompi_errhandler_destruct(ompi_errhandler_t *eh);


/*
 * Class instance
 */
OBJ_CLASS_INSTANCE(ompi_errhandler_t, ompi_object_t, ompi_errhandler_construct,
                   ompi_errhandler_destruct);


/*
 * MPI_ERRHANDLER_NULL
 */
ompi_errhandler_t ompi_mpi_errhandler_null = {
    { NULL, 0 },

    "MPI_ERRHANDLER_NULL",
    true,
    false,
    OMPI_ERRHANDLER_TYPE_COMM,
    { NULL }
};


/*
 * MPI_ERRORS_ARE_FATAL
 */
ompi_errhandler_t ompi_mpi_errors_are_fatal = {
    { NULL, 0 },

    "MPI_ERRORS_ARE_FATAL",
    true,
    false,
    OMPI_ERRHANDLER_TYPE_COMM,
    { ompi_mpi_errors_are_fatal_handler },
    -1
};


/*
 * MPI_ERRORS_RETURN
 */
ompi_errhandler_t ompi_mpi_errors_return = {
    { NULL, 0 },

    "MPI_ERRORS_ARE_RETURN",
    true,
    false,
    OMPI_ERRHANDLER_TYPE_COMM,
    { ompi_mpi_errors_return_handler },
    -1
};


/*
 * Initialize OMPI errhandler infrastructure
 */
int ompi_errhandler_init(void)
{
  int ret_val;
  
  /* initialize ompi_errhandler_f_to_c_table */

  ompi_errhandler_f_to_c_table = OBJ_NEW(ompi_pointer_array_t);
  if (NULL == ompi_errhandler_f_to_c_table){
    return OMPI_ERROR;
  }

  /* Initialize the predefined error handlers */
  OBJ_CONSTRUCT( &ompi_mpi_errhandler_null, ompi_errhandler_t );
  if( ompi_mpi_errhandler_null.eh_f_to_c_index != OMPI_ERRHANDLER_NULL_FORTRAN )
      return OMPI_ERROR;

  OBJ_CONSTRUCT( &ompi_mpi_errors_are_fatal, ompi_errhandler_t );
  if( ompi_mpi_errors_are_fatal.eh_f_to_c_index != OMPI_ERRORS_ARE_FATAL_FORTRAN )
      return OMPI_ERROR;
  
  OBJ_CONSTRUCT( &ompi_mpi_errors_return, ompi_errhandler_t );
  if( ompi_mpi_errors_return.eh_f_to_c_index != OMPI_ERRORS_RETURN_FORTRAN )
      return OMPI_ERROR;

  /* All done */

  return OMPI_SUCCESS;
}


/*
 * Clean up the errorhandler resources
 */
int ompi_errhandler_finalize(void)
{
  /* Remove errhandler F2C table */
  
  OBJ_RELEASE(ompi_errhandler_f_to_c_table);
  
  /* All done */

  return OMPI_SUCCESS;
}


ompi_errhandler_t *ompi_errhandler_create(ompi_errhandler_type_t object_type,
                                        ompi_errhandler_fortran_handler_fn_t *func)
{
  ompi_errhandler_t *new_errhandler;

  /* Create a new object and ensure that it's valid */

  new_errhandler = OBJ_NEW(ompi_errhandler_t);
  if (NULL == new_errhandler) {
    if (OMPI_ERROR == new_errhandler->eh_f_to_c_index) {
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
  return OMPI_SUCCESS;
}


/**************************************************************************
 *
 * Static functions
 *
 **************************************************************************/

/**
 * Errhandler constructor
 */
static void ompi_errhandler_construct(ompi_errhandler_t *new_errhandler)
{
  int ret_val;

  /* assign entry in fortran <-> c translation array */

  ret_val = ompi_pointer_array_add(ompi_errhandler_f_to_c_table, 
                                  new_errhandler);
  new_errhandler->eh_f_to_c_index = ret_val;
}


/**
 * Errhandler destructor
 */
static void ompi_errhandler_destruct(ompi_errhandler_t *errhandler)
{
  /* reset the ompi_errhandler_f_to_c_table entry - make sure that the
     entry is in the table */

  if (NULL!= ompi_pointer_array_get_item(ompi_errhandler_f_to_c_table,
                                        errhandler->eh_f_to_c_index)) {
    ompi_pointer_array_set_item(ompi_errhandler_f_to_c_table,
                               errhandler->eh_f_to_c_index, NULL);
  }
}


