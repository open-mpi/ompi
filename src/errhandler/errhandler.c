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
    OMPI_ERRHANDLER_TYPE_PREDEFINED,
    false,
    NULL,
    NULL,
    NULL,
    NULL,
    -1
};
void ompi_mpi_errors_return_handler(struct ompi_communicator_t **comm,
                                   int *error_code, ...);


/*
 * MPI_ERRORS_ARE_FATAL
 */
ompi_errhandler_t ompi_mpi_errors_are_fatal = {
    { NULL, 0 },

    "MPI_ERRORS_ARE_FATAL",
    OMPI_ERRHANDLER_TYPE_PREDEFINED,
    false,
    ompi_mpi_errors_are_fatal_comm_handler,
    ompi_mpi_errors_are_fatal_file_handler,
    ompi_mpi_errors_are_fatal_win_handler,
    NULL,
    -1
};


/*
 * MPI_ERRORS_RETURN
 */
ompi_errhandler_t ompi_mpi_errors_return = {
    { NULL, 0 },

    "MPI_ERRORS_RETURN",
    OMPI_ERRHANDLER_TYPE_PREDEFINED,
    false,
    ompi_mpi_errors_return_comm_handler,
    ompi_mpi_errors_return_file_handler,
    ompi_mpi_errors_return_win_handler,
    NULL,
    -1
};


/*
 * Initialize OMPI errhandler infrastructure
 */
int ompi_errhandler_init(void)
{
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
					  ompi_errhandler_generic_handler_fn_t *func)
{
  ompi_errhandler_t *new_errhandler;

  /* Create a new object and ensure that it's valid */

  new_errhandler = OBJ_NEW(ompi_errhandler_t);
  if (NULL != new_errhandler) {
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
      new_errhandler->eh_fortran_function = false;
      switch (object_type ) {
	  case (OMPI_ERRHANDLER_TYPE_COMM):
	      new_errhandler->eh_comm_fn = (MPI_Comm_errhandler_fn *)func;
	      break;
	  case (OMPI_ERRHANDLER_TYPE_FILE):
	      new_errhandler->eh_file_fn = (MPI_File_errhandler_fn *)func;
	      break;
	  case (OMPI_ERRHANDLER_TYPE_WIN):
	      new_errhandler->eh_win_fn = (MPI_Win_errhandler_fn *)func;
	      break;
	  default:
	      break;
      }

      new_errhandler->eh_fort_fn = (ompi_errhandler_fortran_handler_fn_t *)func;
    }
  }

  /* All done */

  return new_errhandler;
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

  new_errhandler->eh_fortran_function = 0;

  new_errhandler->eh_comm_fn      = NULL;
  new_errhandler->eh_win_fn       = NULL;
  new_errhandler->eh_file_fn      = NULL;
  new_errhandler->eh_fort_fn      = NULL;

  memset (new_errhandler->eh_name, 0, MPI_MAX_OBJECT_NAME);
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


