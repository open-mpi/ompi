/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "ompi/file/file.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/errhandler/errhandler_predefined.h"
#include "ompi/class/ompi_pointer_array.h"


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
OBJ_CLASS_INSTANCE(ompi_errhandler_t, opal_object_t, ompi_errhandler_construct,
                   ompi_errhandler_destruct);


ompi_errhandler_t ompi_mpi_errhandler_null;
ompi_errhandler_t ompi_mpi_errors_are_fatal;
ompi_errhandler_t ompi_mpi_errors_return;
ompi_errhandler_t ompi_mpi_errors_throw_exceptions;

/*
 * Local state to know when the three intrinsics have been freed; see
 * the errhandler destructor for more info.
 */
static bool null_freed = false;
static bool fatal_freed = false;
static bool return_freed = false;
static bool throw_freed = false;


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
  ompi_mpi_errhandler_null.eh_mpi_object_type = OMPI_ERRHANDLER_TYPE_PREDEFINED;
  ompi_mpi_errhandler_null.eh_fortran_function = false;
  ompi_mpi_errhandler_null.eh_comm_fn = NULL;
  ompi_mpi_errhandler_null.eh_file_fn = NULL;
  ompi_mpi_errhandler_null.eh_win_fn  = NULL ;
  ompi_mpi_errhandler_null.eh_fort_fn = NULL;
  strncpy (ompi_mpi_errhandler_null.eh_name, "MPI_ERRHANDLER_NULL", 
	   strlen("MPI_ERRHANDLER_NULL")+1 );


  OBJ_CONSTRUCT( &ompi_mpi_errors_are_fatal, ompi_errhandler_t );
  if( ompi_mpi_errors_are_fatal.eh_f_to_c_index != OMPI_ERRORS_ARE_FATAL_FORTRAN )
      return OMPI_ERROR;
  ompi_mpi_errors_are_fatal.eh_mpi_object_type = OMPI_ERRHANDLER_TYPE_PREDEFINED;
  ompi_mpi_errors_are_fatal.eh_fortran_function = false;
  ompi_mpi_errors_are_fatal.eh_comm_fn = ompi_mpi_errors_are_fatal_comm_handler;
  ompi_mpi_errors_are_fatal.eh_file_fn = ompi_mpi_errors_are_fatal_file_handler;
  ompi_mpi_errors_are_fatal.eh_win_fn  = ompi_mpi_errors_are_fatal_win_handler ;
  ompi_mpi_errors_are_fatal.eh_fort_fn = NULL;
  strncpy (ompi_mpi_errors_are_fatal.eh_name, "MPI_ERRORS_ARE_FATAL", 
	   strlen("MPI_ERRORS_ARE_FATAL")+1 );
  
  OBJ_CONSTRUCT( &ompi_mpi_errors_return, ompi_errhandler_t );
  if( ompi_mpi_errors_return.eh_f_to_c_index != OMPI_ERRORS_RETURN_FORTRAN )
      return OMPI_ERROR;
  ompi_mpi_errors_return.eh_mpi_object_type  = OMPI_ERRHANDLER_TYPE_PREDEFINED;
  ompi_mpi_errors_return.eh_fortran_function = false;
  ompi_mpi_errors_return.eh_comm_fn = ompi_mpi_errors_return_comm_handler;
  ompi_mpi_errors_return.eh_file_fn = ompi_mpi_errors_return_file_handler;
  ompi_mpi_errors_return.eh_win_fn  = ompi_mpi_errors_return_win_handler;
  ompi_mpi_errors_return.eh_fort_fn = NULL;
  strncpy (ompi_mpi_errors_return.eh_name, "MPI_ERRORS_RETURN", 
	   strlen("MPI_ERRORS_RETURN")+1 );

  /* If we're going to use C++, functions will be fixed up during MPI::Init */
  OBJ_CONSTRUCT( &ompi_mpi_errors_throw_exceptions, ompi_errhandler_t );
  ompi_mpi_errors_are_fatal.eh_mpi_object_type = OMPI_ERRHANDLER_TYPE_PREDEFINED;
  ompi_mpi_errors_are_fatal.eh_fortran_function = false;
  ompi_mpi_errors_are_fatal.eh_comm_fn = ompi_mpi_errors_are_fatal_comm_handler;
  ompi_mpi_errors_are_fatal.eh_file_fn = ompi_mpi_errors_are_fatal_file_handler;
  ompi_mpi_errors_are_fatal.eh_win_fn  = ompi_mpi_errors_are_fatal_win_handler ;
  ompi_mpi_errors_are_fatal.eh_fort_fn = NULL;
  strncpy (ompi_mpi_errors_are_fatal.eh_name, "MPI_ERRORS_THROW_EXCEPTIONS", 
	   strlen("MPI_ERRORS_THROW_EXCEPTIONS")+1 );

  /* All done */

  return OMPI_SUCCESS;
}


/*
 * Clean up the errorhandler resources
 */
int ompi_errhandler_finalize(void)
{
    /* Forcibly release the intrinsic error handlers because in order
       to be safe, we increase the refcount on error handlers in
       MPI_*_GET_ERRHANDLER and MPI_ERRHANDLER_GET.  If these handles
       are never ERRHANDLER_FREEd, then the refcount will not be
       decremented and they will not naturally get to 0 during
       FINALIZE.  Hence, we RELEASE on the intrinsics until they are
       freed. */

    while (!null_freed) {
        OBJ_DESTRUCT(&ompi_mpi_errhandler_null);
    }
    while (!fatal_freed) {
        OBJ_DESTRUCT(&ompi_mpi_errors_are_fatal);
    }
    while (!return_freed) {
        OBJ_DESTRUCT(&ompi_mpi_errors_return);
    }
    while (!throw_freed) {
        OBJ_DESTRUCT(&ompi_mpi_errors_throw_exceptions);
    }
  
    /* JMS Add stuff here checking for unreleased errorhandlers,
       similar to communicators, info handles, etc. */

    /* Remove errhandler F2C table */

  /* Forcibly release the intrinsic error handlers because in order to
     be safe, we increase the refcount on error handlers in
     MPI_*_GET_ERRHANDLER and MPI_ERRHANDLER_GET.  If these handles
     are never ERRHANDLER_FREEd, then the refcount will not be
     decremented and they will not naturally get to 0 during FINALIZE.
     Hence, we RELEASE on the intrinsics until they are freed. */
  
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
  size_t ret_val;

  /* assign entry in fortran <-> c translation array */

  ret_val = ompi_pointer_array_add(ompi_errhandler_f_to_c_table, 
                                   new_errhandler);
  new_errhandler->eh_f_to_c_index = ret_val;

  new_errhandler->eh_fortran_function = false;

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

  /* Reset the static state if we're releasing one of the
     intrinsics */

  if (&ompi_mpi_errhandler_null == errhandler) {
      null_freed = true;
  } else if (&ompi_mpi_errors_are_fatal == errhandler) {
      fatal_freed = true;
  } else if (&ompi_mpi_errors_return == errhandler) {
      return_freed = true;
  } else if (&ompi_mpi_errors_throw_exceptions == errhandler) {
      throw_freed = true;
  }
}
