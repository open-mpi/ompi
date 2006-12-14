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
 * Copyright (c) 2006      University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <stdarg.h>

#include "opal/util/output.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/errhandler/errhandler_predefined.h"
#include "ompi/errhandler/errcode.h"
#include "ompi/communicator/communicator.h"
#include "ompi/file/file.h"
#include "ompi/win/win.h"
#include "orte/runtime/runtime.h"
#include "opal/util/printf.h"

/*
 * Local functions
 */
static void backend_fatal(char *type, struct ompi_communicator_t *comm, 
                          char *name, int *error_code, va_list arglist);
static void out(char *str, char *arg);


void ompi_mpi_errors_are_fatal_comm_handler(struct ompi_communicator_t **comm,
					    int *error_code, ...)
{
  char *name;
  struct ompi_communicator_t *abort_comm;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != comm) {
      name = (*comm)->c_name;
      abort_comm = *comm;
  } else {
      name = NULL;
      abort_comm = NULL;
  }
  backend_fatal("communicator", abort_comm, name, error_code, arglist);
}


void ompi_mpi_errors_are_fatal_file_handler(struct ompi_file_t **file,
					    int *error_code, ...)
{
  char *name;
  struct ompi_communicator_t *abort_comm;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != file) {
      name = (*file)->f_filename;
      abort_comm = (*file)->f_comm;
  } else {
      name = NULL;
      abort_comm = NULL;
  }
  backend_fatal("file", abort_comm, name, error_code, arglist);
}


void ompi_mpi_errors_are_fatal_win_handler(struct ompi_win_t **win,
					   int *error_code, ...)
{
  char *name;
  struct ompi_communicator_t *abort_comm = NULL;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != win) {
      name = (*win)->w_name;
  } else {
      name = NULL;
  }
  backend_fatal("win", abort_comm, name, error_code, arglist);
}

void ompi_mpi_errors_return_comm_handler(struct ompi_communicator_t **comm,
					 int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
    /* Silence some compiler warnings */
    
    va_list arglist;
    va_start(arglist, error_code);
    va_end(arglist);
}


void ompi_mpi_errors_return_file_handler(struct ompi_file_t **file,
					 int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
    /* Silence some compiler warnings */
    
    va_list arglist;
    va_start(arglist, error_code);
    va_end(arglist);
}


void ompi_mpi_errors_return_win_handler(struct ompi_win_t **win,
					int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
    /* Silence some compiler warnings */
    
    va_list arglist;
    va_start(arglist, error_code);
    va_end(arglist);
}


static void out(char *str, char *arg)
{
    if (ompi_mpi_initialized && !ompi_mpi_finalized) {
        if (NULL != arg) {
            opal_output(0, str, arg);
        } else {
            opal_output(0, str);
        }
    } else {
        if (NULL != arg) {
            fprintf(stderr, str, arg);
        } else {
            fprintf(stderr, str);
        }
    }
}

static void backend_fatal(char *type, struct ompi_communicator_t *comm,
                          char *name, int *error_code, 
                          va_list arglist)
{
    char *arg;
    char str[MPI_MAX_PROCESSOR_NAME * 2];

    fflush(stdout);
    fflush(stderr);
    arg = va_arg(arglist, char*);
    if (NULL != arg) {
        out("*** An error occurred in %s\n", arg);
    } else {
        out("*** An error occurred\n", NULL);
    }

    if (NULL != name && ompi_mpi_initialized && !ompi_mpi_finalized) {
        /* Don't use asprintf() here because there may be stack / heap
           corruption by the time we're invoked, so just do it on the
           stack */
        str[0] = '\0';
        strcat(str, type);
        strcat(str, " ");
        strcat(str, name);
        out("*** on %s", str);
    } else if (!ompi_mpi_initialized) {
        out("*** before MPI was initialized\n", NULL);
    } else if (ompi_mpi_finalized) {
        out("*** after MPI was finalized\n", NULL);
    } else if (NULL == name) {
        out("*** on a NULL %s\n", type);
    }

    if (NULL != error_code) {
        char *tmp = ompi_mpi_errnum_get_string(*error_code);
        if (NULL != tmp) {
            out("*** %s\n", tmp);
        } else {
            char intbuf[32];
            snprintf(intbuf, 32, "%d", *error_code);
            out("*** Error code: %d (no associated error message)\n", intbuf);
        }
    }
    out("*** MPI_ERRORS_ARE_FATAL (goodbye)\n", NULL);
    va_end(arglist);

    /* Should we do something more intelligent here? */
    if (comm == NULL) {
        comm = &ompi_mpi_comm_self;
    }

    ompi_mpi_abort(comm, 1, false);
}
