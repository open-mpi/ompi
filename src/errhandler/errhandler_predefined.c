/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <stdlib.h>
#include <stdarg.h>

#include "util/output.h"
#include "errhandler/errhandler.h"
#include "errhandler/errhandler_predefined.h"
#include "errhandler/errcode.h"
#include "communicator/communicator.h"
#include "file/file.h"
#include "win/win.h"
#include "runtime/runtime.h"


/*
 * Local functions
 */
static void backend_fatal(char *type, char *name, int *error_code, 
                          va_list arglist);
static void out(char *str, char *arg);


void ompi_mpi_errors_are_fatal_comm_handler(struct ompi_communicator_t **comm,
					    int *error_code, ...)
{
  char *name;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != comm) {
      name = (*comm)->c_name;
  } else {
      name = NULL;
  }
  backend_fatal("communicator", name, error_code, arglist);
}


void ompi_mpi_errors_are_fatal_file_handler(struct ompi_file_t **file,
					    int *error_code, ...)
{
  char *name;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != file) {
      name = (*file)->f_filename;
  } else {
      name = NULL;
  }
  backend_fatal("file", name, error_code, arglist);
}


void ompi_mpi_errors_are_fatal_win_handler(struct ompi_win_t **win,
					   int *error_code, ...)
{
  char *name;
  va_list arglist;

  va_start(arglist, error_code);

  if (NULL != win) {
      name = (*win)->w_name;
  } else {
      name = NULL;
  }
  backend_fatal("win", name, error_code, arglist);
}


void ompi_mpi_errors_return_comm_handler(struct ompi_communicator_t **comm,
					 int *error_code, ...)
{
  /* Don't need anything more -- just need this function to exist */
}


void ompi_mpi_errors_return_file_handler(struct ompi_file_t **file,
					 int *error_code, ...)
{
    /* Don't need anything more -- just need this function to exist */
}


void ompi_mpi_errors_return_win_handler(struct ompi_win_t **win,
					int *error_code, ...)
{
  /* Don't need anything more -- just need this function to exist */
}


static void out(char *str, char *arg)
{
    if (ompi_mpi_initialized && !ompi_mpi_finalized) {
        if (NULL != arg) {
            ompi_output(0, str, arg);
        } else {
            ompi_output(0, str);
        }
    } else {
        if (NULL != arg) {
            fprintf(stderr, str, arg);
        } else {
            fprintf(stderr, str);
        }
    }
}

static void backend_fatal(char *type, char *name, int *error_code, 
                          va_list arglist)
{
    char *arg;

    fflush(stdout);
    fflush(stderr);
    arg = va_arg(arglist, char*);
    if (NULL != arg) {
        out("*** An error occurred in %s\n", arg);
    } else {
        out("*** An error occurred\n", NULL);
    }

    if (NULL != name && ompi_mpi_initialized && !ompi_mpi_finalized) {
        out("*** on %s ", type);
        out("%s\n", name);
    } else if (!ompi_mpi_initialized) {
        out("*** before MPI was initialized\n", NULL);
    } else if (ompi_mpi_finalized) {
        out("*** after MPI was finalized\n", NULL);
    } else if (NULL == name) {
        out("*** on a NULL %s\n", type);
    }

    if (NULL != error_code) {
        char *tmp = ompi_mpi_errcode_get_string(*error_code);
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
    
    abort();
}
