/*
 * $HEADER$
 */

#ifndef OMPI_C_BINDINGS_H
#define OMPI_C_BINDINGS_H

#include "ompi_config.h"
#include "mpi.h"

/* If compiling in the profile directory, then we don't have weak
   symbols and therefore we need the defines to map from MPI->PMPI.
   NOTE: pragma weak stuff is handled on a file-by-file basis; it
   doesn't work to simply list all of the pragmas in a top-level
   header file. */

/* This variable is actually in src/mpi/runtime/ompi_mpi_init.c, but it
   is used by every MPI function. */

extern bool ompi_mpi_param_check;

#endif /* OMPI_C_BINDINGS_H */
