/*
 * $HEADER$
 */

#ifndef LAM_C_BINDINGS_H
#define LAM_C_BINDINGS_H

#include "lam_config.h"
#include "mpi.h"

/* If compiling in the profile directory, then we don't have weak
   symbols and therefore we need the defines to map from MPI->PMPI.
   NOTE: pragma weak stuff is handled on a file-by-file basis; it
   doesn't work to simply list all of the pragmas in a top-level
   header file. */

#if LAM_PROFILING_DEFINES
#include "mpi/interface/c/profile/defines.h"
#endif

/* This variable is actually in src/mpi/runtime/lam_mpi_init.c, but it
   is used by every MPI function. */

extern bool lam_mpi_param_check;

#endif /* LAM_C_BINDINGS_H */
