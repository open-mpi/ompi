/*
 * $HEADER$
 */

#ifndef LAM_F77_BINDINGS_H
#define LAM_F77_BINDINGS_H

#include "lam_config.h"
#include "mpi.h"

/*
 * If we have no weak symbols, then get the #defines to map from the
 * LAM-native mpi_foo_f function to the profiled or non-profiled
 * function name in the right fortran external symbol convention.
 * Yes, kids, that's one of 8 possible sets of #define's.  :-\
 */

#include "mpi/interface/f77/prototypes.h"

#if LAM_HAVE_WEAK_SYMBOLS
/* If we have weak symbols, then we compile the functions as the
   LAM-native mpi_foo_f and use the weak symbols to create the
   aliases.  We include the prototypes for the profiled versions of
   the MPI functions here because some compilers won't generate weak
   symbols unless the real symbols are at least prototyped.  We only
   include the profiled prototypes if we're doing weak symbols because
   the profiled prototypes are generated with #defines that map
   mpi->pmpi and MPI->PMPI, which screws up the non-weak-symbols stuff
   (because they have their own #defines). */
#include "mpi/interface/f77/profile/prototypes.h"
#else
/* We don't have weak symbols. */
#if LAM_PROFILING_DEFINES
/* If we're compiling in the profile/ directory, then
   LAM_PROFILING_DEFINES will be 1.  In this case, get the defines
   mapping from mpi_foo_f to the profiling name in the proper symbol
   convention. */
#include "mpi/interface/f77/profile/defines.h"
#else
/* Otherwise, we're compiling in the main directory, so get the
   defines mapping from mpi_foo_f to the proper symbol convention. */
#include "mpi/interface/f77/defines.h"
#endif
#endif

#endif /* LAM_F77_BINDINGS_H */
