/*
 * $HEADER$
 */

#ifndef LAM_F77_BINDINGS_H
#define LAM_F77_BINDINGS_H

#include "lam_config.h"
#include "mpi.h"
/*
 * We now build all four fortran bindings and dont care too much about 
 * which convention (lowercase, underscore, double underscore or 
 * all uppercase) is supported by the compiler. The policy now is to 
 * have the mpi_*_f functions be the default symbols and then wrap
 * the four signature types around it. The macro below achieves this.
 */
#define LAM_GENERATE_F77_BINDINGS(upper_case, \
                                  lower_case, \
                                  single_underscore, \
                                  double_underscore, \
                                  wrapper_function, \
                                  signature, \
                                  params) \
            void upper_case signature { wrapper_function params; } \
            void lower_case signature { wrapper_function params; } \
            void single_underscore signature { wrapper_function params; } \
            void double_underscore signature { wrapper_function params; } 
/*
 * We maintain 2 seperate sets of defines and prototypes. This ensures that
 * we can build MPI_* bindings or PMPI_* bindings as ad when needed. The 
 * top-level always builds MPI_* bindings and bottom level will always build
 * PMPI_* bindings. This means that top-level includes "src/mpi/interface/f77"
 * .h files and lower-level includes "src/mpi/interface/f77/profile" .h files
 */

#include "mpi/interface/f77/prototypes_mpi.h"
#include "mpi/interface/f77/profile/prototypes_pmpi.h"

#endif /* LAM_F77_BINDINGS_H */
