/*
 * $HEADER$
 */

#ifndef OMPI_F77_BINDINGS_H
#define OMPI_F77_BINDINGS_H

#include "ompi_config.h"
#include "mpi.h"
/*
 * We now build all four fortran bindings and dont care too much about 
 * which convention (lowercase, underscore, double underscore or 
 * all uppercase) is supported by the compiler. The policy now is to 
 * have the mpi_*_f functions be the default symbols and then wrap
 * the four signature types around it. The macro below achieves this.
 */
#define OMPI_GENERATE_F77_BINDINGS(upper_case, \
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

/* 
 * Define MACROS to take account of different size of MPI_Fint from int
 */

#if OMPI_SIZEOF_FORTRAN_INT == SIZEOF_INT
  #define OMPI_ARRAY_NAME_DECL(a)
  #define OMPI_ARRAY_NAME_CONVERT(a) a
  #define OMPI_INT_2_FINT(a) a
  #define OMPI_FINT_2_INT(a) a
  #define OMPI_ARRAY_FINT_2_INT_ALLOC(in, n) 
  #define OMPI_ARRAY_FINT_2_INT(in, n)
  #define OMPI_ARRAY_INT_2_FINT(in, n)
  #define OMPI_ARRAY_FINT_2_INT_CLEANUP(in)

#elif OMPI_SIZEOF_FORTRAN_INT > SIZEOF_INT
  #define OMPI_ARRAY_NAME_DECL(a) int *c_##a
  #define OMPI_ARRAY_NAME_CONVERT(a) c_##a
  #define OMPI_INT_2_FINT(a) (a)
  #define OMPI_FINT_2_INT(a) (int) (a)

  /* This is for OUT parameters. Does only alloc */
  #define OMPI_ARRAY_FINT_2_INT_ALLOC(in, n) \
    OMPI_ARRAY_NAME_CONVERT(in) = malloc(n * sizeof(int))

  /* This is for IN/IN-OUT parameters. Does alloc and assignment */
  #define OMPI_ARRAY_FINT_2_INT(in, n) \
    OMPI_ARRAY_NAME_CONVERT(in) = malloc(n * sizeof(int)); \
    while(n > 0) { \
      OMPI_ARRAY_NAME_CONVERT(in)[n - 1] = (int) in[n - 1]; \
      --n; \
    } 

  /* This is for OUT/IN-OUT parametes. Does back assignment and free */
  #define OMPI_ARRAY_INT_2_FINT(in, n) \
    while(n > 0) {\
      in[n - 1] = OMPI_ARRAY_NAME_CONVERT(in)[n - 1]; \
      --n; \
    } \
    free(OMPI_ARRAY_NAME_CONVERT(in))

  /* This is for IN parameters. Does only free */
  #define OMPI_ARRAY_FINT_2_INT_CLEANUP(in) \
    free(OMPI_ARRAY_NAME_CONVERT(in))

#else /* int > MPI_Fint  */
  #define OMPI_ARRAY_NAME_DECL(a) int *c_##a
  #define OMPI_ARRAY_NAME_CONVERT(a) c_##a
  #define OMPI_INT_2_FINT(a) (MPI_Fint)(a)
  #define OMPI_FINT_2_INT(a) (a)

  #define OMPI_ARRAY_INT_2_FINT(in, n) \
    while(n > 0) {\
      in[n - 1] = (MPI_Fint) OMPI_ARRAY_NAME_CONVERT(in)[n - 1]; \
      --n; \
    } \
    free(OMPI_ARRAY_NAME_CONVERT(in))

  #define OMPI_ARRAY_FINT_2_INT_CLEANUP(in) \
    free(OMPI_ARRAY_NAME_CONVERT(in))

  #define OMPI_ARRAY_FINT_2_INT(in, n) \
    OMPI_ARRAY_NAME_CONVERT(in) = malloc(n * sizeof(int)); \
    while(n > 0) { \
      OMPI_ARRAY_NAME_CONVERT(in)[n - 1] = in[n - 1]; \
      --n; \
    }

#endif

#include "mpi/f77/prototypes_mpi.h"
#include "mpi/f77/profile/prototypes_pmpi.h"

#endif /* OMPI_F77_BINDINGS_H */
