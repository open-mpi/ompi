/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#ifndef OMPI_FINT_2_INT_H
#define OMPI_FINT_2_INT_H

#include "ompi_config.h"

#include <stdlib.h>

/* 
 * Define MACROS to take account of different size of MPI_Fint from int
 */

#if OMPI_SIZEOF_FORTRAN_INT == SIZEOF_INT
  #define OMPI_ARRAY_NAME_DECL(a)
  #define OMPI_2_DIM_ARRAY_NAME_DECL(a, dim2)
  #define OMPI_SINGLE_NAME_DECL(a)
  #define OMPI_ARRAY_NAME_CONVERT(a) a
  #define OMPI_SINGLE_NAME_CONVERT(a) a
  #define OMPI_INT_2_FINT(a) a
  #define OMPI_FINT_2_INT(a) a
  #define OMPI_ARRAY_FINT_2_INT_ALLOC(in, n) 
  #define OMPI_ARRAY_FINT_2_INT(in, n)
  #define OMPI_2_DIM_ARRAY_FINT_2_INT(in, n, dim2) 
  #define OMPI_ARRAY_FINT_2_INT_CLEANUP(in)
  #define OMPI_SINGLE_FINT_2_INT(in)
  #define OMPI_SINGLE_INT_2_FINT(in)
  #define OMPI_ARRAY_INT_2_FINT(in, n)

#elif OMPI_SIZEOF_FORTRAN_INT > SIZEOF_INT
  #define OMPI_ARRAY_NAME_DECL(a) int *c_##a
  #define OMPI_2_DIM_ARRAY_NAME_DECL(a, dim2) int (*c_##a)[dim2], dim2_index
  #define OMPI_SINGLE_NAME_DECL(a) int c_##a
  #define OMPI_ARRAY_NAME_CONVERT(a) c_##a
  #define OMPI_SINGLE_NAME_CONVERT(a) &c_##a
  #define OMPI_INT_2_FINT(a) a
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

  /* This is for 2-dim arrays */
  #define OMPI_2_DIM_ARRAY_FINT_2_INT(in, n, dim2) \
    OMPI_ARRAY_NAME_CONVERT(in) = (int (*)[dim2]) malloc(n * sizeof(*OMPI_ARRAY_NAME_CONVERT(in))); \
    while(n > 0) { \
      for(dim2_index = 0; dim2_index < dim2; ++dim2_index) { \
        OMPI_ARRAY_NAME_CONVERT(in)[n - 1][dim2_index] = (int)in[n - 1][dim2_index]; \
      } \
      --n; \
    }

  /* This is for IN parameters. Does only free */
  #define OMPI_ARRAY_FINT_2_INT_CLEANUP(in) \
    free(OMPI_ARRAY_NAME_CONVERT(in))

  /* This is for single IN parameter */
  #define OMPI_SINGLE_FINT_2_INT(in) \
    OMPI_ARRAY_NAME_CONVERT(in) = (int) *(in)

  /* This is for single OUT parameter */
  #define OMPI_SINGLE_INT_2_FINT(in) \
    *(in) = OMPI_ARRAY_NAME_CONVERT(in)

  /* This is for OUT/IN-OUT parametes. Does back assignment and free */
  #define OMPI_ARRAY_INT_2_FINT(in, n) \
    while(n > 0) {\
      in[n - 1] = OMPI_ARRAY_NAME_CONVERT(in)[n - 1]; \
      --n; \
    } \
    free(OMPI_ARRAY_NAME_CONVERT(in))

#else /* int > MPI_Fint  */
  #define OMPI_ARRAY_NAME_DECL(a) int *c_##a
  #define OMPI_2_DIM_ARRAY_NAME_DECL(a, dim2) int (*c_##a)[dim2], dim2_index
  #define OMPI_SINGLE_NAME_DECL(a) int c_##a
  #define OMPI_ARRAY_NAME_CONVERT(a) c_##a
  #define OMPI_SINGLE_NAME_CONVERT(a) &c_##a
  #define OMPI_INT_2_FINT(a) (MPI_Fint)(a)
  #define OMPI_FINT_2_INT(a) (a)

  /* This is for OUT parameters. Does only alloc */
  #define OMPI_ARRAY_FINT_2_INT_ALLOC(in, n) \
    OMPI_ARRAY_NAME_CONVERT(in) = malloc(n * sizeof(int))

  #define OMPI_ARRAY_FINT_2_INT(in, n) \
    OMPI_ARRAY_NAME_CONVERT(in) = malloc(n * sizeof(int)); \
    while(n > 0) { \
      OMPI_ARRAY_NAME_CONVERT(in)[n - 1] = in[n - 1]; \
      --n; \
    }

  #define OMPI_2_DIM_ARRAY_FINT_2_INT(in, n, dim2) \
    OMPI_ARRAY_NAME_CONVERT(in) = (int (*)[dim2]) malloc(n * sizeof(*OMPI_ARRAY_NAME_CONVERT(in))); \
    while(n > 0) { \
      for(dim2_index = 0; dim2_index < dim2; ++dim2_index) { \
        OMPI_ARRAY_NAME_CONVERT(in)[n - 1][dim2_index] = in[n - 1][dim2_index]; \
      } \
      --n; \
    }

  #define OMPI_ARRAY_FINT_2_INT_CLEANUP(in) \
    free(OMPI_ARRAY_NAME_CONVERT(in))

  #define OMPI_SINGLE_FINT_2_INT(in) \
     OMPI_ARRAY_NAME_CONVERT(in) = *(in)

  #define OMPI_SINGLE_INT_2_FINT(in) \
    *in = (MPI_Fint) OMPI_ARRAY_NAME_CONVERT(in)

  #define OMPI_ARRAY_INT_2_FINT(in, n) \
    while(n > 0) {\
      in[n - 1] = (MPI_Fint) OMPI_ARRAY_NAME_CONVERT(in)[n - 1]; \
      --n; \
    } \
    free(OMPI_ARRAY_NAME_CONVERT(in))


#endif

#endif /* OMPI_FINT_2_INT_H */
