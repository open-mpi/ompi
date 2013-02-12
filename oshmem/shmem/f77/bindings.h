/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef SHMEM_F77_BINDINGS_H
#define SHMEM_F77_BINDINGS_H
#include "prototypes_shmem.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"
#define OMPI_GENERATE_F77_BINDINGS(ret, \
        upper_case, \
        single_underscore, \
        double_underscore, \
        wrapper_function, \
        signature, \
        params) \
ret upper_case signature {return wrapper_function params; } \
ret single_underscore signature {return wrapper_function params; } \
ret double_underscore signature {return wrapper_function params; } 
#endif /*SHMEM_F77_BINDINGS_H*/
