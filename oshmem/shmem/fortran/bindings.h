/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef SHMEM_FORTRAN_BINDINGS_H
#define SHMEM_FORTRAN_BINDINGS_H

#include "prototypes_shmem.h"
#include "ompi/mpi/fortran/base/fint_2_int.h"

#define OMPI_GENERATE_FORTRAN_BINDINGS(ret, \
        upper_case, \
        single_underscore, \
        double_underscore, \
        wrapper_function, \
        signature, \
        params) \
ret upper_case signature {return wrapper_function params; } \
ret single_underscore signature {return wrapper_function params; } \
ret double_underscore signature {return wrapper_function params; } 

#endif /*SHMEM_FORTRAN_BINDINGS_H*/
