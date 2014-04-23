/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef SHMEM_FORTRAN_PBINDINGS_H
#define SHMEM_FORTRAN_PBINDINGS_H

#include "prototypes_pshmem.h"

#define SHMEM_GENERATE_WEAK_PRAGMA(x) _Pragma(#x)

#define SHMEM_GENERATE_WEAK_BINDINGS(UPPER_NAME, lower_name)                 \
    SHMEM_GENERATE_WEAK_PRAGMA(weak UPPER_NAME = P ## UPPER_NAME)            \
    SHMEM_GENERATE_WEAK_PRAGMA(weak lower_name ## _ = p ## lower_name ## _)  \
    SHMEM_GENERATE_WEAK_PRAGMA(weak lower_name ## __ = p ## lower_name ## __)

#endif /*SHMEM_FORTRAN_PBINDINGS_H*/
