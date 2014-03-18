/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OSHMEM_RUNTIME_PARAMS_H
#define OSHMEM_RUNTIME_PARAMS_H

#include "oshmem_config.h"

BEGIN_C_DECLS

/*
 * Global variables
 */

/**
 * Whether an MPI_ABORT should print out a stack trace or not.
 */
OSHMEM_DECLSPEC extern bool ompi_mpi_abort_print_stack;

/** 
 * Whether or not the lock routines are recursive 
 * (ie support embedded calls) 
 */
OSHMEM_DECLSPEC extern int oshmem_shmem_lock_recursive;

/** 
 * Level of shmem API verbosity 
 */
OSHMEM_DECLSPEC extern int oshmem_shmem_api_verbose;

/**
 * Whether to force SHMEM processes to fully
 * wire-up the connections between SHMEM
 */
OSHMEM_DECLSPEC extern int oshmem_preconnect_all;

END_C_DECLS

#endif /* OSHMEM_RUNTIME_PARAMS_H */
