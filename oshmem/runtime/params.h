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
 * Whether an abort should print out a stack trace or not.
 */
OSHMEM_DECLSPEC extern bool oshmem_shmem_abort_print_stack;

/**
 * Whether  abort  should  print  out an  identifying  message
 * (e.g., hostname  and PID)  and loop waiting  for a  debugger to
 * attach.  The value of the integer is how many seconds to wait:
 *
 * 0 = do not print the message and do not loop
 * negative value = print the message and loop forever
 * positive value = print the message and delay for that many seconds
 */
OSHMEM_DECLSPEC extern int oshmem_shmem_abort_delay;

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
