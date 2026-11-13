/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Triad National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * Declaration of the MPI Standard ABI converters for MPI_T event payloads.
 * The implementations live in mpit_abi_handle_convert.c and are compiled only
 * into libmpi_abi.  The Standard-ABI init entry points install all converters
 * downward atomically via ompi_mpit_register_abi_converters() so the
 * libopen_mpi producer raise sites can reach them without an upward link
 * dependency.
 */

#ifndef OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H
#define OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H

#include "ompi_config.h"

#include <stdint.h>

#include "ompi/runtime/ompi_mpit_events.h"

BEGIN_C_DECLS

/* Immutable Standard-ABI converter set, installed atomically during
   initialization to avoid data races under MPI_THREAD_MULTIPLE.  The individual
   converter implementations (_impl functions) are private to
   mpit_abi_handle_convert.c. */
OMPI_DECLSPEC extern const struct ompi_mpit_abi_converters ompi_mpit_abi_standard_converters;

END_C_DECLS

#endif /* OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H */
