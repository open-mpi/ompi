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

/* Convert an internal MPI object handle (pointer) to its MPI Standard ABI
   integer handle, widened to uint64_t.  `handle_kind` is a public MPI_T_BIND_*
   binding constant (MPI_T_BIND_MPI_COMM / _WIN / _SESSION / _ERRHANDLER /
   _FILE).  This is the ompi_mpit_abi_handle_convert_fn_t in the converter set. */
OMPI_DECLSPEC uint64_t ompi_mpit_abi_handle_convert_impl(void *object,
                                                         int handle_kind);

/* Convert an internal MPI error code to its MPI Standard ABI value.  This is
   the ompi_mpit_abi_value_convert_fn_t in the converter set. */
OMPI_DECLSPEC int32_t ompi_mpit_abi_error_convert_impl(int32_t err_code);

/* Convert an internal MPI_T_BIND_* binding kind to its MPI Standard ABI value.
   This is the ompi_mpit_abi_value_convert_fn_t in the converter set. */
OMPI_DECLSPEC int32_t ompi_mpit_abi_bind_convert_impl(int32_t object_bind);

/* Convert an internal MPI_THREAD_* thread support level to its MPI Standard
   ABI value.  This is the ompi_mpit_abi_value_convert_fn_t in the converter set. */
OMPI_DECLSPEC int32_t ompi_mpit_abi_thread_level_convert_impl(int32_t thread_level);

/* Immutable Standard-ABI converter set, installed atomically during
   initialization to avoid data races under MPI_THREAD_MULTIPLE. */
OMPI_DECLSPEC extern const struct ompi_mpit_abi_converters ompi_mpit_abi_standard_converters;

END_C_DECLS

#endif /* OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H */