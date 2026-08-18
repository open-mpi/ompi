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
 * Declaration of the MPI Standard ABI handle converter for MPI_T event
 * payloads.  The implementation lives in mpit_abi_handle_convert.c and is
 * compiled only into libmpi_abi.  The Standard-ABI init entry points install
 * it downward via ompi_mpit_register_abi_handle_convert() so the libopen_mpi
 * producer raise sites can reach it without an upward link dependency.
 */

#ifndef OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H
#define OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H

#include "ompi_config.h"

#include <stdint.h>

BEGIN_C_DECLS

/* Convert an internal MPI object handle (pointer) to its MPI Standard ABI
   integer handle, widened to uint64_t.  `handle_kind` is a public MPI_T_BIND_*
   binding constant (MPI_T_BIND_MPI_COMM / _WIN / _SESSION / _ERRHANDLER /
   _FILE).  This is the ompi_mpit_abi_handle_convert_fn_t installed via
   ompi_mpit_register_abi_handle_convert(). */
OMPI_DECLSPEC uint64_t ompi_mpit_abi_handle_convert_impl(void *object,
                                                         int handle_kind);

/* Convert an internal MPI error code to its MPI Standard ABI value.  This is
   the ompi_mpit_abi_value_convert_fn_t installed via
   ompi_mpit_register_abi_error_convert(). */
OMPI_DECLSPEC int32_t ompi_mpit_abi_error_convert_impl(int32_t err_code);

/* Convert an internal MPI_T_BIND_* binding kind to its MPI Standard ABI value.
   This is the ompi_mpit_abi_value_convert_fn_t installed via
   ompi_mpit_register_abi_bind_convert(). */
OMPI_DECLSPEC int32_t ompi_mpit_abi_bind_convert_impl(int32_t object_bind);

END_C_DECLS

#endif /* OMPI_MPI_C_MPIT_ABI_HANDLE_CONVERT_H */