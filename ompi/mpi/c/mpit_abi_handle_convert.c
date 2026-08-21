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
 * MPI Standard ABI handle converter for MPI_T event payloads.
 *
 * The MPI_T event producers in libopen_mpi publish MPI object handles
 * (communicator, window, session, error handler, file) in their event
 * payloads.  Under the MPI Standard ABI a tool expects those handles as the
 * Standard-ABI integer handle, not the internal object pointer.  The
 * intern->ABI converters (ompi_convert_comm_ompi_to_standard(), etc.) are
 * generated into abi_converters.h and compiled only into libmpi_abi (the upper
 * layer), so the raise sites in libopen_mpi cannot call them directly (the
 * OPAL->OMPI layering forbids an upward link dependency).
 *
 * This translation unit lives in libmpi_abi and provides the converter.  The
 * Standard-ABI init entry points (MPI_Init / MPI_Init_thread / MPI_Session_init)
 * install it downward with ompi_mpit_register_abi_handle_convert(), mirroring
 * ompi_mpi_instance_register_mpiext_init().
 */

#include "ompi_config.h"

#include <stddef.h>
#include <stdint.h>

#include "ompi/communicator/communicator.h"
#include "ompi/win/win.h"
#include "ompi/file/file.h"
#include "ompi/instance/instance.h"
#include "ompi/errhandler/errhandler.h"

#include "ompi/mpi/c/abi.h"
#include "ompi/mpi/c/abi_converters.h"
#include "ompi/mpi/c/mpit_abi_handle_convert.h"

#include "ompi/runtime/ompi_mpit_events.h"

uint64_t ompi_mpit_abi_handle_convert_impl(void *object, int handle_kind)
{
    if (NULL == object) {
        return 0;
    }

    /* handle_kind is a public MPI_T_BIND_* binding constant naming the class of
       the object; convert the internal object pointer to the Standard-ABI
       integer handle with the matching generated intern->ABI converter, then
       widen to uint64_t.  Each converter returns a mangled *_ABI_INTERNAL
       handle (a pointer-width value carrying either a small reserved-handle
       index or the object pointer), so route it through uintptr_t. */
    switch (handle_kind) {
    case MPI_T_BIND_MPI_COMM:
        return (uint64_t) (uintptr_t)
            ompi_convert_comm_ompi_to_standard((ompi_communicator_t *) object);
    case MPI_T_BIND_MPI_WIN:
        return (uint64_t) (uintptr_t)
            ompi_convert_win_ompi_to_standard((ompi_win_t *) object);
    case MPI_T_BIND_MPI_SESSION:
        return (uint64_t) (uintptr_t)
            ompi_convert_session_ompi_to_standard((ompi_instance_t *) object);
    case MPI_T_BIND_MPI_ERRHANDLER:
        return (uint64_t) (uintptr_t)
            ompi_convert_intern_errorhandler_abi_errorhandler(
                (ompi_errhandler_t *) object);
    case MPI_T_BIND_MPI_FILE:
        return (uint64_t) (uintptr_t)
            ompi_convert_file_ompi_to_standard((ompi_file_t *) object);
    default:
        return 0;
    }
}

int32_t ompi_mpit_abi_error_convert_impl(int32_t err_code)
{
    /* Map an internal MPI error code to its MPI Standard ABI value. */
    return (int32_t) ompi_convert_intern_error_abi_error((int) err_code);
}

int32_t ompi_mpit_abi_bind_convert_impl(int32_t object_bind)
{
    /* Map an internal MPI_T_BIND_* value to its MPI Standard ABI value. */
    return (int32_t) ompi_convert_t_bind_ompi_to_standard((int) object_bind);
}
