/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
 *
 * Registration of Open MPI's in-tree MPI_T event producers, and the event
 * type handles the producer raise sites use.  See specs/mpi-t-events/spec.md
 * section 7.
 */

#ifndef OMPI_MPIT_EVENTS_H
#define OMPI_MPIT_EVENTS_H

#include "ompi_config.h"

#include "opal/mca/base/mca_base_event.h"

BEGIN_C_DECLS

/* The "model" element of the ompi.mpi.initialization / ompi.mpi.finalization
   event payloads uses the public OMPI_T_MODEL_* values from <mpi.h>
   (OMPI_T_MODEL_WORLD / OMPI_T_MODEL_SESSION).  The world_rank / world_size
   elements are meaningful only for the world model (a process has no rank in a
   session) and are -1 otherwise. */

/* Which MPI ABI a registering MPI_T tool is using.  This governs the
   representation of the MPI object handles carried in event payloads
   (communicator, window, error-handler, session): the Open MPI ABI uses the
   internal object pointer; the MPI Standard ABI uses an integer handle.  A
   process links exactly one ABI, so this is process-global; it is set by
   MPI_T_event_register_callback() and read by the producer raise sites. */
typedef enum {
    OMPI_MPIT_ABI_OMPI = 0,     /* Open MPI ABI: handle == internal object pointer */
    OMPI_MPIT_ABI_STANDARD = 1  /* MPI Standard ABI: handle == integer handle */
} ompi_mpit_abi_t;

/* Defaults to the Open MPI ABI.  The MPI Standard ABI variants of MPI_Init,
   MPI_Init_thread, and MPI_Session_init set this to OMPI_MPIT_ABI_STANDARD
   (see open-mpi/ompi#13280), which makes the producer raise sites publish
   Standard-ABI integer handle values instead of internal object pointers. */
OMPI_DECLSPEC extern ompi_mpit_abi_t ompi_mpit_callback_abi;

/* Convert an internal MPI object handle to the value an MPI Standard ABI
   MPI_T tool expects to see in an event payload.  `object` is the internal
   object pointer (ompi_communicator_t *, ompi_win_t *, ompi_instance_t *,
   ompi_errhandler_t *, ompi_file_t *); `handle_kind` selects which object
   class it is, using the public MPI_T_BIND_* binding constants
   (MPI_T_BIND_MPI_COMM / _WIN / _SESSION / _ERRHANDLER / _FILE).  Returns the
   Standard-ABI integer handle widened to uint64_t.

   The intern->ABI converters live in libmpi_abi (the upper layer); the raise
   sites live in libopen_mpi (the lower layer), which must not depend upward.
   So the Standard-ABI init path installs this converter downward via
   ompi_mpit_register_abi_handle_convert(), mirroring
   ompi_mpi_instance_register_mpiext_init().  The raise sites call
   ompi_mpit_abi_handle(), which forwards to the registered converter (or
   returns 0 if none was registered -- the same fallback as the old stub). */
typedef uint64_t (*ompi_mpit_abi_handle_convert_fn_t)(void *object,
                                                      int handle_kind);

OMPI_DECLSPEC void ompi_mpit_register_abi_handle_convert(
    ompi_mpit_abi_handle_convert_fn_t fn);

OMPI_DECLSPEC uint64_t ompi_mpit_abi_handle(void *object, int handle_kind);

/* Some event payloads also carry integer values whose numeric encoding differs
   between the Open MPI ABI and the MPI Standard ABI -- notably MPI error codes
   (roughly a third of the MPI_ERR_* space differs) and the MPI_T_BIND_* object
   binding kind (the Standard-ABI values are the internal values + 1).  These
   value converters live in libmpi_abi (the upper layer) just like the handle
   converter, so they are installed downward the same way.  The raise sites call
   ompi_mpit_abi_error() / ompi_mpit_abi_bind(), which forward to the registered
   converter, or return the value unchanged if none was registered (the Open MPI
   ABI never installs one, and there the internal encoding is what the tool
   expects). */
typedef int32_t (*ompi_mpit_abi_value_convert_fn_t)(int32_t value);

OMPI_DECLSPEC void ompi_mpit_register_abi_error_convert(
    ompi_mpit_abi_value_convert_fn_t fn);
OMPI_DECLSPEC void ompi_mpit_register_abi_bind_convert(
    ompi_mpit_abi_value_convert_fn_t fn);

OMPI_DECLSPEC int32_t ompi_mpit_abi_error(int32_t err_code);
OMPI_DECLSPEC int32_t ompi_mpit_abi_bind(int32_t object_bind);

/* Event type handles for the in-tree producers.  NULL until (and unless) the
   producers are registered, so a raise site must NULL-check before raising. */
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_comm_created;
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_comm_freed;
/* Object-bound (MPI_T_BIND_MPI_COMM): raised for the specific communicator being
   named, so only registrations bound to that communicator are notified. */
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_comm_name_set;
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_initialization;
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_finalization;
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_errhandler_invoked;
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_win_created;
OMPI_DECLSPEC extern mca_base_event_t *ompi_event_win_freed;

/* Register all in-tree MPI_T event producers (sources + event types).  This is
   the single core-producer entry point; it is idempotent and one-shot, so it
   may be called from every path that registers MCA parameters (ompi_info,
   instance init, MPI_T_init_thread).  Gated by the master MCA parameter
   mca_base_event_register_producers (default on). */
OMPI_DECLSPEC void ompi_mpit_register_events(void);

END_C_DECLS

#endif /* OMPI_MPIT_EVENTS_H */
