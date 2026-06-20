/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
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

/* Hard-coded to the Open MPI ABI for now.  When the MPI Standard ABI lands
   (open-mpi/ompi#13280), its MPI_T_event_register_callback entry point will set
   this to OMPI_MPIT_ABI_STANDARD, and the producers' "else" branches (marked
   "TODO ABI") will fill in the Standard-ABI handle values. */
OMPI_DECLSPEC extern ompi_mpit_abi_t ompi_mpit_callback_abi;

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
