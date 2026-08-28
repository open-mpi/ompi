/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2021 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2018      Triad National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.

 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PML_BASE_H
#define MCA_PML_BASE_H

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "ompi/mca/pml/pml.h"

/*
 * Global functions for the PML
 */

BEGIN_C_DECLS

/*
 * This is the base priority for a PML wrapper component
 * If there exists more than one then it is undefined
 * which one is picked.
 */
#define PML_SELECT_WRAPPER_PRIORITY -128

/*
 * MCA framework
 */
OMPI_DECLSPEC extern mca_base_framework_t ompi_pml_base_framework;
/*
 * Select an available component.
 */
OMPI_DECLSPEC  int mca_pml_base_select(bool enable_progress_threads,
                                       bool enable_mpi_threads);
OMPI_DECLSPEC  int mca_pml_base_progress(void);
    /* share in modex the name of the selected component */
OMPI_DECLSPEC int mca_pml_base_pml_selected(const char *name);

/**
 * The name of the PML this process selected, as it went into the modex.
 *
 * Not read back from mca_pml_base_selected_component: vprotocol renames
 * that copy ("ob1]vpessimist") as the losing components close, while what
 * a peer compares itself against is the host PML's own name. Empty until
 * a PML has been selected.
 */
OMPI_DECLSPEC const char *mca_pml_base_pml_selected_name(void);

/**
 * Verify, once per process, that this job agrees on the PML.
 *
 * In the full-modex mode only rank 0 publishes its choice, so this is a
 * single comparison against rank 0 that needs no proc list, and it is
 * enough: every rank runs it, so a rank that differs from rank 0 --
 * either of the two, in a two-PML job -- says so. In the mode where
 * every rank publishes, this is a no-op and the per-peer check on the
 * wire-up path does the work instead.
 *
 * Nothing waits here. The fence has only just been started, so usually
 * rank 0's data is not local yet and there is nothing to compare against;
 * the comparison is then made by the first wire-up, which cannot happen
 * any earlier than that data arriving.
 *
 * A mismatch aborts: there is no caller to report to, and a job whose
 * ranks picked different PMLs cannot communicate.
 */
OMPI_DECLSPEC int mca_pml_base_pml_check_start(void);

/**
 * Verify one peer as it is being wired.
 *
 * A no-op unless every rank published its choice, which is the mode
 * where a peer can be asked about individually. Folded into the lazy
 * wire-up on purpose: the first Get for a peer is what caches that
 * peer's data, so asking for this key there costs a round trip that the
 * wire-up was about to make anyway.
 *
 * @retval OMPI_SUCCESS        the peer agrees, or nothing to check.
 * @retval OMPI_ERR_NOT_READY  the peer has not published yet; retry.
 *
 * A mismatch aborts, for the same reason as above: the caller is a
 * first send or a fragment arrival, with no way to report it.
 */
OMPI_DECLSPEC int mca_pml_base_pml_check_peer(struct ompi_proc_t *proc);

/**
 * Verify a list of procs on its way into add_procs.
 *
 * Covers this job's own procs, in the mode where the check is per peer.
 * Procs of another job are left alone: neither job can name a rank of
 * the other whose published data it is sure to hold, so those two
 * compare through the exchange their roots make in
 * ompi_dpm_connect_accept().
 *
 * Unlike the two above this reports rather than aborts -- returning
 * OMPI_ERR_UNREACH on a mismatch and OMPI_ERR_NOT_READY if a blob has
 * not landed -- because its callers can hand the failure back to the
 * MPI call that asked.
 */
OMPI_DECLSPEC int mca_pml_base_pml_check_selected(struct ompi_proc_t **procs,
                                                  size_t nprocs);

/* not #if conditional on OPAL_ENABLE_FT_MPI for ABI */
OMPI_DECLSPEC int mca_pml_base_revoke_comm(struct ompi_communicator_t *comm, bool coll_only);

/*
 * Globals
 */
/* The component every rank publishes its selected PML name under. The
 * key it maps to is also the one used to tell whether a peer has
 * committed its connection info at all. */
OMPI_DECLSPEC extern mca_base_component_t mca_pml_base_modex_component;
OMPI_DECLSPEC extern mca_pml_base_component_t mca_pml_base_selected_component;
OMPI_DECLSPEC extern mca_pml_base_module_t mca_pml;
OMPI_DECLSPEC extern opal_pointer_array_t mca_pml_base_pml;
OMPI_DECLSPEC extern bool ompi_pml_base_check_pml;

END_C_DECLS

#endif /* MCA_PML_BASE_H */
