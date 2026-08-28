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
 * SPDX-License-Identifier: BSD-3-Clause-Open-MPI
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

/* The PML name this process published in the modex; empty before
 * selection. Not the selected component's own name, which vprotocol
 * renames. */
OMPI_DECLSPEC const char *mca_pml_base_pml_selected_name(void);

/**
 * Verify, once per process, that this job agrees on the PML. Only where
 * rank 0 alone published: one comparison against rank 0 suffices, since
 * every rank makes it. Never waits -- rank 0's data is usually not local
 * this early, and the first wire-up makes the comparison instead. A
 * mismatch aborts, there being no caller to report to.
 */
OMPI_DECLSPEC int mca_pml_base_pml_check_start(void);

/**
 * Verify one peer as it is being wired. A no-op unless every rank
 * published its choice. Folded into the lazy wire-up because the first
 * Get for a peer caches its data anyway. A mismatch aborts: the caller
 * is a first send or a fragment arrival, with no way to report it.
 *
 * @retval OMPI_SUCCESS        the peer agrees, or nothing to check.
 * @retval OMPI_ERR_NOT_READY  the peer has not published yet; retry.
 */
OMPI_DECLSPEC int mca_pml_base_pml_check_peer(struct ompi_proc_t *proc);

/**
 * Verify a list of procs on its way into add_procs. This job's own procs
 * only, and only in the per-peer mode; procs of another job are left to
 * the exchange their roots make in ompi_dpm_connect_accept(). Reports
 * rather than aborts -- OMPI_ERR_UNREACH on a mismatch,
 * OMPI_ERR_NOT_READY if a blob has not landed -- since its callers can
 * fail the MPI call that asked.
 */
OMPI_DECLSPEC int mca_pml_base_pml_check_selected(struct ompi_proc_t **procs,
                                                  size_t nprocs);

/* not #if conditional on OPAL_ENABLE_FT_MPI for ABI */
OMPI_DECLSPEC int mca_pml_base_revoke_comm(struct ompi_communicator_t *comm, bool coll_only);

/*
 * Globals
 */
/* Component the selected PML name is published under; its key also
 * tells whether a peer has committed its connection info at all. */
OMPI_DECLSPEC extern mca_base_component_t mca_pml_base_modex_component;
OMPI_DECLSPEC extern mca_pml_base_component_t mca_pml_base_selected_component;
OMPI_DECLSPEC extern mca_pml_base_module_t mca_pml;
OMPI_DECLSPEC extern opal_pointer_array_t mca_pml_base_pml;
OMPI_DECLSPEC extern bool ompi_pml_base_check_pml;

END_C_DECLS

#endif /* MCA_PML_BASE_H */
