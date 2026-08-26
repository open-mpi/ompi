/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BML_BASE_H
#define MCA_BML_BASE_H

#include "ompi_config.h"

#include <assert.h>

#include "ompi/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "ompi/mca/bml/bml.h"
#include "ompi/proc/proc.h"


/*
 * Global functions for the BML
 */

BEGIN_C_DECLS

struct mca_bml_base_selected_module_t {
    opal_list_item_t super;
    mca_bml_base_component_t *bml_component;
    mca_bml_base_module_t *bml_module;
};
typedef struct mca_bml_base_selected_module_t mca_bml_base_selected_module_t;

OMPI_DECLSPEC OBJ_CLASS_DECLARATION(mca_bml_base_selected_module_t);

/*
 * Global functions for MCA: overall BTL open and close
 */

OMPI_DECLSPEC  int mca_bml_base_init(bool enable_progress_threads,
                                     bool enable_mpi_threads);
OMPI_DECLSPEC  bool mca_bml_base_inited(void);

/*
 * Globals
 */
OMPI_DECLSPEC extern mca_bml_base_component_t mca_bml_component;
OMPI_DECLSPEC extern mca_bml_base_module_t mca_bml;
OMPI_DECLSPEC extern mca_base_framework_t ompi_bml_base_framework;
OMPI_DECLSPEC extern opal_mutex_t mca_bml_lock;
OMPI_DECLSPEC extern bool mca_bml_component_init_called;

static inline struct mca_bml_base_endpoint_t *
mca_bml_base_endpoint_peek (struct ompi_proc_t *proc)
{
    return (struct mca_bml_base_endpoint_t *)
        proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_BML];
}

/**
 * Slow path of mca_bml_base_get_endpoint: call that one instead.
 * status is required there, so it is required here too.
 */
OMPI_DECLSPEC struct mca_bml_base_endpoint_t *
mca_bml_base_endpoint_create (struct ompi_proc_t *proc, int *status);

/**
 * This peer's endpoint, constructed on first use. Completes whatever
 * proc data add_proc needs, then lets the BML select the BTLs.
 *
 * A NULL return is not necessarily fatal, so the reason is reported
 * separately in status, which is always written and must not be NULL:
 *
 *   OMPI_ERR_NOT_READY  the peer's connection info is not local yet.
 *                       The caller may stage the operation and retry;
 *                       ob1 does exactly that.
 *   OMPI_ERR_UNREACH    the info is available and no BTL claimed the
 *                       peer.
 *   anything else        a hard failure from add_proc.
 *
 * Callers that cannot defer work must check the return value: with a
 * lazy MPI_Init any peer can be NOT_READY on first use. A caller with
 * nothing to do about the reason still passes an int and ignores it.
 */
static inline struct mca_bml_base_endpoint_t *
mca_bml_base_get_endpoint (struct ompi_proc_t *proc, int *status) {
    struct mca_bml_base_endpoint_t *endpoint = mca_bml_base_endpoint_peek (proc);

    assert (NULL != status);

    /* One load: a second one could be answered by a del_procs that ran
     * in between, handing back NULL with a SUCCESS status. */
    if (OPAL_LIKELY(NULL != endpoint)) {
        *status = OMPI_SUCCESS;
        return endpoint;
    }

    return mca_bml_base_endpoint_create (proc, status);
}


END_C_DECLS
#endif /* MCA_BML_BASE_H */
