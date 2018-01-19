/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_CID_BASE_H
#define MCA_CID_BASE_H

#include "ompi_config.h"

#include "ompi/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"
#include "opal/class/opal_list.h"
#include "opal/class/opal_pointer_array.h"

#include "ompi/mca/cid/cid.h"

/*
 * Global functions for the PML
 */

BEGIN_C_DECLS

/*
 * MCA framework
 */
OMPI_DECLSPEC extern mca_base_framework_t ompi_cid_base_framework;
/* select all components */
OMPI_DECLSPEC    int ompi_cid_base_select(bool ompi_mpi_thread_multiple);

struct ompi_cid_base_cid_context_t;

typedef int (*ompi_cid_base_allreduce_impl_fn_t) (int *inbuf, int *outbuf, int count, struct ompi_op_t *op,
                                                  struct ompi_cid_base_cid_context_t *cid_context,
                                                  ompi_request_t **req);

struct ompi_cid_base_cid_context_t {
    opal_object_t super;

    ompi_communicator_t *newcomm;
    ompi_communicator_t **newcommp;
    ompi_communicator_t *comm;
    ompi_communicator_t *bridgecomm;

    ompi_cid_base_allreduce_impl_fn_t allreduce_fn;

    int nextcid;
    int nextlocal_cid;
    int start;
    int flag, rflag;
    int local_leader;
    int remote_leader;
    int iter;
    /** storage for activate barrier */
    int ok;
    char *port_string;
    bool send_first;
    int pml_tag;
    char *pmix_tag;
};

typedef struct ompi_cid_base_cid_context_t ompi_cid_base_cid_context_t;

OBJ_CLASS_DECLARATION(ompi_cid_base_cid_context_t);

OMPI_DECLSPEC int ompi_cid_base_cid_context_init(ompi_cid_base_cid_context_t *context,
                                                 ompi_communicator_t *newcomm, ompi_communicator_t *comm,
                                                 ompi_communicator_t *bridgecomm, const void *arg0,
                                                 const void *arg1, const char *pmix_tag, bool send_first,
                                                 int mode);

OMPI_DECLSPEC int ompi_cid_base_comm_activate (ompi_communicator_t **newcomm, ompi_communicator_t *comm,
                                               ompi_communicator_t *bridgecomm, const void *arg0,
                                               const void *arg1, bool send_first, int mode);

/**
 * Non-blocking variant of comm_activate.
 *
 * @param[inout] newcomm    New communicator
 * @param[in]    comm       Parent communicator
 * @param[in]    bridgecomm Bridge communicator (used for PMIX and bridge modes)
 * @param[in]    arg0       Mode argument 0
 * @param[in]    arg1       Mode argument 1
 * @param[in]    send_first Send first from this process (PMIX mode only)
 * @param[in]    mode       Collective mode
 * @param[out]   req        New request object to track this operation
 */
OMPI_DECLSPEC int ompi_cid_base_comm_activate_nb (ompi_communicator_t **newcomm, ompi_communicator_t *comm,
                                                  ompi_communicator_t *bridgecomm, const void *arg0,
                                                  const void *arg1, bool send_first, int mode, ompi_request_t **req);

END_C_DECLS

#endif /* MCA_CID_BASE_H */
