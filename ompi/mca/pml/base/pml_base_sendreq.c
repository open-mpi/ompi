/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2022      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <string.h>
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/pml/base/pml_base_sendreq.h"
#if MPI_VERSION >= 4
#include "opal/sys/atomic.h"
#endif

static void mca_pml_base_send_request_construct(mca_pml_base_send_request_t* req);
static void mca_pml_base_send_request_destruct(mca_pml_base_send_request_t* req);


OBJ_CLASS_INSTANCE(
    mca_pml_base_send_request_t,
    mca_pml_base_request_t,
    mca_pml_base_send_request_construct,
    mca_pml_base_send_request_destruct
);


static void mca_pml_base_send_request_construct(mca_pml_base_send_request_t* request)
{
    /* no need to reinit for every send -- never changes */
    request->req_base.req_type = MCA_PML_REQUEST_SEND;
    OBJ_CONSTRUCT(&request->req_base.req_convertor, opal_convertor_t);
}

static void mca_pml_base_send_request_destruct(mca_pml_base_send_request_t* req)
{
    /* For each request the convertor get cleaned after each message
     * (in the base _FINI macro). Therefore, as the convertor is a static object
     * we don't have to call OBJ_DESTRUCT here.
     */
}

#if MPI_VERSION >= 4
int mca_pml_cancel_send_callback(struct ompi_request_t *request, int flag)
{
    static opal_atomic_int32_t send_deprecate_count = 0;
    int32_t val;

    val = opal_atomic_add_fetch_32(&send_deprecate_count, 1);
    if ( (OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_ONCE   == ompi_pml_base_warn_dep_cancel_send_level && (1 == val)) ||
         (OMPI_PML_BASE_WARN_DEP_CANCEL_SEND_ALWAYS == ompi_pml_base_warn_dep_cancel_send_level) ) {
        opal_output(0, "WARNING: Calling MPI_Cancel for a request created by a non-blocking send is deprecated.");
    }
    return OMPI_SUCCESS;
}
#endif
