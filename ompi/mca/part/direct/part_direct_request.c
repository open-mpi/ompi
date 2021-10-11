/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2020-2021 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "ompi/mca/part/persist/part_persist.h"
#include "ompi/mca/part/persist/part_persist_request.h"


static void mca_part_persist_request_construct( mca_part_persist_request_t* req) {
    OBJ_CONSTRUCT(&req->req_convertor, opal_convertor_t);
    req->req_ompi.req_type = OMPI_REQUEST_PART;
}

static void mca_part_persist_request_destruct( mca_part_persist_request_t* req) {
    OBJ_DESTRUCT(&req->req_convertor);
}

OBJ_CLASS_INSTANCE(mca_part_persist_request_t,
                   ompi_request_t,
                   mca_part_persist_request_construct,
                   mca_part_persist_request_destruct);
