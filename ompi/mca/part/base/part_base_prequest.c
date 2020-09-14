/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2015 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/part_base_prequest.h"

/**
 * If you wonder why these 2 freelists are declared here read the comment
 * in the part_base_request.h file.
 */
opal_free_list_t mca_part_base_psend_requests = {{{0}}};
opal_free_list_t mca_part_base_precv_requests = {{{0}}};

static void mca_part_base_prequest_construct(mca_part_base_prequest_t* req)
{
    req->req_ompi.req_type = OMPI_REQUEST_PART;
}

OBJ_CLASS_INSTANCE(
    mca_part_base_prequest_t,
    ompi_request_t,
    mca_part_base_prequest_construct,
    NULL
);

