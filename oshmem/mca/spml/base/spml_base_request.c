/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/spml_base_request.h"

/**
 * If you wonder why these 2 freelists are declared here read the comment
 * in the spml_base_request.h file.
 */
ompi_free_list_t mca_spml_base_put_requests/* = {{{0}}}*/;
ompi_free_list_t mca_spml_base_get_requests /*= {{{0}}}*/;
ompi_free_list_t mca_spml_base_atomic_requests = { { { 0 } } };

static void mca_spml_base_request_construct(mca_spml_base_request_t* req)
{
    req->req_oshmem.req_type = OSHMEM_REQUEST_SPML;
}

static void mca_spml_base_request_destruct(mca_spml_base_request_t* req)
{
}

OBJ_CLASS_INSTANCE(mca_spml_base_request_t,
                   oshmem_request_t,
                   mca_spml_base_request_construct,
                   mca_spml_base_request_destruct);

