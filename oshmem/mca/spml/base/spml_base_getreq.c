/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "oshmem_config.h"
#include "oshmem/types.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/spml_base_getreq.h"

static void mca_spml_base_get_request_construct(mca_spml_base_get_request_t*);
static void mca_spml_base_get_request_destruct(mca_spml_base_get_request_t*);

OBJ_CLASS_INSTANCE( mca_spml_base_get_request_t,
                   mca_spml_base_request_t,
                   mca_spml_base_get_request_construct,
                   mca_spml_base_get_request_destruct);

static void mca_spml_base_get_request_construct(mca_spml_base_get_request_t* request)
{
    /* no need to reinit for every get -- never changes */
    request->req_base.req_type = MCA_SPML_REQUEST_GET;
    OBJ_CONSTRUCT(&request->req_base.req_convertor, opal_convertor_t);
}

static void mca_spml_base_get_request_destruct(mca_spml_base_get_request_t* request)
{
    /* For each request the convertor get cleaned after each message
     * (in the base _FINI macro). Therefore, as the convertor is a static object
     * we don't have to call OBJ_DESTRUCT here.
     */
}

