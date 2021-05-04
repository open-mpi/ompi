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
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "ompi_config.h"
#include "ompi/mca/part/base/part_base_precvreq.h"
#include "ompi/mca/part/part.h"
#include "ompi/types.h"

static void mca_part_base_precv_request_construct(mca_part_base_precv_request_t *request)
{
    OBJ_CONSTRUCT(&request->req_base.req_convertor, opal_convertor_t);
}

/* For each request the convertor get cleaned after each message
 * (in the base _FINI macro). Therefore, as the convertor is a static object
 * we don't have to call OBJ_DESTRUCT here.
 */

OBJ_CLASS_INSTANCE(mca_part_base_precv_request_t, mca_part_base_prequest_t,
                   mca_part_base_precv_request_construct, 0);
