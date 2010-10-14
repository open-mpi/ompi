/*
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mtl_portals4.h"

int
ompi_mtl_portals4_irecv(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm,
                        int src,
                        int tag,
                        struct opal_convertor_t *convertor,
                        mca_mtl_request_t *mtl_request)
{
    return OMPI_SUCCESS;
}

