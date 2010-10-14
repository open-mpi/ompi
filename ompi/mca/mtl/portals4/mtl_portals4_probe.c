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
ompi_mtl_portals4_iprobe(struct mca_mtl_base_module_t* mtl,
                         struct ompi_communicator_t *comm,
                         int src,
                         int tag,
                         int *flag,
                         struct ompi_status_public_t *status)
{
    return OMPI_SUCCESS;
}
