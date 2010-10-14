/*
 * Copyright (c) 2010      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_PORTALS4_ENDPOINT_H
#define OMPI_MTL_PORTALS4_ENDPOINT_H

#include <portals4.h>

struct mca_mtl_base_endpoint_t {
    ptl_process_t ptl_proc;
};
typedef struct mca_mtl_base_endpoint_t mca_mtl_base_endpoint_t;

#endif
