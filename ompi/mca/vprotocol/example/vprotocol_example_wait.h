/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef __VPROTOCOL_EXAMPLE_WAIT_H__
#define __VPROTOCOL_EXAMPLE_WAIT_H__

#include "ompi_config.h"
#include "vprotocol_example.h"

OMPI_DECLSPEC int mca_vprotocol_example_wait_any(size_t count, ompi_request_t ** requests, int *index, ompi_status_public_t * status);
OMPI_DECLSPEC int mca_vprotocol_example_wait_some(size_t count, ompi_request_t ** requests, int *indexes, ompi_status_public_t * statuses);
OMPI_DECLSPEC int mca_vprotocol_example_wait_all(size_t count, ompi_request_t ** requests, ompi_status_public_t * statuses);

#endif /* __VPROTOCOL_EXAMPLE_WAIT_H__ */
