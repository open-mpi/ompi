/*
 * Copyright (c) 2004-2007 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "vprotocol_example.h"
#include "vprotocol_example_wait.h"


int mca_vprotocol_example_wait_any(size_t count, ompi_request_t ** requests, int *index, ompi_status_public_t * status)
{
  return OMPI_ERROR;
}


int mca_vprotocol_example_wait_some(size_t count, ompi_request_t ** requests, int *indexes, ompi_status_public_t * statuses)
{
  return mca_vprotocol_example_wait_any(count, requests, indexes, statuses);
}


int mca_vprotocol_example_wait_all(size_t count, ompi_request_t ** requests, ompi_status_public_t * statuses)
{
  return OMPI_ERROR;
}
