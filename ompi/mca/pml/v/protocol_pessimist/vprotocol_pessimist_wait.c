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
#include "vprotocol_pessimist.h"
#include "vprotocol_pessimist_wait.h"


int vprotocol_pessimist_request_null_free(ompi_request_t **req)
{
  return OMPI_SUCCESS;
}



int mca_vprotocol_pessimist_wait_any(size_t count, ompi_request_t ** requests, int *index, ompi_status_public_t * status)
{
  int ret;
  size_t i;
  int c;


  VPROTOCOL_PESSIMIST_DELIVERY_REPLAY(count, requests, *index, c, status);
    
# define pml_req ((mca_pml_base_request_t *) requests[i])
  /* Avoid the request to be disposed by waitall */
  for(i = 0; i < count; i++)
  {
    if(requests[i] == MPI_REQUEST_NULL) continue;
    requests[i]->req_free = vprotocol_pessimist_request_null_free;  
  }
  
  ret = ompi_request_wait_any(count, requests, index, status);
  
  /* Parse the result */
  for(i = 0; i < count; i++)
  {
    if(requests[i] == MPI_REQUEST_NULL) continue;
    /* Restore requests and store they've been probed for termination */
    pml_req->req_ompi.req_free = mca_vprotocol_pessimist_request_free;

    if(i == (size_t) *index)
    {
      VPROTOCOL_PESSIMIST_DELIVERY_LOG(pml_req);

      /* only free request without error status */
      if(pml_req->req_ompi.req_status.MPI_ERROR == MPI_SUCCESS)
        ompi_request_free(&(requests[i]));
      else
        ret = pml_req->req_ompi.req_status.MPI_ERROR;
    }
#   undef pml_req
  }
  return ret;
}

int mca_vprotocol_pessimist_wait_some(size_t count, ompi_request_t ** requests, int *indexes, ompi_status_public_t * statuses)
{
  return mca_vprotocol_pessimist_wait_any(count, requests, indexes, statuses);
}
