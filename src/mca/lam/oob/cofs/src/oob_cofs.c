/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "mca/lam/oob/oob.h"
#include "mca/lam/oob/cofs/src/oob_cofs.h"

int
mca_oob_cofs_send(char* parallel_job_id, int vpid, int tag, 
                  void* data, size_t data_len)
{
  return 0;
}


int
mca_oob_cofs_recv(char* parallel_job_id, int* tag, int* vpid, 
                  void** data, size_t* data_len)
{
  return 0;
}


int
mca_oob_cofs_recv_nb(char* parallel_job_id, int* tag, int* vpid, 
                     void** data, size_t* data_len)
{
  return 0;
}


int
mca_oob_cofs_recv_cb(char* parallel_job_id, int tag, 
                     mca_oob_recv_cb_t callback)
{
  return 0;
}
