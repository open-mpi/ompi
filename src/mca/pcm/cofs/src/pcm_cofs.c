/* -*- C -*-
 *
 * $HEADER$
 * 
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/cofs/src/pcm_cofs.h"
#include "include/types.h"

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>


int 
mca_pcm_cofs_get_peers(ompi_process_name_t **procs, size_t *num_procs)
{
  *num_procs = mca_pcm_cofs_num_procs;
  *procs = mca_pcm_cofs_procs;
  return OMPI_SUCCESS;
}


ompi_process_name_t* mca_pcm_cofs_get_self(void)
{
  return &mca_pcm_cofs_procs[mca_pcm_cofs_procid];
}

