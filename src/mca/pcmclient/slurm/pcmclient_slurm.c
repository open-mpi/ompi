/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/slurm/pcmclient_slurm.h"
#include "include/types.h"
#include "include/constants.h"

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>

extern int mca_pcmclient_slurm_num_procs;
extern int mca_pcmclient_slurm_procid;
extern ompi_process_name_t *mca_pcmclient_slurm_procs;


int 
mca_pcmclient_slurm_get_peers(ompi_process_name_t **procs, 
                                  size_t *num_procs)
{
    if (NULL == mca_pcmclient_slurm_procs) return OMPI_ERROR;

    *num_procs = mca_pcmclient_slurm_num_procs;
    *procs = mca_pcmclient_slurm_procs;
    return OMPI_SUCCESS;
}


ompi_process_name_t*
mca_pcmclient_slurm_get_self(void)
{
    if (NULL == mca_pcmclient_slurm_procs) return NULL;

    return &mca_pcmclient_slurm_procs[mca_pcmclient_slurm_procid];
}
