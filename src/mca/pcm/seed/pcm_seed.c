/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "mca/pcm/seed/pcm_seed.h"
#include "include/types.h"

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>


ompi_process_name_t peers[] = {
    {0, 0, 0} /* 0,0,0 is the seed daemon's name */
};

extern char *mca_pcm_seed_unique_name;


int 
mca_pcm_seed_get_peers(ompi_process_name_t **procs, size_t *num_procs)
{
    *num_procs = 1;
    *procs = peers;
    return OMPI_SUCCESS;
}


ompi_process_name_t* mca_pcm_seed_get_self(void)
{
    return peers;
}


bool
mca_pcm_seed_can_spawn(void)
{
    return false;
}


char*
mca_pcm_seed_get_unique_name(void)
{
    return mca_pcm_seed_unique_name;
}
