/*
 * $HEADER$
 */


#include "ompi_config.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/seed/pcmclient_seed.h"
#include "include/types.h"
#include "include/constants.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


static ompi_process_name_t peers[] = {
    {0, 0, 0}
};


int 
mca_pcmclient_seed_get_peers(ompi_process_name_t **procs, 
                                  size_t *num_procs)
{
    *num_procs = 1;
    *procs = peers;
    return OMPI_SUCCESS;
}


ompi_process_name_t*
mca_pcmclient_seed_get_self(void)
{
    return peers;
}
