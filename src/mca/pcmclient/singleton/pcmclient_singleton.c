/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/singleton/pcmclient_singleton.h"
#include "include/types.h"
#include "include/constants.h"

#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>

extern ompi_process_name_t *mca_pcmclient_singleton_procs;

static
int
init_proclist(void)
{
    if (NULL != mca_pcmclient_singleton_procs) return OMPI_SUCCESS;

    /* need to create us an array of these things... */
    mca_pcmclient_singleton_procs = 
        (ompi_process_name_t*) malloc(sizeof(ompi_process_name_t));
    if (NULL == mca_pcmclient_singleton_procs) return OMPI_ERROR;

    /* BWB - when name server is running, get value from it... */
    mca_pcmclient_singleton_procs[0].cellid = 0;
    mca_pcmclient_singleton_procs[0].jobid = 1;
    mca_pcmclient_singleton_procs[0].vpid = 0;

    return OMPI_SUCCESS;
}


int 
mca_pcmclient_singleton_get_peers(ompi_process_name_t **procs, 
                                  size_t *num_procs)
{
    int ret;

    if (NULL == mca_pcmclient_singleton_procs) {
        ret = init_proclist();
        if (OMPI_SUCCESS != ret) return ret;
    }

    *num_procs = 1;
    *procs = mca_pcmclient_singleton_procs;

    return OMPI_SUCCESS;
}


ompi_process_name_t*
mca_pcmclient_singleton_get_self(void)
{
    int ret;

    if (NULL == mca_pcmclient_singleton_procs) {
        ret = init_proclist();
        if (OMPI_SUCCESS != ret) return ret;
    }

    return &mca_pcmclient_singleton_procs[0];
}
