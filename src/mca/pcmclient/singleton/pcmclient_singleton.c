/*
 * $HEADER$
 */


#include "ompi_config.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/singleton/pcmclient_singleton.h"
#include "mca/oob/base/base.h"
#include "include/types.h"
#include "include/constants.h"
#include "mca/ns/ns.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

OMPI_COMP_EXPORT extern ompi_process_name_t *mca_pcmclient_singleton_procs;

static
int
init_proclist(void)
{
    if (NULL != mca_pcmclient_singleton_procs) return OMPI_SUCCESS;

    /* need to create us an array of these things... */
    mca_pcmclient_singleton_procs = 
        (ompi_process_name_t*) malloc(sizeof(ompi_process_name_t));
    if (NULL == mca_pcmclient_singleton_procs) return OMPI_ERROR;

    /* assign illegal name - someone will repair later */
    mca_pcmclient_singleton_procs[0].cellid =  MCA_NS_BASE_CELLID_MAX;
    mca_pcmclient_singleton_procs[0].jobid = MCA_NS_BASE_JOBID_MAX;
    mca_pcmclient_singleton_procs[0].vpid = MCA_NS_BASE_VPID_MAX;
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
        if (OMPI_SUCCESS != ret) {
            errno = ret;
            return NULL;
        }
    }

    return &mca_pcmclient_singleton_procs[0];
}
