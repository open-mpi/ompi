/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <errno.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/singleton/pcmclient_singleton.h"
#include "mca/oob/base/base.h"
#include "include/types.h"
#include "include/constants.h"
#include "mca/ns/ns.h"
#include "mca/gpr/base/base.h"
#include "runtime/runtime.h"

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

static void
proc_registered_cb(ompi_registry_notify_message_t *match, 
                   void *cbdata)
{
    ompi_rte_job_startup(mca_pcmclient_singleton_procs[0].jobid);
}


int
mca_pcmclient_singleton_init_cleanup(void)
{
    int ret;
    char *segment;
    ompi_registry_notify_id_t rc_tag;

    if (NULL == mca_pcmclient_singleton_procs) {
        ret = init_proclist();
        if (OMPI_SUCCESS != ret) return ret;
    }

    /* register syncro for when we register (the compound command has
       started executing).  At this point, do the broadcast code */

    /* setup segment for this job */
    asprintf(&segment, "%s-%s", OMPI_RTE_JOB_STATUS_SEGMENT,
	     ompi_name_server.convert_jobid_to_string(mca_pcmclient_singleton_procs[0].jobid));

    /* register a synchro on the segment so we get notified for startup */
    rc_tag = ompi_registry.synchro(
	     OMPI_REGISTRY_SYNCHRO_MODE_LEVEL|OMPI_REGISTRY_SYNCHRO_MODE_ONE_SHOT|
	     OMPI_REGISTRY_SYNCHRO_MODE_STARTUP,
	     OMPI_REGISTRY_OR,
	     segment,
	     NULL,
             1,
	     proc_registered_cb, NULL);

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
