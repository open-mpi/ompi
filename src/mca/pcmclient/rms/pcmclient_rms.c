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
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/rms/pcmclient_rms.h"
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

extern int mca_pcmclient_rms_num_procs;
extern int mca_pcmclient_rms_procid;
extern ompi_process_name_t *mca_pcmclient_rms_procs;

int
mca_pcmclient_rms_init_cleanup(void)
{
    return OMPI_SUCCESS;
}


int 
mca_pcmclient_rms_get_peers(ompi_process_name_t **procs, 
                                  size_t *num_procs)
{
    if (NULL == mca_pcmclient_rms_procs) return OMPI_ERROR;

    *num_procs = mca_pcmclient_rms_num_procs;
    *procs = mca_pcmclient_rms_procs;
    return OMPI_SUCCESS;
}


ompi_process_name_t*
mca_pcmclient_rms_get_self(void)
{
    if (NULL == mca_pcmclient_rms_procs) return NULL;

    return &mca_pcmclient_rms_procs[mca_pcmclient_rms_procid];
}
