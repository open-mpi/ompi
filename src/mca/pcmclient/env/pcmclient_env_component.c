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
#include "pcmclient-env-version.h"

#include "include/constants.h"
#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/env/pcmclient_env.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*
 * Struct of function pointers and all that to let us be initialized
 */
OMPI_COMP_EXPORT mca_pcmclient_base_component_1_0_0_t mca_pcmclient_env_component = {
  {
    MCA_PCMCLIENT_BASE_VERSION_1_0_0,

    "env", /* MCA component name */
    MCA_pcmclient_env_MAJOR_VERSION,  /* MCA component major version */
    MCA_pcmclient_env_MINOR_VERSION,  /* MCA component minor version */
    MCA_pcmclient_env_RELEASE_VERSION,  /* MCA component release version */
    mca_pcmclient_env_open,  /* component open */
    mca_pcmclient_env_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcmclient_env_init,    /* component init */
  mca_pcmclient_env_finalize
};


struct mca_pcmclient_base_module_1_0_0_t mca_pcmclient_env_1_0_0 = {
    mca_pcmclient_env_init_cleanup,
    mca_pcmclient_env_get_self,
    mca_pcmclient_env_get_peers,
};


static int param_cellid;
static int param_jobid;
static int param_vpid_start;
static int param_num_procs;
static int param_procid;

int mca_pcmclient_env_cellid;
int mca_pcmclient_env_jobid;
int mca_pcmclient_env_num_procs;
int mca_pcmclient_env_procid;
ompi_process_name_t *mca_pcmclient_env_procs = NULL;


int
mca_pcmclient_env_open(void)
{
    param_cellid = mca_base_param_register_int("pcmclient", "env", "cellid",
                                               NULL, -1);
    mca_base_param_set_internal(param_cellid, true);
    param_jobid = mca_base_param_register_int("pcmclient", "env", "jobid",
                                               NULL, -1);
    param_procid = mca_base_param_register_int("pcmclient", "env", "procid",
                                               NULL, -1);
    param_vpid_start = mca_base_param_register_int("pcmclient", "env", 
                                                   "vpid_start", NULL, 0);
    param_num_procs = mca_base_param_register_int("pcmclient", "env", 
                                                  "num_procs", NULL, -1);

    return OMPI_SUCCESS;
}


int
mca_pcmclient_env_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcmclient_base_module_1_0_0_t *
mca_pcmclient_env_init(int *priority, 
                             bool *allow_multiple_user_threads, 
                             bool *have_hidden_threads)
{
    int vpid_start;
    int i;

    *priority = 2; /* make sure we are above singleton */
    *allow_multiple_user_threads = true;
    *have_hidden_threads = false;

    mca_base_param_lookup_int(param_cellid, &mca_pcmclient_env_cellid);
    if (mca_pcmclient_env_cellid < 0) return NULL;

    mca_base_param_lookup_int(param_jobid, &mca_pcmclient_env_jobid);
    if (mca_pcmclient_env_jobid < 0) return NULL;

    mca_base_param_lookup_int(param_procid, &mca_pcmclient_env_procid);
    if (mca_pcmclient_env_procid < 0) return NULL;

    mca_base_param_lookup_int(param_vpid_start, &vpid_start);
    if (vpid_start < 0) return NULL;

    mca_base_param_lookup_int(param_num_procs, &mca_pcmclient_env_num_procs);
    if (mca_pcmclient_env_num_procs < 0) return NULL;

    mca_pcmclient_env_procs = 
        (ompi_process_name_t*) malloc(sizeof(ompi_process_name_t) * 
                                      mca_pcmclient_env_num_procs);
    if (NULL == mca_pcmclient_env_procs) return NULL;

    for ( i = 0 ; i < mca_pcmclient_env_num_procs ; ++i) {
        mca_pcmclient_env_procs[i].cellid = mca_pcmclient_env_cellid;
        mca_pcmclient_env_procs[i].jobid = mca_pcmclient_env_jobid;
        mca_pcmclient_env_procs[i].vpid = i + vpid_start;
    }
    
    return &mca_pcmclient_env_1_0_0;
}


int
mca_pcmclient_env_finalize(void)
{
    if (NULL != mca_pcmclient_env_procs) {
        free(mca_pcmclient_env_procs);
        mca_pcmclient_env_procs = NULL;
        mca_pcmclient_env_num_procs = 0;
    }

    return OMPI_SUCCESS;
}


