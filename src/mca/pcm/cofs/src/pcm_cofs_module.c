/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "include/types.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/cofs/src/pcm_cofs.h"
#include "mca/base/mca_base_param.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_cofs_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "cofs", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_cofs_open,  /* component open */
    mca_pcm_cofs_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_cofs_init,    /* component init */
  mca_pcm_cofs_finalize
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_cofs_1_0_0 = {
  mca_pcm_cofs_get_peers,
  mca_pcm_cofs_get_self
};


char mca_pcm_cofs_comm_loc[OMPI_PATH_MAX];
char *mca_pcm_cofs_my_handle = NULL;
ompi_process_name_t *mca_pcm_cofs_procs = NULL;
size_t mca_pcm_cofs_num_procs = 0;
size_t mca_pcm_cofs_procid = 0;
static int mca_pcm_cofs_num_procs_param;
static int mca_pcm_cofs_cellid_param;
static int mca_pcm_cofs_jobid_param;
static int mca_pcm_cofs_procid_param;

int
mca_pcm_cofs_open(void)
{
   mca_pcm_cofs_num_procs_param = mca_base_param_register_int("pcm","cofs","num_procs",NULL,-1);
   mca_pcm_cofs_cellid_param = mca_base_param_register_int("pcm","cofs","cellid",NULL,-1);
   mca_pcm_cofs_jobid_param = mca_base_param_register_int("pcm","cofs","jobid",NULL,-1);
   mca_pcm_cofs_procid_param = mca_base_param_register_int("pcm","cofs","procid",NULL,-1);
   return OMPI_SUCCESS;
}


int
mca_pcm_cofs_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcm_base_module_1_0_0_t *
mca_pcm_cofs_init(int *priority, bool *allow_multi_user_threads, 
                  bool *have_hidden_threads)
{
    int i,value;
    mca_ns_base_cellid_t cellid;
    mca_ns_base_jobid_t jobid;

    *priority = 1;
    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    /* lookup parameters for local name */
    mca_base_param_lookup_int(mca_pcm_cofs_num_procs_param, &value);
    if(value <= 0) {
        ompi_output(0, "mca_pcm_cofs_init: missing/invalid value for OMPI_MCA_pcm_cofs_num_procs\n");
        return NULL;
    }
    mca_pcm_cofs_num_procs = value;

    mca_base_param_lookup_int(mca_pcm_cofs_cellid_param, &value);
    if(value < 0) {
        ompi_output(0, "mca_pcm_cofs_init: missing/invalid value for OMPI_MCA_pcm_cofs_cellid\n");
        return NULL;
    }
    cellid = value;
                                                                                                                      
    mca_base_param_lookup_int(mca_pcm_cofs_jobid_param, &value);
    if(value < 0) {
        ompi_output(0, "mca_pcm_cofs_init: missing/invalid value for OMPI_MCA_pcm_cofs_jobid\n");
        return NULL;
    }
    jobid = value;
                                                                                                                      
    mca_base_param_lookup_int(mca_pcm_cofs_procid_param, &value);
    if(value < 0) {
        ompi_output(0, "mca_pcm_cofs_init: missing value for OMPI_MCA_pcm_cofs_procid\n");
        return NULL;
    }
    mca_pcm_cofs_procid = value;

    mca_pcm_cofs_procs = (ompi_process_name_t*)malloc(sizeof(ompi_process_name_t) * mca_pcm_cofs_num_procs);
    if(NULL == mca_pcm_cofs_procs) {
        ompi_output(0, "mca_pcm_cofs_init: missing value for OMPI_MCA_pcm_cofs_num_procs\n");
        return NULL;
    }

    for(i=0; i<mca_pcm_cofs_num_procs; i++) {
        mca_pcm_cofs_procs[i].cellid = cellid;
        mca_pcm_cofs_procs[i].jobid = jobid;
        mca_pcm_cofs_procs[i].vpid = i;
    }
    return &mca_pcm_cofs_1_0_0;
}


int
mca_pcm_cofs_finalize(void)
{
  if (mca_pcm_cofs_procs != NULL) {
    free(mca_pcm_cofs_procs);
    mca_pcm_cofs_procs = NULL;
    mca_pcm_cofs_num_procs = 0;
  }

  return OMPI_SUCCESS;
}

