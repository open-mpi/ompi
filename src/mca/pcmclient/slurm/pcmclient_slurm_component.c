/*
 * $HEADER$
 */


#include "ompi_config.h"
#include "pcmclient-slurm-version.h"

#include "include/constants.h"
#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/slurm/pcmclient_slurm.h"
#include "util/proc_info.h"
#include "mca/base/mca_base_param.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcmclient_base_component_1_0_0_t mca_pcmclient_slurm_component = {
  {
    MCA_PCMCLIENT_BASE_VERSION_1_0_0,

    "slurm", /* MCA component name */
    MCA_pcmclient_slurm_MAJOR_VERSION,  /* MCA component major version */
    MCA_pcmclient_slurm_MINOR_VERSION,  /* MCA component minor version */
    MCA_pcmclient_slurm_RELEASE_VERSION,  /* MCA component release version */
    mca_pcmclient_slurm_open,  /* component open */
    mca_pcmclient_slurm_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcmclient_slurm_init,    /* component init */
  mca_pcmclient_slurm_finalize
};


struct mca_pcmclient_base_module_1_0_0_t mca_pcmclient_slurm_1_0_0 = {
    mca_pcmclient_slurm_get_self,
    mca_pcmclient_slurm_get_peers,
};

/*
 * component-global variables
 */
int mca_pcmclient_slurm_num_procs;
int mca_pcmclient_slurm_procid;

ompi_process_name_t *mca_pcmclient_slurm_procs = NULL;

/*
 * local variables
 */

static int slurm_jobid_handle;
static int slurm_start_vpid_handle;
static int slurm_cellid_handle;

int mca_pcmclient_slurm_cellid;
int mca_pcmclient_slurm_jobid;

int
mca_pcmclient_slurm_open(void)
{
    slurm_jobid_handle = 
        mca_base_param_register_int("pcmclient", "slurm", "jobid", NULL, -1);
    slurm_start_vpid_handle = 
        mca_base_param_register_int("pcmclient", "slurm", "start_vpid", NULL, 0);
    slurm_cellid_handle =
        mca_base_param_register_int("pcmclient", "slurm", "cellid", NULL, 0);

    return OMPI_SUCCESS;
}


int
mca_pcmclient_slurm_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcmclient_base_module_1_0_0_t *
mca_pcmclient_slurm_init(int *priority, 
                             bool *allow_multiple_user_threads, 
                             bool *have_hidden_threads)
{
    int i;
    char *tmp;
    int start_vpid;

    *priority = 5; /* make sure we are above env / singleton */
    *allow_multiple_user_threads = true;
    *have_hidden_threads = false;

    mca_base_param_lookup_int(slurm_jobid_handle, &mca_pcmclient_slurm_jobid);
    mca_base_param_lookup_int(slurm_cellid_handle, &mca_pcmclient_slurm_cellid);
    mca_base_param_lookup_int(slurm_start_vpid_handle, &start_vpid);

    if (mca_pcmclient_slurm_jobid < 0) {
        tmp = getenv("SLURM_JOBID");
        if (NULL == tmp) return NULL;
        mca_pcmclient_slurm_jobid = atoi(tmp);
    }

    tmp = getenv("SLURM_PROCID");
    if (NULL == tmp) return NULL;
    mca_pcmclient_slurm_procid = atoi(tmp);

    tmp = getenv("SLURM_NPROCS");
    if (NULL == tmp) return NULL;
    mca_pcmclient_slurm_num_procs = atoi(tmp);

    mca_pcmclient_slurm_procs = 
        (ompi_process_name_t*) malloc(sizeof(ompi_process_name_t) * 
                                      mca_pcmclient_slurm_num_procs);
    if (NULL == mca_pcmclient_slurm_procs) return NULL;

    for ( i = 0 ; i < mca_pcmclient_slurm_num_procs ; ++i) {
        mca_pcmclient_slurm_procs[i].cellid = mca_pcmclient_slurm_cellid;
        mca_pcmclient_slurm_procs[i].jobid = mca_pcmclient_slurm_jobid;
        mca_pcmclient_slurm_procs[i].vpid = start_vpid + i;
    }
    
    return &mca_pcmclient_slurm_1_0_0;
}


int
mca_pcmclient_slurm_finalize(void)
{
    if (NULL != mca_pcmclient_slurm_procs) {
        free(mca_pcmclient_slurm_procs);
        mca_pcmclient_slurm_procs = NULL;
        mca_pcmclient_slurm_num_procs = 0;
    }

    return OMPI_SUCCESS;
}


