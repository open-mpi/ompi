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
#include "pcmclient-bproc-version.h"

#include "include/constants.h"
#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/bproc/pcmclient_bproc.h"
#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
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
mca_pcmclient_base_component_1_0_0_t mca_pcmclient_bproc_component = {
  {
    MCA_PCMCLIENT_BASE_VERSION_1_0_0,

    "bproc", /* MCA component name */
    MCA_pcmclient_bproc_MAJOR_VERSION,  /* MCA component major version */
    MCA_pcmclient_bproc_MINOR_VERSION,  /* MCA component minor version */
    MCA_pcmclient_bproc_RELEASE_VERSION,  /* MCA component release version */
    mca_pcmclient_bproc_open,  /* component open */
    mca_pcmclient_bproc_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcmclient_bproc_init,    /* component init */
  mca_pcmclient_bproc_finalize
};


struct mca_pcmclient_base_module_1_0_0_t mca_pcmclient_bproc_1_0_0 = {
    mca_pcmclient_bproc_init_cleanup,
    mca_pcmclient_bproc_get_self,
    mca_pcmclient_bproc_get_peers,
};


/*
 * component-global variables
 */
int mca_pcmclient_bproc_num_procs;
int mca_pcmclient_bproc_proc_index;
ompi_process_name_t *mca_pcmclient_bproc_procs = NULL;


/*
 * local variables
 */

static int param_base_proc_name;
static int param_num_procs;
static int param_proc_index;
static int param_rank_offset;

int
mca_pcmclient_bproc_open(void)
{
    param_base_proc_name =
        mca_base_param_register_string("pcmclient", "bproc",
                                       "base_name", NULL, NULL);
    param_num_procs =
        mca_base_param_register_int("pcmclient", "bproc", "num_procs",
                                    NULL, -1);
    param_proc_index =
        mca_base_param_register_int("pcmclient", "bproc",
                                    "proc_index", NULL, -1);
    param_rank_offset =
        mca_base_param_register_int("pcmclient", "bproc",
                                    "rank_offset", NULL, -1);

    return OMPI_SUCCESS;
}


int
mca_pcmclient_bproc_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcmclient_base_module_1_0_0_t *
mca_pcmclient_bproc_init(int *priority, 
                             bool *allow_multiple_user_threads, 
                             bool *have_hidden_threads)
{
    int i;
    char *tmp;
    ompi_process_name_t *base_name;

    *priority = 5; /* make sure we are above env / singleton */
    *allow_multiple_user_threads = true;
    *have_hidden_threads = false;
    
    /* get our index in the proc array.  Do this first, since it is a
     * test of whether we are in a BProc environment or not
     *
     * Use BPROC_RANK, the index in the call to bproc_vexecmove()
     * plus the rank_offset field to find our rank in the spawn call,
     * which is our index in the generated proc array (since more
     * than one call to vexecmove may have been required to start
     * this job.
     *
     * Not all versions of bproc support the BPROC_RANK feature of
     * vexecmove.  In these cases, we don't do vexecmoves, but
     * instead set set an MCA param containing the proc index (no
     * offset computation needed)
     */
    tmp = getenv("BPROC_RANK");
    if (NULL != tmp) {
        int rank_offset, bproc_rank;

        bproc_rank = atoi(tmp);
        mca_base_param_lookup_int(param_rank_offset, &rank_offset);
        if (rank_offset < 0) return NULL;

        mca_pcmclient_bproc_proc_index = bproc_rank + rank_offset;
    } else {
        mca_base_param_lookup_int(param_proc_index,
                                  &mca_pcmclient_bproc_proc_index);
    }
    if (mca_pcmclient_bproc_proc_index < 0) return NULL;

    /* get our number of procs */
    mca_base_param_lookup_int(param_num_procs,
                              &mca_pcmclient_bproc_num_procs);
    if (mca_pcmclient_bproc_num_procs < 0) return NULL;

    /* get the base process name string */
    mca_base_param_lookup_string(param_base_proc_name, &tmp);
    if (tmp == NULL) return NULL;

    base_name = 
        ompi_name_server.convert_string_to_process_name(tmp);
    if (base_name == NULL) return NULL;

    /* create the list of names */
    mca_pcmclient_bproc_procs = 
        (ompi_process_name_t*) malloc(sizeof(ompi_process_name_t) * 
                                      mca_pcmclient_bproc_num_procs);
    if (NULL == mca_pcmclient_bproc_procs) return NULL;

    for ( i = 0 ; i < mca_pcmclient_bproc_num_procs ; ++i) {
        /* BWB - this needs to suck less - possibly by changing the
           return type of get_peer_names */
        ompi_process_name_t *tmp_name;

        tmp_name = ompi_name_server.copy_process_name(base_name);
        /* BWB - this will eventually be a function in the NS */
        tmp_name->vpid = ompi_name_server.get_vpid(base_name) + i;
        mca_pcmclient_bproc_procs[i] = *tmp_name;
        ompi_name_server.free_name(tmp_name);
    }
    
    return &mca_pcmclient_bproc_1_0_0;
}


int
mca_pcmclient_bproc_finalize(void)
{
    if (NULL != mca_pcmclient_bproc_procs) {
        free(mca_pcmclient_bproc_procs);
        mca_pcmclient_bproc_procs = NULL;
    }
    mca_pcmclient_bproc_num_procs = 0;
    mca_pcmclient_bproc_proc_index = -1;

    return OMPI_SUCCESS;
}


