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
#include "pcmclient-seed-version.h"

#include "include/constants.h"
#include "include/types.h"
#include "mca/mca.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/seed/pcmclient_seed.h"
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
OMPI_COMP_EXPORT mca_pcmclient_base_component_1_0_0_t mca_pcmclient_seed_component = {
  {
    MCA_PCMCLIENT_BASE_VERSION_1_0_0,

    "seed", /* MCA component name */
    MCA_pcmclient_seed_MAJOR_VERSION,  /* MCA component major version */
    MCA_pcmclient_seed_MINOR_VERSION,  /* MCA component minor version */
    MCA_pcmclient_seed_RELEASE_VERSION,  /* MCA component release version */
    mca_pcmclient_seed_open,  /* component open */
    mca_pcmclient_seed_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcmclient_seed_init,    /* component init */
  mca_pcmclient_seed_finalize
};


struct mca_pcmclient_base_module_1_0_0_t mca_pcmclient_seed_1_0_0 = {
    mca_pcmclient_seed_init_cleanup,
    mca_pcmclient_seed_get_self,
    mca_pcmclient_seed_get_peers,
};


int
mca_pcmclient_seed_open(void)
{
    return OMPI_SUCCESS;
}


int
mca_pcmclient_seed_close(void)
{
  return OMPI_SUCCESS;
}


struct mca_pcmclient_base_module_1_0_0_t *
mca_pcmclient_seed_init(int *priority, 
                             bool *allow_multiple_user_threads, 
                             bool *have_hidden_threads)
{
    if (!ompi_process_info.seed) {
        *priority = 0;
        return NULL;
    }

    *priority = 10;
    *allow_multiple_user_threads = true;
    *have_hidden_threads = false;
    
    return &mca_pcmclient_seed_1_0_0;
}


int
mca_pcmclient_seed_finalize(void)
{
  return OMPI_SUCCESS;
}


