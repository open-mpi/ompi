/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "lam_config.h"

#include "lam/constants.h"
#include "lam/types.h"
#include "lam/mem/malloc.h"
#include "lam/lfc/lam_list.h"
#include "mca/mca.h"
#include "mca/lam/base/mca_base_param.h"
#include "mca/lam/pcm/pcm.h"
#include "mca/lam/pcm/rsh/src/pcm_rsh.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_module_1_0_0_t mca_pcm_rsh_module = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "rsh", /* MCA module name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_pcm_rsh_open,  /* module open */
    mca_pcm_rsh_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_rsh_init,    /* module init */
  mca_pcm_rsh_finalize
};


struct mca_pcm_1_0_0_t mca_pcm_rsh_1_0_0 = {
  mca_pcm_rsh_query_get_nodes,

  mca_pcm_rsh_handle_new,
  mca_pcm_rsh_handle_get,
  mca_pcm_rsh_handle_free,

  mca_pcm_rsh_job_can_spawn,
  mca_pcm_rsh_job_set_arguments,
  mca_pcm_rsh_job_launch_procs,
  mca_pcm_rsh_job_rendezvous,
  mca_pcm_rsh_job_wait,
  mca_pcm_rsh_job_running,
  mca_pcm_rsh_job_list_running,

  mca_pcm_rsh_proc_startup,
  mca_pcm_rsh_proc_get_peers,
  mca_pcm_rsh_proc_get_me,
  mca_pcm_rsh_proc_get_parent
};

lam_list_t mca_pcm_rsh_connections;

lam_job_handle_t mca_pcm_rsh_my_job_handle = NULL;
int mca_pcm_rsh_my_vpid = -1;

char *mca_pcm_rsh_rsh = NULL;
char *mca_pcm_rsh_hostfile = NULL;

int
mca_pcm_rsh_open(void)
{
  int id;

  OBJ_CONSTRUCT(&mca_pcm_rsh_connections, lam_list_t);

  /* BWB - we should really make sure that we have an RSH in the path 
     somewhere and make that a configure option and all that */
  id = mca_base_param_register_string("pcm", "rsh", "rsh", NULL, "ssh");
  mca_base_param_lookup_string(id, &mca_pcm_rsh_rsh);

  id = mca_base_param_register_int("pcm", "rsh", "vpid", NULL, 0);
  mca_base_param_lookup_int(id, &mca_pcm_rsh_my_vpid);

  id = mca_base_param_register_string("pcm", "rsh", "job_handle", NULL, NULL);
  mca_base_param_lookup_string(id, &mca_pcm_rsh_my_job_handle);

  id = mca_base_param_register_string("pcm", "rsh", "hostfile", NULL, NULL);
  mca_base_param_lookup_string(id, &mca_pcm_rsh_hostfile);

  return LAM_SUCCESS;
}


int
mca_pcm_rsh_close(void)
{
  /* BWB - free list? */

  OBJ_DESTRUCT(&mca_pcm_rsh_connections);

  return LAM_SUCCESS;
}


struct mca_pcm_1_0_0_t*
mca_pcm_rsh_init(int *priority, bool *allow_multi_user_threads, 
                  bool *have_hidden_threads)
{
  *priority = 0;
  /* BWB - reevaluate this setting at some point */
  *allow_multi_user_threads = true;
  *have_hidden_threads = false;

  /* That's about all we wrote thus far */

  return &mca_pcm_rsh_1_0_0;
}


int
mca_pcm_rsh_finalize(void)
{
  return LAM_SUCCESS;
}

