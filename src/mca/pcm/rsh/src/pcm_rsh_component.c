/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "include/types.h"
#include "util/malloc.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_rsh_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "rsh", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_rsh_component_open,  /* component open */
    mca_pcm_rsh_component_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_rsh_init,    /* component init */
  mca_pcm_rsh_finalize
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_rsh_1_0_0 = {
    mca_pcm_rsh_get_unique_name,
    mca_pcm_rsh_allocate_resources,
    mca_pcm_rsh_register_monitor,
    mca_pcm_rsh_can_spawn,
    mca_pcm_rsh_spawn_procs,
    mca_pcm_rsh_get_peers,
    mca_pcm_rsh_get_self,
    mca_pcm_rsh_kill_proc,
    mca_pcm_rsh_kill_job,
    mca_pcm_rsh_deallocate_resources
};


/*
 * Module variables handles
 */
static int mca_pcm_rsh_param_username;
static int mca_pcm_rsh_param_no_n;
static int mca_pcm_rsh_param_no_profile;
static int mca_pcm_rsh_param_fast;
static int mca_pcm_rsh_param_ignore_stderr;
static int mca_pcm_rsh_param_priority;
static int mca_pcm_rsh_param_agent;
static int mca_pcm_rsh_param_degree;
static int mca_pcm_rsh_param_is_client;

/*
 * Module variables
 */
char *mca_pcm_rsh_username;
int mca_pcm_rsh_no_n;
int mca_pcm_rsh_no_profile;
int mca_pcm_rsh_fast;
int mca_pcm_rsh_ignore_stderr;
int mca_pcm_rsh_priority;
char *mca_pcm_rsh_agent;
int mca_pcm_rsh_degree;
int mca_pcm_rsh_is_client;

int
mca_pcm_rsh_component_open(void)
{
  mca_pcm_rsh_param_username =
    mca_base_param_register_string("pcm", "rsh", "username", NULL, NULL);

  mca_pcm_rsh_param_agent = 
      mca_base_param_register_string("pcm", "rsh", "agent", NULL, 
				      "ssh");
  mca_pcm_rsh_param_no_n =
    mca_base_param_register_int("pcm", "rsh", "no_n", NULL, 0);
  mca_pcm_rsh_param_no_profile =
    mca_base_param_register_int("pcm", "rsh", "no_profile", NULL, 0);
  mca_pcm_rsh_param_fast =
    mca_base_param_register_int("pcm", "rsh", "fast", NULL, 0);
  mca_pcm_rsh_param_ignore_stderr =
    mca_base_param_register_int("pcm", "rsh", "ignore_stderr", NULL, 0);

  mca_pcm_rsh_param_priority =
    mca_base_param_register_int("pcm", "rsh", "priority", NULL, 1);
  mca_pcm_rsh_param_degree =
      mca_base_param_register_int("pcm", "rsh", "degree", NULL, 2);

  mca_pcm_rsh_param_is_client = 
      mca_base_param_register_int("pcm", "rsh", "is_client", NULL, 0);

  return OMPI_SUCCESS;
}


int
mca_pcm_rsh_component_close(void)
{
  return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_rsh_init(int *priority, 
		  bool *allow_multi_user_threads, 
                  bool *have_hidden_threads)
{
    mca_base_param_lookup_int(mca_pcm_rsh_param_priority, priority);
 
    mca_base_param_lookup_int(mca_pcm_rsh_param_no_n, 
			      &mca_pcm_rsh_no_n);
    mca_base_param_lookup_string(mca_pcm_rsh_param_username, 
                                 &mca_pcm_rsh_username);
    mca_base_param_lookup_int(mca_pcm_rsh_param_no_profile, 
			      &mca_pcm_rsh_no_profile);
    mca_base_param_lookup_int(mca_pcm_rsh_param_fast, 
			      &mca_pcm_rsh_fast);
    mca_base_param_lookup_int(mca_pcm_rsh_param_ignore_stderr, 
			      &mca_pcm_rsh_ignore_stderr);
    mca_base_param_lookup_int(mca_pcm_rsh_param_ignore_stderr, 
			      &mca_pcm_rsh_ignore_stderr);
    mca_base_param_lookup_int(mca_pcm_rsh_param_degree,
			      &mca_pcm_rsh_degree);
    mca_base_param_lookup_string(mca_pcm_rsh_param_agent,
                                 &mca_pcm_rsh_agent);
    mca_base_param_lookup_int(mca_pcm_rsh_param_is_client,
                              &mca_pcm_rsh_is_client);

    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    return &mca_pcm_rsh_1_0_0;
}


int
mca_pcm_rsh_finalize(void)
{
  return OMPI_SUCCESS;
}

