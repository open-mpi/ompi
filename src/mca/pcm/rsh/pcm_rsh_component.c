/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "pcm_rsh.h"
#include "include/constants.h"
#include "include/types.h"
#include "util/malloc.h"
#include "util/output.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"
#include "runtime/ompi_rte_wait.h"

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
  NULL                 /* unique name */
};

/*
 * component variables handles
 */
static int mca_pcm_rsh_param_no_profile;
static int mca_pcm_rsh_param_fast;
static int mca_pcm_rsh_param_ignore_stderr;
static int mca_pcm_rsh_param_priority;
static int mca_pcm_rsh_param_agent;
static int mca_pcm_rsh_param_delay_time;


int
mca_pcm_rsh_component_open(void)
{
  mca_pcm_rsh_param_agent = 
      mca_base_param_register_string("pcm", "rsh", "agent", NULL, 
				      "ssh");

  mca_pcm_rsh_param_no_profile =
    mca_base_param_register_int("pcm", "rsh", "no_profile", NULL, 1);
  mca_pcm_rsh_param_fast =
    mca_base_param_register_int("pcm", "rsh", "fast", NULL, 1);
  mca_pcm_rsh_param_ignore_stderr =
    mca_base_param_register_int("pcm", "rsh", "ignore_stderr", NULL, 0);

  mca_pcm_rsh_param_priority =
    mca_base_param_register_int("pcm", "rsh", "priority", NULL, 1);

  mca_pcm_rsh_param_delay_time =
    mca_base_param_register_int("pcm", "rsh", "delay", NULL, 0);

  return OMPI_SUCCESS;
}


int
mca_pcm_rsh_component_close(void)
{
    return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_rsh_init(int *priority, 
                 bool have_threads,
                 int constraints)
{
    int ret;
    mca_pcm_rsh_module_t *me;

    /* get our priority */
    mca_base_param_lookup_int(mca_pcm_rsh_param_priority, priority);

    me = (mca_pcm_rsh_module_t*) malloc(sizeof(mca_pcm_rsh_module_t));
    if (NULL == me) return NULL;

    /* fill in params */
    mca_base_param_lookup_int(mca_pcm_rsh_param_no_profile, 
			      &(me->no_profile));
    mca_base_param_lookup_int(mca_pcm_rsh_param_fast, 
                              &(me->fast_boot));
    mca_base_param_lookup_int(mca_pcm_rsh_param_ignore_stderr, 
                              &(me->ignore_stderr));
    mca_base_param_lookup_string(mca_pcm_rsh_param_agent,
                                 &(me->rsh_agent));
    mca_base_param_lookup_int(mca_pcm_rsh_param_delay_time,
                              &(me->delay_time));

    ret = mca_llm_base_select("rsh", &(me->llm), have_threads);

    if (OMPI_SUCCESS != ret) {
        /* well, that can't be good.  guess we can't run */
        ompi_output_verbose(5, mca_pcm_base_output, "init: no llm found");
        free(me);
        return NULL;
    }

    me->constraints = constraints;

    /*
     * fill in the function pointers
     */
    me->super.pcm_allocate_resources = mca_pcm_rsh_allocate_resources;
    me->super.pcm_spawn_procs = mca_pcm_rsh_spawn_procs;
    me->super.pcm_kill_proc = mca_pcm_rsh_kill_proc;
    me->super.pcm_kill_job = mca_pcm_rsh_kill_job;
    me->super.pcm_deallocate_resources = mca_pcm_rsh_deallocate_resources;
    me->super.pcm_finalize = mca_pcm_rsh_finalize;

    me->jobs = mca_pcm_base_job_list_init();

    return (mca_pcm_base_module_t*) me;
}


int
mca_pcm_rsh_finalize(struct mca_pcm_base_module_1_0_0_t* me_super)
{
    mca_pcm_rsh_module_t *me = (mca_pcm_rsh_module_t*) me_super;
    pid_t *pids;
    size_t i, len;
    int status;

    if (NULL == me) return OMPI_ERR_BAD_PARAM;

    me->llm->llm_finalize(me->llm);

    /* remove all the job entries and keep them from having callbacks
       triggered (calling back into us once we are unmapped is
       *bad*) */
    ompi_rte_wait_cb_disable();
    mca_pcm_base_job_list_get_all_starters(me->jobs, &pids, &len, true);
    for (i = 0 ; i < len ; ++i) {
        ompi_rte_wait_cb_cancel(pids[i]);
    }
    ompi_rte_wait_cb_enable();

    for (i = 0 ; i < len ; ++i) {
        ompi_rte_waitpid(pids[i], &status, 0);
    }

    mca_pcm_base_job_list_fini(me->jobs);

    if (NULL != me->rsh_agent) free(me->rsh_agent);
    free(me);

    return OMPI_SUCCESS;
}
