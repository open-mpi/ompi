/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "pcm_rms.h"
#include "include/constants.h"
#include "include/types.h"
#include "util/malloc.h"
#include "util/output.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/llm/base/base.h"
#include "util/path.h"
#include "runtime/runtime.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <rms/rmsapi.h>

extern char **environ;

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_rms_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "rms", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_rms_component_open,  /* component open */
    mca_pcm_rms_component_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_rms_init,    /* component init */
  NULL                 /* unique name */
};



/*
 * Module variables handles
 */
static int mca_pcm_rms_param_priority;
static int mca_pcm_rms_param_prun_args;
static int mca_pcm_rms_param_partition;
static int mca_pcm_rms_param_prun_args;

/*
 * Component variables.  All of these are shared among the module
 * instances, so they don't need to go in a special structure or
 * anything.
 */
int mca_pcm_rms_output = -1;


int
mca_pcm_rms_component_open(void)
{
  mca_pcm_rms_param_priority =
    mca_base_param_register_int("pcm", "rms", "priority", NULL, 5);

  mca_pcm_rms_param_partition =
    mca_base_param_register_string("pcm", "rms", "patition", NULL, NULL);

  mca_pcm_rms_param_prun_args =
    mca_base_param_register_string("pcm", "rms", "prun_args", NULL, NULL);

  return OMPI_SUCCESS;
}


int
mca_pcm_rms_component_close(void)
{
    return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_rms_init(int *priority, 
                 bool have_threads,
                 int constraints)
{
    char *prun;
    int num_cpus;
    mca_pcm_rms_module_t *me;

    /* get our priority - if 0, we don't run */
    mca_base_param_lookup_int(mca_pcm_rms_param_priority, priority);
    if (0 == priority) return NULL;

    /* check constrains */
    /* no daemon */
    if (0 != (constraints & OMPI_RTE_SPAWN_DAEMON)) return NULL;
    /* no MPI_COMM_SPAWN* */
    if (0 != (constraints & OMPI_RTE_SPAWN_FROM_MPI)) return NULL;

    /* see if we are an RMS system */
    num_cpus = rms_numCpus(NULL);
    if (num_cpus <= 0) return NULL;

    prun = ompi_path_env_findv("prun", X_OK, environ, NULL);
    if (NULL == prun) return NULL;
    free(prun);

    /* ok, now let's try to fire up */
    me = malloc(sizeof(mca_pcm_rms_module_t));
    if (NULL == me) return NULL;

    me->super.pcm_allocate_resources = mca_pcm_rms_allocate_resources;
    me->super.pcm_spawn_procs = mca_pcm_rms_spawn_procs;
    me->super.pcm_kill_proc = mca_pcm_rms_kill_proc;
    me->super.pcm_kill_job = mca_pcm_rms_kill_job;
    me->super.pcm_deallocate_resources = mca_pcm_rms_deallocate_resources;
    me->super.pcm_finalize = mca_pcm_rms_finalize;

    mca_base_param_lookup_string(mca_pcm_rms_param_partition,
                                 &(me->partition));

    mca_base_param_lookup_string(mca_pcm_rms_param_prun_args,
                                 &(me->prun_args));

    return me;
}


int
mca_pcm_rms_finalize(struct mca_pcm_base_module_1_0_0_t* me_super)
{
    mca_pcm_rms_module_t *me = (mca_pcm_rms_module_t*) me_super;

    if (NULL == me) return OMPI_ERR_BAD_PARAM;

    if (NULL != me->partition) free(me->partition);
    if (NULL != me->prun_args) free(me->prun_args);

    free(me);

    return OMPI_SUCCESS;
}

