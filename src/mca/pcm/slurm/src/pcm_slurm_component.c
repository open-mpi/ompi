/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "pcm_slurm.h"
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

extern char **environ;

/*
 * Struct of function pointers and all that to let us be initialized
 */
mca_pcm_base_component_1_0_0_t mca_pcm_slurm_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "slurm", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_slurm_component_open,  /* component open */
    mca_pcm_slurm_component_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_slurm_init,    /* component init */
  NULL                 /* unique name */
};


/* need to create output stream to dump in file */
ompi_output_stream_t mca_pcm_slurm_output_stream = {
    false, /* lds_is_debugging  BWB - change me for release */
    0,     /* lds_verbose_level */
    false, /* lds_want_syslog */
    0,     /* lds_syslog_priority */
    NULL,  /* lds_syslog_ident */
    "pcm: slurm: ", /* lds_prefix */
    true,  /* lds_want_stdout */
    false, /* lds_want_stderr */
    true,  /* lds_want_file */
    true,  /* lds_want_file_append */
    "pcm_slurm" /* lds_file_suffix */
};


/*
 * Module variables handles
 */
static int mca_pcm_slurm_param_priority;
static int mca_pcm_slurm_param_debug;

/*
 * Component variables.  All of these are shared among the module
 * instances, so they don't need to go in a special structure or
 * anything.
 */
int mca_pcm_slurm_output = -1;


int
mca_pcm_slurm_component_open(void)
{
  mca_pcm_slurm_param_debug =
    mca_base_param_register_int("pcm", "slurm", "debug", NULL, 100);

  mca_pcm_slurm_param_priority =
    mca_base_param_register_int("pcm", "slurm", "priority", NULL, 5);

  mca_pcm_slurm_output = ompi_output_open(&mca_pcm_slurm_output_stream);

  return OMPI_SUCCESS;
}


int
mca_pcm_slurm_component_close(void)
{
    return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_slurm_init(int *priority, 
                 bool have_threads,
                 int constraints)
{
    int debug;
    char *srun;
    mca_pcm_base_module_t *me;

    /* BWB - temporarily disable */
    return NULL;

    /* debugging gorp */
    mca_base_param_lookup_int(mca_pcm_slurm_param_debug, &debug);
    ompi_output_set_verbosity(mca_pcm_slurm_output, debug);

    /* get our priority - if 0, we don't run */
    mca_base_param_lookup_int(mca_pcm_slurm_param_priority, priority);
    if (0 == priority) return NULL;

    /* check constrains */
    /* no daemon */
    if (0 != (constraints & OMPI_RTE_SPAWN_DAEMON)) return NULL;
    /* no MPI_COMM_SPAWN* */
    if (0 != (constraints & OMPI_RTE_SPAWN_FROM_MPI)) return NULL;

    srun = ompi_path_env_findv("srun", X_OK, environ, NULL);
    if (NULL == srun) return NULL;
    free(srun);

    /* ok, now let's try to fire up */
    me = malloc(sizeof(mca_pcm_base_module_t));
    if (NULL == me) return NULL;

    me->pcm_allocate_resources = mca_pcm_slurm_allocate_resources;
    me->pcm_spawn_procs = mca_pcm_slurm_spawn_procs;
    me->pcm_kill_proc = mca_pcm_slurm_kill_proc;
    me->pcm_kill_job = mca_pcm_slurm_kill_job;
    me->pcm_deallocate_resources = mca_pcm_slurm_deallocate_resources;
    me->pcm_finalize = mca_pcm_slurm_finalize;

    return me;
}


int
mca_pcm_slurm_finalize(struct mca_pcm_base_module_1_0_0_t* me)
{
    if (mca_pcm_slurm_output > 0) {
        ompi_output_close(mca_pcm_slurm_output);
    }

    return OMPI_SUCCESS;
}

