/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "pcm_bproc.h"
#include "include/constants.h"
#include "include/types.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/llm/base/base.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
OMPI_EXPORT
mca_pcm_base_component_1_0_0_t mca_pcm_bproc_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "bproc", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_bproc_component_open,  /* component open */
    mca_pcm_bproc_component_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_bproc_init,    /* component init */
  NULL                 /* unique name */
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_bproc_1_0_0 = {
    mca_pcm_bproc_allocate_resources,
    mca_pcm_bproc_spawn_procs,
    mca_pcm_bproc_kill_proc,
    mca_pcm_bproc_kill_job,
    mca_pcm_bproc_deallocate_resources,
    mca_pcm_bproc_finalize
};


/* need to create output stream to dump in file */
ompi_output_stream_t mca_pcm_bproc_output_stream = {
    false, /* lds_is_debugging  BWB - change me for release */
    0,     /* lds_verbose_level */
    false, /* lds_want_syslog */
    0,     /* lds_syslog_priority */
    NULL,  /* lds_syslog_ident */
    "pcm: bproc: ", /* lds_prefix */
    true,  /* lds_want_stdout */
    false, /* lds_want_stderr */
    true,  /* lds_want_file */
    true,  /* lds_want_file_append */
    "pcm_bproc" /* lds_file_suffix */
};


/*
 * Module variables handles
 */
static int mca_pcm_bproc_param_priority;
static int mca_pcm_bproc_param_debug;

/*
 * Component variables.  All of these are shared among the module
 * instances, so they don't need to go in a special structure or
 * anything.
 */
int mca_pcm_bproc_output = 0;

int
mca_pcm_bproc_component_open(void)
{
    mca_pcm_bproc_param_debug =
        mca_base_param_register_int("pcm", "bproc", "debug", NULL, 100);

  mca_pcm_bproc_param_priority =
    mca_base_param_register_int("pcm", "bproc", "priority", NULL, 5);

  return OMPI_SUCCESS;
}


int
mca_pcm_bproc_component_close(void)
{
    return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_bproc_init(int *priority, 
                 bool *allow_multi_user_threads, 
                 bool *have_hidden_threads,
                 int constraints)
{
    int debug;

    mca_base_param_lookup_int(mca_pcm_bproc_param_debug, &debug);
    mca_pcm_bproc_output = ompi_output_open(&mca_pcm_bproc_output_stream);
    ompi_output_set_verbosity(mca_pcm_bproc_output, debug);

    mca_base_param_lookup_int(mca_pcm_bproc_param_priority, priority);

    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    return NULL;
}


int
mca_pcm_bproc_finalize(struct mca_pcm_base_module_1_0_0_t* me)
{
    return OMPI_SUCCESS;
}

