/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "include/constants.h"
#include "include/types.h"
#include "util/malloc.h"
#include "util/output.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/pcm/rsh/src/pcm_rsh.h"
#include "mca/llm/llm.h"
#include "mca/llm/base/base.h"

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
    mca_pcm_base_no_unique_name,
    NULL,
    mca_pcm_rsh_can_spawn,
    mca_pcm_rsh_spawn_procs,
    mca_pcm_rsh_kill_proc,
    mca_pcm_rsh_kill_job,
    NULL
};


/* need to create output stream to dump in file */
ompi_output_stream_t mca_pcm_rsh_output_stream = {
    false, /* lds_is_debugging  BWB - change me for release */
    0,     /* lds_verbose_level */
    false, /* lds_want_syslog */
    0,     /* lds_syslog_priority */
    NULL,  /* lds_syslog_ident */
    "pcm: rsh: ", /* lds_prefix */
    true,  /* lds_want_stdout */
    false, /* lds_want_stderr */
    true,  /* lds_want_file */
    true,  /* lds_want_file_append */
    "pcm_rsh" /* lds_file_suffix */
};


/*
 * Module variables handles
 */
static int mca_pcm_rsh_param_no_profile;
static int mca_pcm_rsh_param_fast;
static int mca_pcm_rsh_param_ignore_stderr;
static int mca_pcm_rsh_param_priority;
static int mca_pcm_rsh_param_agent;
static int mca_pcm_rsh_param_debug;
static int mca_pcm_rsh_param_use_ns;

/*
 * Module variables
 */
/* should we avoid running .profile, even if the shell says we should */
int mca_pcm_rsh_no_profile;
/* should we assume same shell on remote as locally? */
int mca_pcm_rsh_fast;
/* should we ignore things on stderr? */
int mca_pcm_rsh_ignore_stderr;
/* how should we fire procs up on the remote side? */
char *mca_pcm_rsh_agent;

int mca_pcm_rsh_output = 0;
int mca_pcm_rsh_use_ns;

static mca_llm_base_module_t mca_pcm_rsh_llm;

int
mca_pcm_rsh_component_open(void)
{
    mca_pcm_rsh_param_debug =
        mca_base_param_register_int("pcm", "rsh", "debug", NULL, 100);

  mca_pcm_rsh_param_agent = 
      mca_base_param_register_string("pcm", "rsh", "agent", NULL, 
				      "ssh");

  mca_pcm_rsh_param_no_profile =
    mca_base_param_register_int("pcm", "rsh", "no_profile", NULL, 1);
  mca_pcm_rsh_param_fast =
    mca_base_param_register_int("pcm", "rsh", "fast", NULL, 1);
  mca_pcm_rsh_param_ignore_stderr =
    mca_base_param_register_int("pcm", "rsh", "ignore_stderr", NULL, 0);
  mca_pcm_rsh_param_use_ns =
    mca_base_param_register_int("pcm", "rsh", "use_ns", NULL, 0);

  mca_pcm_rsh_param_priority =
    mca_base_param_register_int("pcm", "rsh", "priority", NULL, 1);

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
    int debug;
    int ret;

    mca_base_param_lookup_int(mca_pcm_rsh_param_debug, &debug);
    mca_pcm_rsh_output = ompi_output_open(&mca_pcm_rsh_output_stream);
    ompi_output_set_verbosity(mca_pcm_rsh_output, debug);

    mca_base_param_lookup_int(mca_pcm_rsh_param_priority, priority);
 
    mca_base_param_lookup_int(mca_pcm_rsh_param_no_profile, 
			      &mca_pcm_rsh_no_profile);
    mca_base_param_lookup_int(mca_pcm_rsh_param_fast, 
			      &mca_pcm_rsh_fast);
    mca_base_param_lookup_int(mca_pcm_rsh_param_ignore_stderr, 
			      &mca_pcm_rsh_ignore_stderr);
    mca_base_param_lookup_int(mca_pcm_rsh_param_ignore_stderr, 
			      &mca_pcm_rsh_ignore_stderr);
    mca_base_param_lookup_string(mca_pcm_rsh_param_agent,
                                 &mca_pcm_rsh_agent);
    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    mca_base_param_lookup_int(mca_pcm_rsh_param_use_ns, 
			      &mca_pcm_rsh_use_ns);

    ret = mca_llm_base_select("pcm", &mca_pcm_rsh_llm,
                              allow_multi_user_threads,
                              have_hidden_threads);
    if (OMPI_SUCCESS != ret) {
        /* well, that can't be good.  guess we can't run */
        ompi_output_verbose(5, mca_pcm_rsh_output, "select: no llm found");
        return NULL;
    }

    /* copy over the function pointers */
    mca_pcm_rsh_1_0_0.pcm_allocate_resources = 
        (mca_pcm_base_allocate_resources_fn_t) 
          mca_pcm_rsh_llm.llm_allocate_resources;
    mca_pcm_rsh_1_0_0.pcm_deallocate_resources = 
        (mca_pcm_base_deallocate_resources_fn_t)
          mca_pcm_rsh_llm.llm_deallocate_resources;

    /* DO SOME PARAM "FIXING" */
    /* BWB - remove param fixing before 1.0 */
    if (0 == mca_pcm_rsh_no_profile) {
        printf("WARNING: reseting mca_pcm_rsh_no_profile to 1\n");
        mca_pcm_rsh_no_profile = 1;
    }
    if (0 == mca_pcm_rsh_fast) {
        printf("WARNING: reseting mca_pcm_rsh_fast to 1\n");
        mca_pcm_rsh_fast = 1;
    }

    return &mca_pcm_rsh_1_0_0;
}


int
mca_pcm_rsh_finalize(void)
{
    if (mca_pcm_rsh_output > 0) {
        ompi_output_close(mca_pcm_rsh_output);
    }

    if (NULL == mca_pcm_rsh_1_0_0.pcm_allocate_resources) {
        mca_pcm_rsh_1_0_0.pcm_allocate_resources = NULL;
        mca_pcm_rsh_1_0_0.pcm_deallocate_resources = NULL;
    }

    return OMPI_SUCCESS;
}
