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
#include "mca/pcm/rms/pcm_rms.h"
#include "mca/llm/base/base.h"
#include "util/path.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

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
  mca_pcm_rms_finalize
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_rms_1_0_0 = {
    mca_pcm_base_no_unique_name,
    mca_pcm_rms_allocate_resources,
    mca_pcm_rms_can_spawn,
    mca_pcm_rms_spawn_procs,
    mca_pcm_rms_kill_proc,
    mca_pcm_rms_kill_job,
    mca_pcm_rms_deallocate_resources
};


/* need to create output stream to dump in file */
ompi_output_stream_t mca_pcm_rms_output_stream = {
    false, /* lds_is_debugging  BWB - change me for release */
    0,     /* lds_verbose_level */
    false, /* lds_want_syslog */
    0,     /* lds_syslog_priority */
    NULL,  /* lds_syslog_ident */
    "pcm: rms: ", /* lds_prefix */
    true,  /* lds_want_stdout */
    false, /* lds_want_stderr */
    true,  /* lds_want_file */
    true,  /* lds_want_file_append */
    "pcm_rms" /* lds_file_suffix */
};


/*
 * Module variables handles
 */
static int mca_pcm_rms_param_priority;
static int mca_pcm_rms_param_debug;

/*
 * Module variables
 */
ompi_list_t mca_pcm_rms_jobs;
int mca_pcm_rms_output = 0;

int
mca_pcm_rms_component_open(void)
{
    mca_pcm_rms_param_debug =
        mca_base_param_register_int("pcm", "rms", "debug", NULL, 100);

  mca_pcm_rms_param_priority =
    mca_base_param_register_int("pcm", "rms", "priority", NULL, 0);

  OBJ_CONSTRUCT(&mca_pcm_rms_jobs, ompi_list_t);

  return OMPI_SUCCESS;
}


int
mca_pcm_rms_component_close(void)
{
  return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_rms_init(int *priority, 
		  bool *allow_multi_user_threads, 
                  bool *have_hidden_threads)
{
    int debug;
    char *prun;

    mca_base_param_lookup_int(mca_pcm_rms_param_debug, &debug);
    mca_pcm_rms_output = ompi_output_open(&mca_pcm_rms_output_stream);
    ompi_output_set_verbosity(mca_pcm_rms_output, debug);

    mca_base_param_lookup_int(mca_pcm_rms_param_priority, priority);

    *allow_multi_user_threads = false;
    *have_hidden_threads = false;

    /* poke around for prun */
    prun = ompi_path_env_findv("prun", X_OK, environ, NULL);
    if (NULL == prun) return NULL;

    return &mca_pcm_rms_1_0_0;
}


int
mca_pcm_rms_finalize(void)
{
    if (mca_pcm_rms_output > 0) {
        ompi_output_close(mca_pcm_rms_output);
    }

    OBJ_DESTRUCT(&mca_pcm_rms_jobs);

    return OMPI_SUCCESS;
}


static
void
mca_pcm_rms_job_item_construct(ompi_object_t *obj)
{
    mca_pcm_rms_job_item_t *data = (mca_pcm_rms_job_item_t*) obj;
    data->pids = OBJ_NEW(ompi_list_t);
}

static
void
mca_pcm_rms_job_item_destruct(ompi_object_t *obj)
{
    mca_pcm_rms_job_item_t *data = (mca_pcm_rms_job_item_t*) obj;
    if (data->pids != NULL) {
        ompi_list_item_t *item;
        while (NULL != (item = ompi_list_remove_first(data->pids))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(data->pids);
    }
}

OBJ_CLASS_INSTANCE(mca_pcm_rms_job_item_t,
                   ompi_list_item_t,
                   mca_pcm_rms_job_item_construct,
                   mca_pcm_rms_job_item_destruct);

OBJ_CLASS_INSTANCE(mca_pcm_rms_pids_t,
                   ompi_list_item_t,
                   NULL, NULL);

