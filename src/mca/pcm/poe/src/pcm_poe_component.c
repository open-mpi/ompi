/* -*- C -*-
 * 
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
 *
 */


#include "ompi_config.h"
#include "pcm_poe.h"
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
mca_pcm_base_component_1_0_0_t mca_pcm_poe_component = {
  {
    MCA_PCM_BASE_VERSION_1_0_0,

    "poe", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    mca_pcm_poe_component_open,  /* component open */
    mca_pcm_poe_component_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  mca_pcm_poe_init,    /* component init */
  NULL                 /* unique name */
};


struct mca_pcm_base_module_1_0_0_t mca_pcm_poe_1_0_0 = {
    mca_pcm_poe_allocate_resources,
    mca_pcm_poe_spawn_procs,
    mca_pcm_poe_kill_proc,
    mca_pcm_poe_kill_job,
    mca_pcm_poe_deallocate_resources,
    mca_pcm_poe_finalize
};


/* need to create output stream to dump in file */
ompi_output_stream_t mca_pcm_poe_output_stream = {
    false, /* lds_is_debugging  BWB - change me for release */
    0,     /* lds_verbose_level */
    false, /* lds_want_syslog */
    0,     /* lds_syslog_priority */
    NULL,  /* lds_syslog_ident */
    "pcm: poe: ", /* lds_prefix */
    true,  /* lds_want_stdout */
    false, /* lds_want_stderr */
    true,  /* lds_want_file */
    true,  /* lds_want_file_append */
    "pcm_poe" /* lds_file_suffix */
};


/*
 * Module variables handles
 */
static int mca_pcm_poe_param_priority;
static int mca_pcm_poe_param_debug;

/*
 * Component variables.  All of these are shared among the module
 * instances, so they don't need to go in a special structure or
 * anything.
 */
int mca_pcm_poe_output = 0;

int
mca_pcm_poe_component_open(void)
{
    mca_pcm_poe_param_debug =
        mca_base_param_register_int("pcm", "poe", "debug", NULL, 100);

  mca_pcm_poe_param_priority =
    mca_base_param_register_int("pcm", "poe", "priority", NULL, 5);

  return OMPI_SUCCESS;
}


int
mca_pcm_poe_component_close(void)
{
    return OMPI_SUCCESS;
}


mca_pcm_base_module_t*
mca_pcm_poe_init(int *priority, 
                 bool *allow_multi_user_threads, 
                 bool *have_hidden_threads,
                 int constraints)
{

    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    return NULL;
}


int
mca_pcm_poe_finalize(struct mca_pcm_base_module_1_0_0_t* me)
{
    return OMPI_SUCCESS;
}
