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

#include "pcm_bproc.h"
#include "include/constants.h"
#include "include/types.h"
#include "class/ompi_list.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "mca/llm/base/base.h"
#include "runtime/runtime.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/bproc.h>

/*
 * Struct of function pointers and all that to let us be initialized
 */
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
static int param_priority;

/* disable stdio */
static int param_stdin_dev_null;
static int param_no_io_forwarding;

/* use forwarding */
static int param_line_buffer;
static int param_prefix_io;
static int param_line_buffer_size;
static int param_stdin_rank;

/* use files */
static int param_stdin_file;
static int param_stdout_file;
static int param_stderr_file;


int
mca_pcm_bproc_component_open(void)
{
  param_priority =
    mca_base_param_register_int("pcm", "bproc", "priority", NULL, 5);
  param_stdin_dev_null =
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
		   bool have_threads,
		   int constraints)
{
    int ret;
    mca_pcm_bproc_module_t *me;
    struct bproc_version_t vers;

    mca_base_param_lookup_int(param_priority, priority);

    /* we can start daemons, we can do qos, and it looks like we can spawn,
       so no constrains searching */
    if (0 != (constraints & OMPI_RTE_SPAWN_MULTI_CELL)) {
      errno = OMPI_ERR_BAD_PARAM;
      return NULL;
    }

    /* ok, now let's try to fire up */
    me = malloc(sizeof(mca_pcm_bproc_module_t));
    if (NULL == me) return NULL;

    ret = mca_llm_base_select("rsh", &(me->llm), have_threads);

    if (OMPI_SUCCESS != ret) {
        /* well, that can't be good.  guess we can't run */
        ompi_output_verbose(5, mca_pcm_base_output, "init: no llm found");
        free(me);
        return NULL;
    }

    /* see if bproc is running */
    ret = bproc_version(&vers);
    if (ret != 0) {
      ompi_output_verbose(5, mca_pcm_base_output, 
			  "bproc: bproc_version() failed");
      return NULL;
    }
    
    /* If we're not on the master, forget it */
    if (bproc_currnode() != BPROC_NODE_MASTER) {
      ompi_output_verbose(5, mca_pcm_base_output, 
			  "bproc: not on BPROC_NODE_MASTER");
      return NULL;
    }

    me->constraints = constraints;
    me->jobs = mca_pcm_base_job_list_init();

    /*
     * fill in the function pointers
     */
    me->super.pcm_allocate_resources = mca_pcm_bproc_allocate_resources;
    me->super.pcm_spawn_procs = mca_pcm_bproc_spawn_procs;
    me->super.pcm_kill_proc = mca_pcm_bproc_kill_proc;
    me->super.pcm_kill_job = mca_pcm_bproc_kill_job;
    me->super.pcm_deallocate_resources = mca_pcm_bproc_deallocate_resources;
    me->super.pcm_finalize = mca_pcm_bproc_finalize;

    return (mca_pcm_base_module_t*) me;
}


int
mca_pcm_bproc_finalize(struct mca_pcm_base_module_1_0_0_t* me_super)
{
    mca_pcm_bproc_module_t *me = (mca_pcm_bproc_module_t*) me_super;

    if (NULL == me) return OMPI_ERR_BAD_PARAM;

    me->llm->llm_finalize(me->llm);

    free(me);

    return OMPI_SUCCESS;
}

