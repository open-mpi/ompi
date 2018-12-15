/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2010 QLogic Corporation. All rights reserved.
 * Copyright (c) 2012-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Intel, Inc. All rights reserved
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/event/event.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/opal_environ.h"
#include "ompi/proc/proc.h"

#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "mtl_psm2_request.h"

#include "psm2.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <glob.h>

static int param_priority;
/* MPI_THREAD_MULTIPLE_SUPPORT */
opal_mutex_t mtl_psm2_mq_mutex = OPAL_MUTEX_STATIC_INIT;

#if OPAL_CUDA_SUPPORT
static bool cuda_envvar_set = false;
#endif

static int ompi_mtl_psm2_component_open(void);
static int ompi_mtl_psm2_component_close(void);
static int ompi_mtl_psm2_component_query(mca_base_module_t **module, int *priority);
static int ompi_mtl_psm2_component_register(void);

static mca_mtl_base_module_t* ompi_mtl_psm2_component_init( bool enable_progress_threads,
                                                          bool enable_mpi_threads );

mca_mtl_psm2_component_t mca_mtl_psm2_component = {

    {
        /* First, the mca_base_component_t struct containing meta
         * information about the component itself */

        .mtl_version = {
            MCA_MTL_BASE_VERSION_2_0_0,

            .mca_component_name = "psm2",
            MCA_BASE_MAKE_VERSION(component, OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION,
                                  OMPI_RELEASE_VERSION),
            .mca_open_component = ompi_mtl_psm2_component_open,
            .mca_close_component = ompi_mtl_psm2_component_close,
            .mca_query_component = ompi_mtl_psm2_component_query,
            .mca_register_component_params = ompi_mtl_psm2_component_register,
        },
        .mtl_data = {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },

        .mtl_init = ompi_mtl_psm2_component_init,
    }
};

static int
get_num_total_procs(int *out_ntp)
{
  *out_ntp = (int)ompi_process_info.num_procs;
  return OMPI_SUCCESS;
}

static int
get_num_local_procs(int *out_nlp)
{
    /* num_local_peers does not include us in
     * its calculation, so adjust for that */
    *out_nlp = (int)(1 + ompi_process_info.num_local_peers);
    return OMPI_SUCCESS;
}

static int
ompi_mtl_psm2_component_register(void)
{
    int num_local_procs, num_total_procs;

    ompi_mtl_psm2.connect_timeout = 180;
    (void) mca_base_component_var_register(&mca_mtl_psm2_component.super.mtl_version,
                                           "connect_timeout",
                                           "PSM2 connection timeout value in seconds",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm2.connect_timeout);


    (void) get_num_local_procs(&num_local_procs);
    (void) get_num_total_procs(&num_total_procs);

    /* set priority high enough to beat ob1's default (also set higher than psm) */
    if ((num_local_procs == num_total_procs) && (1 < num_total_procs)) {
        /* Disable hfi if all processes are local. However, if running only one
         * process assume it is ompi_info or this is most likely going to spawn, for
         * which all PSM2 devices are needed */
        setenv("PSM2_DEVICES", "self,shm", 0);
    }

    param_priority = 40;
    (void) mca_base_component_var_register (&mca_mtl_psm2_component.super.mtl_version,
                                            "priority", "Priority of the PSM2 MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);

    ompi_mtl_psm2_register_pvars();

    return OMPI_SUCCESS;
}

static int
ompi_mtl_psm2_component_open(void)
{
  int res;
  glob_t globbuf = {0};

  /* Component available only if Omni-Path hardware is present */
  res = glob("/dev/hfi1_[0-9]", GLOB_DOOFFS, NULL, &globbuf);
  if (globbuf.gl_pathc > 0 || GLOB_NOMATCH==res) {
      globfree(&globbuf);
  }
  if (0 != res) {
      res = glob("/dev/hfi1_[0-9][0-9]", GLOB_APPEND, NULL, &globbuf);
      if (globbuf.gl_pathc > 0) {
          globfree(&globbuf);
      }
      if (0 != res) {
          return OPAL_ERR_NOT_AVAILABLE;
      }
  }

  /* Component available only if at least one hfi1 port is ACTIVE */
  bool foundOnlineHfi1Port = false;
  size_t i;
  char portState[128];
  FILE *devFile;
  if (glob("/sys/class/infiniband/hfi1_*/ports/*/state",
        GLOB_DOOFFS, NULL, &globbuf) != 0) {
    return OPAL_ERR_NOT_AVAILABLE;
  }

  for (i=0;i < globbuf.gl_pathc; i++) {
    devFile = fopen(globbuf.gl_pathv[i], "r");
    fgets(portState, sizeof(portState), devFile);
    fclose(devFile);

    if (strstr(portState, "ACTIVE") != NULL) {
      /* Found at least one ACTIVE port */
      foundOnlineHfi1Port = true;
      break;
    }
  }

  globfree(&globbuf);

  if (!foundOnlineHfi1Port) {
    return OPAL_ERR_NOT_AVAILABLE;
  }

  return OMPI_SUCCESS;
}

static int
ompi_mtl_psm2_component_query(mca_base_module_t **module, int *priority)
{
    /*
     * if we get here it means that PSM2 is available so give high priority
     */

    *priority = param_priority;
    *module = (mca_base_module_t *)&ompi_mtl_psm2.super;
    return OMPI_SUCCESS;
}

static int
ompi_mtl_psm2_component_close(void)
{
#if OPAL_CUDA_SUPPORT
    if (cuda_envvar_set) {
        opal_unsetenv("PSM2_CUDA", &environ);
    }
#endif
    return OMPI_SUCCESS;
}

static int
get_local_rank(int *out_rank)
{
    ompi_node_rank_t my_node_rank;

    *out_rank = 0;

    if (OMPI_NODE_RANK_INVALID == (my_node_rank =
        ompi_process_info.my_node_rank)) {
        return OMPI_ERROR;
    }
    *out_rank = (int)my_node_rank;
    return OMPI_SUCCESS;
}

static mca_mtl_base_module_t *
ompi_mtl_psm2_component_init(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    psm2_error_t	err;
    int	verno_major = PSM2_VERNO_MAJOR;
    int verno_minor = PSM2_VERNO_MINOR;
    int local_rank = -1, num_local_procs = 0;
#if OPAL_CUDA_SUPPORT
    int ret;
    char *cuda_env;
    glob_t globbuf = {0};
#endif

    /* Compute the total number of processes on this host and our local rank
     * on that node. We need to provide PSM2 with these values so it can
     * allocate hardware contexts appropriately across processes.
     */
    if (OMPI_SUCCESS != get_num_local_procs(&num_local_procs)) {
        opal_output(0, "Cannot determine number of local processes. "
                    "Cannot continue.\n");
        return NULL;
    }
    if (OMPI_SUCCESS != get_local_rank(&local_rank)) {
        opal_output(0, "Cannot determine local rank. Cannot continue.\n");
        return NULL;
    }

    err = psm2_error_register_handler(NULL /* no ep */,
			             PSM2_ERRHANDLER_NOP);
    if (err) {
        opal_output(0, "Error in psm2_error_register_handler (error %s)\n",
		    psm2_error_get_string(err));
	return NULL;
    }

#if OPAL_CUDA_SUPPORT
    /*
     * If using CUDA enabled Open MPI, the user likely intends to
     * run with CUDA buffers. So, force-set the envvar here if user failed
     * to set it.
     */
    ret = glob("/sys/module/nvidia", GLOB_DOOFFS, NULL, &globbuf);
    if (globbuf.gl_pathc > 0) {
        globfree(&globbuf);
    }

    cuda_env = getenv("PSM2_CUDA");
    if (!cuda_env && (0 == ret)) {
        opal_show_help("help-mtl-psm2.txt",
                       "no psm2 cuda env", true,
                       ompi_process_info.nodename);
        opal_setenv("PSM2_CUDA", "1", false, &environ);
        cuda_envvar_set = true;
    }
#endif

    err = psm2_init(&verno_major, &verno_minor);
    if (err) {
      opal_show_help("help-mtl-psm2.txt",
		     "psm2 init", true,
		     psm2_error_get_string(err));
      return NULL;
    }

    /* Complete PSM2 initialization */
    ompi_mtl_psm2_module_init(local_rank, num_local_procs);

    ompi_mtl_psm2.super.mtl_request_size =
      sizeof(mca_mtl_psm2_request_t) -
      sizeof(struct mca_mtl_request_t);

    return &ompi_mtl_psm2.super;
}
