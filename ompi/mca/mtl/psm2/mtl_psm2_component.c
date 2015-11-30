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
 * Copyright (c) 2012-2015 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
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
#include "ompi/proc/proc.h"

#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "mtl_psm2_request.h"

#include "psm2.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

static int param_priority;

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
ompi_mtl_psm2_component_register(void)
{
    ompi_mtl_psm2.connect_timeout = 180;
    (void) mca_base_component_var_register(&mca_mtl_psm2_component.super.mtl_version,
                                           "connect_timeout",
                                           "PSM2 connection timeout value in seconds",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm2.connect_timeout);

    /* set priority high enough to beat ob1's default (also set higher than psm) */
    param_priority = 40;
    (void) mca_base_component_var_register (&mca_mtl_psm2_component.super.mtl_version,
                                            "priority", "Priority of the PSM2 MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);

    return OMPI_SUCCESS;
}

static int
ompi_mtl_psm2_component_open(void)
{
  struct stat st;

  /* Component available only if Omni-Path hardware is present */
  if (0 == stat("/dev/hfi1", &st)) {
    return OMPI_SUCCESS;
  }
  else {
    return OPAL_ERR_NOT_AVAILABLE;
  }
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
    return OMPI_SUCCESS;
}

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
    int num_total_procs = 0;

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
    if (OMPI_SUCCESS != get_num_total_procs(&num_total_procs)) {
        opal_output(0, "Cannot determine total number of processes. "
                    "Cannot continue.\n");
        return NULL;
    }

    err = psm2_error_register_handler(NULL /* no ep */,
			             PSM2_ERRHANDLER_NOP);
    if (err) {
        opal_output(0, "Error in psm2_error_register_handler (error %s)\n",
		    psm2_error_get_string(err));
	return NULL;
    }

    if (num_local_procs == num_total_procs) {
      setenv("PSM2_DEVICES", "self,shm", 0);
    }

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
