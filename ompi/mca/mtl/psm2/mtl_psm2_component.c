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
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2017      Research Organization for Information Science
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

struct ompi_mtl_psm2_shadow_variable {
    int variable_type;
    void *storage;
    mca_base_var_storage_t default_value;
    const char *env_name;
    mca_base_var_info_lvl_t info_level;
    const char *mca_name;
    const char *description;
};

struct ompi_mtl_psm2_shadow_variable ompi_mtl_psm2_shadow_variables[] = {
    {MCA_BASE_VAR_TYPE_STRING, &ompi_mtl_psm2.psm2_devices, {.stringval = "self,shm,hfi"}, "PSM2_DEVICES", OPAL_INFO_LVL_3,
     "devices", "Comma-delimited list of PSM2 devices. Valid values: self, shm, hfi (default: self,shm,hfi)"},
    {MCA_BASE_VAR_TYPE_STRING, &ompi_mtl_psm2.psm2_memory, {.stringval = "normal"}, "PSM2_MEMORY", OPAL_INFO_LVL_9,
     "memory_model", "PSM2 memory usage mode (default: normal)"},
    {MCA_BASE_VAR_TYPE_UNSIGNED_LONG, &ompi_mtl_psm2.psm2_mq_sendreqs_max, {.ulval = 1048576}, "PSM2_MQ_SENDREQS_MAX", OPAL_INFO_LVL_3,
     "mq_sendreqs_max", "PSM2 maximum number of isend requests in flight (default: 1M)"},
    {MCA_BASE_VAR_TYPE_UNSIGNED_LONG, &ompi_mtl_psm2.psm2_mq_recvreqs_max, {.ulval = 1048576}, "PSM2_MQ_RECVREQS_MAX", OPAL_INFO_LVL_3,
     "mq_recvreqs_max", "PSM2 maximum number of irecv requests in flight (default: 1M)"},
    {MCA_BASE_VAR_TYPE_UNSIGNED_LONG, &ompi_mtl_psm2.psm2_mq_rndv_hfi_threshold, {.ulval = 64000}, "PSM2_MQ_RNDV_HFI_THRESH", OPAL_INFO_LVL_3,
     "hfi_eager_limit", "PSM2 eager to rendezvous threshold (default: 64000)"},
    {MCA_BASE_VAR_TYPE_UNSIGNED_LONG, &ompi_mtl_psm2.psm2_mq_rndv_shm_threshold, {.ulval = 16000}, "PSM2_MQ_RNDV_SHM_THRESH", OPAL_INFO_LVL_3,
     "shm_eager_limit", "PSM2 shared memory eager to rendezvous threshold (default: 16000)"},
    {MCA_BASE_VAR_TYPE_BOOL, &ompi_mtl_psm2.psm2_recvthread, {.boolval = true}, "PSM2_RCVTHREAD", OPAL_INFO_LVL_3,
     "use_receive_thread", "Use PSM2 progress thread (default: true)"},
    {MCA_BASE_VAR_TYPE_BOOL, &ompi_mtl_psm2.psm2_shared_contexts, {.boolval = true}, "PSM2_SHAREDCONTEXTS", OPAL_INFO_LVL_6,
     "use_shared_contexts", "Share PSM contexts between MPI processes (default: true)"},
    {MCA_BASE_VAR_TYPE_UNSIGNED_LONG, &ompi_mtl_psm2.psm2_shared_contexts_max, {.ulval = 8}, "PSM2_SHAREDCONTEXTS_MAX", OPAL_INFO_LVL_9,
     "max_shared_contexts", "Maximum number of contexts available on a node (default: 8, max: 8)"},
    {MCA_BASE_VAR_TYPE_UNSIGNED_LONG, &ompi_mtl_psm2.psm2_tracemask, {.ulval = 1}, "PSM2_TRACEMASK", OPAL_INFO_LVL_9,
     "trace_mask", "PSM2 tracemask value. See PSM2 documentation for accepted values (default: 1)"},
    {-1},
};

static void ompi_mtl_psm2_set_shadow_env (struct ompi_mtl_psm2_shadow_variable *variable)
{
    mca_base_var_storage_t *storage = variable->storage;
    char *env_value;
    int ret = 0;

    switch (variable->variable_type) {
    case MCA_BASE_VAR_TYPE_BOOL:
        ret = asprintf (&env_value, "%s=%s", variable->env_name, storage->boolval ? "YES" : "NO");
        break;
    case MCA_BASE_VAR_TYPE_UNSIGNED_LONG:
        if (0 == strcmp (variable->env_name, "PSM2_TRACEMASK")) {
            /* PSM2 documentation shows the tracemask as a hexidecimal number. to be consitent
             * use hexidecimal here. */
            ret = asprintf (&env_value, "%s=0x%lx", variable->env_name, storage->ulval);
        } else {
            ret = asprintf (&env_value, "%s=%lu", variable->env_name, storage->ulval);
        }
        break;
    case MCA_BASE_VAR_TYPE_STRING:
        ret = asprintf (&env_value, "%s=%s", variable->env_name, storage->stringval);
        break;
    }

    if (0 > ret) {
        fprintf (stderr, "ERROR setting PSM2 environment variable: %s\n", variable->env_name);
    } else {
        putenv (env_value);
    }
}

static void ompi_mtl_psm2_register_shadow_env (struct ompi_mtl_psm2_shadow_variable *variable)
{
    mca_base_var_storage_t *storage = variable->storage;
    char *env_value;

    env_value = getenv (variable->env_name);
    switch (variable->variable_type) {
    case MCA_BASE_VAR_TYPE_BOOL:
        if (env_value) {
            int tmp;
            (void) mca_base_var_enum_bool.value_from_string (&mca_base_var_enum_bool, env_value, &tmp);
            storage->boolval = !!tmp;
        } else {
            storage->boolval = variable->default_value.boolval;
        }
        break;
    case MCA_BASE_VAR_TYPE_UNSIGNED_LONG:
        if (env_value) {
            storage->ulval = strtol (env_value, NULL, 0);
        } else {
            storage->ulval = variable->default_value.ulval;
        }
        break;
    case MCA_BASE_VAR_TYPE_STRING:
        if (env_value) {
            storage->stringval = env_value;
        } else {
            storage->stringval = variable->default_value.stringval;
        }
        break;
    }

    (void) mca_base_component_var_register (&mca_mtl_psm2_component.super.mtl_version, variable->mca_name, variable->description,
                                            variable->variable_type, NULL, 0, 0, variable->info_level, MCA_BASE_VAR_SCOPE_READONLY,
                                            variable->storage);
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
ompi_mtl_psm2_component_register(void)
{
    int num_local_procs, num_total_procs;
#if OPAL_CUDA_SUPPORT
    char *cuda_env;
#endif

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
    if (num_local_procs == num_total_procs) {
        /* disable hfi if all processes are local */
        setenv("PSM2_DEVICES", "self,shm", 0);
        /* ob1 is much faster than psm2 with shared memory */
        param_priority = 10;
    } else {
        param_priority = 40;
    }

#if OPAL_CUDA_SUPPORT
    /*
     * If using CUDA enabled OpenMPI, the user likely intends to
     * run with CUDA buffers. So, force-set the envvar here if user failed
     * to set it.
     */
    cuda_env = getenv("PSM2_CUDA");
    if (!cuda_env) {
        opal_show_help("help-mtl-psm2.txt",
                       "no psm2 cuda env", true,
                       "not set",
                       "Host buffers,\nthere will be a performance penalty"
                       " due to OMPI force setting this variable now.\n"
                       "Set environment variable to 0 if using Host buffers" );
        setenv("PSM2_CUDA", "1", 0);
    } else if (strcmp(cuda_env, "0") == 0) {
        opal_show_help("help-mtl-psm2.txt",
                       "no psm2 cuda env", true,
                       "set to 0",
                       "CUDA buffers,\nthe execution will SEGFAULT."
                       " Set environment variable to 1 if using CUDA buffers");
    }
#endif

    (void) mca_base_component_var_register (&mca_mtl_psm2_component.super.mtl_version,
                                            "priority", "Priority of the PSM2 MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);

    for (int i = 0 ; ompi_mtl_psm2_shadow_variables[i].variable_type >= 0 ; ++i) {
        ompi_mtl_psm2_register_shadow_env (ompi_mtl_psm2_shadow_variables + i);
    }

    ompi_mtl_psm2_register_pvars();

    return OMPI_SUCCESS;
}

static int
ompi_mtl_psm2_component_open(void)
{
  int res;
  glob_t globbuf;
  globbuf.gl_offs = 0;

  /* Component available only if Omni-Path hardware is present */
  res = glob("/dev/hfi1_[0-9]", GLOB_DOOFFS, NULL, &globbuf);
  if (0 == res || GLOB_NOMATCH == res) {
      globfree(&globbuf);
  }
  if (0 != res) {
      res = glob("/dev/hfi1_[0-9][0-9]", GLOB_APPEND, NULL, &globbuf);
      if (0 == res || GLOB_NOMATCH == res) {
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

    for (int i = 0 ; ompi_mtl_psm2_shadow_variables[i].variable_type >= 0 ; ++i) {
        ompi_mtl_psm2_set_shadow_env (ompi_mtl_psm2_shadow_variables + i);
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
