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
 * Copyright (c) 2014      Intel Corporation. All rights reserved.
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

#include "mtl_psm.h"
#include "mtl_psm_types.h"
#include "mtl_psm_request.h"

#include "psm.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

static int param_priority;

static int ompi_mtl_psm_component_open(void);
static int ompi_mtl_psm_component_close(void);
static int ompi_mtl_psm_component_query(mca_base_module_t **module, int *priority);
static int ompi_mtl_psm_component_register(void);

static mca_mtl_base_module_t* ompi_mtl_psm_component_init( bool enable_progress_threads, 
                                                          bool enable_mpi_threads );

mca_mtl_psm_component_t mca_mtl_psm_component = {

    {
        /* First, the mca_base_component_t struct containing meta
         * information about the component itself */
        
        {
            MCA_MTL_BASE_VERSION_2_0_0,
            
            "psm", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            ompi_mtl_psm_component_open,  /* component open */
            ompi_mtl_psm_component_close,  /* component close */
            ompi_mtl_psm_component_query,  /* component close */
            ompi_mtl_psm_component_register
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        
        ompi_mtl_psm_component_init  /* component init */
    }
};

#if PSM_VERNO >= 0x010d
static mca_base_var_enum_value_t path_query_values[] = {
    {PSM_PATH_RES_NONE, "none"},
    {PSM_PATH_RES_OPP, "opp"},
    {0, NULL}
};
#endif
    
static int
ompi_mtl_psm_component_register(void)
{
#if PSM_VERNO >= 0x010d
    mca_base_var_enum_t *new_enum;
#endif
    

    param_priority = 100;
    (void) mca_base_component_var_register (&mca_mtl_psm_component.super.mtl_version,
                                            "priority", "Priority of the PSM MTL component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &param_priority);

    ompi_mtl_psm.connect_timeout = 180;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "connect_timeout",
                                           "PSM connection timeout value in seconds",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.connect_timeout);

    ompi_mtl_psm.debug_level = 1;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "debug", "PSM debug level",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.debug_level);

    ompi_mtl_psm.ib_unit = -1;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "ib_unit", "Truescale unit to use",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.ib_unit);

    ompi_mtl_psm.ib_port = 0;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "ib_port", "Truescale port on unit to use",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.ib_port);

    ompi_mtl_psm.ib_service_level = 0;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "ib_service_level", "Infiniband service level"
                                           "(0 <= SL <= 15)", MCA_BASE_VAR_TYPE_INT,
                                           NULL, 0, 0, OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.ib_service_level);
  
    ompi_mtl_psm.ib_pkey = 0x7fffUL;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "ib_pkey", "Infiniband partition key",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.ib_pkey);
    
#if PSM_VERNO >= 0x010d
    ompi_mtl_psm.ib_service_id = 0x1000117500000000ull;
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                           "ib_service_id",
                                           "Infiniband service ID to use for application (default is 0)",
                                           MCA_BASE_VAR_TYPE_UNSIGNED_LONG_LONG, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &ompi_mtl_psm.ib_service_id);

    ompi_mtl_psm.path_res_type = PSM_PATH_RES_NONE;
    mca_base_var_enum_create("mtl_psm_path_query", path_query_values, &new_enum);
    (void) mca_base_component_var_register(&mca_mtl_psm_component.super.mtl_version,
                                          "path_query",
                                          "Path record query mechanisms",
                                          MCA_BASE_VAR_TYPE_INT, new_enum, 0, 0,
                                          OPAL_INFO_LVL_9,
                                          MCA_BASE_VAR_SCOPE_READONLY,
                                          &ompi_mtl_psm.path_res_type);
    OBJ_RELEASE(new_enum);
#endif

    return OMPI_SUCCESS;
}

static int
ompi_mtl_psm_component_open(void)
{
  struct stat st;
  
    if (ompi_mtl_psm.ib_service_level < 0)  {
      ompi_mtl_psm.ib_service_level = 0;
    } else if (ompi_mtl_psm.ib_service_level > 15) {
      ompi_mtl_psm.ib_service_level = 15;
    }

  /* Component available only if Truescale hardware is present */
  if (0 == stat("/dev/ipath", &st)) {
    return OMPI_SUCCESS;
  }
  else {
    return OPAL_ERR_NOT_AVAILABLE;
  }
}

static int
ompi_mtl_psm_component_query(mca_base_module_t **module, int *priority)
{
    /*
     * if we get here it means that PSM is available so give high priority
     */

    *priority = param_priority;
    *module = (mca_base_module_t *)&ompi_mtl_psm.super;
    return OMPI_SUCCESS;
}


static int
ompi_mtl_psm_component_close(void)
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
ompi_mtl_psm_component_init(bool enable_progress_threads,
                            bool enable_mpi_threads)
{
    psm_error_t	err;
    int	verno_major = PSM_VERNO_MAJOR;
    int verno_minor = PSM_VERNO_MINOR;
    int local_rank = -1, num_local_procs = 0;
    int num_total_procs = 0;

    /* Compute the total number of processes on this host and our local rank
     * on that node. We need to provide PSM with these values so it can 
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

     
    err = psm_error_register_handler(NULL /* no ep */,
			             PSM_ERRHANDLER_NOP);
    if (err) {
        opal_output(0, "Error in psm_error_register_handler (error %s)\n", 
		    psm_error_get_string(err));
	return NULL;
    }
    
#if PSM_VERNO >= 0x010c
    /* Set infinipath debug level */
    err = psm_setopt(PSM_COMPONENT_CORE, 0, PSM_CORE_OPT_DEBUG, 
		     (const void*) &ompi_mtl_psm.debug_level, 
		     sizeof(unsigned));
    if (err) {
      /* Non fatal error. Can continue */
      opal_show_help("help-mtl-psm.txt",
		     "psm init", false,
		     psm_error_get_string(err));
    }
#endif
    
    if (getenv("PSM_DEVICES") == NULL) {
        /* Only allow for shm and ipath devices in 2.0 and earlier releases 
         * (unless the user overrides the setting).
         */
        if (PSM_VERNO >= 0x0104) {
            if (num_local_procs == num_total_procs) {
                setenv("PSM_DEVICES", "self,shm", 0);
	    } else {
                setenv("PSM_DEVICES", "self,shm,ipath", 0);
	    }
        }
        else {
            if (num_local_procs == num_total_procs) {
                setenv("PSM_DEVICES", "shm", 0);
	    } else {
                setenv("PSM_DEVICES", "shm,ipath", 0);
	    }
        }
    }
    
    err = psm_init(&verno_major, &verno_minor);
    if (err) {
      opal_show_help("help-mtl-psm.txt",
		     "psm init", true,
		     psm_error_get_string(err));
      return NULL;
    }
    
    /* Complete PSM initialization */
    ompi_mtl_psm_module_init(local_rank, num_local_procs);

    ompi_mtl_psm.super.mtl_request_size = 
      sizeof(mca_mtl_psm_request_t) - 
      sizeof(struct mca_mtl_request_t);
    
    return &ompi_mtl_psm.super;
}

