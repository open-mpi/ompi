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
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/event/event.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "mtl_psm.h"
#include "mtl_psm_types.h"
#include "mtl_psm_request.h"

#include "psm.h"

static int ompi_mtl_psm_component_open(void);
static int ompi_mtl_psm_component_close(void);

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
            ompi_mtl_psm_component_close  /* component close */
        },
        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        
        ompi_mtl_psm_component_init  /* component init */
    }
};

    
static int
ompi_mtl_psm_component_open(void)
{
    
  mca_base_param_reg_int(&mca_mtl_psm_component.super.mtl_version, 
			 "connect_timeout",
			 "PSM connection timeout value in seconds",
			 false, false, 30, &ompi_mtl_psm.connect_timeout);
  
  mca_base_param_reg_int(&mca_mtl_psm_component.super.mtl_version, 
			 "debug",
			 "PSM debug level",
			 false, false, 1, 
			 &ompi_mtl_psm.debug_level);
  
  mca_base_param_reg_int(&mca_mtl_psm_component.super.mtl_version, 
			 "ib_unit",
			 "Truescale unit to use",
			 false, false, -1, 
			 &ompi_mtl_psm.ib_unit);

  mca_base_param_reg_int(&mca_mtl_psm_component.super.mtl_version, 
			 "ib_port",
			 "Truescale port on unit to use",
			 false, false, 0, 
			 &ompi_mtl_psm.ib_port);

  mca_base_param_reg_int(&mca_mtl_psm_component.super.mtl_version, 
			 "ib_service_level",
			 "Infiniband service level"
			 "(0 <= SL <= 15)",
			 false, false, 0, &ompi_mtl_psm.ib_service_level);
  
  ompi_mtl_psm.ib_pkey = 0x7fffUL;
  mca_base_param_reg_int(&mca_mtl_psm_component.super.mtl_version, 
			 "ib_pkey",
			 "Infiniband partition key",
			 false, false, 0x7fffUL, 
			 &ompi_mtl_psm.ib_pkey);
  
  if (ompi_mtl_psm.ib_service_level < 0) 
    ompi_mtl_psm.ib_service_level = 0;
  else if (ompi_mtl_psm.ib_service_level > 15)
    ompi_mtl_psm.ib_service_level = 15;
  
  return OMPI_SUCCESS;
    
}


static int
ompi_mtl_psm_component_close(void)
{
    return OMPI_SUCCESS;
}


static mca_mtl_base_module_t*
ompi_mtl_psm_component_init(bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    psm_error_t	err;
    int	verno_major = PSM_VERNO_MAJOR;
    int verno_minor = PSM_VERNO_MINOR;

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
      opal_output(0, "Unable to set infinipath debug level (error %s)\n",
		  psm_error_get_string(err));
    }
#endif
    
    /* Only allow for shm and ipath devices in 2.0 and earlier releases 
     * (unless the user overrides the setting).
     */
    setenv("PSM_DEVICES", "shm,ipath", 0);

    err = psm_init(&verno_major, &verno_minor);
    if (err) {
        opal_output(0, "Error in psm_init (error %s)\n", 
		    psm_error_get_string(err));
        return NULL;
    }

    /*
     * Enable 'self' device only in a post-2.0 release(s)
     */
    if (PSM_VERNO >= 0x0104)
      setenv("PSM_DEVICES", "self,shm,ipath", 0);

    ompi_mtl_psm_module_init();
    
    ompi_mtl_psm.super.mtl_request_size = 
        sizeof(mca_mtl_psm_request_t) - 
        sizeof(struct mca_mtl_request_t);
    
    return &ompi_mtl_psm.super;
}

