/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include "include/constants.h"

#include "util/output.h"
#include "threads/thread.h"

#include "ptl_portals.h"
#include "ptl_portals_nal.h"


/*
 * The portals component
 */

mca_ptl_portals_component_t mca_ptl_portals_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_PTL_BASE_VERSION_1_0_0,

        "portals", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_ptl_portals_component_open,  /* module open */
        mca_ptl_portals_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_ptl_portals_component_init,  
      mca_ptl_portals_component_control,
      mca_ptl_portals_component_progress,
    }
};


static ompi_output_stream_t portals_output_stream = {
    true,
    0,
    0,
    0,
    NULL,
    "ptl_portals: ",
    false,
    true,
    false,
    false,
    NULL
};


static inline char*
mca_ptl_portals_param_register_string(const char* param_name, 
                                 const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl", "portals",
                                            param_name, NULL, 
                                            default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}


static inline int
mca_ptl_portals_param_register_int(const char* param_name, 
                              int default_value)
{
    int id = mca_base_param_register_int("ptl", "portals", param_name,
                                         NULL, default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}


/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */
int
mca_ptl_portals_component_open(void)
{
    int debug_level;

    /* initialize state */
    mca_ptl_portals_component.portals_num_modules = 0;
    mca_ptl_portals_component.portals_modules = NULL;

    /* initialize objects */

    /* register portals module parameters */
    mca_ptl_portals_component.portals_free_list_num =
        mca_ptl_portals_param_register_int("free_list_num", 256);
    mca_ptl_portals_component.portals_free_list_max =
        mca_ptl_portals_param_register_int("free_list_max", -1);
    mca_ptl_portals_component.portals_free_list_inc =
        mca_ptl_portals_param_register_int("free_list_inc", 256);
    mca_ptl_portals_component.portals_ifname = 
        mca_ptl_portals_param_register_string("ifname", "eth0");
    portals_output_stream.lds_verbose_level = 
        mca_ptl_portals_param_register_int("debug_level", 100);

    /* finish with objects */
    mca_ptl_portals_component.portals_output = 
        ompi_output_open(&portals_output_stream);

    return OMPI_SUCCESS;
}


/*
 * module cleanup - sanity checking of queue lengths
 */
int
mca_ptl_portals_component_close(void)
{
    /* finalize interface? */

    /* print out debugging if anything is pending */

    /* release resources */
    if (NULL != mca_ptl_portals_component.portals_ifname) {
        free(mca_ptl_portals_component.portals_ifname);
    }

    return OMPI_SUCCESS;
}


/*
 *  portals module initialization.
 */
mca_ptl_base_module_t**
mca_ptl_portals_component_init(int *num_ptls, 
                               bool enable_progress_threads,
                               bool enable_mpi_threads)
{
    mca_ptl_base_module_t** ptls;
    *num_ptls = 0;

    /* do the non-portable global initialization stuff for a
       particular network link */
    if (OMPI_SUCCESS != mca_ptl_portals_nal_init()) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "nal_init() failed - failing component_init()\n");
        return NULL;
    }

    if (OMPI_SUCCESS != mca_ptl_portals_module_init()) {
        ompi_output_verbose(10, mca_ptl_portals_component.portals_output,
                            "module_init() failed - failing component_init()\n");
        return NULL;
    }

    /* BWB - why do we duplicate / copy here?  do we need to free
       anything? */
    mca_ptl_portals_component.portals_num_modules = 1;
    ptls = malloc(mca_ptl_portals_component.portals_num_modules * 
                  sizeof(mca_ptl_base_module_t*));
    if (NULL == ptls) return NULL;

    memcpy(ptls, 
           mca_ptl_portals_component.portals_modules, 
           mca_ptl_portals_component.portals_num_modules * 
           sizeof(mca_ptl_portals_module_t*));
    *num_ptls = mca_ptl_portals_component.portals_num_modules;
    return ptls;
}


/*
 *  Portals module control
 */
int
mca_ptl_portals_component_control(int param, void* value, size_t size)
{
    switch(param) {
        case MCA_PTL_ENABLE:
            if(*(int*)value) {
                /* BWB - enable the ptl */
            } else
                /* BWB - disable the ptl */
            break;
        default:
            break;
    }
    return OMPI_SUCCESS;
}


/*
 *  Portals module progress.
 */
int
mca_ptl_portals_component_progress(mca_ptl_tstamp_t tstamp)
{
    int num_progressed = 0;

    /* BWB - write me */

    return num_progressed;
}

