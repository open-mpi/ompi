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
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>  /* for mkfifo */

#include "include/constants.h"
#include "include/sys/cache.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "mca/pml/pml.h"
#include "mca/base/mca_base_param.h"
#include "bmi_self.h"
#include "bmi_self_frag.h"



/*
 * Shared Memory (SELF) component instance. 
 */

mca_bmi_self_component_t mca_bmi_self_component = {
    {  /* super is being filled in */
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */
            MCA_BMI_BASE_VERSION_1_0_0,
            "self", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_bmi_self_component_open,  /* component open */
            mca_bmi_self_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */
        {
            /* Whether the component is checkpointable or not */
            false
        },

        mca_bmi_self_component_init,  
        NULL,
    }  /* end super */
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_bmi_self_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","self",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_bmi_self_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","self",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_bmi_self_component_open(void)
{
    /* register SELF component parameters */
    mca_bmi_self_component.free_list_num =
        mca_bmi_self_param_register_int("free_list_num", 256);
    mca_bmi_self_component.free_list_max =
        mca_bmi_self_param_register_int("free_list_max", -1);
    mca_bmi_self_component.free_list_inc =
        mca_bmi_self_param_register_int("free_list_inc", 256);
    mca_bmi_self.bmi_eager_limit = 
        mca_bmi_self_param_register_int("eager_limit", 64*1024);
    mca_bmi_self.bmi_min_send_size = 
    mca_bmi_self.bmi_max_send_size = 
        mca_bmi_self_param_register_int("max_send_size", 256*1024);
    mca_bmi_self.bmi_min_rdma_size = 
    mca_bmi_self.bmi_max_rdma_size =
        mca_bmi_self_param_register_int("max_rdma_size", INT_MAX);
    mca_bmi_self.bmi_exclusivity = 
        mca_bmi_self_param_register_int("exclusivity", 64*1024);
    mca_bmi_self.bmi_flags =
        mca_bmi_self_param_register_int("flags", MCA_BMI_FLAGS_RDMA);

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_bmi_self_component.self_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_bmi_self_component.self_frags_eager, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_bmi_self_component.self_frags_send, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_bmi_self_component.self_frags_rdma, ompi_free_list_t);
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_bmi_self_component_close(void)
{
    OBJ_DESTRUCT(&mca_bmi_self_component.self_lock);
    OBJ_DESTRUCT(&mca_bmi_self_component.self_frags_eager);
    OBJ_DESTRUCT(&mca_bmi_self_component.self_frags_send);
    OBJ_DESTRUCT(&mca_bmi_self_component.self_frags_rdma);
    return OMPI_SUCCESS;
}


/*
 *  SELF component initialization
 */
mca_bmi_base_module_t** mca_bmi_self_component_init(
    int *num_ptls, 
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_bmi_base_module_t **ptls = NULL;
    *num_ptls = 0;

    /* allocate the Shared Memory PTL */
    *num_ptls = 1;
    ptls = malloc((*num_ptls)*sizeof(mca_bmi_base_module_t*));
    if (NULL == ptls) {
        return NULL;
    }

    /* initialize free lists */
    ompi_free_list_init(&mca_bmi_self_component.self_frags_eager, 
        sizeof(mca_bmi_self_frag_eager_t) + mca_bmi_self.bmi_eager_limit,
        OBJ_CLASS(mca_bmi_self_frag_eager_t),
        mca_bmi_self_component.free_list_num,
        mca_bmi_self_component.free_list_max,
        mca_bmi_self_component.free_list_inc,
        NULL);
    ompi_free_list_init(&mca_bmi_self_component.self_frags_send, 
        sizeof(mca_bmi_self_frag_send_t) + mca_bmi_self.bmi_max_send_size,
        OBJ_CLASS(mca_bmi_self_frag_send_t),
        mca_bmi_self_component.free_list_num,
        mca_bmi_self_component.free_list_max,
        mca_bmi_self_component.free_list_inc,
        NULL);
    ompi_free_list_init(&mca_bmi_self_component.self_frags_rdma, 
        sizeof(mca_bmi_self_frag_rdma_t),
        OBJ_CLASS(mca_bmi_self_frag_rdma_t),
        mca_bmi_self_component.free_list_num,
        mca_bmi_self_component.free_list_max,
        mca_bmi_self_component.free_list_inc,
        NULL);

    /* get pointer to the ptls */
    ptls[0] = (mca_bmi_base_module_t *)(&mca_bmi_self);
    return ptls;
}

