/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H*/
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/allocator/base/base.h"
#include "mpool_sm.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"
#include "ompi/proc/proc.h"

/*
 * Local functions
 */
static int mca_mpool_sm_open(void);
static int mca_mpool_sm_close( void );
static mca_mpool_base_module_t* mca_mpool_sm_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_sm_component_t mca_mpool_sm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_MPOOL_BASE_VERSION_1_0_0,

        "sm", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_mpool_sm_open,  /* component open  */
        mca_mpool_sm_close
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_sm_init
    }
};

static int max_size_param, min_size_param, peer_size_param;


/**
  * component open/close/init function
  */
static int mca_mpool_sm_open(void)
{
    /* register SM component parameters */
    mca_base_param_reg_string(&mca_mpool_sm_component.super.mpool_version,
                              "allocator",
                              "Name of allocator component to use with sm mpool",
                              false, false,
                              "bucket",
                              &mca_mpool_sm_component.sm_allocator_name);

    max_size_param = 
        mca_base_param_reg_int(&mca_mpool_sm_component.super.mpool_version,
                               "max_size",
                               "Maximum size of the sm mpool shared memory file",
                               false, false, 512 * 1024 * 1024, NULL);

    min_size_param = 
        mca_base_param_reg_int(&mca_mpool_sm_component.super.mpool_version,
                               "min_size",
                               "Minimum size of the sm mpool shared memory file",
                               false, false, 128 * 1024 * 1024, NULL);

    peer_size_param = 
        mca_base_param_reg_int(&mca_mpool_sm_component.super.mpool_version,
                               "per_peer_size",
                               "Size (in bytes) to allocate per local peer in "
                               "the sm mpool shared memory file, bounded by "
                               "min_size and max_size",
                               false, false, 32 * 1024 * 1024, NULL);

    mca_mpool_sm_component.sm_size = 0;

    return OMPI_SUCCESS;
}

static int mca_mpool_sm_close( void )
{
    if( NULL != mca_common_sm_mmap ) {
        if( OMPI_SUCCESS == mca_common_sm_mmap_fini( mca_common_sm_mmap ) ) {
            unlink( mca_common_sm_mmap->map_path );
        }
        OBJ_RELEASE( mca_common_sm_mmap );
    }
    return OMPI_SUCCESS;
}

static mca_mpool_base_module_t* mca_mpool_sm_init(
    struct mca_mpool_base_resources_t* resources)
{
    char *file_name;
    int len;
    mca_mpool_sm_module_t* mpool_module; 
    mca_allocator_base_component_t* allocator_component; 
    int max_size, min_size, peer_size;
    ompi_proc_t **procs;
    size_t num_all_procs, i, num_local_procs = 0;

    /* determine size of shared memory file */
    mca_base_param_lookup_int(max_size_param, &max_size);
    mca_base_param_lookup_int(min_size_param, &min_size);
    mca_base_param_lookup_int(peer_size_param, &peer_size);

    /* README: this needs to change if procs in different jobs (even
       spawned ones) are to talk using shared memory */
    procs = ompi_proc_world(&num_all_procs);
    for (i = 0 ; i < num_all_procs ; ++i) {
        if (procs[i]->proc_flags & OMPI_PROC_FLAG_LOCAL) {
            num_local_procs++;
        }
    }

    if (min_size > max_size) {
        opal_output(0, "mca_mpool_sm_init: adjusting max_size to be min_size (%d)",
                    min_size);
        max_size = min_size;
    }

    /* set sm_size based on num_procs, then adjust from there */
    mca_mpool_sm_component.sm_size = peer_size * num_local_procs;
    if ((size_t) min_size > mca_mpool_sm_component.sm_size) {
        mca_mpool_sm_component.sm_size = min_size;
    }
    if ((size_t) max_size < mca_mpool_sm_component.sm_size) {
        mca_mpool_sm_component.sm_size = max_size;
    }

    allocator_component = mca_allocator_component_lookup(
        mca_mpool_sm_component.sm_allocator_name);

    /* if specified allocator cannout be loaded - look for an alternative */
    if(NULL == allocator_component) {
        if(opal_list_get_size(&mca_allocator_base_components) == 0) {
            mca_base_component_list_item_t* item = (mca_base_component_list_item_t*)
                opal_list_get_first(&mca_allocator_base_components);
            allocator_component = (mca_allocator_base_component_t*)item->cli_component;
            opal_output(0, "mca_mpool_sm_init: unable to locate allocator: %s - using %s\n",
                mca_mpool_sm_component.sm_allocator_name, allocator_component->allocator_version.mca_component_name);
        } else {
            opal_output(0, "mca_mpool_sm_init: unable to locate allocator: %s\n",
                mca_mpool_sm_component.sm_allocator_name);
            return NULL;
        }
    }
    
    
    mpool_module = (mca_mpool_sm_module_t*)malloc(sizeof(mca_mpool_sm_module_t)); 
    mca_mpool_sm_module_init(mpool_module); 

    /* create initial shared memory mapping */
    len = asprintf( &file_name, "%s"OPAL_PATH_SEP"shared_mem_pool.%s",
                    orte_process_info.job_session_dir,
                    orte_system_info.nodename );
    if ( 0 > len ) {
        return NULL;
    }

    if(NULL == 
            (mca_common_sm_mmap = 
             mca_common_sm_mmap_init(mca_mpool_sm_component.sm_size,
                 file_name,sizeof(mca_common_sm_mmap_t), 8 )
             )) 
    {
        opal_output(0, "mca_mpool_sm_init: unable to create shared memory mapping (%s)", file_name);
        free(file_name);
        return NULL;
    }
    free(file_name);

    /* setup allocator */
    mpool_module->sm_allocator = 
      allocator_component->allocator_init(true,
                                          mca_common_sm_mmap_seg_alloc, NULL, NULL);
    if(NULL == mpool_module->sm_allocator) {
      opal_output(0, "mca_mpool_sm_init: unable to initialize allocator");
        return NULL;
    }
   
    return &mpool_module->super;
}

