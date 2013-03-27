/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Myricom. All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "ompi/constants.h"
#include "common_mx.h"

#include <errno.h>
#include "opal/memoryhooks/memory.h"
#include "opal/util/output.h"
#include "ompi/runtime/params.h"


int mx__regcache_clean(void *ptr, size_t size);

static int ompi_common_mx_initialize_ref_cnt = 0;
static int ompi_common_mx_available = 0;
static int ompi_common_mx_memory_cb_registered = 0;

static void
memory_release_cb(void *buf, size_t length, void *cbdata, bool from_alloc)
{
  mx__regcache_clean(buf, length);
}


int
ompi_common_mx_initialize(void)
{
    mx_return_t mx_return;
    int ret = OMPI_SUCCESS;
    
    ompi_common_mx_initialize_ref_cnt++;
    
    if(ompi_common_mx_initialize_ref_cnt == 1) { 
        /* set the MX error handle to always return. This function is the
         * only MX function allowed to be called before mx_init in order
         * to make sure that if the MX is not up and running the MX
         * library does not exit the application.
         */
        mx_set_error_handler(MX_ERRORS_RETURN);
	
	/* If we have a memory manager available, then tell MX to use
	   the rcache */
	if (0 != (OPAL_MEMORY_FREE_SUPPORT & opal_mem_hooks_support_level())) {
	  if (OMPI_SUCCESS == 
	      opal_mem_hooks_register_release(memory_release_cb, NULL)) {
	    ompi_common_mx_memory_cb_registered = 1;
	    setenv("MX_RCACHE", "2", 1);
	  }
	}
	
        /* initialize the mx library */
        mx_return = mx_init(); 
        
        if(MX_SUCCESS != mx_return) {
            ompi_common_mx_available = -1;
	    if (0 != ompi_common_mx_memory_cb_registered) {
	      opal_mem_hooks_unregister_release(memory_release_cb);
	      ompi_common_mx_memory_cb_registered = 0;
	    }
            opal_output(0,
                        "Error in mx_init (error %s)\n",
                        mx_strerror(mx_return));
            /* We did not succeed to initialize the MX device */
            ompi_common_mx_initialize_ref_cnt = 0;
            return OMPI_ERR_NOT_AVAILABLE;
        }
        ompi_common_mx_available = 1;
    } else if (ompi_common_mx_available < 0) {
        ret = OMPI_ERR_NOT_AVAILABLE;
    }

    return ret;
} 

int
ompi_common_mx_finalize(void)
{
    ompi_common_mx_initialize_ref_cnt--;
    if( 0 == ompi_common_mx_initialize_ref_cnt ) { 
        mx_return_t mx_return;

	if (0 != ompi_common_mx_memory_cb_registered) {
	  opal_mem_hooks_unregister_release(memory_release_cb);
	  ompi_common_mx_memory_cb_registered = 0;
	}
        
        mx_return = mx_finalize(); 
        if(mx_return != MX_SUCCESS){ 
            opal_output(0, "Error in mx_finalize (error %s)\n", mx_strerror(mx_return));
            return OMPI_ERROR;
        } 
    }
    return OMPI_SUCCESS;
}
