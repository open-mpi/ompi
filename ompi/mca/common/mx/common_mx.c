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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "opal/util/output.h"
#include "ompi/constants.h"
#include "common_mx.h"

int ompi_common_mx_initialize_ref_cnt = 0;
int
ompi_common_mx_initialize(void)
{
    mx_return_t mx_return;
    ompi_common_mx_initialize_ref_cnt++;
    
    if(ompi_common_mx_initialize_ref_cnt == 1) { 
        /* set the MX error handle to always return. This function is the
         * only MX function allowed to be called before mx_init in order
         * to make sure that if the MX is not up and running the MX
         * library does not exit the application.
         */
        mx_set_error_handler(MX_ERRORS_RETURN);
        
        /* initialize the mx library */
        mx_return = mx_init(); 
        
        if(MX_SUCCESS != mx_return){
            opal_output(0,
                        "Error in mx_init (error %s)\n",
                        mx_strerror(mx_return));
            return OMPI_ERR_NOT_AVAILABLE;
        }
        
    } 
    return OMPI_SUCCESS;
}


int
ompi_common_mx_finalize(void)
{
    mx_return_t mx_return;
    ompi_common_mx_initialize_ref_cnt--;
    if(ompi_common_mx_initialize == 0) { 
        mx_return = mx_finalize(); 
        if(mx_return != MX_SUCCESS){ 
            opal_output(0, "Error in mx_finalize (error %s)\n", mx_strerror(mx_return));
            return OMPI_ERROR;
        } 
    } else {
        return OMPI_SUCCESS;
    }
}
