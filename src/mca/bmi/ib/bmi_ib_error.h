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

#ifndef MCA_BMI_IB_ERROR_H
#define MCA_BMI_IB_ERROR_H

#include <vapi.h>
#include <mtl_common.h>
#include <vapi_common.h>

/* 
 * 
 * 
 */ 
#define MCA_BMI_IB_VAPI_ERROR(vapi_ret, func_name) {                  \
    ompi_output(0,"[%s:%d] ", __FILE__, __LINE__);                  \
    ompi_output(0,"%s : %s",func_name,VAPI_strerror(vapi_ret));     \
}

/* Debug Print */
#if 0
#define DEBUG_OUT(fmt, args...) {                                     \
    ompi_output(0, "[%s:%d:%s] " fmt, __FILE__, __LINE__, __func__, \
        ##args);                                                    \
}
#else
#define DEBUG_OUT(fmt, args...)
#endif

#endif
