#ifndef MCA_PTL_IB_VAPI_H
#define MCA_PTL_IB_VAPI_H

#include <vapi.h>
#include <mpga.h>
#include <mtl_common.h>
#include <vapi_common.h>

/* HACK: Alert, these are dumb defines,
 * all this stuff should be runtime. Ignoring for now.
 */

#define DEFAULT_PORT        (1)
#define DEFAULT_CQ_SIZE     (40000)
#define DEFAULT_UD_WQ_SIZE  (10000)
#define DEFAULT_UD_SG_LIST  (1)
#define DEFAULT_PKEY_IX     (0)
#define DEFAULT_PSN         (0)

/* This is a convinence macro.
 * 
 * ret : The value to return if call failed
 * vapi_ret : The value which was returned from the last VAPI call
 * func_name : The VAPI function which was called
 */
#define MCA_PTL_IB_VAPI_RET(vapi_ret, func_name) {                  \
    ompi_output(0,"[%s:%d] ", __FILE__, __LINE__);                  \
    ompi_output(0,"%s : %s",func_name,VAPI_strerror(vapi_ret));     \
}

/* Debug Print */
#if 1
#define D_PRINT(fmt, args...) {                                     \
    ompi_output(0, "[%s:%d:%s] " fmt, __FILE__, __LINE__, __func__, \
        ##args);                                                    \
}
#else
#define D_PRINT(fmt, args...)
#endif


#endif
