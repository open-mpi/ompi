/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * $HEADER$
 */
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "ptl_self.h"

mca_ptl_self_module_1_0_0_t mca_ptl_self_module = {
    {
        /* First, the mca_base_module_t struct containing meta information
           about the module itself */
        
        {
            /* Indicate that we are a pml v1.0.0 module (which also implies a
               specific MCA version) */
            
            MCA_PTL_BASE_VERSION_1_0_0,
            
            "self", /* MCA module name */
            1,  /* MCA module major version */
            0,  /* MCA module minor version */
            0,  /* MCA module release version */
            mca_ptl_self_module_open,  /* module open */
            mca_ptl_self_module_close  /* module close */
        },
        
        /* Next the MCA v1.0.0 module meta data */
        
        {
            /* Whether the module is checkpointable or not */
            true
        },
        
        mca_ptl_self_module_init,  
        NULL,
        NULL,
    }
};

static void mca_ptl_self_send_request_construct(mca_ptl_self_send_request_t* request)
{
    OBJ_CONSTRUCT(&request->req_frag, mca_ptl_base_recv_frag_t);
}

static void mca_ptl_self_send_request_destruct(mca_ptl_self_send_request_t* request)
{
    OBJ_DESTRUCT(&request->req_frag);
}

OBJ_CLASS_INSTANCE( mca_ptl_self_send_request_t, 
                    mca_pml_base_send_request_t,
                    mca_ptl_self_send_request_construct,
                    mca_ptl_self_send_request_destruct );

/*
 * utility routines for parameter registration
 */

static inline int mca_ptl_self_param_register_int(
   const char* param_name, 
   int default_value )
{
   int id = mca_base_param_register_int("ptl","self",param_name,NULL,default_value);
   int param_value = default_value;
   mca_base_param_lookup_int(id,&param_value);
   return param_value;
}
                                                                                                                            
/*
 *  Called by MCA framework to open the module, registers
 *  module parameters.
 */

int mca_ptl_self_module_open(void)
{
    /* initialize state */
    mca_ptl_self_module.self_ptls = NULL;
    mca_ptl_self_module.self_num_ptls = 0;
    
    /* initialize objects */

    /* register SELF module parameters */
    mca_ptl_self_module.self_buf_size =
        mca_ptl_self_param_register_int("buffer_size", 64*1024);
    mca_ptl_self_module.self_is_non_blocking =
        mca_ptl_self_param_register_int("nonblocking", 1);
    return OMPI_SUCCESS;
}

int mca_ptl_self_module_close(void)
{
    if (mca_ptl_self_module.self_send_requests.fl_num_allocated != 
        mca_ptl_self_module.self_send_requests.super.ompi_list_length) {
        ompi_output(0, "self send requests: %d allocated %d returned\n",
            mca_ptl_self_module.self_send_requests.fl_num_allocated, 
            mca_ptl_self_module.self_send_requests.super.ompi_list_length);
    }

    if (NULL != mca_ptl_self_module.self_ptls) {
        free(mca_ptl_self_module.self_ptls);
        OBJ_DESTRUCT( &(mca_ptl_self_module.self_send_requests) );
    }
    return OMPI_SUCCESS;
}

extern mca_ptl_t mca_ptl_self;
mca_ptl_t** mca_ptl_self_module_init(int *num_ptls,
                                     bool *allow_multi_user_threads,
                                     bool *have_hidden_threads)
{
    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    mca_ptl_self_module.self_ptls = malloc(sizeof(mca_ptl_t*));
    if( NULL == mca_ptl_self_module.self_ptls )
        return NULL;
    mca_ptl_self_module.self_ptls[0] = &mca_ptl_self;
    mca_ptl_self_module.self_num_ptls = 1;
    mca_ptl_self_module.self_max_ptls = 1;
    mca_ptl_self_module.self_free_list_num = 4;
    mca_ptl_self_module.self_free_list_max = -1;
    mca_ptl_self_module.self_free_list_inc = 4;
    *num_ptls = 1;

    OBJ_CONSTRUCT(&mca_ptl_self_module.self_send_requests, ompi_free_list_t);
    ompi_free_list_init(&mca_ptl_self_module.self_send_requests, 
                        sizeof(mca_ptl_self_send_request_t),
                        OBJ_CLASS(mca_ptl_self_send_request_t),
                        mca_ptl_self_module.self_free_list_num,
                        mca_ptl_self_module.self_free_list_max,
                        mca_ptl_self_module.self_free_list_inc,
                        NULL); /* use default allocator */

    return mca_ptl_self_module.self_ptls;
}
