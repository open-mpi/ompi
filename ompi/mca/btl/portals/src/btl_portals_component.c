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

#include <sys/types.h>
#include <unistd.h>

#include "include/constants.h"

#include "opal/util/output.h"
#include "opal/threads/thread.h"

#include "btl_portals.h"
#include "btl_portals_compat.h"
#include "btl_portals_frag.h"
#include "btl_portals_send.h"
#include "btl_portals_recv.h"
#include "btl_portals_rdma.h"


mca_btl_portals_component_t mca_btl_portals_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_BTL_BASE_VERSION_1_0_0,

        "portals", /* MCA module name */
        OMPI_MAJOR_VERSION,  /* MCA module major version */
        OMPI_MINOR_VERSION,  /* MCA module minor version */
        OMPI_RELEASE_VERSION,  /* MCA module release version */
        mca_btl_portals_component_open,  /* module open */
        mca_btl_portals_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_btl_portals_component_init,  
      mca_btl_portals_component_progress,
    }
};


static opal_output_stream_t portals_output_stream = {
    true,  /* is debugging */
    0,     /* verbose level */
    0,     /* want syslog */
    0,     /* syslog priority */
    NULL,  /* syslog ident */
    NULL,  /* prefix */
    true, /* want stdout */
    false,  /* want stderr */
    false, /* want file */
    false, /* file append */
    "btl-portals"   /* file suffix */
};


static inline char*
param_register_string(const char* param_name, 
                                 const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl", "portals",
                                            param_name, NULL, 
                                            default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}


static inline int
param_register_int(const char* param_name, 
                              int default_value)
{
    int id = mca_base_param_register_int("btl", "portals", param_name,
                                         NULL, default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}



int
mca_btl_portals_component_open(void)
{
    int i;

    /* initialize component state */
    mca_btl_portals_component.portals_num_modules = 0;
    mca_btl_portals_component.portals_modules = NULL;

    /* initalize component objects */
    OBJ_CONSTRUCT(&mca_btl_portals_component.portals_lock,
                  opal_mutex_t);

    /* get configured state for component */
#if BTL_PORTALS_UTCP
    mca_btl_portals_component.portals_ifname = 
        param_register_string("ifname", "eth0");
#endif
    mca_btl_portals_component.portals_free_list_init_num = 
        param_register_int("free_list_init_num",
                           BTL_PORTALS_DEFAULT_FREE_LIST_INIT_NUM);
    mca_btl_portals_component.portals_free_list_max_num = 
        param_register_int("free_list_max_num",
                           BTL_PORTALS_DEFAULT_FREE_LIST_MAX_NUM);
    mca_btl_portals_component.portals_free_list_inc_num = 
        param_register_int("free_list_inc_num",
                           BTL_PORTALS_DEFAULT_FREE_LIST_INC_NUM);

    /* start up debugging output */
    portals_output_stream.lds_verbose_level = 
        param_register_int("debug_level",
                           BTL_PORTALS_DEFAULT_DEBUG_LEVEL);
    asprintf(&(portals_output_stream.lds_prefix), 
             "btl: portals (%5d): ", getpid());
    mca_btl_portals_component.portals_output = 
        opal_output_open(&portals_output_stream);

    /* fill default module state */
    mca_btl_portals_module.super.btl_eager_limit = 
        param_register_int("eager_limit",
                           BTL_PORTALS_DEFAULT_EAGER_LIMIT);
    mca_btl_portals_module.super.btl_min_send_size = 
        param_register_int("min_send_size",
                           BTL_PORTALS_DEFAULT_MIN_SEND_SIZE);
    mca_btl_portals_module.super.btl_max_send_size = 
        param_register_int("max_send_size",
                           BTL_PORTALS_DEFAULT_MAX_SEND_SIZE);
    mca_btl_portals_module.super.btl_min_rdma_size = 
        param_register_int("min_rdma_size",
                           BTL_PORTALS_DEFAULT_MIN_RDMA_SIZE);
    mca_btl_portals_module.super.btl_max_rdma_size = 
        param_register_int("max_rdma_size",
                           BTL_PORTALS_DEFAULT_MAX_RDMA_SIZE);
    mca_btl_portals_module.super.btl_exclusivity = 
        param_register_int("exclusivity", 60);
    mca_btl_portals_module.super.btl_latency = 
        param_register_int("latency", 0);
    mca_btl_portals_module.super.btl_bandwidth = 
        param_register_int("bandwidth", 1000);

    mca_btl_portals_module.super.btl_flags = MCA_BTL_FLAGS_SEND;

    bzero(&(mca_btl_portals_module.portals_reg),
          sizeof(mca_btl_portals_module.portals_reg));

    for (i = 0 ; i < MCA_BTL_PORTALS_EQ_SIZE ; ++i) {
        mca_btl_portals_module.portals_eq_sizes[i] = 0;
        mca_btl_portals_module.portals_eq_handles[i] = PTL_EQ_NONE;
    }
    /* eq handles will be created when the module is instantiated.
       Set sizes here */
    mca_btl_portals_module.portals_eq_sizes[MCA_BTL_PORTALS_EQ_RECV] =
        param_register_int("eq_recv_size", BTL_PORTALS_DEFAULT_RECV_QUEUE_SIZE);
    /* sends_pending * 3 for start, end, ack */
    mca_btl_portals_module.portals_eq_sizes[MCA_BTL_PORTALS_EQ_SEND] = 
        param_register_int("eq_send_max_pending", BTL_PORTALS_MAX_SENDS_PENDING) * 3;
    mca_btl_portals_module.portals_eq_sizes[MCA_BTL_PORTALS_EQ_RDMA] =
        param_register_int("eq_rdma_size", 512); /* BWB - FIXME - make param */

    mca_btl_portals_module.portals_recv_reject_me_h = PTL_INVALID_HANDLE;

    mca_btl_portals_module.portals_recv_mds_num = 
        param_register_int("recv_md_num", 3); /* BWB - FIXME - make param */
    mca_btl_portals_module.portals_recv_mds_size =
        param_register_int("recv_md_size", 524288); /* BWB - FIXME - make param */

    mca_btl_portals_module.portals_ni_h = PTL_INVALID_HANDLE;
    mca_btl_portals_module.portals_sr_dropped = 0;
    mca_btl_portals_module.portals_outstanding_sends = 0;

    return OMPI_SUCCESS;
}


int
mca_btl_portals_component_close(void)
{
    /* release resources */
    OBJ_DESTRUCT(&mca_btl_portals_component.portals_lock);

    if (NULL != mca_btl_portals_component.portals_modules) {
        free(mca_btl_portals_component.portals_modules);
    }

#if BTL_PORTALS_UTCP
    if (NULL != mca_btl_portals_component.portals_ifname) {
        free(mca_btl_portals_component.portals_ifname);
    }
#endif

    if (NULL != portals_output_stream.lds_prefix) {
        free(portals_output_stream.lds_prefix);
    }

    /* close debugging stream */
    opal_output_close(mca_btl_portals_component.portals_output);
    mca_btl_portals_component.portals_output = -1;

    return OMPI_SUCCESS;
}


mca_btl_base_module_t**
mca_btl_portals_component_init(int *num_btls, 
                               bool enable_progress_threads,
                               bool enable_mpi_threads)
{
    mca_btl_base_module_t** btls;
    uint32_t i;

    *num_btls = 0;

    if (enable_progress_threads || enable_mpi_threads) {
        opal_output_verbose(20, mca_btl_portals_component.portals_output,
                            "disabled because threads enabled");
        return NULL;
    }

    /* initialize portals btl.  note that this is in the compat code because
       it's fairly non-portable between implementations */
    if (OMPI_SUCCESS != mca_btl_portals_init_compat(&mca_btl_portals_component)) {
        opal_output_verbose(20, mca_btl_portals_component.portals_output,
                            "disabled because compatibility init failed");
        return NULL;
    }

    /* create an array of btl* to return */
    btls = malloc(mca_btl_portals_component.portals_num_modules *
                  sizeof(mca_btl_portals_module_t*));

    /* fill in all the portable parts of the module structs - the
       compat code filled in the other bits already */
    for (i = 0 ; i < mca_btl_portals_component.portals_num_modules ; ++i) {
        mca_btl_portals_module_t* ptl_btl = 
            (mca_btl_portals_component.portals_modules + i);
        btls[i] = (mca_btl_base_module_t*) ptl_btl;
        

        OBJ_CONSTRUCT(&(ptl_btl->portals_frag_eager), ompi_free_list_t);
        OBJ_CONSTRUCT(&(ptl_btl->portals_frag_max), ompi_free_list_t);
        OBJ_CONSTRUCT(&(ptl_btl->portals_frag_user), ompi_free_list_t);

        /* eager frags */
        ompi_free_list_init(&(ptl_btl->portals_frag_eager),
                            sizeof(mca_btl_portals_frag_eager_t) + 
                            ptl_btl->super.btl_eager_limit,
                            OBJ_CLASS(mca_btl_portals_frag_eager_t),
                            mca_btl_portals_component.portals_free_list_init_num,
                            mca_btl_portals_component.portals_free_list_max_num,
                            mca_btl_portals_component.portals_free_list_inc_num,
                            NULL);

        /* send frags */
        ompi_free_list_init(&(ptl_btl->portals_frag_max),
                            sizeof(mca_btl_portals_frag_max_t) + 
                            ptl_btl->super.btl_max_send_size,
                            OBJ_CLASS(mca_btl_portals_frag_max_t),
                            mca_btl_portals_component.portals_free_list_init_num,
                            mca_btl_portals_component.portals_free_list_max_num,
                            mca_btl_portals_component.portals_free_list_inc_num,
                            NULL);

        /* user frags */
        ompi_free_list_init(&(ptl_btl->portals_frag_user),
                            sizeof(mca_btl_portals_frag_user_t),
                            OBJ_CLASS(mca_btl_portals_frag_user_t),
                            mca_btl_portals_component.portals_free_list_init_num,
                            mca_btl_portals_component.portals_free_list_max_num,
                            mca_btl_portals_component.portals_free_list_inc_num,
                            NULL);

        /* endpoint list */
        OBJ_CONSTRUCT(&(ptl_btl->portals_endpoint_list), opal_list_t);

        /* receive chunk list */
        OBJ_CONSTRUCT(&(ptl_btl->portals_recv_chunks), opal_list_t);

        /* lock */
        OBJ_CONSTRUCT(&(ptl_btl->portals_lock), opal_mutex_t);
    }
    *num_btls = mca_btl_portals_component.portals_num_modules;

    opal_output_verbose(20, mca_btl_portals_component.portals_output,
                        "initialized %d modules",
                        *num_btls);

    return btls;
}


int
mca_btl_portals_component_progress(void)
{
    int num_progressed = 0;
    size_t i;

    for (i = 0 ; i < mca_btl_portals_component.portals_num_modules ; ++i) {
        struct mca_btl_portals_module_t *module = 
            &(mca_btl_portals_component.portals_modules)[i];
        ptl_event_t ev;
        ptl_sr_value_t numdropped;
        int which;
        int ret;

        if (module->portals_eq_handles[MCA_BTL_PORTALS_EQ_SIZE - 1] == 
            PTL_EQ_NONE) continue; /* they are all initialized at once */

#if OMPI_ENABLE_DEBUG
        /* check for dropped packets.  In theory, our protocol covers
           this, but it can't hurt to check while we're debugging */
        PtlNIStatus(module->portals_ni_h,
                    PTL_SR_DROP_COUNT,
                    &numdropped);
        if (numdropped != module->portals_sr_dropped) {
            opal_output_verbose(30, mca_btl_portals_component.portals_output,
                                "*** Dropped message count changed.  %lld, %lld",
                                module->portals_sr_dropped, numdropped);
            module->portals_sr_dropped = numdropped;
        }
#endif

        ret = PtlEQPoll(module->portals_eq_handles,
                        MCA_BTL_PORTALS_EQ_SIZE, /* number of eq handles */
                        10, /* poll time */
                        &ev,
                        &which);
        if (PTL_EQ_EMPTY == ret) {
            /* nothing to see here - move along */
            continue;
        } else if (!(PTL_OK == ret || PTL_EQ_DROPPED == ret)) {
            /* BWB - how can we report errors? */
            opal_output(mca_btl_portals_component.portals_output,
                        "Error calling PtlEQGet: %d", ret);
            continue;
        } else if (PTL_EQ_DROPPED == ret) {
            opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                "*** Event queue entries were dropped");
        }

        switch (which) {
        case MCA_BTL_PORTALS_EQ_RECV:
            mca_btl_portals_process_recv(module, &ev);
            break;
        case MCA_BTL_PORTALS_EQ_SEND:
            mca_btl_portals_process_send(module, &ev);
            break;
        case MCA_BTL_PORTALS_EQ_RDMA:
            mca_btl_portals_process_rdma(module, &ev);
            break;
        default:
            abort();
        }

        num_progressed++;
    }

    return num_progressed;
}

