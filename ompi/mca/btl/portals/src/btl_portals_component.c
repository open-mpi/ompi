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

int
mca_btl_portals_component_open(void)
{
    int i;
    int dummy;

    /* initialize component state */
    mca_btl_portals_component.portals_num_modules = 0;
    mca_btl_portals_component.portals_modules = NULL;

    /* initalize component objects */
    OBJ_CONSTRUCT(&mca_btl_portals_component.portals_lock,
                  opal_mutex_t);

    /* get configured state for component */
#if OMPI_BTL_PORTALS_UTCP
    mca_base_param_reg_string(&mca_btl_portals_component.super.btl_version,
                              "ifname",
                              "Interface name to use for communication",
                              false,
                              false,
                              "eth0",
                              &(mca_btl_portals_component.portals_ifname));
#endif

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "free_list_init_num",
                           "Initial number of elements to initialize in free lists",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_FREE_LIST_INIT_NUM,
                           &(mca_btl_portals_component.portals_free_list_init_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "free_list_max_num",
                           "Max number of elements to initialize in free lists",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_FREE_LIST_MAX_NUM,
                           &(mca_btl_portals_component.portals_free_list_max_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "free_list_inc_num",
                           "Increment count for free lists",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_FREE_LIST_INC_NUM,
                           &(mca_btl_portals_component.portals_free_list_inc_num));

    /* start up debugging output */
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "debug_level",
                           "Debugging verbosity (0 - 100)",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_DEBUG_LEVEL,
                           &(portals_output_stream.lds_verbose_level));
    asprintf(&(portals_output_stream.lds_prefix), 
             "btl: portals (%5d): ", getpid());
    mca_btl_portals_component.portals_output = 
        opal_output_open(&portals_output_stream);

    /* fill default module state */
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eager_limit",
                           "Maximum size for eager frag",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_EAGER_LIMIT,
                           &dummy);
    mca_btl_portals_module.super.btl_eager_limit = dummy;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "min_send_size",
                           "Minimum size for a send frag",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_MIN_SEND_SIZE,
                           &dummy);
    mca_btl_portals_module.super.btl_min_send_size = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "max_send_size",
                           "Maximum size for a send frag",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_MAX_SEND_SIZE,
                           &dummy);
    mca_btl_portals_module.super.btl_max_send_size = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "min_rdma_size",
                           "Minimum size for a rdma frag",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_MIN_RDMA_SIZE,
                           &dummy);
    mca_btl_portals_module.super.btl_min_rdma_size = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "max_rdma_size",
                           "Maximum size for a rdma frag",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_MAX_RDMA_SIZE,
                           &dummy);
    mca_btl_portals_module.super.btl_max_rdma_size = dummy;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "exclusivity",
                           "Exclusivity level for selection process",
                           false,
                           false,
                           60,
                           &dummy);
    mca_btl_portals_module.super.btl_exclusivity = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "latency",
                           "Latency level for short message scheduling",
                           false,
                           false,
                           0,
                           &dummy);
    mca_btl_portals_module.super.btl_latency = dummy;
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "bandwidth",
                           "Bandwidth level for frag scheduling",
                           false,
                           false,
                           1000,
                           &dummy);
    mca_btl_portals_module.super.btl_bandwidth = dummy;

    mca_btl_portals_module.super.btl_flags = MCA_BTL_FLAGS_RDMA;

    bzero(&(mca_btl_portals_module.portals_reg),
          sizeof(mca_btl_portals_module.portals_reg));

    for (i = 0 ; i < OMPI_BTL_PORTALS_EQ_SIZE ; ++i) {
        mca_btl_portals_module.portals_eq_sizes[i] = 0;
        mca_btl_portals_module.portals_eq_handles[i] = PTL_EQ_NONE;
    }
    /* eq handles will be created when the module is instantiated.
       Set sizes here */
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eq_recv_size",
                           "Size of the receive event queue",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_RECV_QUEUE_SIZE,
                           &(mca_btl_portals_module.portals_eq_sizes[OMPI_BTL_PORTALS_EQ_RECV]));

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eq_send_max_pending",
                           "Maximum number of pending send frags",
                           false,
                           false,
                           OMPI_BTL_PORTALS_MAX_SENDS_PENDING,
                           &(mca_btl_portals_module.portals_max_outstanding_sends));
    /* sends_pending * 3 for start, end, ack */
    mca_btl_portals_module.portals_eq_sizes[OMPI_BTL_PORTALS_EQ_SEND] = 
        mca_btl_portals_module.portals_max_outstanding_sends * 3;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "eq_rdma_size",
                           "Size of the rdma event queue",
                           false,
                           false,
                           512,
                           &(mca_btl_portals_module.portals_eq_sizes[OMPI_BTL_PORTALS_EQ_RDMA]));

    mca_btl_portals_module.portals_recv_reject_me_h = PTL_INVALID_HANDLE;

    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "recv_md_num",
                           "Number of send frag receive descriptors",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_RECV_MD_NUM,
                           &(mca_btl_portals_module.portals_recv_mds_num));
    mca_base_param_reg_int(&mca_btl_portals_component.super.btl_version,
                           "recv_md_size",
                           "Size of send frag receive descriptors",
                           false,
                           false,
                           OMPI_BTL_PORTALS_DEFAULT_RECV_MD_SIZE,
                           &(mca_btl_portals_module.portals_recv_mds_size));

    mca_btl_portals_module.portals_ni_h = PTL_INVALID_HANDLE;
    mca_btl_portals_module.portals_sr_dropped = 0;
    mca_btl_portals_module.portals_outstanding_sends = 0;
    mca_btl_portals_module.portals_rdma_key = 1;

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

#if OMPI_BTL_PORTALS_UTCP
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

    if (enable_progress_threads) {
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

        /* pending sends */
        OBJ_CONSTRUCT(&(ptl_btl->portals_queued_sends), opal_list_t);

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

        if (module->portals_eq_handles[OMPI_BTL_PORTALS_EQ_SIZE - 1] == 
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

        while (true) {
            ret = PtlEQPoll(module->portals_eq_handles,
                            OMPI_BTL_PORTALS_EQ_SIZE, /* number of eq handles */
                            0, /* poll time */
                            &ev,
                            &which);
            if (PTL_EQ_EMPTY == ret) {
                /* nothing to see here - move along */
                mca_btl_portals_progress_queued_sends(module);
                break;
            } else if (!(PTL_OK == ret || PTL_EQ_DROPPED == ret)) {
                /* BWB - how can we report errors? */
                opal_output(mca_btl_portals_component.portals_output,
                            "*** Error calling PtlEQGet: %d ***", ret);
                break;
            } else if (PTL_EQ_DROPPED == ret) {
                opal_output_verbose(10, mca_btl_portals_component.portals_output,
                                    "*** Event queue entries were dropped ***");
            }

            switch (which) {
            case OMPI_BTL_PORTALS_EQ_RECV:
                mca_btl_portals_process_recv(module, &ev);
                break;
            case OMPI_BTL_PORTALS_EQ_SEND:
                mca_btl_portals_process_send(module, &ev);
                break;
            case OMPI_BTL_PORTALS_EQ_RDMA:
                mca_btl_portals_process_rdma(module, &ev);
                break;
            default:
                abort();
                break;
            }

            num_progressed++;
        }
    }

    return num_progressed;
}

