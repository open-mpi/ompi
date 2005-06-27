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
#include "portals_config.h"

#include <sys/types.h>
#include <unistd.h>

#include "include/constants.h"

#include "util/output.h"
#include "threads/thread.h"

#include "bmi_portals.h"
#include "bmi_portals_compat.h"


mca_bmi_portals_component_t mca_bmi_portals_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_BMI_BASE_VERSION_1_0_0,

        "portals", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_bmi_portals_component_open,  /* module open */
        mca_bmi_portals_component_close  /* module close */
      },
      
      /* Next the MCA v1.0.0 module meta data */
      
      {
        /* Whether the module is checkpointable or not */
        
        false
      },
      
      mca_bmi_portals_component_init,  
      mca_bmi_portals_component_progress,
    }
};


static ompi_output_stream_t portals_output_stream = {
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
    "bmi-portals"   /* file suffix */
};


static inline char*
param_register_string(const char* param_name, 
                                 const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("bmi", "portals",
                                            param_name, NULL, 
                                            default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}


static inline int
param_register_int(const char* param_name, 
                              int default_value)
{
    int id = mca_base_param_register_int("bmi", "portals", param_name,
                                         NULL, default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id, &param_value);
    return param_value;
}



int
mca_bmi_portals_component_open(void)
{
    int i;

    /* initialize state */
    mca_bmi_portals_component.portals_num_modules = 0;
    mca_bmi_portals_component.portals_modules = NULL;

    /* initialize objects */
#if 0
    OBJ_CONSTRUCT(&mca_bmi_portals_component.portals_send_frags, 
                  ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_bmi_portals_component.portals_recv_frags, 
                  ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_bmi_portals_component.portals_pending_acks, 
                  ompi_list_t);
#endif
    OBJ_CONSTRUCT(&mca_bmi_portals_component.portals_lock, 
                  ompi_mutex_t);

    /* register portals module parameters */
#if BMI_PORTALS_UTCP
    mca_bmi_portals_component.portals_ifname = 
        param_register_string("ifname", "eth0");
#endif
    portals_output_stream.lds_verbose_level = 
        param_register_int("debug_level",
                           BMI_PORTALS_DEFAULT_DEBUG_LEVEL);

    mca_bmi_portals_component.portals_free_list_init_num = 
        param_register_int("free_list_init_num",
                           BMI_PORTALS_DEFAULT_FREE_LIST_INIT_NUM);
    mca_bmi_portals_component.portals_free_list_max_num = 
        param_register_int("free_list_max_num",
                           BMI_PORTALS_DEFAULT_FREE_LIST_MAX_NUM);
    mca_bmi_portals_component.portals_free_list_inc_num = 
        param_register_int("free_list_inc_num",
                           BMI_PORTALS_DEFAULT_FREE_LIST_inc_NUM);

#if 0
    mca_bmi_portals_module.super.bmi_cache_size =
        param_register_int("request_cache_size",
                           BMI_PORTALS_DEFAULT_REQUEST_CACHE_SIZE);
    mca_bmi_portals_module.super.bmi_first_frag_size =
        param_register_int("first_frag_size",
                           BMI_PORTALS_DEFAULT_FIRST_FRAG_SIZE);
    mca_bmi_portals_module.super.bmi_min_frag_size =
        param_register_int("rndv_frag_min_size",
                           BMI_PORTALS_DEFAULT_RNDV_FRAG_MIN_SIZE);
    mca_bmi_portals_module.super.bmi_max_frag_size =
        param_register_int("rndv_frag_max_size",
                           BMI_PORTALS_DEFAULT_RNDV_FRAG_MAX_SIZE);
#endif

    mca_bmi_portals_module.first_frag_num_entries = 
        param_register_int("first_frag_num_entries",
                           BMI_PORTALS_DEFAULT_FIRST_FRAG_NUM_ENTRIES);
    mca_bmi_portals_module.first_frag_entry_size = 
        param_register_int("first_frag_entry_size",
                           BMI_PORTALS_DEFAULT_FIRST_FRAG_ENTRY_SIZE);

    mca_bmi_portals_module.eq_sizes[MCA_BMI_PORTALS_EQ_RECV] = 
        param_register_int("recv_queue_size",
                           BMI_PORTALS_DEFAULT_RECV_QUEUE_SIZE);
    mca_bmi_portals_module.eq_sizes[MCA_BMI_PORTALS_EQ_SEND] = 
        (param_register_int("send_queue_size",
                            BMI_PORTALS_DEFAULT_SEND_QUEUE_SIZE)) * 3;

    /* finish with objects */
    asprintf(&(portals_output_stream.lds_prefix), 
             "bmi: portals (%5d): ", getpid());

    mca_bmi_portals_component.portals_output = 
        ompi_output_open(&portals_output_stream);

    /* fill in remaining defaults for module data */
    for (i = 0 ; i < MCA_BMI_PORTALS_EQ_SIZE ; ++i) {
        mca_bmi_portals_module.eq_handles[i] = PTL_EQ_NONE;
    }

    mca_bmi_portals_module.ni_handle = PTL_INVALID_HANDLE;
    mca_bmi_portals_module.dropped = 0;

    return OMPI_SUCCESS;
}


int
mca_bmi_portals_component_close(void)
{
    /* print out debugging if anything is pending */
    /* BWB - implement me, if possible */

    /* release resources */
    OBJ_DESTRUCT(&mca_bmi_portals_component.portals_lock);
#if 0
    OBJ_DESTRUCT(&mca_bmi_portals_component.portals_recv_frags);
    OBJ_DESTRUCT(&mca_bmi_portals_component.portals_pending_acks);
    OBJ_DESTRUCT(&mca_bmi_portals_component.portals_lock);
#endif

    if (NULL != mca_bmi_portals_component.portals_ifname) {
        free(mca_bmi_portals_component.portals_ifname);
    }

    if (NULL != portals_output_stream.lds_prefix) {
        free(portals_output_stream.lds_prefix);
    }

    ompi_output_close(mca_bmi_portals_component.portals_output);
    mca_bmi_portals_component.portals_output = -1;

    return OMPI_SUCCESS;
}


mca_bmi_base_module_t**
mca_bmi_portals_component_init(int *num_bmis, 
                               bool enable_progress_threads,
                               bool enable_mpi_threads)
{
    mca_bmi_base_module_t** bmis;
    *num_bmis = 0;

    if (enable_progress_threads) {
        ompi_output_verbose(20, mca_bmi_portals_component.portals_output,
                            "disabled because progress threads enabled");
        return NULL;
    }

#if 0
    ompi_free_list_init(&mca_bmi_portals_component.portals_send_frags, 
        sizeof(mca_bmi_portals_send_frag_t),
        OBJ_CLASS(mca_bmi_portals_send_frag_t),
        mca_bmi_portals_component.portals_free_list_init_num,
        mca_bmi_portals_component.portals_free_list_max_num,
        mca_bmi_portals_component.portals_free_list_inc_num,
        NULL); /* use default allocator */

    ompi_free_list_init(&mca_bmi_portals_component.portals_recv_frags, 
        sizeof(mca_bmi_portals_recv_frag_t),
        OBJ_CLASS(mca_bmi_portals_recv_frag_t),
        mca_bmi_portals_component.portals_free_list_init_num,
        mca_bmi_portals_component.portals_free_list_max_num,
        mca_bmi_portals_component.portals_free_list_inc_num,
        NULL); /* use default allocator */
#endif

    /* initialize portals bmi.  note that this is in the compat code because
       it's fairly non-portable between implementations */
    if (OMPI_SUCCESS != mca_bmi_portals_init(&mca_bmi_portals_component)) {
        ompi_output_verbose(20, mca_bmi_portals_component.portals_output,
                            "disabled because compatibility init failed");
        return NULL;
    }

    /* return array of bmis */
    bmis = malloc(mca_bmi_portals_component.portals_num_modules * 
                  sizeof(mca_bmi_base_module_t*));
    if (NULL == bmis) return NULL;

    memcpy(bmis, 
           mca_bmi_portals_component.portals_modules, 
           mca_bmi_portals_component.portals_num_modules * 
           sizeof(mca_bmi_base_module_t*));
    *num_bmis = mca_bmi_portals_component.portals_num_modules;

    ompi_output_verbose(20, mca_bmi_portals_component.portals_output,
                        "initialized %d modules",
                        *num_bmis);

    return bmis;
}


int
mca_bmi_portals_component_progress(void)
{
    int num_progressed = 0;
    size_t i;

    for (i = 0 ; i < mca_bmi_portals_component.portals_num_modules ; ++i) {
        struct mca_bmi_portals_module_t *module = 
            mca_bmi_portals_component.portals_modules[i];
        ptl_event_t ev;
        ptl_sr_value_t numdropped;
        int which;
        int ret;

        if (module->eq_handles[MCA_BMI_PORTALS_EQ_SIZE - 1] == 
            PTL_EQ_NONE) continue; /* they are all initialized at once */

#if OMPI_ENABLE_DEBUG
        /* BWB - this is going to kill performance */
        PtlNIStatus(module->ni_handle,
                    PTL_SR_DROP_COUNT,
                    &numdropped);
        if (numdropped != module->dropped) {
            ompi_output_verbose(30, mca_bmi_portals_component.portals_output,
                                "*** Dropped message count changed.  %lld, %lld",
                                module->dropped, numdropped);
            module->dropped = numdropped;
        }
#endif

        ret = PtlEQPoll(module->eq_handles,
                        MCA_BMI_PORTALS_EQ_SIZE, /* number of eq handles */
                        10,
                        &ev,
                        &which);
        if (PTL_EQ_EMPTY == ret) {
            /* nothing to see here - move along */
            continue;
        } else if (!(PTL_OK == ret || PTL_EQ_DROPPED == ret)) {
            /* BWB - how can we report errors? */
            ompi_output(mca_bmi_portals_component.portals_output,
                        "Error calling PtlEQGet: %d", ret);
            continue;
        } else if (PTL_EQ_DROPPED == ret) {
            ompi_output_verbose(10, mca_bmi_portals_component.portals_output,
                                "*** Event queue entries were dropped");
        }

#if BMI_PORTALS_HAVE_EVENT_UNLINK
        /* not everyone has UNLINK.  Use it only to print the event,
           so we can make sure we properly re-initialize the ones that
           need to be re-initialized */
        if (PTL_EVENT_UNLINK == ev.type) {
            OMPI_OUTPUT_VERBOSE((100, mca_bmi_portals_component.portals_output,
                                 "unlink event occurred"));
            continue;
        }
#endif

        num_progressed++;
    }

    return num_progressed;
}

