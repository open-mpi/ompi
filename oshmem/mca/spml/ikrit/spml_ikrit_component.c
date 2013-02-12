/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#define _GNU_SOURCE
#include <stdio.h>

#include <sys/types.h>
#include <unistd.h>


#include "oshmem_config.h"
#include "orte/util/show_help.h"
#include "shmem.h"
#include "oshmem/runtime/params.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "spml_ikrit_component.h"
#include "oshmem/mca/spml/ikrit/spml_ikrit.h"

#include "orte/util/show_help.h"
#include "ompi/runtime/ompi_module_exchange.h"


static int mca_spml_ikrit_component_open(void);
static int mca_spml_ikrit_component_close(void);
static mca_spml_base_module_t*
mca_spml_ikrit_component_init( int* priority, bool enable_progress_threads,
                            bool enable_mpi_threads );
static int mca_spml_ikrit_component_fini(void);
mca_spml_base_component_2_0_0_t mca_spml_ikrit_component = {

    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
      MCA_SPML_BASE_VERSION_2_0_0,
    
      "ikrit",                        /* MCA component name */
      OSHMEM_MAJOR_VERSION,          /* MCA component major version */
      OSHMEM_MINOR_VERSION,          /* MCA component minor version */
      OSHMEM_RELEASE_VERSION,        /* MCA component release version */
      mca_spml_ikrit_component_open,  /* component open */
      mca_spml_ikrit_component_close  /* component close */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },

    mca_spml_ikrit_component_init,    /* component init */
    mca_spml_ikrit_component_fini     /* component finalize */
    
};


static inline int mca_spml_ikrit_param_register_int(
    const char* param_name,
    int default_value,
    const char *help_msg)
{
    int param_value;

    param_value = default_value;
    mca_base_param_reg_int(
            &mca_spml_ikrit_component.spmlm_version,
            param_name,
            help_msg,
            false, false,
            default_value, &param_value);

    return param_value;
}


int spml_ikrit_progress(void)
{
    mxm_error_t err;

    err = mxm_progress(mca_spml_ikrit.mxm_context);
    if ((MXM_OK != err) && (MXM_ERR_NO_PROGRESS != err) ) {
        orte_show_help("help-spml-ikrit.txt", "errors during mxm_progress", true, mxm_error_string(err));
    }
    return 1;
}


static int mca_spml_ikrit_component_open(void)
{
    mxm_error_t err;
    int np;
    unsigned long cur_ver;

    cur_ver = mxm_get_version();
    if (cur_ver != MXM_API) {
        char *str;
        if (asprintf(&str, "SHMEM was compiled with MXM version %d.%d but "
                "version %ld.%ld detected.", MXM_VERNO_MAJOR,
                MXM_VERNO_MINOR, (cur_ver >> MXM_MAJOR_BIT)& 0xff,
                (cur_ver >> MXM_MINOR_BIT) & 0xff)>0) {
                    orte_show_help("help-spml-ikrit.txt", "mxm init", true, str);

                    free(str);
                }
        return OSHMEM_ERROR;
    }


#if MXM_API < MXM_VERSION(1,5)
    mxm_context_opts_t mxm_opts;

    mxm_fill_context_opts(&mxm_opts);
    // only enable rmda and self ptls
    mxm_opts.ptl_bitmap = (MXM_BIT(MXM_PTL_SELF) | MXM_BIT(MXM_PTL_RDMA));


#else
    mxm_context_opts_t *mxm_opts;

    err = mxm_config_read_context_opts(&mxm_opts);
    mxm_opts->ptl_bitmap = (MXM_BIT(MXM_PTL_SELF) | MXM_BIT(MXM_PTL_RDMA));
    if (MXM_OK != err) {
        SPML_ERROR("Failed to parse MXM configuration");
        return OSHMEM_ERROR;
    }
#endif


    mca_spml_ikrit.free_list_num =
        mca_spml_ikrit_param_register_int("free_list_num", 1024, 0);
    mca_spml_ikrit.free_list_max =
        mca_spml_ikrit_param_register_int("free_list_max", 1024, 0);
    mca_spml_ikrit.free_list_inc =
        mca_spml_ikrit_param_register_int("free_list_inc", 16, 0);
    mca_spml_ikrit.priority =
        mca_spml_ikrit_param_register_int("priority", 20, "[integer] ikrit priority");

    mca_spml_ikrit.n_relays = 
        mca_spml_ikrit_param_register_int("use_relays", -1, "[integer] First N ranks on host will receive and forward put messages to other ranks running on it. Can be used to as work around Sandy Bridge far socket problem");

    np = mca_spml_ikrit_param_register_int("np", 128, "[integer] Minimal allowed job's NP to activate ikrit");
    if (oshmem_num_procs() < np) {
        SPML_VERBOSE(1, "Not enough ranks (%d<%d), disqualifying spml/ikrit", oshmem_num_procs(), np);
        return OSHMEM_ERR_NOT_AVAILABLE;
    }

#if MXM_API < MXM_VERSION(1,5)
    err = mxm_init(&mxm_opts, &mca_spml_ikrit.mxm_context);
#else
    err = mxm_init(mxm_opts, &mca_spml_ikrit.mxm_context);
    mxm_config_free(mxm_opts);
#endif

    if (MXM_OK != err) {
        if (MXM_ERR_NO_DEVICE == err) {
            SPML_VERBOSE(1, "No supported device found, disqualifying spml/ikrit");
        } else {
            orte_show_help("help-spml-ikrit.txt", "mxm init", true,
                    mxm_error_string(err));
        }
        return OSHMEM_ERR_NOT_AVAILABLE;
    }

    err = mxm_mq_create(mca_spml_ikrit.mxm_context, MXM_SHMEM_MQ_ID, &mca_spml_ikrit.mxm_mq);
    if (MXM_OK != err) {
        orte_show_help("help-spml-ikrit.txt", "mxm mq create", true, mxm_error_string(err));
        return OSHMEM_ERROR;
    }


    return OSHMEM_SUCCESS;
}


static int mca_spml_ikrit_component_close(void)
{
    if (mca_spml_ikrit.mxm_context)
        mxm_cleanup(mca_spml_ikrit.mxm_context);
    mca_spml_ikrit.mxm_context = NULL;
    return OSHMEM_SUCCESS;
}

static int spml_ikrit_mxm_init(void)
{
    mxm_error_t err;
    mxm_ep_opts_t *p_ep_opts;

#if MXM_API < MXM_VERSION(1,5)
    mxm_ep_opts_t ep_opt;
    struct sockaddr_mxm_local_proc sa_bind_self;
    struct sockaddr_mxm_ib_local sa_bind_rdma;

    p_ep_opts = &ep_opt;
    /* Setup the endpoint options and local addresses to bind to. */
    mxm_fill_ep_opts(&ep_opt);

    sa_bind_self.sa_family = AF_MXM_LOCAL_PROC;
    sa_bind_self.context_id = 0;
    sa_bind_self.process_id = oshmem_proc_local()->proc_name.vpid;

    sa_bind_rdma.sa_family = AF_MXM_IB_LOCAL;
    sa_bind_rdma.lid = 0;
    sa_bind_rdma.pkey = 0;
    sa_bind_rdma.qp_num = 0;
    sa_bind_rdma.sl = 0;

    ep_opt.ptl_bind_addr[MXM_PTL_SELF] = (struct sockaddr*)&sa_bind_self;
    ep_opt.ptl_bind_addr[MXM_PTL_RDMA] = (struct sockaddr*)&sa_bind_rdma;

#else
    err = mxm_config_read_ep_opts(&p_ep_opts);
    if (err != MXM_OK) {
        SPML_ERROR("Failed to parse MXM configuration");
        return OSHMEM_ERROR;
    }

    /* Only relevant for SHM PTL - ignore */
    p_ep_opts->job_id = 0;
    p_ep_opts->local_rank = 0;
    p_ep_opts->num_local_procs = 0;

#endif
    p_ep_opts->rdma.drain_cq = 1;

    /* Open MXM endpoint */
    err = mxm_ep_create(mca_spml_ikrit.mxm_context, p_ep_opts, &mca_spml_ikrit.mxm_ep);
    if (MXM_OK != err) {
        orte_show_help("help-spml-ikrit.txt", "unable to create endpoint", true,
                mxm_error_string(err));
        return OSHMEM_ERROR;
    }

#if MXM_API >= MXM_VERSION(1,5)
    mxm_config_free(p_ep_opts);
#endif

    return OSHMEM_SUCCESS;
}

static mca_spml_base_module_t*
mca_spml_ikrit_component_init( int* priority, 
                            bool enable_progress_threads,
                            bool enable_mpi_threads )
{
    SPML_VERBOSE( 10,  
            "in ikrit, my priority is %d\n", mca_spml_ikrit.priority);
    
    if((*priority) > mca_spml_ikrit.priority) { 
        *priority = mca_spml_ikrit.priority;
        return NULL;
    }
    *priority = mca_spml_ikrit.priority;

    if (OSHMEM_SUCCESS != spml_ikrit_mxm_init())
        return NULL;

    mca_spml_ikrit.n_active_puts = 0;
    mca_spml_ikrit.n_active_gets = 0;
    mca_spml_ikrit.n_mxm_fences = 0;
    SPML_VERBOSE(50, "*** ikrit initialized ****");
    return &mca_spml_ikrit.super;
}

static int mca_spml_ikrit_component_fini(void)
{
    opal_progress_unregister(spml_ikrit_progress);
    if (NULL != mca_spml_ikrit.mxm_ep)
    {
        mxm_ep_destroy(mca_spml_ikrit.mxm_ep);
    }
    return OSHMEM_SUCCESS;
}

