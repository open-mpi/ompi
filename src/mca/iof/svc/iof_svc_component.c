/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "util/proc_info.h"
#include "util/output.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/oob/oob.h"
#include "iof_svc.h"
#include "iof_svc_proxy.h"

/*
 * Local functions
 */
static int mca_iof_svc_open(void);
static int mca_iof_svc_close(void);

static mca_iof_base_module_t* mca_iof_svc_init(
    int* priority, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads);


mca_iof_svc_component_t mca_iof_svc_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_IOF_BASE_VERSION_1_0_0,

        "svc", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        mca_iof_svc_open,  /* component open  */
        mca_iof_svc_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_iof_svc_init
    }
};

static char* mca_iof_svc_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("iof","svc",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static  int mca_iof_svc_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("iof","svc",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int mca_iof_svc_open(void)
{
    mca_iof_svc_component.svc_debug = mca_iof_svc_param_register_int("debug", 1);
    OBJ_CONSTRUCT(&mca_iof_svc_component.svc_subscribed, ompi_list_t);
    OBJ_CONSTRUCT(&mca_iof_svc_component.svc_published, ompi_list_t);
    OBJ_CONSTRUCT(&mca_iof_svc_component.svc_lock, ompi_mutex_t);
    return OMPI_SUCCESS;
}


static int mca_iof_svc_close(void)
{
    ompi_list_item_t* item;
    OMPI_THREAD_LOCK(&mca_iof_svc_component.svc_lock);
    while((item = ompi_list_remove_first(&mca_iof_svc_component.svc_subscribed)) != NULL) {
        OBJ_RELEASE(item);
    }
    while((item = ompi_list_remove_first(&mca_iof_svc_component.svc_published)) != NULL) {
        OBJ_RELEASE(item);
    }
    OMPI_THREAD_UNLOCK(&mca_iof_svc_component.svc_lock);
    mca_oob_recv_cancel(MCA_OOB_NAME_ANY, MCA_OOB_TAG_IOF_SVC);
}




static mca_iof_base_module_t* 
mca_iof_svc_init(int* priority, bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int rc;
    if(ompi_process_info.seed == false)
        return NULL;

    *priority = 1;
    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    /* post non-blocking recv */
    mca_iof_svc_component.svc_iov[0].iov_base = NULL;
    mca_iof_svc_component.svc_iov[0].iov_len = 0;

    rc = mca_oob_recv_nb(
        MCA_OOB_NAME_ANY,
        mca_iof_svc_component.svc_iov,
        1,
        MCA_OOB_TAG_IOF_SVC,
        MCA_OOB_ALLOC,
        mca_iof_svc_proxy_recv,
        NULL
    );
    if(rc != OMPI_SUCCESS) {
        ompi_output(0, "mca_iof_svc_init: unable to post non-blocking recv");
        return NULL;
    }
    return &mca_iof_svc_module;
}

