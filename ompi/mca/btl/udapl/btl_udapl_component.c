/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/include/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "mca/errmgr/errmgr.h"
#include "mca/mpool/base/base.h" 
#include "btl_udapl.h"
#include "btl_udapl_frag.h"
#include "btl_udapl_endpoint.h" 
#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h"
#include "ompi/datatype/convertor.h" 
#include "btl_udapl_endpoint.h"
#include "orte/util/proc_info.h"
#include "ompi/mca/pml/base/pml_base_module_exchange.h"

mca_btl_udapl_component_t mca_btl_udapl_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */

            MCA_BTL_BASE_VERSION_1_0_0,

            "udapl", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_udapl_component_open,  /* component open */
            mca_btl_udapl_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* Whether the component is checkpointable or not */

            false
        },

        mca_btl_udapl_component_init,  
        mca_btl_udapl_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_udapl_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","udapl",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_udapl_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","udapl",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_udapl_component_open(void)
{  
    int param, value;

    opal_output(0, "udapl_component_open\n");

    /* initialize state */
    mca_btl_udapl_component.udapl_num_btls=0;
    mca_btl_udapl_component.udapl_btls=NULL;
    
    /* initialize objects */ 
    OBJ_CONSTRUCT(&mca_btl_udapl_component.udapl_procs, opal_list_t);
    OBJ_CONSTRUCT(&mca_btl_udapl_component.udapl_lock, opal_mutex_t);

    /* register uDAPL component parameters */
    mca_btl_udapl_component.udapl_free_list_num =
        mca_btl_udapl_param_register_int ("free_list_num", 8);
    mca_btl_udapl_component.udapl_free_list_max =
        mca_btl_udapl_param_register_int ("free_list_max", -1);
    mca_btl_udapl_component.udapl_free_list_inc =
        mca_btl_udapl_param_register_int ("free_list_inc", 8);
    mca_btl_udapl_component.udapl_debug = 
        mca_btl_udapl_param_register_int("debug", 1); 
    mca_btl_udapl_component.udapl_mpool_name = 
        mca_btl_udapl_param_register_string("mpool", "udapl"); 
    mca_btl_udapl_component.udapl_max_btls = 
        mca_btl_udapl_param_register_int("max_modules", 4);
    mca_btl_udapl_component.udapl_evd_qlen =
        mca_btl_udapl_param_register_int("evd_qlen", 8);
    mca_btl_udapl_component.udapl_num_high_priority = 
        mca_btl_udapl_param_register_int("num_high_priority", 8); 
    mca_btl_udapl_component.udapl_num_repost = 
        mca_btl_udapl_param_register_int("num_repost", 4); 
    mca_btl_udapl_component.udapl_num_mru = 
        mca_btl_udapl_param_register_int("num_mru", 64); 
    mca_btl_udapl_component.udapl_port_name=
        mca_btl_udapl_param_register_string("port_name", "OMPI"); 

    /* register uDAPL module parameters */
    mca_btl_udapl_module.super.btl_exclusivity =
        mca_btl_udapl_param_register_int ("exclusivity", MCA_BTL_EXCLUSIVITY_DEFAULT - 10);
    mca_btl_udapl_module.super.btl_eager_limit = 
        mca_btl_udapl_param_register_int ("eager_limit", 32*1024);
    mca_btl_udapl_module.super.btl_min_send_size =
        mca_btl_udapl_param_register_int ("min_send_size", 32*1024);
    mca_btl_udapl_module.super.btl_max_send_size =
        mca_btl_udapl_param_register_int ("max_send_size", 64*1024);
    mca_btl_udapl_module.super.btl_min_rdma_size = 
        mca_btl_udapl_param_register_int("min_rdma_size", 512*1024); 
    mca_btl_udapl_module.super.btl_max_rdma_size = 
        mca_btl_udapl_param_register_int("max_rdma_size", 128*1024); 
    mca_btl_udapl_module.super.btl_bandwidth  = 
        mca_btl_udapl_param_register_int("bandwidth", 225); 

    /* compute the eager and max frag sizes */
    mca_btl_udapl_component.udapl_eager_frag_size =
            mca_btl_udapl_module.super.btl_eager_limit;
    /*mca_btl_udapl_component.udapl_eager_limit =
            mca_btl_udapl_module.super.btl_eager_limit -
            sizeof(mca_btl_base_header_t);*/

    mca_btl_udapl_component.udapl_max_frag_size =
        mca_btl_udapl_module.super.btl_max_send_size;
    mca_btl_udapl_module.super.btl_max_send_size =
        mca_btl_udapl_module.super.btl_max_send_size -
        sizeof(mca_btl_base_header_t);
#if 0
    mca_btl_udapl_component.udapl_eager_frag_size =
        udapl_min_size_for_length(mca_btl_udapl_module.super.btl_eager_limit) - 1;
    mca_btl_udapl_module.super.btl_eager_limit = 
        udapl_max_length_for_size(mca_btl_udapl_component.udapl_eager_frag_size) -
        sizeof(mca_btl_base_header_t);
#endif

    /* compute the max frag size */
#if 0
    mca_btl_udapl_component.udapl_max_frag_size = 
        udapl_min_size_for_length(mca_btl_udapl_module.super.btl_max_send_size) - 1;
    mca_btl_udapl_module.super.btl_max_send_size = 
        udapl_max_length_for_size(mca_btl_udapl_component.udapl_max_frag_size) -
        sizeof(mca_btl_base_header_t);
#endif

    /* leave pinned option */
    value = 0;
    param = mca_base_param_find("mpi", NULL, "leave_pinned");
    mca_base_param_lookup_int(param, &value);
    mca_btl_udapl_component.leave_pinned = value;
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_udapl_component_close(void)
{
    opal_output(0, "udapl_component_close\n");

    /* TODO - what needs to be done here? */
    return OMPI_SUCCESS;
}


/*
 *  Register uDAPL component addressing information. The MCA framework
 *  will make this available to all peers.
 */

static int
mca_btl_udapl_modex_send(void)
{
    int         rc;
    size_t      i;
    size_t      size;
    mca_btl_udapl_addr_t *addrs = NULL;

    size = sizeof(mca_btl_udapl_addr_t) *
            mca_btl_udapl_component.udapl_num_btls;

    if(mca_btl_udapl_component.udapl_debug) {
        opal_output(0, "udapl_modex_send %d addrs %d bytes\n",
                mca_btl_udapl_component.udapl_num_btls, size);
    }

    if (0 != size) {
        addrs = (mca_btl_udapl_addr_t *)malloc (size);
        if (NULL == addrs) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (i = 0; i < mca_btl_udapl_component.udapl_num_btls; i++) {
            mca_btl_udapl_module_t *btl = mca_btl_udapl_component.udapl_btls[i];
            addrs[i] = btl->udapl_addr;
        }
    }
    rc = mca_pml_base_modex_send (&mca_btl_udapl_component.super.btl_version, addrs, size);
    if (NULL != addrs) {
        free (addrs);
    }
    return rc;
}


/*
 * Initialize the uDAPL component,
 * check how many interfaces are available and create a btl module for each.
 */

mca_btl_base_module_t **
mca_btl_udapl_component_init (int *num_btl_modules,
                           bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    DAT_PROVIDER_INFO* datinfo;
    mca_btl_base_module_t **btls;
    mca_btl_udapl_module_t *btl;
    DAT_COUNT num_ias;
    int32_t i;

    opal_output(0, "udapl_component_init\n");

    /* enumerate uDAPL interfaces */
    datinfo = malloc(sizeof(DAT_PROVIDER_INFO) *
            mca_btl_udapl_component.udapl_max_btls);
    if(NULL == datinfo) {
        return NULL;
    }
    if(DAT_SUCCESS != dat_registry_list_providers(
            mca_btl_udapl_component.udapl_max_btls,
            (DAT_COUNT*)&num_ias, &datinfo)) {
        free(datinfo);
        return NULL;
    }

    /* allocate space for the each possible BTL */
    mca_btl_udapl_component.udapl_btls = (mca_btl_udapl_module_t *)
            malloc(num_ias * sizeof(mca_btl_udapl_module_t *));
    if(NULL == mca_btl_udapl_component.udapl_btls) {
        free(datinfo);
        return NULL;
    }

    /* create a BTL module for each interface */
    for(mca_btl_udapl_component.udapl_num_btls = i = 0; i < num_ias; i++) {
        opal_output(0, "udapl creating btl for %s\n", datinfo[i].ia_name);

        btl = malloc(sizeof(mca_btl_udapl_module_t));
        if(NULL == btl) {
            free(datinfo);
            free(mca_btl_udapl_component.udapl_btls);
            return NULL;
        }

        /* copy default values into the new BTL */
        memcpy(btl, &mca_btl_udapl_module, sizeof(mca_btl_udapl_module_t));

        /* initialize this BTL */
        /* TODO - make use of the thread-safety info in datinfo also */
        if(OMPI_SUCCESS != mca_btl_udapl_init(datinfo[i].ia_name, btl)) {
            opal_output(0, "udapl module init for %s failed\n",
                    datinfo[i].ia_name);
            free(btl);
            continue;
        }

        /* successful btl creation */
        mca_btl_udapl_component.udapl_btls[i] = btl;
        if(++mca_btl_udapl_component.udapl_num_btls >=
                mca_btl_udapl_component.udapl_max_btls) {
            break;
        }
    }

    /* finished with datinfo */
    free(datinfo);

    /* Make sure we have some interfaces */
    if(0 == mca_btl_udapl_component.udapl_num_btls) {
        mca_btl_base_error_no_nics("uDAPL", "NIC");
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    /* publish uDAPL parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_btl_udapl_modex_send()) {
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    /* return array of BTLs */
    btls = (mca_btl_base_module_t**) malloc(sizeof(mca_btl_base_module_t *) *
            mca_btl_udapl_component.udapl_num_btls);
    if (NULL == btls) {
        free(mca_btl_udapl_component.udapl_btls);
        return NULL;
    }

    memcpy(btls, mca_btl_udapl_component.udapl_btls,
           mca_btl_udapl_component.udapl_num_btls *
           sizeof(mca_btl_udapl_module_t *));
    *num_btl_modules = mca_btl_udapl_component.udapl_num_btls;
    return btls;
}


/*
 *  uDAPL component progress.
 */


int mca_btl_udapl_component_progress()
{
    static int32_t inprogress = 0;
    int count = 0;
    size_t i;

    /* prevent deadlock - only one thread should be 'progressing' at a time */
    if(OPAL_THREAD_ADD32(&inprogress, 1) > 1) {
        OPAL_THREAD_ADD32(&inprogress, -1);
        return OMPI_SUCCESS;
    }

    opal_output(0, "udapl_component_progress\n");
    
    /* check for work to do on each uDAPL btl */
    for( i = 0; i < mca_btl_udapl_component.udapl_num_btls; ) {
        mca_btl_udapl_module_t *btl = mca_btl_udapl_component.udapl_btls[i];

        /* TODO - check the DTO EVD for events */
        i++;
    }

    /* unlock and return */
    OPAL_THREAD_ADD32(&inprogress, -1);
    return count;
}

