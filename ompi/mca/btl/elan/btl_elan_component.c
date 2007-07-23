/*
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "ompi_config.h"
#include "ompi/constants.h"
#include "opal/event/event.h"
#include "opal/util/if.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"

#include "opal/mca/base/mca_base_param.h"
#include "ompi/runtime/ompi_module_exchange.h"

#include "orte/mca/errmgr/errmgr.h"
#include "ompi/mca/mpool/base/base.h" 
#include "btl_elan.h"
#include "btl_elan_frag.h"
#include "btl_elan_endpoint.h" 

#include "ompi/mca/btl/base/base.h" 
#include "ompi/mca/btl/base/btl_base_error.h" 
#include "ompi/datatype/convertor.h" 

#include "elan/elan.h"

#include "opal/util/os_path.h"
#include "opal/util/opal_environ.h"
#include "ompi/communicator/communicator.h" 
mca_btl_elan_component_t mca_btl_elan_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
           about the component itself */

        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */
            MCA_BTL_BASE_VERSION_1_0_1,
            "elan", /* MCA component name */
            OMPI_MAJOR_VERSION,  /* MCA component major version */
            OMPI_MINOR_VERSION,  /* MCA component minor version */
            OMPI_RELEASE_VERSION,  /* MCA component release version */
            mca_btl_elan_component_open,  /* component open */
            mca_btl_elan_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */

        {
            /* The component is not checkpoint ready */
            MCA_BASE_METADATA_PARAM_NONE
        },
        mca_btl_elan_component_init,  
        mca_btl_elan_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_btl_elan_param_register_string(
                                                     const char* param_name, 
                                                     const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("btl","elan",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static inline int mca_btl_elan_param_register_int(
        const char* param_name, 
        int default_value)
{
    int id = mca_base_param_register_int("btl","elan",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_btl_elan_component_open(void)
{
    /* initialize state */
    mca_btl_elan_component.elan_num_btls=0;
    mca_btl_elan_component.elan_btls=NULL;

    /* register Elan4 component parameters */
    mca_btl_elan_component.elan_free_list_num =
        mca_btl_elan_param_register_int ("free_list_num", 8);
    mca_btl_elan_component.elan_free_list_max =
        mca_btl_elan_param_register_int ("free_list_max", 128);
    mca_btl_elan_component.elan_free_list_inc =
        mca_btl_elan_param_register_int ("free_list_inc", 32);
    mca_btl_elan_component.elan_mpool_name = 
        mca_btl_elan_param_register_string("mpool", "elan"); 
    mca_btl_elan_module.super.btl_exclusivity = 0;
    mca_btl_elan_module.super.btl_eager_limit =  32*1024;
    mca_btl_elan_module.super.btl_min_send_size = 32*1024;
    mca_btl_elan_module.super.btl_max_send_size = 64*1024; /*64*1024;*/
    mca_btl_elan_module.super.btl_rdma_pipeline_send_length = 512 * 1024;
    mca_btl_elan_module.super.btl_rdma_pipeline_frag_size = 128 * 1024;
    mca_btl_elan_module.super.btl_min_rdma_pipeline_size = 128 * 1024;
    mca_btl_elan_module.super.btl_flags = MCA_BTL_FLAGS_SEND_INPLACE /*| MCA_BTL_FLAGS_GET */| MCA_BTL_FLAGS_PUT | MCA_BTL_FLAGS_SEND ;
    /* mca_btl_elan_module.super.btl_flags = MCA_BTL_FLAGS_SEND_INPLACE|MCA_BTL_FLAGS_RDMA | MCA_BTL_FLAGS_SEND ;*/
    mca_btl_elan_module.super.btl_bandwidth = 2000;
    mca_btl_elan_module.super.btl_latency = 5;
    mca_btl_base_param_register(&mca_btl_elan_component.super.btl_version,
                                &mca_btl_elan_module.super);
 
    return OMPI_SUCCESS;
}

/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_btl_elan_component_close(void)
{
    if( NULL != mca_btl_elan_component.elan_btls ) {
        free( mca_btl_elan_component.elan_btls );
        /* release resources */

        OBJ_DESTRUCT(&mca_btl_elan_component.elan_procs);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_frag_eager);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_frag_user);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_frag_max);
        OBJ_DESTRUCT(&mca_btl_elan_component.elan_lock);
    }
    return OMPI_SUCCESS;
}

/*
 *  Elan4 component initialization:
 *  (1) read interface list from kernel and compare against component parameters
 *      then create a BTL instance for selected interfaces
 *  (2) setup Elan4 listen socket for incoming connection attempts
 *  (3) register BTL parameters with the MCA
 */
mca_btl_base_module_t** mca_btl_elan_component_init( int *num_btl_modules,
						      bool enable_progress_threads,
						      bool enable_mpi_threads )
{

    mca_btl_base_module_t** btls;
    size_t rails, i , count, vpid;
    ELAN_BASE    * base;
    ELAN_STATE   * state;
    ELAN_QUEUE   * q= NULL;
    ELAN_TPORT   * p= NULL;
            
    *num_btl_modules = 0;
    if (enable_progress_threads) { 
        ompi_modex_send(&mca_btl_elan_component.super.btl_version, NULL, 0);
        return NULL;
    }
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_lock, opal_mutex_t);
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_frag_eager, ompi_free_list_t);
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_frag_max, ompi_free_list_t);
    OBJ_CONSTRUCT (&mca_btl_elan_component.elan_frag_user, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_btl_elan_component.elan_procs, opal_list_t);
    ompi_free_list_init( &mca_btl_elan_component.elan_frag_eager,
                         sizeof(mca_btl_elan_frag_t) + mca_btl_elan_module.super.btl_eager_limit,
                         OBJ_CLASS(mca_btl_elan_frag_t),
                         mca_btl_elan_component.elan_free_list_num,
                         mca_btl_elan_component.elan_free_list_max,
                         mca_btl_elan_component.elan_free_list_inc,
                         NULL ); /* use default allocator */

    ompi_free_list_init( &mca_btl_elan_component.elan_frag_user,
                         sizeof(mca_btl_elan_frag_t),
                         OBJ_CLASS(mca_btl_elan_frag_t),
                         mca_btl_elan_component.elan_free_list_num,
                         mca_btl_elan_component.elan_free_list_max,
                         mca_btl_elan_component.elan_free_list_inc,
                         NULL ); /* use default allocator */

    ompi_free_list_init( &mca_btl_elan_component.elan_frag_max,
                         sizeof(mca_btl_elan_frag_t)+mca_btl_elan_module.super.btl_max_send_size,
                         OBJ_CLASS(mca_btl_elan_frag_t),
                         mca_btl_elan_component.elan_free_list_num,
                         mca_btl_elan_component.elan_free_list_max,
                         mca_btl_elan_component.elan_free_list_inc,
                         NULL ); /* use default allocator */
    

    opal_setenv( "MPIRUN_ELANIDMAP_FILE", "/etc/elanidmap", false, &environ );
    vpid = orte_process_info.my_name->vpid;
   
    ompi_modex_send( &mca_btl_elan_component.super.btl_version, &vpid, sizeof(vpid));
    mca_btl_elan_component.elan_num_btls = 1;
    mca_btl_elan_component.elan_btls = malloc( (mca_btl_elan_component.elan_num_btls) * sizeof(mca_btl_base_module_t*) );
    for( i = count = 0; i < mca_btl_elan_component.elan_num_btls; i++ ) {
        mca_btl_elan_module_t* btl =  malloc (sizeof (mca_btl_elan_module_t));	
        if(NULL == btl)
            continue;
        memcpy( btl, &mca_btl_elan_module, sizeof(mca_btl_elan_module_t) );
        OBJ_CONSTRUCT (&btl->elan_lock, opal_mutex_t);
    	btl->tportFIFOHead=NULL;
    	btl->tportFIFOTail=NULL;
        mca_btl_elan_component.elan_btls[count++] = btl;
    }
    mca_btl_elan_component.elan_num_btls = count ;
    btls = (mca_btl_base_module_t**)malloc( (mca_btl_elan_component.elan_num_btls) * sizeof(mca_btl_base_module_t*) );
    if( NULL == btls ) {
        free( mca_btl_elan_component.elan_btls );
        mca_btl_elan_component.elan_num_btls = 0;  /* no active BTL modules */
        return NULL;
    }
    memcpy( btls,  mca_btl_elan_component.elan_btls, mca_btl_elan_component.elan_num_btls *sizeof(mca_btl_elan_module_t*) );
    *num_btl_modules = mca_btl_elan_component.elan_num_btls;
    return btls;
}

/*
 *  Elan4 component progress.
 */




int mca_btl_elan_component_progress()
{
    size_t num_progressed = 0, i, no_btls, size;
    mca_btl_elan_frag_t*    frag;
    bufdesc_t*    desc;
    no_btls = mca_btl_elan_component.elan_num_btls;
    for (i = 0; i < no_btls; i++) {
    	mca_btl_elan_module_t* elan_btl = mca_btl_elan_component.elan_btls[i];
        OPAL_THREAD_LOCK(&elan_btl->elan_lock);
        desc = elan_ipeek(elan_btl);
        OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
        if(desc ==NULL)
            continue;
        frag = (mca_btl_elan_frag_t*) desc->frag;
        if(frag!=NULL)
            {
                if(frag->type== MCA_BTL_ELAN_HDR_TYPE_SEND )
                    {
                        /* it's a send */
                        /* call the completion callback */
                        elan_tportTxWait(desc->eve);
                        frag->base.des_cbfunc( &(elan_btl->super), frag->endpoint, &(frag->base), OMPI_SUCCESS );
                        free(desc);
                
                    }
                else if(frag->type== MCA_BTL_ELAN_HDR_TYPE_PUT || frag->type== MCA_BTL_ELAN_HDR_TYPE_GET )
                    {
                        /* it's a put*/
                        /* call the completion callback */
                        //opal_output(0, "I am a RDMA\n");            
                        elan_wait(desc->eve,ELAN_WAIT_EVENT);
                        //opal_output(0, "I am a RDMA_done\n");
                        frag->base.des_cbfunc( &(elan_btl->super), frag->endpoint, &(frag->base), OMPI_SUCCESS );
                        //opal_output(0, "I am a RDMA_cb_done\n");
                        free(desc);
                    }
                else{
                    /* and this one is a receive */
                    mca_btl_base_recv_reg_t* reg;
                    reg = &(elan_btl->elan_reg[frag->tag]);
                    elan_tportRxWait(desc->eve, NULL, NULL, &size);
                    frag->base.des_dst->seg_len = size;
                    reg->cbfunc( &(elan_btl->super), frag->tag, &(frag->base),reg->cbdata );
                    /**
                     * The upper level extract the data from the fragment.
                     * Now we can register the fragment
                     * again with the elan BTL.
                     */
                    desc->eve = elan_tportRxStart (elan_btl->tport, 0 ,  0,  0, 0xffffffff, frag->tag, frag->base.des_dst->seg_addr.pval, mca_btl_elan_module.super.btl_eager_limit) ;
                    /*desc->eve = elan_tportRxStart (elan_btl->tport, ELAN_TPORT_RXANY , 0,  0, 0, 0, frag->base.des_dst->seg_addr.pval, mca_btl_elan_module.super.btl_eager_limit) ;*/

                    desc->frag = frag;
                    desc->next  = NULL;
                    OPAL_THREAD_LOCK(&elan_btl->elan_lock);
                    if(elan_btl->tportFIFOTail)
                        {
                            elan_btl->tportFIFOTail->next = desc;
                            elan_btl->tportFIFOTail=desc;
                        }
                    else{
                        elan_btl->tportFIFOHead = desc;
                        elan_btl->tportFIFOTail = desc;   
                    }
                    OPAL_THREAD_UNLOCK(&elan_btl->elan_lock);
                }
            } else {
                opal_output( 0, "Something bad happened the frag == NULL\n" );
            }
        num_progressed++;
       
    }

    return num_progressed;
}
