/*
 * $HEADER$
 */
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "include/constants.h"
#include "event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/ptl/sm/src/ptl_sm.h"
#include "mca/mpool/base/base.h"
#include "ptl_sm.h"
#include "ptl_sm_sendreq.h"
#include "ptl_sm_sendfrag.h"
#include "ptl_sm_recvfrag.h"



/*
 * Local utility functions.
 */

static int mca_ptl_sm_component_exchange(void);


/*
 * Shared Memory (SM) component instance. 
 */

mca_ptl_sm_component_t mca_ptl_sm_component = {
    {
        /* First, the mca_base_component_t struct containing meta information
          about the component itself */
        {
            /* Indicate that we are a pml v1.0.0 component (which also implies a
               specific MCA version) */
            MCA_PTL_BASE_VERSION_1_0_0,
            "sm", /* MCA component name */
            1,  /* MCA component major version */
            0,  /* MCA component minor version */
            0,  /* MCA component release version */
            mca_ptl_sm_component_open,  /* component open */
            mca_ptl_sm_component_close  /* component close */
        },

        /* Next the MCA v1.0.0 component meta data */
        {
            /* Whether the component is checkpointable or not */
            false
        },

        mca_ptl_sm_component_init,  
        mca_ptl_sm_component_control,
        mca_ptl_sm_component_progress,
    }
};


/*
 * utility routines for parameter registration
 */

static inline char* mca_ptl_sm_param_register_string(
    const char* param_name, 
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ptl","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                            
static inline int mca_ptl_sm_param_register_int(
    const char* param_name, 
    int default_value)
{
    int id = mca_base_param_register_int("ptl","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/*
 *  Called by MCA framework to open the component, registers
 *  component parameters.
 */

int mca_ptl_sm_component_open(void)
{
    /* register SM component parameters */
    mca_ptl_sm_component.sm_free_list_num =
        mca_ptl_sm_param_register_int("free_list_num", 256);
    mca_ptl_sm_component.sm_free_list_max =
        mca_ptl_sm_param_register_int("free_list_max", -1);
    mca_ptl_sm_component.sm_free_list_inc =
        mca_ptl_sm_param_register_int("free_list_inc", 256);
    mca_ptl_sm_component.sm_max_procs =
        mca_ptl_sm_param_register_int("max_procs", -1);
    mca_ptl_sm_component.sm_mpool_name =
        mca_ptl_sm_param_register_string("mpool", "sm");

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_send_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_recv_frags, ompi_free_list_t);
   
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_ptl_sm_component_close(void)
{
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_lock);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_send_requests);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_send_frags);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_recv_frags);
    return OMPI_SUCCESS;
}


/*
 *  SM component initialization
 */
mca_ptl_base_module_t** mca_ptl_sm_component_init(
    int *num_ptls, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads)
{
    mca_ptl_base_module_t **ptls = NULL;
    mca_mpool_base_component_t *sm_mpool_component; /* RLG */

    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    /* lookup shared memory pool */
    sm_mpool_component = mca_mpool_component_lookup(mca_ptl_sm_component.sm_mpool_name);

    mca_ptl_sm_component.sm_mpool =
        sm_mpool_component->mpool_init(allow_multi_user_threads); /* RLG */
    /*mca_ptl_sm_component.sm_mpool =
     * mca_mpool_component_lookup(mca_ptl_sm_component.sm_mpool_name);  RLG */
    if(NULL == mca_ptl_sm_component.sm_mpool) {
        ompi_output(0, "mca_ptl_sm_component_init: unable to locate shared memory pool: %s\n",
            mca_ptl_sm_component.sm_mpool_name);
        return NULL;
    }
    mca_ptl_sm_component.sm_mpool_base = mca_ptl_sm_component.sm_mpool->mpool_base();

    /* initialize free lists */
    ompi_free_list_init(&mca_ptl_sm_component.sm_send_requests, 
        sizeof(mca_ptl_sm_send_request_t),
        OBJ_CLASS(mca_ptl_sm_send_request_t),
        mca_ptl_sm_component.sm_free_list_num,
        mca_ptl_sm_component.sm_free_list_max,
        mca_ptl_sm_component.sm_free_list_inc,
        mca_ptl_sm_component.sm_mpool); /* use shared-memory pool */

    ompi_free_list_init(&mca_ptl_sm_component.sm_recv_frags, 
        sizeof(mca_ptl_sm_recv_frag_t),
        OBJ_CLASS(mca_ptl_sm_recv_frag_t),
        mca_ptl_sm_component.sm_free_list_num,
        mca_ptl_sm_component.sm_free_list_max,
        mca_ptl_sm_component.sm_free_list_inc,
        mca_ptl_sm_component.sm_mpool); /* use shared-memory pool */

    /* publish shared memory parameters with the MCA framework */
    if(mca_ptl_sm_component_exchange() != OMPI_SUCCESS)
        return 0;

    /* allocate the Shared Memory PTL.  Only one is being allocated */
    ptls = malloc(sizeof(mca_ptl_base_module_t*));
    if(NULL == ptls)
        return NULL;

    *ptls = &mca_ptl_sm.super;
    *num_ptls = 1;

    /* initialize some PTL data */
    /* start with no SM procs */
    mca_ptl_sm.num_smp_procs=0;
    mca_ptl_sm.my_smp_rank=-1;

    return ptls;
}

/*
 *  SM component control
 */

int mca_ptl_sm_component_control(int param, void* value, size_t size)
{
    switch(param) {
        case MCA_PTL_ENABLE:
            break;
        default:
            break;
    }
    return OMPI_SUCCESS;
}


/*
 *  SM component progress.
 */

int mca_ptl_sm_component_progress(mca_ptl_tstamp_t tstamp)
{
    return OMPI_SUCCESS;
}


/*
 *
 */

static int mca_ptl_sm_component_exchange()
{
    /*
     *  !!!!  This is temporary, and will be removed when the
     *  registry is implemented
     */
    mca_ptl_sm_exchange_t mca_ptl_sm_setup_info;
    size_t len,size;
    char *ptr;
    int rc;

    /* determine length of host name */
    len=strlen(ompi_system_info.nodename);
    /* check if string is zero length or there is an error */
    if( 0 >= len) {
        return OMPI_ERROR;
    }
    /* check if string is too long */
    if( MCA_PTL_SM_MAX_HOSTNAME_LEN < (len+1) ){
        return OMPI_ERROR;
    }

    /* copy string into structure that will be used to send data around */
    ptr=NULL;
    ptr=strncpy(&(mca_ptl_sm_setup_info.host_name[0]),
            ompi_system_info.nodename, len);
    if( NULL == ptr ) {
        return OMPI_ERROR;
    }
    mca_ptl_sm_setup_info.host_name[len]='\0';


    /* exchange setup information */
    size=sizeof(mca_ptl_sm_exchange_t);
    rc =  mca_base_modex_send(&mca_ptl_sm_component.super.ptlm_version, 
            &mca_ptl_sm_setup_info, size);
    
    return OMPI_SUCCESS;
}

