/*
 * $HEADER$
 */
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "include/constants.h"
#include "include/sys/cache.h"
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
#include "mca/common/sm/common_sm_mmap.h"



/*
 * Local utility functions.
 */

static int mca_ptl_sm_component_exchange(void);


/*
 * Shared Memory (SM) component instance. 
 */

mca_ptl_sm_component_t mca_ptl_sm_component = {
    {  /* super is being filled in */
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
    }  /* end super */
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
    mca_ptl_sm_component.sm_first_frag_free_list_num =
        mca_ptl_sm_param_register_int("first_frag_free_list_num", 256);
    mca_ptl_sm_component.sm_first_frag_free_list_max =
        mca_ptl_sm_param_register_int("first_frag_free_list_max", -1);
    mca_ptl_sm_component.sm_first_frag_free_list_inc =
        mca_ptl_sm_param_register_int("first_frag_free_list_inc", 256);
    mca_ptl_sm_component.sm_second_frag_free_list_num =
        mca_ptl_sm_param_register_int("second_frag_free_list_num", 256);
    mca_ptl_sm_component.sm_second_frag_free_list_max =
        mca_ptl_sm_param_register_int("second_frag_free_list_max", -1);
    mca_ptl_sm_component.sm_second_frag_free_list_inc =
        mca_ptl_sm_param_register_int("second_frag_free_list_inc", 256);
    mca_ptl_sm_component.sm_max_procs =
        mca_ptl_sm_param_register_int("max_procs", -1);
    mca_ptl_sm_component.sm_mpool_name =
        mca_ptl_sm_param_register_string("mpool", "sm");
    mca_ptl_sm_component.first_fragment_size =
        mca_ptl_sm_param_register_int("first_fragment_size", 1024);
    mca_ptl_sm_component.max_fragment_size =
        mca_ptl_sm_param_register_int("max_fragment_size", 8*1024);
    mca_ptl_sm_component.fragment_alignment =
        mca_ptl_sm_param_register_int("fragment_alignment",
                CACHE_LINE_SIZE);
    mca_ptl_sm_component.size_of_cb_queue =
        mca_ptl_sm_param_register_int("size_of_cb_queue", 128);
    mca_ptl_sm_component.cb_lazy_free_freq =
        mca_ptl_sm_param_register_int("cb_lazy_free_freq", 128);
    /* make sure that queue size and lazy free frequency are consistent -
     * want to make sure that slots are freed at a rate they can be
     * reused, w/o allocating extra new circular buffer fifo arrays */
    if( (float)(mca_ptl_sm_component.cb_lazy_free_freq) >=
            0.95*(float)(mca_ptl_sm_component.size_of_cb_queue) ) {
        /* upper limit */
        mca_ptl_sm_component.cb_lazy_free_freq=
            (int)(0.95*(float)(mca_ptl_sm_component.size_of_cb_queue));
        /* lower limit */
        if( 0>= mca_ptl_sm_component.cb_lazy_free_freq ) {
            mca_ptl_sm_component.cb_lazy_free_freq=1;
        }
    }

    /* default number of extra procs to allow for future growth */
    mca_ptl_sm_component.sm_extra_procs =
        mca_ptl_sm_param_register_int("sm_extra_procs", 2);

    /* initialize objects */
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_sm.sm_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm.sm_first_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm.sm_second_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm.sm_pending_ack_lock, ompi_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_sm.sm_pending_ack, ompi_list_t);
   
    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_ptl_sm_component_close(void)
{
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_lock);
    OBJ_DESTRUCT(&mca_ptl_sm.sm_send_requests);
    OBJ_DESTRUCT(&mca_ptl_sm.sm_first_frags);
    OBJ_DESTRUCT(&mca_ptl_sm.sm_second_frags);
    OBJ_DESTRUCT(&mca_ptl_sm.sm_pending_ack_lock);
    OBJ_DESTRUCT(&mca_ptl_sm.sm_pending_ack);
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
    size_t length;

    *num_ptls = 0;
    *allow_multi_user_threads = true;
    *have_hidden_threads = OMPI_HAVE_THREADS;

    /* lookup shared memory pool */
    mca_ptl_sm_component.sm_mpool =
        mca_mpool_module_lookup(mca_ptl_sm_component.sm_mpool_name);

    mca_ptl_sm_component.sm_mpool_base = mca_ptl_sm_component.sm_mpool->mpool_base();

    /* initialize fragment descriptor free list */

    /* 
     * first fragment 
     */

    /* allocation will be for the fragment descriptor, payload buffer,
     * and padding to ensure proper alignment can be acheived */
    length=sizeof(mca_ptl_sm_frag_t)+mca_ptl_sm_component.fragment_alignment+
        mca_ptl_sm_component.first_fragment_size;

    ompi_free_list_init(&mca_ptl_sm.sm_first_frags, length,
        OBJ_CLASS(mca_ptl_sm_frag_t),
        mca_ptl_sm_component.sm_first_frag_free_list_num,
        mca_ptl_sm_component.sm_first_frag_free_list_max,
        mca_ptl_sm_component.sm_first_frag_free_list_inc,
        mca_ptl_sm_component.sm_mpool); /* use shared-memory pool */

    /* 
     * second and beyond fragments 
     */

    /* allocation will be for the fragment descriptor, payload buffer,
     * and padding to ensure proper alignment can be acheived */
    length=sizeof(mca_ptl_sm_frag_t)+mca_ptl_sm_component.fragment_alignment+
        mca_ptl_sm_component.max_fragment_size;

    ompi_free_list_init(&mca_ptl_sm.sm_second_frags, length,
        OBJ_CLASS(mca_ptl_sm_second_frag_t),
        mca_ptl_sm_component.sm_second_frag_free_list_num,
        mca_ptl_sm_component.sm_second_frag_free_list_max,
        mca_ptl_sm_component.sm_second_frag_free_list_inc,
        mca_ptl_sm_component.sm_mpool); /* use shared-memory pool */

    /* publish shared memory parameters with the MCA framework */
    if(mca_ptl_sm_component_exchange() != OMPI_SUCCESS)
        return 0;

    /* allocate the Shared Memory PTL.  Only one is being allocated */
    ptls = malloc(sizeof(mca_ptl_base_module_t*));
    if(NULL == ptls)
        return NULL;

    /* only one copy of this ptl is created */
    *ptls = &mca_ptl_sm.super;
    *num_ptls = 1;

    /* set scheduling parameters */
    mca_ptl_sm.super.ptl_cache_size=mca_ptl_sm_component.sm_first_frag_free_list_max;
    mca_ptl_sm.super.ptl_cache_bytes=sizeof(mca_ptl_sm_send_request_t) -
                sizeof(mca_pml_base_send_request_t);
    mca_ptl_sm.super.ptl_first_frag_size=mca_ptl_sm_component.first_fragment_size;

    mca_ptl_sm.super.ptl_min_frag_size=mca_ptl_sm_component.max_fragment_size;
    mca_ptl_sm.super.ptl_max_frag_size=mca_ptl_sm_component.max_fragment_size;
    mca_ptl_sm.super.ptl_exclusivity=100;  /* always use this ptl */
    mca_ptl_sm.super.ptl_latency=100;      /* lowest latency */
    mca_ptl_sm.super.ptl_bandwidth=900; /* not really used now since
                                     exclusivity is set to 100 */

    /* initialize some PTL data */
    /* start with no SM procs */
    mca_ptl_sm_component.num_smp_procs=0;
    mca_ptl_sm_component.my_smp_rank=-1;

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
    /* local variables */
    int my_local_smp_rank, return_status;
    unsigned int peer_local_smp_rank ;
    mca_ptl_sm_frag_t *header_ptr;
    ompi_fifo_t *send_fifo;
    bool frag_matched;
    mca_ptl_base_match_header_t *matching_header;
    mca_pml_base_send_request_t *base_send_req;
    ompi_list_item_t *item;
    char *sm_frag_desc_rel_to_base;

    my_local_smp_rank=mca_ptl_sm_component.my_smp_rank;

    /* send progress is made by the PML */

    /* 
     * receive progress 
     */

    /* poll each fifo */

    /* loop over fifo's */
    for( peer_local_smp_rank=0 ; 
            peer_local_smp_rank < mca_ptl_sm_component.num_smp_procs
            ; peer_local_smp_rank++ ) 
    {

        /* we don't use the shared memory ptl to send to ourselves */
        if( peer_local_smp_rank == my_local_smp_rank ) {
            continue;
        }

        send_fifo=&(mca_ptl_sm_component.fifo
                [my_local_smp_rank][peer_local_smp_rank]);

        /* if fifo is not yet setup - continue - not data has been sent*/
        if(OMPI_CB_FREE == send_fifo->tail){
            continue;
        }

        /* aquire thread lock */
        if( ompi_using_threads() ) {
            ompi_atomic_lock(&(send_fifo->tail_lock));
        }

        /* get pointer */
        header_ptr=(mca_ptl_sm_frag_t *)ompi_fifo_read_from_tail( send_fifo,
                mca_ptl_sm_component.sm_offset);
        if( OMPI_CB_FREE == header_ptr ) {
            /* release thread lock */
            if( ompi_using_threads() ) {
                ompi_atomic_unlock(&(send_fifo->tail_lock));
            }
            continue;
        }

        /* release thread lock */
        if( ompi_using_threads() ) {
            ompi_atomic_unlock(&(send_fifo->tail_lock));
        }

        /* change the address from address relative to the shared
         * memory address, to a true virtual address */
        header_ptr = (mca_ptl_sm_frag_t *)( (char *)header_ptr+
                mca_ptl_sm_component.sm_offset);

        /* figure out what type of message this is */
        switch
            (header_ptr->super.frag_base.frag_header.hdr_common.hdr_type)
            {
        
                case MCA_PTL_HDR_TYPE_MATCH:
                    /* attempt match */
                    matching_header= &(header_ptr->super.frag_base.
                            frag_header.hdr_match);
                    frag_matched=mca_ptl_base_match_in_order_network_delivery(
                            matching_header,
                            (mca_ptl_base_recv_frag_t *)header_ptr);
                    if( NULL != frag_matched ) {
                        /* deliver data, and ack */
                        mca_ptl_sm_matched((mca_ptl_base_module_t *)&mca_ptl_sm,
                                (mca_ptl_base_recv_frag_t *)header_ptr);
                                    
                    }
                    break;

                case MCA_PTL_HDR_TYPE_FRAG:
                    /* second and beyond fragment - just need to deliver
                     * the data, and ack */
                    mca_ptl_sm_matched((mca_ptl_base_module_t *)&mca_ptl_sm,
                            (mca_ptl_base_recv_frag_t *)header_ptr);
                    break;

                case MCA_PTL_HDR_TYPE_ACK:
                    /* ack */
                    /* update the send statistics */
                    /* NOTE !!! : need to change the update stats,
                     *   so that MPI_Wait/Test on the send can complete
                     *   as soon as the data is copied intially into
                     *   the shared memory buffers */
                    base_send_req=header_ptr->super.frag_base.frag_header.
                        hdr_frag.hdr_src_ptr.pval;
                    ((mca_ptl_base_recv_frag_t *)header_ptr)->
                        frag_base.frag_owner->ptl_send_progress(
                                (mca_ptl_base_module_t *)&mca_ptl_sm,
                                base_send_req,
                                header_ptr->super.frag_base.frag_size);

                    /* if this is not the first fragment, recycle
                     * resources.  The first fragment is handled by
                     * the PML */
                    if( 0 < header_ptr->super.frag_base.frag_header.
                            hdr_frag.hdr_frag_offset ) {
                        OMPI_FREE_LIST_RETURN(&mca_ptl_sm.sm_second_frags,
                                (ompi_list_item_t *)header_ptr);
                    }
                    break;

                default:

            }

    }  /* end peer_local_smp_rank loop */

    /* progress acks */
    if( !ompi_list_is_empty(&(mca_ptl_sm.sm_pending_ack)) ) {

        OMPI_THREAD_LOCK(&(mca_ptl_sm.sm_pending_ack_lock));

        /* remove ack from list - need to remove from list before
         *   sending the ack, so that when the ack is recieved,
         *   manipulated, and put on a new list, it is not also
         *   on a different list */
        item = ompi_list_remove_first(&(mca_ptl_sm.sm_pending_ack));
        while ( item != ompi_list_get_end(&(mca_ptl_sm.sm_pending_ack)) ) {

            /* get fragment pointer */
            header_ptr = (mca_ptl_sm_frag_t *)item;

            /* change address to address relative to the shared memory
             *   segment base */
            sm_frag_desc_rel_to_base= (char *) ( (char *)header_ptr -
                    mca_ptl_sm_component.sm_offset );

            /* try and send an ack */
            return_status=ompi_fifo_write_to_head( sm_frag_desc_rel_to_base,
                    send_fifo,
                    mca_ptl_sm_component.sm_mpool,
                    mca_ptl_sm_component.sm_offset);

            /* if ack failed, break */
            if( OMPI_SUCCESS != return_status ) {
                /* put the descriptor back on the list */
                ompi_list_prepend(&(mca_ptl_sm.sm_pending_ack),item);
                break;
            }

            /* get next fragment to ack */
            item = ompi_list_remove_first(&(mca_ptl_sm.sm_pending_ack));

        }

        OMPI_THREAD_UNLOCK(&(mca_ptl_sm.sm_pending_ack_lock));
    }

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

