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
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>  /* for mkfifo */

#include "include/constants.h"
#include "include/sys/cache.h"
#include "opal/event/event.h"
#include "util/if.h"
#include "util/argv.h"
#include "util/output.h"
#include "util/sys_info.h"
#include "util/proc_info.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/base/mca_base_param.h"
#include "mca/ptl/sm/src/ptl_sm.h"
#include "mca/mpool/base/base.h"
#include "mca/oob/base/base.h"
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
    mca_ptl_sm_component.sm_extra_procs =
        mca_ptl_sm_param_register_int("sm_extra_procs", -1);
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
        mca_ptl_sm_param_register_int("cb_lazy_free_freq", 120);
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
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_send_requests, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_first_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_second_frags, ompi_free_list_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_pending_ack_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_pending_ack, opal_list_t);

    return OMPI_SUCCESS;
}


/*
 * component cleanup - sanity checking of queue lengths
 */

int mca_ptl_sm_component_close(void)
{
    int return_value=OMPI_SUCCESS;

    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_lock);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_send_requests);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_first_frags);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_second_frags);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_pending_ack_lock);
    OBJ_DESTRUCT(&mca_ptl_sm_component.sm_pending_ack);

    /* unmap the shared memory control structure */
    if(mca_ptl_sm_component.mmap_file != NULL) {
        return_value=munmap(mca_ptl_sm_component.mmap_file->map_addr,
                mca_ptl_sm_component.mmap_file->map_size);
        if(-1 == return_value) {
            return_value=OMPI_ERROR;
            ompi_output(0," munmap failed :: file - %s :: errno - %d \n",
                    mca_ptl_sm_component.mmap_file->map_addr,
                    errno);
            goto CLEANUP;
        }
    
        /* unlink file, so that it will be deleted when all references
         * to it are gone - no error checking, since we want all procs
         * to call this, so that in an abnormal termination scanario,
         * this file will still get cleaned up */
        unlink(mca_ptl_sm_component.mmap_file->map_path);
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* close/cleanup fifo create for event notification */
    if(mca_ptl_sm_component.sm_fifo_fd > 0) {
        /* write a done message down the pipe */
        unsigned char cmd = DONE;
        if( write(mca_ptl_sm_component.sm_fifo_fd,&cmd,sizeof(cmd)) != 
                sizeof(cmd)){
            ompi_output(0, "mca_ptl_sm_component_close: write fifo failed: errno=%d\n",
                    errno);
        }
        opal_thread_join(&mca_ptl_sm_component.sm_fifo_thread, NULL);
        close(mca_ptl_sm_component.sm_fifo_fd);
        unlink(mca_ptl_sm_component.sm_fifo_path);
    }
#endif


CLEANUP:

    /* return */
    return return_value;
}


/*
 *  SM component initialization
 */
mca_ptl_base_module_t** mca_ptl_sm_component_init(
    int *num_ptls, 
    bool enable_progress_threads,
    bool enable_mpi_threads)
{
    mca_ptl_base_module_t **ptls = NULL;
    int i;

    *num_ptls = 0;

    /* lookup/create shared memory pool only when used */
    mca_ptl_sm_component.sm_mpool = NULL;
    mca_ptl_sm_component.sm_mpool_base = NULL;

    /* publish shared memory parameters with the MCA framework */
    if (OMPI_SUCCESS != mca_ptl_sm_component_exchange()) {
        return NULL;
    }

#if OMPI_ENABLE_PROGRESS_THREADS == 1
    /* create a named pipe to receive events  */
    sprintf(mca_ptl_sm_component.sm_fifo_path, 
        "%s/sm_fifo.%d", orte_process_info.job_session_dir,
         orte_process_info.my_name->vpid);
    if(mkfifo(mca_ptl_sm_component.sm_fifo_path, 0660) < 0) {
        ompi_output(0, "mca_ptl_sm_component_init: mkfifo failed with errno=%d\n",errno);
        return NULL;
    }
    mca_ptl_sm_component.sm_fifo_fd = open(mca_ptl_sm_component.sm_fifo_path, O_RDWR);
    if(mca_ptl_sm_component.sm_fifo_fd < 0) {
        ompi_output(0, "mca_ptl_sm_component_init: open(%s) failed with errno=%d\n",
            mca_ptl_sm_component.sm_fifo_path, errno);
        return NULL;
    }

    OBJ_CONSTRUCT(&mca_ptl_sm_component.sm_fifo_thread, opal_thread_t);
    mca_ptl_sm_component.sm_fifo_thread.t_run = (opal_thread_fn_t) mca_ptl_sm_component_event_thread;
    opal_thread_start(&mca_ptl_sm_component.sm_fifo_thread);
#endif

    /* allocate the Shared Memory PTL */
    *num_ptls = 2;
    ptls = malloc((*num_ptls)*sizeof(mca_ptl_base_module_t*));
    if (NULL == ptls) {
        return NULL;
    }

    /* get pointer to the ptls */
    ptls[0] = (mca_ptl_base_module_t *)(&(mca_ptl_sm[0]));
    ptls[1] = (mca_ptl_base_module_t *)(&(mca_ptl_sm[1]));

    /* set scheduling parameters */
    for( i=0 ; i < 2 ; i++ ) {
        mca_ptl_sm[i].super.ptl_cache_size=mca_ptl_sm_component.sm_first_frag_free_list_max;
        mca_ptl_sm[i].super.ptl_cache_bytes=sizeof(mca_ptl_sm_send_request_t) -
                sizeof(mca_ptl_base_send_request_t);
        mca_ptl_sm[i].super.ptl_first_frag_size=mca_ptl_sm_component.first_fragment_size;
        mca_ptl_sm[i].super.ptl_min_frag_size=mca_ptl_sm_component.max_fragment_size;
        mca_ptl_sm[i].super.ptl_max_frag_size=mca_ptl_sm_component.max_fragment_size;
        mca_ptl_sm[i].super.ptl_exclusivity=100;  /* always use this ptl */
        mca_ptl_sm[i].super.ptl_latency=100;      /* lowest latency */
        mca_ptl_sm[i].super.ptl_bandwidth=900; /* not really used now since
                                     exclusivity is set to 100 */
    }

    /* initialize some PTL data */
    /* start with no SM procs */
    mca_ptl_sm_component.num_smp_procs = 0;
    mca_ptl_sm_component.my_smp_rank   = 0xFFFFFFFF;  /* not defined */

    /* set flag indicating ptl not inited */
    mca_ptl_sm[0].ptl_inited=false;
    mca_ptl_sm[1].ptl_inited=false;

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

#if OMPI_ENABLE_PROGRESS_THREADS == 1
void mca_ptl_sm_component_event_thread(opal_object_t* thread)
{
    while(1) {
        unsigned char cmd;
        if(read(mca_ptl_sm_component.sm_fifo_fd, &cmd, sizeof(cmd)) != sizeof(cmd)) {
            /* error condition */
            return;
        }
        if( DONE == cmd ){
            /* return when done message received */
            return;
        } 
        mca_ptl_sm_component_progress(0);
    }
}
#endif


int mca_ptl_sm_component_progress(mca_ptl_tstamp_t tstamp)
{
    /* local variables */
    int my_local_smp_rank, proc;
    unsigned int peer_local_smp_rank ;
    mca_ptl_sm_frag_t *header_ptr;
    ompi_fifo_t *send_fifo = NULL;
    bool frag_matched;
    mca_ptl_base_match_header_t *matching_header;
    mca_ptl_base_send_request_t *base_send_req;
    opal_list_item_t *item;
    int return_status = 0;

    my_local_smp_rank=mca_ptl_sm_component.my_smp_rank;

    /* send progress is made by the PML */

    /* 
     * receive progress 
     */

    /* poll each fifo */

    /* loop over fifo's - procs with same base shared memory 
     * virtual address as this process */
    for( proc=0 ; proc < mca_ptl_sm_component.num_smp_procs_same_base_addr
            ; proc++ ) 
    {
        peer_local_smp_rank=
            mca_ptl_sm_component.list_smp_procs_same_base_addr[proc];

        send_fifo=&(mca_ptl_sm_component.fifo
                [peer_local_smp_rank][my_local_smp_rank]);

        /* if fifo is not yet setup - continue - not data has been sent*/
        if(OMPI_CB_FREE == send_fifo->tail){
            continue;
        }

        /* aquire thread lock */
        if( opal_using_threads() ) {
            opal_atomic_lock( &(send_fifo->tail_lock) );
        }

        /* get pointer - pass in offset to change queue pointer
         * addressing from that of the sender */
        header_ptr = (mca_ptl_sm_frag_t *)
	    ompi_fifo_read_from_tail_same_base_addr( send_fifo );
        if( OMPI_CB_FREE == header_ptr ) {
            /* release thread lock */
            if( opal_using_threads() ) {
                opal_atomic_unlock(&(send_fifo->tail_lock));
            }
            continue;
        }

        /* release thread lock */
        if( opal_using_threads() ) {
            opal_atomic_unlock(&(send_fifo->tail_lock));
        }

        /* figure out what type of message this is */
        return_status++;
        switch
            (header_ptr->super.frag_base.frag_header.hdr_common.hdr_type)
            {
        
                case MCA_PTL_HDR_TYPE_MATCH:
                    /* set the owning ptl */
                    header_ptr->super.frag_base.frag_owner=
                        (mca_ptl_base_module_t *) (&mca_ptl_sm[0]);
                    /* attempt match */
                    matching_header= &(header_ptr->super.frag_base.frag_header.hdr_match);
                    frag_matched = header_ptr->super.frag_base.frag_owner->ptl_match(
                        header_ptr->super.frag_base.frag_owner, &(header_ptr->super),
                        matching_header );
                    break;

                case MCA_PTL_HDR_TYPE_FRAG:
                    /* set the owning ptl */
                    header_ptr->super.frag_base.frag_owner=
                        (mca_ptl_base_module_t *) (&mca_ptl_sm[0]);
                    /* second and beyond fragment - just need to deliver
                     * the data, and ack */
                    mca_ptl_sm_matched_same_base_addr(
                            (mca_ptl_base_module_t *)&mca_ptl_sm,
                            (mca_ptl_base_recv_frag_t *)header_ptr);
                    break;

                case MCA_PTL_HDR_TYPE_ACK:
                    /* ack */
                    /* update the send statistics */
                    /* NOTE !!! : need to change the update stats,
                     *   so that MPI_Wait/Test on the send can complete
                     *   as soon as the data is copied intially into
                     *   the shared memory buffers */

                    header_ptr->send_ptl->ptl_send_progress(
                                (mca_ptl_base_module_t *)&mca_ptl_sm,
                                header_ptr->send_req,
                                header_ptr->super.frag_base.frag_size);

                    /* if this is not the first fragment, recycle
                     * resources.  The first fragment is handled by
                     * the PML */
                    if( 0 < header_ptr->send_offset ) {
                        OMPI_FREE_LIST_RETURN(&mca_ptl_sm_component.sm_second_frags,
                                (opal_list_item_t *)header_ptr);
                    } 
                    break;

                default:
                    fprintf(stderr," Warnning: mca_ptl_sm_component_progress - unrecognized fragment type \n");
                    fflush(stderr);

            }

    }  /* end peer_local_smp_rank loop */

    /* loop over fifo's - procs with different base shared memory 
     * virtual address as this process */
    for( proc=0 ; proc < mca_ptl_sm_component.num_smp_procs_different_base_addr
            ; proc++ ) 
    {
        peer_local_smp_rank=
            mca_ptl_sm_component.list_smp_procs_different_base_addr[proc];

        send_fifo=&(mca_ptl_sm_component.fifo
                [peer_local_smp_rank][my_local_smp_rank]);

        /* if fifo is not yet setup - continue - not data has been sent*/
        if(OMPI_CB_FREE == send_fifo->tail){
            continue;
        }

        /* aquire thread lock */
        if( opal_using_threads() ) {
            opal_atomic_lock(&(send_fifo->tail_lock));
        }

        /* get pointer - pass in offset to change queue pointer
         * addressing from that of the sender */
        header_ptr=(mca_ptl_sm_frag_t *)ompi_fifo_read_from_tail( send_fifo,
                mca_ptl_sm_component.sm_offset[peer_local_smp_rank]);
        if( OMPI_CB_FREE == header_ptr ) {
            /* release thread lock */
            if( opal_using_threads() ) {
                opal_atomic_unlock(&(send_fifo->tail_lock));
            }
            continue;
        }

        /* release thread lock */
        if( opal_using_threads() ) {
            opal_atomic_unlock(&(send_fifo->tail_lock));
        }

        /* change the address from address relative to the shared
         * memory address, to a true virtual address */
        header_ptr = (mca_ptl_sm_frag_t *)( (char *)header_ptr+
                mca_ptl_sm_component.sm_offset[peer_local_smp_rank]);


        /* figure out what type of message this is */
        return_status++;
        switch
            (header_ptr->super.frag_base.frag_header.hdr_common.hdr_type)
            {
        
                case MCA_PTL_HDR_TYPE_MATCH:
                    /* set the owning ptl */
                    header_ptr->super.frag_base.frag_owner=
                        (mca_ptl_base_module_t *) (&mca_ptl_sm[1]);
                    /* attempt match */
                    matching_header= &(header_ptr->super.frag_base.frag_header.hdr_match);
                    frag_matched = header_ptr->super.frag_base.frag_owner->ptl_match(
                        header_ptr->super.frag_base.frag_owner, &(header_ptr->super),
                        matching_header );
                    break;

                case MCA_PTL_HDR_TYPE_FRAG:
                    /* set the owning ptl */
                    header_ptr->super.frag_base.frag_owner=
                        (mca_ptl_base_module_t *) (&mca_ptl_sm[1]);
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
                        hdr_rndv.hdr_src_ptr.pval;

                    header_ptr->send_ptl->ptl_send_progress(
                                (mca_ptl_base_module_t *)&mca_ptl_sm,
                                base_send_req,
                                header_ptr->super.frag_base.frag_size);

                    /* if this is not the first fragment, recycle
                     * resources.  The first fragment is handled by
                     * the PML */
                    if( 0 < header_ptr->send_offset ) {
                        OMPI_FREE_LIST_RETURN(&mca_ptl_sm_component.sm_second_frags,
                                (opal_list_item_t *)header_ptr);
                    } 
                    break;

                default:
                    fprintf(stderr," Warnning: mca_ptl_sm_component_progress - unrecognized fragment type \n");
                    fflush(stderr);

            }

    }  /* end peer_local_smp_rank loop */


    /* progress acks */
    if( !opal_list_is_empty(&(mca_ptl_sm_component.sm_pending_ack)) ) {

        OPAL_THREAD_LOCK(&(mca_ptl_sm_component.sm_pending_ack_lock));

        /* remove ack from list - need to remove from list before
         *   sending the ack, so that when the ack is recieved,
         *   manipulated, and put on a new list, it is not also
         *   on a different list */
        item = opal_list_remove_first(&(mca_ptl_sm_component.sm_pending_ack));
        while ( item != opal_list_get_end(&(mca_ptl_sm_component.sm_pending_ack)) ) {
            int rc;
            /* get fragment pointer */
            header_ptr = (mca_ptl_sm_frag_t *)item;

            /* try and send an ack - no need to check and see if a send
             * queue has been allocated, since entries are put here only
             * if the queue was previously full */

            /* fragment already marked as an ack */

            rc=ompi_fifo_write_to_head_same_base_addr(header_ptr,
                    send_fifo, mca_ptl_sm_component.sm_mpool);

            /* if ack failed, break */
            if( 0 > rc ) {
                /* put the descriptor back on the list */
                opal_list_prepend(&(mca_ptl_sm_component.sm_pending_ack),item);
                break;
            }
            MCA_PTL_SM_SIGNAL_PEER(mca_ptl_sm_component.sm_peers[header_ptr->queue_index]);

            /* get next fragment to ack */
            item = opal_list_remove_first(&(mca_ptl_sm_component.sm_pending_ack));

        }

        OPAL_THREAD_UNLOCK(&(mca_ptl_sm_component.sm_pending_ack_lock));
    }
    return return_status;
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
    len=strlen(orte_system_info.nodename);
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
            orte_system_info.nodename, len);
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

