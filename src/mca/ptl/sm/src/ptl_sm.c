/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>

#include "util/output.h"
#include "util/if.h"
#include "mca/pml/pml.h"
#include "mca/ptl/ptl.h"
#include "mca/pml/base/pml_base_sendreq.h"
#include "mca/pml/base/pml_base_recvreq.h"
#include "mca/ptl/base/ptl_base_header.h"
#include "mca/ptl/base/ptl_base_sendfrag.h"
#include "mca/ptl/base/ptl_base_recvfrag.h"
#include "mca/base/mca_base_module_exchange.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "ptl_sm.h"
#include "util/sys_info.h"
#include "mca/ptl/sm/src/ptl_sm_peer.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "util/proc_info.h"
#include "util/printf.h"
#include "mca/ptl/sm/src/ptl_sm_sendreq.h"
#include "class/ompi_fifo.h"
#include "class/ompi_free_list.h"
#include "threads/mutex.h"
#include "datatype/datatype.h"

mca_ptl_sm_t mca_ptl_sm[2] = {
    {
        {
        &mca_ptl_sm_component.super,
        20, /* number of elements in the send descriptor cache */
        sizeof(mca_ptl_sm_send_request_t) -
            sizeof(mca_pml_base_send_request_t),  /* size of shared memory send
                                                     descriptor */
        1, /* ptl_first_frag_size */
        0, /* ptl_min_frag_size */
        0, /* ptl_max_frag_size */
        0, /* ptl_exclusivity */
        0, /* ptl_latency */
        0, /* ptl_bandwidth */
        MCA_PTL_PUT,  /* ptl flags */
        mca_ptl_sm_add_procs_same_base_addr,
        mca_ptl_sm_del_procs,
        mca_ptl_sm_finalize,
        mca_ptl_sm_send,  /* first fragment send function */
        mca_ptl_sm_send_continue,  /* second and subsequent send function */
        NULL,  /* get function */
        mca_ptl_sm_matched_same_base_addr, /* function called after match is made */
        mca_ptl_sm_request_alloc, /* initialization routine */
        mca_ptl_sm_request_return
        }
    },
    {
        {
            &mca_ptl_sm_component.super,
            20, /* number of elements in the send descriptor cache */
            sizeof(mca_ptl_sm_send_request_t) -
                sizeof(mca_pml_base_send_request_t),  /* size of shared memory 
                                                         send descriptor */
            1, /* ptl_first_frag_size */
            0, /* ptl_min_frag_size */
            0, /* ptl_max_frag_size */
            0, /* ptl_exclusivity */
            0, /* ptl_latency */
            0, /* ptl_bandwidth */
            MCA_PTL_PUT,  /* ptl flags */
            mca_ptl_sm_add_procs,
            mca_ptl_sm_del_procs,
            mca_ptl_sm_finalize,
            mca_ptl_sm_send,  /* first fragment send function */
            mca_ptl_sm_send_continue,  /* second and subsequent send function */
            NULL,  /* get function */
            mca_ptl_sm_matched, /* function called after match is made */
            mca_ptl_sm_request_alloc, /* initialization routine */
            mca_ptl_sm_request_return
        }
    }
};

/* track information needed to synchronise a Shared Memory PTL module */
mca_ptl_sm_module_resource_t mca_ptl_sm_module_resource;


int mca_ptl_sm_add_procs_same_base_addr(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_ptl_base_peer_t **peers,
    ompi_bitmap_t* reachability)
{
    int return_code=OMPI_SUCCESS;
    size_t i,j,proc,size,len,my_len,n_to_allocate,length;
    int n_local_procs,cnt;
    mca_ptl_sm_exchange_t **sm_proc_info;
    ompi_proc_t* my_proc; /* pointer to caller's proc structure */
    mca_ptl_sm_t *ptl_sm;
    ompi_fifo_t *my_fifos;
/*
    volatile ompi_fifo_t **fifo_tmp;
*/
    ompi_fifo_t * volatile *fifo_tmp;
    bool same_sm_base;
    ssize_t diff;

    /* initializion */
    for(i=0 ; i < nprocs ; i++ ) {
        peers[i]=NULL;
    }
    ptl_sm=(mca_ptl_sm_t *)ptl;

    /* allocate array to hold setup shared memory from all
     * other procs */
    sm_proc_info=(mca_ptl_sm_exchange_t **)
        malloc(nprocs*sizeof(mca_ptl_sm_exchange_t *));
    if( NULL == sm_proc_info ){
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    mca_ptl_sm_component.sm_proc_connect=(int *) malloc(nprocs*sizeof(int));
    if( NULL == mca_ptl_sm_component.sm_proc_connect ){
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* initialize sm_proc_info and sm_proc_connect*/
    for(proc=0 ; proc < nprocs ; proc++ ) {
        sm_proc_info[proc]=0;
        mca_ptl_sm_component.sm_proc_connect[proc]=0;
    }

    /* get pointer to my proc structure */
    my_proc=ompi_proc_local();
    if( NULL == my_proc ) {
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    my_len=strlen(ompi_system_info.nodename);

    /* Get unique host identifier for each process in the list,
     * and idetify procs that are on this host.  Add procs on this
     * host to shared memory reachbility list.  Also, get number
     * of local procs in the prcs list. */
    n_local_procs=0;
    for( proc=0 ; proc < nprocs; proc++ ) {
        /* check to see if this is me */
        if( my_proc == procs[proc] ) {
            mca_ptl_sm_component.my_smp_rank=n_local_procs;
        }
        return_code = mca_base_modex_recv(
                &mca_ptl_sm_component.super.ptlm_version, procs[proc],
                (void**)(&(sm_proc_info[proc])), &size);
        if(return_code != OMPI_SUCCESS) {
            ompi_output(0, "mca_ptl_sm_add_procs: mca_base_modex_recv: failed with return value=%d", return_code);
            goto CLEANUP;
        }

       /* for zero length, just continue - comparison is meaningless*/
       if( 0 >= size ) {
           continue;
       }

       /* check to see if this proc is on my host */
       len=strlen((char *)(sm_proc_info[proc]));
       if( len == my_len ) {
           if( 0 == strncmp(ompi_system_info.nodename,
                       (char *)(sm_proc_info[proc]),len) ) {

               /* initialize the peers information */
               peers[proc]=malloc(sizeof(struct mca_ptl_base_peer_t));
               if( NULL == peers[proc] ){
                   return_code=OMPI_ERR_OUT_OF_RESOURCE;
                   goto CLEANUP;
               }
               peers[proc]->peer_smp_rank=n_local_procs+
                   mca_ptl_sm_component.num_smp_procs;
               n_local_procs++;

               /* */
               mca_ptl_sm_component.sm_proc_connect[proc]=SM_CONNECTED;
           }
       }

    }

    /* make sure that my_smp_rank has been defined */
    if(-1 == mca_ptl_sm_component.my_smp_rank){
        return_code=OMPI_ERROR;
        goto CLEANUP;
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access */
    for( proc=0 ; proc < nprocs; proc++ ) {
        if(NULL != peers[proc] ) {
            peers[proc]->my_smp_rank=mca_ptl_sm_component.my_smp_rank;
        }
    }
            
    /* see if need to allocate space for extra procs */
    if(  0 > mca_ptl_sm_component.sm_max_procs ) {
        /* no limit */
        if( 0 <= mca_ptl_sm_component.sm_extra_procs ) {
            /* limit */
            mca_ptl_sm_component.sm_max_procs=n_local_procs+
                mca_ptl_sm_component.sm_extra_procs;
        } else {
            /* no limit */
            mca_ptl_sm_component.sm_max_procs=2*n_local_procs;
        }
    }
    n_to_allocate=mca_ptl_sm_component.sm_max_procs;

    /* make sure n_to_allocate is greater than 0 */

    /* set the shared memory offset */
    mca_ptl_sm_component.sm_offset=(size_t *)
        malloc(n_to_allocate*sizeof(size_t));
    if(NULL == mca_ptl_sm_component.sm_offset ) {
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* Allocate Shared Memory PTL process coordination
     * data structure.  This will reside in shared memory */

    /* 
     * Create backing file - only first time through 
     */
    if ( !mca_ptl_sm[0].ptl_inited ) {
        /* set file name */
        len=asprintf(&(mca_ptl_sm_component.sm_resouce_ctl_file),
                "%s/shared_mem_ptl_module.%s",ompi_process_info.job_session_dir,
                ompi_system_info.nodename);
        if( 0 > len ) {
            goto CLEANUP;
        }

        size=sizeof(mca_ptl_sm_module_resource_t);
        if(NULL==(mca_ptl_sm_component.mmap_file=mca_common_sm_mmap_init(size,
                        mca_ptl_sm_component.sm_resouce_ctl_file,
                        sizeof(mca_ptl_sm_module_resource_t), 8 ))) 
        {
            ompi_output(0, "mca_ptl_sm_add_procs: unable to create shared memory PTL coordinating strucure :: size %ld \n",
                    size);
            return_code=OMPI_ERROR;
            goto CLEANUP;
        }

        /* set the pointer to the shared memory control structure */
        mca_ptl_sm_component.sm_ctl_header=(mca_ptl_sm_module_resource_t *)
            mca_ptl_sm_component.mmap_file->map_seg;


        /* Allocate a fixed size pointer array for the 2-D Shared memory queues.
         * Excess slots will be allocated for future growth.  One could
         * make this array growable, but then one would need to uses mutexes
         * for any access to these queues to ensure data consistancy when
         * the array is grown */
   
        if(0 == mca_ptl_sm_component.my_smp_rank ) {
            /* allocate ompi_fifo_t strucutes for each fifo of the queue
             * pairs - one per pair of local processes */
            /* check to make sure number of local procs is within the
             * specified limits */
            if( ( 0 < mca_ptl_sm_component.sm_max_procs ) &&
                    ( n_local_procs > mca_ptl_sm_component.sm_max_procs) ) {
                return_code=OMPI_ERROR;
                goto CLEANUP;
            }

            /* allocate array of ompi_fifo_t* elements -
             * offset relative to base segement is stored, so that
             * this can be used by other procs */
            mca_ptl_sm_component.sm_ctl_header->fifo=
                mca_ptl_sm_component.sm_mpool->mpool_alloc
                (n_to_allocate*sizeof(ompi_fifo_t *),
                 CACHE_LINE_SIZE);
            if ( NULL == mca_ptl_sm_component.sm_ctl_header->fifo ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            /* initiazlize the pointer array */
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_ptl_sm_component.sm_ctl_header->fifo[i]=NULL;
            }

            /*  allocate and initialize the array to hold the virtual address
             *  of the shared memory base */
            mca_ptl_sm_component.sm_ctl_header->segment_header.
                base_shared_mem_segment =  ( volatile char **)
                mca_ptl_sm_component.sm_mpool->mpool_alloc
                (n_to_allocate*sizeof(char *), CACHE_LINE_SIZE);
            if ( NULL == mca_ptl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            /* initialize the pointer array */
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_ptl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment[i]=NULL;
            }

            /*  allocate and initialize the array of flags indicating
             *  when the virtual address of the shared memory address
             *  has been set */
            mca_ptl_sm_component.sm_ctl_header->segment_header.
                base_shared_mem_flags = ( int *)
                mca_ptl_sm_component.sm_mpool->mpool_alloc
                (n_to_allocate*sizeof(int), CACHE_LINE_SIZE);
            if ( NULL == mca_ptl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_flags ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_ptl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_flags[i]=0;
            }

            /* set the fifo address to be a relative address, so that
             * it can be used by other procs */
            mca_ptl_sm_component.sm_ctl_header->fifo=
                (volatile ompi_fifo_t **)
                ( (char *)(mca_ptl_sm_component.sm_ctl_header->fifo)-
                  (char *)(mca_ptl_sm_component.sm_mpool->mpool_base()) );


            /* allow other procs to use this shared memory map */
            mca_ptl_sm_component.mmap_file->map_seg->seg_inited=true;
        }
   
        /* Note:  Need to make sure that proc 0 initializes control
         * structures before any of the other procs can progress */
        if( 0 != mca_ptl_sm_component.my_smp_rank ) 
        {
            /* spin unitl local proc 0 initializes the segment */
            while(!mca_ptl_sm_component.mmap_file->map_seg->seg_inited)
            { ; }
        }
                
        /* set the base of the shared memory segment, and flag
         * indicating that it is set */
        mca_ptl_sm_component.sm_ctl_header->segment_header.
            base_shared_mem_segment[mca_ptl_sm_component.my_smp_rank]=
            mca_ptl_sm_component.sm_mpool->mpool_base();
        /* RLG:  need memory barrier */
        mca_ptl_sm_component.sm_ctl_header->segment_header.
            base_shared_mem_flags[mca_ptl_sm_component.my_smp_rank]=1;

        /*
         * initialize the array of fifo's "owned" by this process
         * The virtual addresses are valid only in the sender's
         * address space - unless the base of the shared memory
         * segment is mapped at the same location in the reader's
         * virtual address space.
         */
        my_fifos=( ompi_fifo_t *)
            mca_ptl_sm_component.sm_mpool->mpool_alloc
            (n_to_allocate*sizeof(ompi_fifo_t *), CACHE_LINE_SIZE);
        if ( NULL == my_fifos ) {
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        for( j=0 ; j < n_to_allocate ; j++ ) {
            my_fifos[j].head=OMPI_CB_FREE;
            my_fifos[j].tail=OMPI_CB_FREE;
            ompi_atomic_unlock(&(my_fifos[j].head_lock));
            ompi_atomic_unlock(&(my_fifos[j].tail_lock));
        }
        fifo_tmp=(ompi_fifo_t **)
                ( (char *)(mca_ptl_sm_component.sm_ctl_header->fifo) +
                  (size_t)(mca_ptl_sm_component.sm_mpool->mpool_base()) );
        /* RLG : need memory barrier */
        fifo_tmp[mca_ptl_sm_component.my_smp_rank]=my_fifos;

        /* cache the pointer to the 2d fifo array.  These addresses
         * are valid in the current process space */
        mca_ptl_sm_component.fifo=(ompi_fifo_t **)
            malloc(sizeof(ompi_fifo_t *)*n_to_allocate);
        if( NULL == mca_ptl_sm_component.fifo ) {
            return_code=OMPI_ERROR;
            goto CLEANUP;
        }
        mca_ptl_sm_component.fifo[mca_ptl_sm_component.my_smp_rank]=my_fifos;

    }

    /* cache the pointers to the rest of the fifo arrays */
    fifo_tmp=(ompi_fifo_t **)
        ( (char *)(mca_ptl_sm_component.sm_ctl_header->fifo) +
          (size_t)(mca_ptl_sm_component.sm_mpool->mpool_base()) );
    for( j=mca_ptl_sm_component.num_smp_procs ; j <
            mca_ptl_sm_component.num_smp_procs+n_local_procs ; j++ ) {

        /* spin until this element is allocated */
        while ( NULL == fifo_tmp[j] )
        { ; }

        diff= (mca_ptl_sm_component.sm_ctl_header->
             segment_header.base_shared_mem_segment
             [mca_ptl_sm_component.my_smp_rank])-
            mca_ptl_sm_component.sm_ctl_header->
            segment_header.base_shared_mem_segment[j];
        mca_ptl_sm_component.fifo[j]=
            ( ompi_fifo_t *)( (char *)fifo_tmp[j]+diff);
        mca_ptl_sm_component.sm_offset[j]=mca_ptl_sm_component.
            sm_ctl_header->segment_header.base_shared_mem_segment[j]-
            mca_ptl_sm_component.sm_ctl_header->
            segment_header.base_shared_mem_segment[mca_ptl_sm_component.my_smp_rank];
                
    }

    /* initialize some of the free-lists */
    if( !mca_ptl_sm[0].ptl_inited ) {
        /* some initialization happens only the first time this routine
         * is called, i.e. when ptl_inited is false */

        /* initialize fragment descriptor free list */

        /* 
         * first fragment 
         */

        /* allocation will be for the fragment descriptor, payload buffer,
         * and padding to ensure proper alignment can be acheived */
        length=sizeof(mca_ptl_sm_frag_t)+mca_ptl_sm_component.fragment_alignment+
            mca_ptl_sm_component.first_fragment_size;

        ompi_free_list_init(&mca_ptl_sm_component.sm_first_frags, length,
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

        ompi_free_list_init(&mca_ptl_sm_component.sm_second_frags, length,
                OBJ_CLASS(mca_ptl_sm_second_frag_t),
                mca_ptl_sm_component.sm_second_frag_free_list_num,
                mca_ptl_sm_component.sm_second_frag_free_list_max,
                mca_ptl_sm_component.sm_second_frag_free_list_inc,
                mca_ptl_sm_component.sm_mpool); /* use shared-memory pool */

        /* set up mca_ptl_sm_component.list_smp_procs_same_base_addr */
        mca_ptl_sm_component.list_smp_procs_same_base_addr=(int *)
            malloc(mca_ptl_sm_component.sm_max_procs*sizeof(int));
        if( NULL == mca_ptl_sm_component.list_smp_procs_same_base_addr ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* set up mca_ptl_sm_component.list_smp_procs_different_base_addr */
        mca_ptl_sm_component.list_smp_procs_different_base_addr=(int *)
            malloc(mca_ptl_sm_component.sm_max_procs*sizeof(int));
        if( NULL == mca_ptl_sm_component.list_smp_procs_different_base_addr ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* set flag indicating ptl has been inited */
        ptl_sm->ptl_inited=true;
    }

    /* set connectivity */
    cnt=0;
    for(proc = 0 ; proc < nprocs ; proc++ ) {

        same_sm_base=mca_ptl_sm_component.sm_ctl_header->
            segment_header.base_shared_mem_segment
            [proc+mca_ptl_sm_component.num_smp_procs] ==
            mca_ptl_sm_component.sm_ctl_header->
            segment_header.base_shared_mem_segment
            [mca_ptl_sm_component.my_smp_rank];

        if( SM_CONNECTED == mca_ptl_sm_component.sm_proc_connect[proc] ) {
            if( same_sm_base ){

                /* don't count if same process */
                if( (mca_ptl_sm_component.num_smp_procs+cnt ) ==
                        mca_ptl_sm_component.my_smp_rank) {
                    cnt++;
                    continue;
                }
                /* set up the list of local processes with the same base
                 * shared memory virtual address as this process */
                mca_ptl_sm_component.list_smp_procs_same_base_addr
                    [mca_ptl_sm_component.num_smp_procs_same_base_addr]=
                    cnt;
                mca_ptl_sm_component.num_smp_procs_same_base_addr++;
                cnt++;
                /* add this proc to shared memory accessability list */
                return_code=ompi_bitmap_set_bit(reachability,proc);
                if( OMPI_SUCCESS != return_code ){
                    goto CLEANUP;
                }
            } else {
                /* set up the list of local processes with the same base
                 * shared memory virtual address as this process */
                mca_ptl_sm_component.list_smp_procs_different_base_addr
                    [mca_ptl_sm_component.num_smp_procs_different_base_addr]=
                    cnt;
                mca_ptl_sm_component.num_smp_procs_different_base_addr++;
                cnt++;
                mca_ptl_sm_component.sm_proc_connect[proc]=
                    SM_CONNECTED_DIFFERENT_BASE_ADDR;
            }
        }
    }

    /* update the local smp process count */
    mca_ptl_sm_component.num_smp_procs+=n_local_procs;

CLEANUP:
    /* free local memory */
    if(sm_proc_info){
       /* free the memory allocated by mca_base_modex_recv */
        for( proc=0 ; proc < nprocs; proc++ ) {
            if(sm_proc_info[proc]){
                free(sm_proc_info[proc]);
                sm_proc_info[proc]=NULL;
            }
        }
        free(sm_proc_info);
        sm_proc_info=NULL;
    }

    return return_code;
}

/* Note:: this routine assumes that mca_ptl_sm_add_procs_same_base_addr
 *  has already been called to set up data structures needed by this
 *  routine */
int mca_ptl_sm_add_procs(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_ptl_base_peer_t **peers,
    ompi_bitmap_t* reachability)
{
    int return_code=OMPI_SUCCESS,proc;

    /* set connectivity */
    for(proc = 0 ; proc < nprocs ; proc++ ) {
        if( SM_CONNECTED_DIFFERENT_BASE_ADDR == 
                mca_ptl_sm_component.sm_proc_connect[proc] ){
            /* add this proc to shared memory accessability list */
            return_code=ompi_bitmap_set_bit(reachability,proc);
            if( OMPI_SUCCESS != return_code ){
                goto CLEANUP;
            }
        }
    }

CLEANUP:
    /* free local memory */
    if(mca_ptl_sm_component.sm_proc_connect){
        free(mca_ptl_sm_component.sm_proc_connect);
        mca_ptl_sm_component.sm_proc_connect=NULL;
    }

    return return_code;
}

int mca_ptl_sm_del_procs(
    struct mca_ptl_base_module_t* ptl, 
    size_t nprocs,
    struct ompi_proc_t **procs, 
    struct mca_ptl_base_peer_t **peers)
{
    return OMPI_SUCCESS;
}


int mca_ptl_sm_finalize(struct mca_ptl_base_module_t* ptl)
{
    return OMPI_SUCCESS;
}


int mca_ptl_sm_request_alloc(struct mca_ptl_base_module_t* ptl, struct mca_pml_base_send_request_t* request)
{
    mca_ptl_sm_send_request_t *sm_request;
    ompi_list_item_t* item;
    int rc;

    /* allocate shared memory, first fragment */
    OMPI_FREE_LIST_GET(&(mca_ptl_sm_component.sm_first_frags),item,rc);
    if( OMPI_SUCCESS != rc ) {
        return rc;
    }

    /* associate this fragment with the send descriptor */
    sm_request=(mca_ptl_sm_send_request_t *)request;
    sm_request->req_frag=(mca_ptl_sm_frag_t *)item;

    return OMPI_SUCCESS;
}


void mca_ptl_sm_request_return(struct mca_ptl_base_module_t* ptl, struct mca_pml_base_send_request_t* request)
{
    mca_ptl_sm_send_request_t *sm_request;
    ompi_list_item_t* item;

    /* return the fragment descriptor to the free list */
    sm_request=(mca_ptl_sm_send_request_t *)request;
    item=(ompi_list_item_t *)sm_request->req_frag;
    OMPI_FREE_LIST_RETURN(&(mca_ptl_sm_component.sm_first_frags),item);

}

/*
 *  Initiate a send.  The fragment descriptor allocated with the 
 *  send requests.  If the send descriptor is NOT obtained from
 *  the cache, this implementation will ONLY return an error code.
 *  If we don't do this, then, because we rely on memory ordering
 *  to provide the required MPI message ordering, we would need to
 *  add logic to check and see if there are any other sends waiting
 *  on resrouces to progress and complete all of them, before the
 *  current one can continue.  To reduce latency, and because the
 *  actual amount of shared memory resrouces can be set at run time,
 *  this ptl implementation does not do this.  Initialize the 
 *  fragment and foward on to the peer.
 *
 *  NOTE: this routine assumes that only one sending thread will be accessing
 *        the send descriptor at a time.
 */

int mca_ptl_sm_send(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    mca_ptl_sm_send_request_t *sm_request;
    mca_ptl_sm_frag_t *send_frag;
    int my_local_smp_rank, peer_local_smp_rank;
    int return_status=OMPI_SUCCESS;
    ompi_fifo_t *send_fifo;
    mca_ptl_base_header_t* hdr;
    void *sm_data_ptr ;

    /* cast to shared memory send descriptor */
    sm_request=(mca_ptl_sm_send_request_t *)sendreq;

    /* determine if send descriptor is obtained from the cache.  If
     * so, all the memory resource needed have been obtained */
    if( !sm_request->super.req_cached) {
        /* in this ptl, we will only use the cache, or fail */
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    send_frag = sm_request->req_frag;

    /* if needed, pack data in payload buffer */
    if( 0 < size ) {
        ompi_convertor_t *convertor;
        unsigned int iov_count, max_data;
        int free_after=0;
        struct iovec address;

        convertor = &sendreq->req_convertor;
        ompi_convertor_copy(&sendreq->req_convertor, convertor);
        ompi_convertor_init_for_send( convertor, 0, 
                sendreq->req_datatype,
                sendreq->req_count, 
                sendreq->req_addr,
                offset, NULL);

        sm_data_ptr=sm_request->req_frag->buff;

        /* set up the shared memory iovec */
        address.iov_base=sm_data_ptr;
        address.iov_len= (size < send_frag->buff_length) ? size : send_frag->buff_length;

        convertor = &sendreq->req_convertor;
        iov_count=1;
        max_data=address.iov_len;
        return_status=ompi_convertor_pack(convertor,&address,&iov_count,
                &max_data, &free_after);
        if( 0 > return_status ) {
            return OMPI_ERROR;
        }
        size = max_data;
    }

    /* fill in the fragment descriptor */
    /* get pointer to the fragment header */
    hdr = &(send_frag->super.frag_base.frag_header);

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_MATCH;
    hdr->hdr_common.hdr_flags = flags;
    hdr->hdr_match.hdr_contextid = sendreq->req_base.req_comm->c_contextid;
    hdr->hdr_match.hdr_src = sendreq->req_base.req_comm->c_my_rank;
    hdr->hdr_match.hdr_dst = sendreq->req_base.req_peer;
    hdr->hdr_match.hdr_tag = sendreq->req_base.req_tag;
    hdr->hdr_match.hdr_msg_length = sendreq->req_bytes_packed;

    /* update the offset within the payload */
    sendreq->req_offset += size;

    /* 
     * update the fragment descriptor 
     */
    send_frag->send_req = sendreq;
    send_frag->send_offset = offset;
    send_frag->super.frag_base.frag_size=size;

    /* 
     * post the descriptor in the queue - post with the relative
     * address 
     */
    /* see if queues are allocated */
    my_local_smp_rank=ptl_peer->my_smp_rank;
    peer_local_smp_rank=ptl_peer->peer_smp_rank;

    send_fifo=&(mca_ptl_sm_component.fifo
            [my_local_smp_rank][peer_local_smp_rank]);

    /* lock for thread safety - using atomic lock, not mutex, since
     * we need shared memory access to these lock, and in some pthread
     * implementation, such mutex's don't work correctly */
    if( ompi_using_threads() ) {
        ompi_atomic_lock(&(send_fifo->head_lock));
    }

    if(OMPI_CB_FREE == send_fifo->head){
        /* no queues have been allocated - allocate now */
        return_status=ompi_fifo_init_same_base_addr(
                mca_ptl_sm_component.size_of_cb_queue,
                mca_ptl_sm_component.cb_lazy_free_freq,
                /* at this stage we are not doing anything with memory
                 * locality */
                0,0,0,
                send_fifo, mca_ptl_sm_component.sm_mpool);
        if( return_status != OMPI_SUCCESS ) {
            ompi_atomic_unlock(&(send_fifo->head_lock));
            return return_status;
        }
    }

    /* post descriptor */
    return_status=ompi_fifo_write_to_head_same_base_addr(sm_request->req_frag,
            send_fifo, mca_ptl_sm_component.sm_mpool);
    if(  0 <= return_status ) {
        return_status=OMPI_SUCCESS;
    }

    /* release threa lock */
    if( ompi_using_threads() ) {
        ompi_atomic_unlock(&(send_fifo->head_lock));
    }

    /* if this is the entire message - signal request is complete */
    if(sendreq->req_bytes_packed == size) {
        ompi_request_complete(sendreq);
    }

    /* return */
    return return_status;
}

/*
 *  Continue a send. Second fragment and beyond.
 *
 *  NOTE: this routine assumes that only one sending thread will be accessing
 *        the send descriptor at a time.
 */

int mca_ptl_sm_send_continue(
    struct mca_ptl_base_module_t* ptl,
    struct mca_ptl_base_peer_t* ptl_peer,
    struct mca_pml_base_send_request_t* sendreq,
    size_t offset,
    size_t size,
    int flags)
{
    mca_ptl_sm_send_request_t *sm_request;
    int my_local_smp_rank, peer_local_smp_rank, return_code;
    int return_status=OMPI_SUCCESS, free_after=0;
    volatile ompi_fifo_t *send_fifo;
    mca_ptl_base_header_t* hdr;
    void *sm_data_ptr ;
    ompi_list_item_t* item;
    mca_ptl_sm_second_frag_t *send_frag;
    ompi_convertor_t *convertor;
    struct iovec address;
    unsigned int max_data,iov_count;

    /* cast to shared memory send descriptor */
    sm_request=(mca_ptl_sm_send_request_t *)sendreq;

    /* obtain fragment descriptor and payload from free list */
    OMPI_FREE_LIST_GET(&mca_ptl_sm_component.sm_second_frags, item, return_code);

    /* if we don't get a fragment descriptor, return w/o
     * updating any counters.  The PML will re-issue the
     * request */
    if(NULL == (send_frag = (mca_ptl_sm_second_frag_t *)item)){
        return return_code;
    }

    /* pack data in payload buffer */
    convertor = &sendreq->req_convertor;
    ompi_convertor_copy(&sendreq->req_convertor, convertor);
    ompi_convertor_init_for_send( convertor, 0, 
            sendreq->req_datatype,
            sendreq->req_count, 
            sendreq->req_addr,
            offset, NULL);

    sm_data_ptr=send_frag->buff;

    /* set up the shared memory iovec */
    address.iov_base=sm_data_ptr;
    address.iov_len=(size < send_frag->buff_length) ? size : send_frag->buff_length;

    convertor = &sendreq->req_convertor;
    iov_count=1;
    max_data=address.iov_len;
    return_status=ompi_convertor_pack(convertor,&address,&iov_count,
            &max_data, &free_after);
    if( 0 > return_status ) {
        return OMPI_ERROR;
    }
    size = max_data;

    /* fill in the fragment descriptor */
    /* get pointer to the fragment header */
    hdr = &(send_frag->super.frag_base.frag_header);

    hdr->hdr_common.hdr_type = MCA_PTL_HDR_TYPE_FRAG;
    hdr->hdr_frag.hdr_src_ptr.pval = sendreq;
    /* set offset into the "packed" user send buffer */
    hdr->hdr_frag.hdr_frag_offset=offset;
    send_frag->super.frag_request=
        ((mca_ptl_base_recv_frag_t *)(sm_request->req_frag))->
        frag_request;

    /* update the offset within the payload */
    sendreq->req_offset += size;

    /* 
     * update the fragment descriptor 
     */
    send_frag->send_req = sendreq;
    send_frag->send_offset = offset;
    send_frag->super.frag_base.frag_size=size;

    /* 
     * post the descriptor in the queue - post with the relative
     * address 
     */
    /* see if queues are allocated */
    my_local_smp_rank=ptl_peer->my_smp_rank;
    peer_local_smp_rank=ptl_peer->peer_smp_rank;
    send_fifo=&(mca_ptl_sm_component.fifo
            [my_local_smp_rank][peer_local_smp_rank]);
    /* since the first fragment has already been posted,
     * the queue has already been initialized, so no need to check */

    /* post descriptor */
    /* lock for thread safety - using atomic lock, not mutex, since
     * we need shared memory access to these lock, and in some pthread
     * implementation, such mutex's don't work correctly */
    if( ompi_using_threads() ) {
        ompi_atomic_lock(&(send_fifo->head_lock));
    }

    return_status=ompi_fifo_write_to_head_same_base_addr(send_frag,
            send_fifo, mca_ptl_sm_component.sm_mpool);
    if( 0 <= return_status ) {
        return_status=OMPI_SUCCESS;
    }

    /* release threa lock */
    if( ompi_using_threads() ) {
        ompi_atomic_unlock(&(send_fifo->head_lock));
    }
    /* return */
    return return_status;
}
