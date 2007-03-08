/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "opal/threads/mutex.h"
#include "ompi/datatype/convertor.h"
#include "opal/sys/atomic.h"
#include "opal/util/output.h"
#include "opal/util/if.h"
#include "orte/util/proc_info.h"
#include "opal/util/printf.h"
#include "orte/util/sys_info.h"
#include "ompi/class/ompi_fifo.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"
#include "btl_sm.h"
#include "btl_sm_endpoint.h"
#include "btl_sm_frag.h"
#include "btl_sm_fifo.h"
#include "ompi/proc/proc.h"

/**
 * @file
 *
 * Note that there are effectively two versions of the btl sm module
 * -- one that assumes that the base address of the shared memory
 * segment is the same between pairs of processes (i.e., mmap()
 * returned the same virtual address for the same segment in both
 * processes), and one that assumes that the base addresses are
 * different.
 *
 * In the former, no translation is necessary -- all pointers can be
 * stored directly as-is and used in both processes.
 *
 * In the latter, we calculate the difference between the base virtual
 * address of the two process' shared memory segments and cache it.
 * This difference is used to access memory and pointers written by
 * the other process.
 *
 * Specifically, a good portion of this btl is implemented in the
 * ompi_fifo_t and ompi_circular_buffer_t classes.  These classes
 * *always* store absolute virtual addresses in their data structures.
 * The virtual addresses are always meaningful in the *sender's*
 * process space.  If the base address is the same in both processes,
 * then we get the happy side effect that the virtual addresses are
 * also valid in the receiver's process space, and therefore no
 * address translation needs to be done when the reader accesses the
 * data.
 *
 * However, in the case where the base addresses are different, the
 * receiver must translate every pointer address in the ompi_fifo_t
 * and ompi_circular_buffer_t data structures (even when writing back
 * to those data structures, such as updating a head or tail pointer).
 *
 * In short, we use a "receiver makes right" scheme, and in some
 * cases, the receiver doesn't have to do anything.
 */

mca_btl_sm_t mca_btl_sm[2] = {
    {
        {
        &mca_btl_sm_component.super,
        0, /* btl_eager_limit */
        0, /* btl_min_send_size */
        0, /* btl_max_send_size */
        0, /* btl_min_rdma_size */
        0, /* btl_max_rdma_size */
        0, /* btl_exclusivity */
        0, /* btl_latency */
        0, /* btl_bandwidth */
        0, /* btl flags */
        mca_btl_sm_add_procs_same_base_addr,
        mca_btl_sm_del_procs,
        mca_btl_sm_register,
        mca_btl_sm_finalize,
        mca_btl_sm_alloc,
        mca_btl_sm_free,
        mca_btl_sm_prepare_src,
        NULL,
        mca_btl_sm_send, 
        NULL,  /* put */
        NULL,  /* get */
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_sm_register_error_cb /* register error */
        }
    },
    {
        {
        &mca_btl_sm_component.super,
        0, /* btl_eager_limit */
        0, /* btl_min_send_size */
        0, /* btl_max_send_size */
        0, /* btl_min_rdma_size */
        0, /* btl_max_rdma_size */
        0, /* btl_exclusivity */
        0, /* btl_latency */
        0, /* btl_bandwidth */
        0, /* btl flags */
        mca_btl_sm_add_procs,
        mca_btl_sm_del_procs,
        mca_btl_sm_register,
        mca_btl_sm_finalize,
        mca_btl_sm_alloc,  
        mca_btl_sm_free,  
        mca_btl_sm_prepare_src,
        NULL,
        mca_btl_sm_send,  
        NULL, /* put function */
        NULL, /* get function */
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_sm_register_error_cb /* register error */
        }
    }
};

/* track information needed to synchronise a Shared Memory BTL module */
mca_btl_sm_module_resource_t mca_btl_sm_module_resource;


int mca_btl_sm_add_procs_same_base_addr(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_btl_base_endpoint_t **peers,
    ompi_bitmap_t* reachability)
{
    int return_code=OMPI_SUCCESS;
    size_t i,j,proc,size,n_to_allocate,length;
    int n_local_procs,cnt,len;
    ompi_proc_t* my_proc; /* pointer to caller's proc structure */
    mca_btl_sm_t *btl_sm;
    ompi_fifo_t *my_fifos;
    ompi_fifo_t * volatile *fifo_tmp;
    bool same_sm_base;
    ssize_t diff;
    volatile char **tmp_ptr;
    volatile int *tmp_int_ptr;
    bool have_connected_peer = false;

    /* initializion */
    for(i=0 ; i < nprocs ; i++ ) {
        peers[i]=NULL;
    }
    btl_sm=(mca_btl_sm_t *)btl;

    /* allocate array to hold setup shared memory from all
     * other procs */
    mca_btl_sm_component.sm_proc_connect=(int *) malloc(nprocs*sizeof(int));
    if( NULL == mca_btl_sm_component.sm_proc_connect ){
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* initialize and sm_proc_connect*/
    for(proc=0 ; proc < nprocs ; proc++ ) {
        mca_btl_sm_component.sm_proc_connect[proc]=0;
    }

    /* get pointer to my proc structure */
    my_proc=ompi_proc_local();
    if( NULL == my_proc ) {
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* Get unique host identifier for each process in the list,
     * and idetify procs that are on this host.  Add procs on this
     * host to shared memory reachbility list.  Also, get number
     * of local procs in the prcs list. */
    n_local_procs=0;
    for( proc=0 ; proc < nprocs; proc++ ) {
#if OMPI_ENABLE_PROGRESS_THREADS == 1
        char path[PATH_MAX];
#endif
        struct mca_btl_base_endpoint_t *peer;

        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ||
                 0 == (procs[proc]->proc_flags & OMPI_PROC_FLAG_LOCAL)) {
            continue;
        }

        /* If we got here, the proc is reachable via sm.  So
           initialize the peers information */

        /* check to see if this is me */
        if( my_proc == procs[proc] ) {
            mca_btl_sm_component.my_smp_rank = n_local_procs;
        } else {
            /* we have someone to talk to */
            have_connected_peer = true;
        }

        peer = peers[proc] = (struct mca_btl_base_endpoint_t*)malloc(sizeof(struct mca_btl_base_endpoint_t));
        if( NULL == peer ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        peer->peer_smp_rank=n_local_procs+
            mca_btl_sm_component.num_smp_procs;
        
#if OMPI_ENABLE_PROGRESS_THREADS == 1
        sprintf(path, "%s"OPAL_PATH_SEP"sm_fifo.%lu", orte_process_info.job_session_dir, 
                (unsigned long)procs[proc]->proc_name.vpid);
        peer->fifo_fd = open(path, O_WRONLY);
        if(peer->fifo_fd < 0) {
            opal_output(0, "mca_btl_sm_add_procs: open(%s) failed with errno=%d\n", path, errno);
            goto CLEANUP;
        }
#endif
        n_local_procs++;
        mca_btl_sm_component.sm_proc_connect[proc]=SM_CONNECTED;
    }

    /* jump out if there's not someone we can talk to */
    if (!have_connected_peer) {
        return_code = OMPI_SUCCESS;
        goto CLEANUP;
    }

    /* lookup shared memory pool */
    if(NULL == mca_btl_sm_component.sm_mpool) {
        mca_btl_sm_component.sm_mpool =
            mca_mpool_base_module_lookup(mca_btl_sm_component.sm_mpool_name);
        if (NULL == mca_btl_sm_component.sm_mpool) {
            mca_btl_sm_component.sm_mpool =
                mca_mpool_base_module_create(mca_btl_sm_component.sm_mpool_name,btl,NULL);
        }

        /* Sanity check to ensure that we found it */
        if (NULL == mca_btl_sm_component.sm_mpool) {
           return_code = OMPI_ERR_OUT_OF_RESOURCE;
           goto CLEANUP;
        }
        mca_btl_sm_component.sm_mpool_base =
            mca_btl_sm_component.sm_mpool->mpool_base((mca_mpool_base_module_t*)mca_btl_sm_component.sm_mpool_base);
    }

    /* make sure that my_smp_rank has been defined */
    if( 0xFFFFFFFF == mca_btl_sm_component.my_smp_rank ) {
        return_code=OMPI_ERROR;
        goto CLEANUP;
    }

    /* see if need to allocate space for extra procs */
    if(  0 > mca_btl_sm_component.sm_max_procs ) {

        /* no limit */
        if( 0 <= mca_btl_sm_component.sm_extra_procs ) {
            /* limit */
            mca_btl_sm_component.sm_max_procs=n_local_procs+
                mca_btl_sm_component.sm_extra_procs;
        } else {
            /* no limit */
            mca_btl_sm_component.sm_max_procs=2*n_local_procs;
        }
    }
    n_to_allocate=mca_btl_sm_component.sm_max_procs;

    /* make sure n_to_allocate is greater than 0 */

    if ( !mca_btl_sm[0].btl_inited ) {
        /* set the shared memory offset */
        mca_btl_sm_component.sm_offset=(ssize_t *)
            malloc(n_to_allocate*sizeof(ssize_t));
            if(NULL == mca_btl_sm_component.sm_offset ) {
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* create a list of peers */
        mca_btl_sm_component.sm_peers=(struct mca_btl_base_endpoint_t**)
            malloc(n_to_allocate*sizeof(struct mca_btl_base_endpoint_t*));
        if(NULL == mca_btl_sm_component.sm_peers ) {
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access */
    for( proc=0 ; proc < nprocs; proc++ ) {
        struct mca_btl_base_endpoint_t* peer = peers[proc];
        if(NULL != peer) {
            mca_btl_sm_component.sm_peers[peer->peer_smp_rank] = peer;
            peer->my_smp_rank=mca_btl_sm_component.my_smp_rank;
        }
    }

    /* Allocate Shared Memory BTL process coordination
     * data structure.  This will reside in shared memory */

    /* 
     * Create backing file - only first time through 
     */
    if ( !mca_btl_sm[0].btl_inited ) {
        /* set file name */
        len=asprintf(&(mca_btl_sm_component.sm_resouce_ctl_file),
                "%s"OPAL_PATH_SEP"shared_mem_btl_module.%s",orte_process_info.job_session_dir,
                orte_system_info.nodename);
        if( 0 > len ) {
            goto CLEANUP;
        }

        /* Pass in a data segment alignment of 0 to get no data
           segment (only the shared control structure */
        size = sizeof(mca_btl_sm_module_resource_t);
        if(NULL==(mca_btl_sm_component.mmap_file=mca_common_sm_mmap_init(size,
                        mca_btl_sm_component.sm_resouce_ctl_file,
                        sizeof(mca_btl_sm_module_resource_t), 0))) 
        {
            opal_output(0, "mca_btl_sm_add_procs: unable to create shared memory BTL coordinating strucure :: size %ld \n",
                    size);
            return_code=OMPI_ERROR;
            goto CLEANUP;
        }

        /* set the pointer to the shared memory control structure */
        mca_btl_sm_component.sm_ctl_header=(mca_btl_sm_module_resource_t *)
            mca_btl_sm_component.mmap_file->map_seg;


        /* Allocate a fixed size pointer array for the 2-D Shared memory queues.
         * Excess slots will be allocated for future growth.  One could
         * make this array growable, but then one would need to uses mutexes
         * for any access to these queues to ensure data consistancy when
         * the array is grown */
   
        if(0 == mca_btl_sm_component.my_smp_rank ) {
            /* allocate ompi_fifo_t strucutes for each fifo of the queue
             * pairs - one per pair of local processes */
            /* check to make sure number of local procs is within the
             * specified limits */
            if( ( 0 < mca_btl_sm_component.sm_max_procs ) &&
                    ( n_local_procs > mca_btl_sm_component.sm_max_procs) ) {
                return_code=OMPI_ERROR;
                goto CLEANUP;
            }

            /* allocate array of ompi_fifo_t* elements -
             * offset relative to base segement is stored, so that
             * this can be used by other procs */
            mca_btl_sm_component.sm_ctl_header->fifo= (volatile ompi_fifo_t**)
                mca_btl_sm_component.sm_mpool->mpool_alloc
                (mca_btl_sm_component.sm_mpool, n_to_allocate*sizeof(ompi_fifo_t *),
                 CACHE_LINE_SIZE, 0, NULL);
            if ( NULL == mca_btl_sm_component.sm_ctl_header->fifo ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            /* initiazlize the pointer array */
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_btl_sm_component.sm_ctl_header->fifo[i]=NULL;
            }

            /*  allocate and initialize the array to hold the virtual address
             *  of the shared memory base */
            mca_btl_sm_component.sm_ctl_header->segment_header.
                base_shared_mem_segment =  ( volatile char **)
                mca_btl_sm_component.sm_mpool->mpool_alloc
                (mca_btl_sm_component.sm_mpool, n_to_allocate*sizeof(char *), CACHE_LINE_SIZE, 0, NULL);
            if ( NULL == mca_btl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            /* initialize the pointer array */
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_btl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment[i]=NULL;
            }

            /*  allocate and initialize the array of flags indicating
             *  when the virtual address of the shared memory address
             *  has been set */
            mca_btl_sm_component.sm_ctl_header->segment_header.
                base_shared_mem_flags = ( int *)
                mca_btl_sm_component.sm_mpool->mpool_alloc
                (mca_btl_sm_component.sm_mpool, n_to_allocate*sizeof(int), CACHE_LINE_SIZE, 0, NULL);
            if ( NULL == mca_btl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_flags ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_btl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_flags[i]=0;
            }

            /* set the addresses to be a relative, so that
             * they can be used by other procs */
            mca_btl_sm_component.sm_ctl_header->fifo=
                (volatile ompi_fifo_t **)
                ( (char *)(mca_btl_sm_component.sm_ctl_header->fifo)-
                  (char *)(mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool)) );

                mca_btl_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment=( volatile char **)
                    ( (char *)(mca_btl_sm_component.sm_ctl_header->
                               segment_header.base_shared_mem_segment) -
                      (char *)(mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool)) );

	    mca_btl_sm_component.sm_ctl_header->segment_header.
	      base_shared_mem_flags = (volatile int *)
                ( ((char *) mca_btl_sm_component.sm_ctl_header->
                   segment_header.base_shared_mem_flags) -
                  (char *) (mca_btl_sm_component.sm_mpool_base));

            /* allow other procs to use this shared memory map */
            mca_btl_sm_component.mmap_file->map_seg->seg_inited=true;

            /* memory barrier to ensure this flag is set before other
             *  flags are set */
            opal_atomic_mb();
        }
   
        /* Note:  Need to make sure that proc 0 initializes control
         * structures before any of the other procs can progress */
        if( 0 != mca_btl_sm_component.my_smp_rank ) 
        {
            /* spin unitl local proc 0 initializes the segment */
            while(!mca_btl_sm_component.mmap_file->map_seg->seg_inited) {
                opal_progress(); 
            }
        }
                
        /* set the base of the shared memory segment, and flag
         * indicating that it is set */
        tmp_ptr=(volatile char **)
            ( (char *)(mca_btl_sm_component.sm_ctl_header->segment_header.
              base_shared_mem_segment)  +
		      (long )(mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool)) );
        tmp_ptr[mca_btl_sm_component.my_smp_rank]=(char*)
            mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool);
        /* memory barrier to ensure this flag is set before other
         *  flags are set */
        opal_atomic_mb();

		/* Set my flag to 1 (convert from relative address first) */
        tmp_int_ptr=(volatile int *)
            ( ((char *) mca_btl_sm_component.sm_ctl_header->segment_header.
               base_shared_mem_flags) +
              ((long) mca_btl_sm_component.sm_mpool_base));
		tmp_int_ptr[mca_btl_sm_component.my_smp_rank]=1;

        /*
         * initialize the array of fifo's "owned" by this process
         * The virtual addresses are valid only in the sender's
         * address space - unless the base of the shared memory
         * segment is mapped at the same location in the reader's
         * virtual address space.
         */
        my_fifos=( ompi_fifo_t *)
            mca_btl_sm_component.sm_mpool->mpool_alloc
            (mca_btl_sm_component.sm_mpool, n_to_allocate*sizeof(ompi_fifo_t), CACHE_LINE_SIZE, 0, NULL);
        if ( NULL == my_fifos ) {
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        for( j=0 ; j < n_to_allocate ; j++ ) {
            my_fifos[j].head = (ompi_cb_fifo_wrapper_t*)OMPI_CB_FREE;
            my_fifos[j].tail = (ompi_cb_fifo_wrapper_t*)OMPI_CB_FREE;
            opal_atomic_unlock(&(my_fifos[j].head_lock));
            opal_atomic_unlock(&(my_fifos[j].tail_lock));
        }
        fifo_tmp=(ompi_fifo_t * volatile *)
                ( (char *)(mca_btl_sm_component.sm_ctl_header->fifo) +
                  (long)(mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool)) );
        fifo_tmp[mca_btl_sm_component.my_smp_rank]=my_fifos;
        opal_atomic_mb();

        /* cache the pointer to the 2d fifo array.  These addresses
         * are valid in the current process space */
        mca_btl_sm_component.fifo=(ompi_fifo_t **)
            malloc(sizeof(ompi_fifo_t *)*n_to_allocate);
        if( NULL == mca_btl_sm_component.fifo ) {
            return_code=OMPI_ERROR;
            goto CLEANUP;
        }
        mca_btl_sm_component.fifo[mca_btl_sm_component.my_smp_rank]=my_fifos;
    }

    /* cache the pointers to the rest of the fifo arrays */
    fifo_tmp=(ompi_fifo_t * volatile *)
        ( (char *)(mca_btl_sm_component.sm_ctl_header->fifo) +
          (long)(mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool)) );
    tmp_ptr=(volatile char **)
        ( (char *)mca_btl_sm_component.sm_ctl_header->
          segment_header.base_shared_mem_segment +
          (long)mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool));
    for( j=mca_btl_sm_component.num_smp_procs ; j <
            mca_btl_sm_component.num_smp_procs+n_local_procs ; j++ ) {

        /* spin until this element is allocated */
        while ( NULL == fifo_tmp[j] )
        { 
            opal_progress(); 
        }

        /* Calculate the difference as (my_base - their_base) */
        diff = tmp_ptr[mca_btl_sm_component.my_smp_rank] - tmp_ptr[j];
        mca_btl_sm_component.fifo[j]=
            ( ompi_fifo_t *)( (char *)fifo_tmp[j]+diff);
        mca_btl_sm_component.sm_offset[j] = diff;
    }

    /* initialize some of the free-lists */
    if( !mca_btl_sm[0].btl_inited ) {
        /* some initialization happens only the first time this routine
         * is called, i.e. when btl_inited is false */

        /* initialize fragment descriptor free lists */

        /* allocation will be for the fragment descriptor and payload buffer */
        length=sizeof(mca_btl_sm_frag_t) + mca_btl_sm_component.eager_limit;
        ompi_free_list_init(&mca_btl_sm_component.sm_frags1, length,
                OBJ_CLASS(mca_btl_sm_frag1_t),
                mca_btl_sm_component.sm_free_list_num,
                mca_btl_sm_component.sm_free_list_max,
                mca_btl_sm_component.sm_free_list_inc,
                mca_btl_sm_component.sm_mpool); /* use shared-memory pool */

        length=sizeof(mca_btl_sm_frag_t) + mca_btl_sm_component.max_frag_size;
        ompi_free_list_init(&mca_btl_sm_component.sm_frags2, length,
                OBJ_CLASS(mca_btl_sm_frag2_t),
                mca_btl_sm_component.sm_free_list_num,
                mca_btl_sm_component.sm_free_list_max,
                mca_btl_sm_component.sm_free_list_inc,
                mca_btl_sm_component.sm_mpool); /* use shared-memory pool */

        /* set up mca_btl_sm_component.list_smp_procs_same_base_addr */
        mca_btl_sm_component.list_smp_procs_same_base_addr=(int *)
            malloc(mca_btl_sm_component.sm_max_procs*sizeof(int));
        if( NULL == mca_btl_sm_component.list_smp_procs_same_base_addr ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* set up mca_btl_sm_component.list_smp_procs_different_base_addr */
        mca_btl_sm_component.list_smp_procs_different_base_addr=(int *)
            malloc(mca_btl_sm_component.sm_max_procs*sizeof(int));
        if( NULL == mca_btl_sm_component.list_smp_procs_different_base_addr ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* set flag indicating btl has been inited */
        btl_sm->btl_inited=true;
    }

    /* set connectivity */
    cnt=0;
    for(proc = 0 ; proc < nprocs ; proc++ ) {

        struct mca_btl_base_endpoint_t* peer = peers[proc];
        if(peer == NULL)
            continue;

        tmp_ptr=(volatile char **)
            ( (char *)mca_btl_sm_component.sm_ctl_header->
              segment_header.base_shared_mem_segment +
              (long)mca_btl_sm_component.sm_mpool->mpool_base(mca_btl_sm_component.sm_mpool));
        same_sm_base=(tmp_ptr[peer->peer_smp_rank] ==
            tmp_ptr[mca_btl_sm_component.my_smp_rank]);

        if( SM_CONNECTED == mca_btl_sm_component.sm_proc_connect[proc] ) {
            if( same_sm_base ){

                /* don't count if same process */
                if( (mca_btl_sm_component.num_smp_procs+cnt ) ==
                        mca_btl_sm_component.my_smp_rank) {
                    cnt++;
                    continue;
                }
                /* set up the list of local processes with the same base
                 * shared memory virtual address as this process */
                mca_btl_sm_component.list_smp_procs_same_base_addr
                    [mca_btl_sm_component.num_smp_procs_same_base_addr]=
                    cnt;
                mca_btl_sm_component.num_smp_procs_same_base_addr++;
                cnt++;
                /* add this proc to shared memory accessability list */
                return_code=ompi_bitmap_set_bit(reachability,proc);
                if( OMPI_SUCCESS != return_code ){
                    goto CLEANUP;
                }
            } else {
                /* set up the list of local processes with the same base
                 * shared memory virtual address as this process */
                mca_btl_sm_component.list_smp_procs_different_base_addr
                    [mca_btl_sm_component.num_smp_procs_different_base_addr]=
                    cnt;
                mca_btl_sm_component.num_smp_procs_different_base_addr++;
                cnt++;
                mca_btl_sm_component.sm_proc_connect[proc]=
                    SM_CONNECTED_DIFFERENT_BASE_ADDR;
            }
        }
    }

    /* make sure we have enough eager fragmnents for each process */
    return_code = ompi_free_list_resize(&mca_btl_sm_component.sm_frags1,
                                        (mca_btl_sm_component.num_smp_procs + n_local_procs) * 2);
    if (OMPI_SUCCESS != return_code) {
        goto CLEANUP;
    }

    /* update the local smp process count */
    mca_btl_sm_component.num_smp_procs+=n_local_procs;

CLEANUP:
    return return_code;
}

/* Note:: this routine assumes that mca_btl_sm_add_procs_same_base_addr
 *  has already been called to set up data structures needed by this
 *  routine */
int mca_btl_sm_add_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_btl_base_endpoint_t **peers,
    ompi_bitmap_t* reachability)
{
    int return_code = OMPI_SUCCESS, tmp_cnt;
    uint32_t proc, n_local_procs;

    /* initializion */
    for(proc=0 ; proc < nprocs ; proc++ ) {
        peers[proc]=NULL;
    }

    /* figure out total number of local procs in current set */
    tmp_cnt=0;
    for(proc = 0 ; proc < nprocs ; proc++ ) {
        if( (SM_CONNECTED_DIFFERENT_BASE_ADDR ==
                    mca_btl_sm_component.sm_proc_connect[proc]) ||
                (SM_CONNECTED ==
                    mca_btl_sm_component.sm_proc_connect[proc]) ) {
            tmp_cnt++;
        }
    }
    /* set connectivity */
    n_local_procs=0;
    for(proc = 0 ; proc < nprocs ; proc++ ) {
        /* Same base address base */
        if (SM_CONNECTED == mca_btl_sm_component.sm_proc_connect[proc]) {
            n_local_procs++;
        }

        /* Different base address case */
        else if (SM_CONNECTED_DIFFERENT_BASE_ADDR ==
                mca_btl_sm_component.sm_proc_connect[proc]) {

            /* add this proc to shared memory accessability list */
            return_code=ompi_bitmap_set_bit(reachability,proc);
            if( OMPI_SUCCESS != return_code ){
                goto CLEANUP;
            }

            /* initialize the peers information */
            peers[proc] = (struct mca_btl_base_endpoint_t*)malloc(sizeof(struct mca_btl_base_endpoint_t));
            if( NULL == peers[proc] ){
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            peers[proc]->my_smp_rank=mca_btl_sm_component.my_smp_rank;
            /* subtract tmp_cnt, since mca_btl_sm_add_procs_same_base_addr
             * already added these into num_smp_procs */
            peers[proc]->peer_smp_rank=n_local_procs+
                mca_btl_sm_component.num_smp_procs-tmp_cnt;
#if OMPI_ENABLE_PROGRESS_THREADS
            peers[proc]->fifo_fd =
                mca_btl_sm_component.sm_peers[peers[proc]->peer_smp_rank]->fifo_fd;
#endif
            n_local_procs++;
        }
    }

CLEANUP:
    /* free local memory */
    if(mca_btl_sm_component.sm_proc_connect){
        free(mca_btl_sm_component.sm_proc_connect);
        mca_btl_sm_component.sm_proc_connect=NULL;
    }

    return return_code;
}

int mca_btl_sm_del_procs(
    struct mca_btl_base_module_t* btl, 
    size_t nprocs,
    struct ompi_proc_t **procs, 
    struct mca_btl_base_endpoint_t **peers)
{
    return OMPI_SUCCESS;
}


/**
 * MCA->BTL Clean up any resources held by BTL module
 * before the module is unloaded.
 *
 * @param btl (IN)   BTL module.
 *
 * Prior to unloading a BTL module, the MCA framework will call
 * the BTL finalize method of the module. Any resources held by
 * the BTL should be released and if required the memory corresponding
 * to the BTL module freed.
 *
 */

int mca_btl_sm_finalize(struct mca_btl_base_module_t* btl)
{
    return OMPI_SUCCESS;
}


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param btl (IN)     BTL module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BTL of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

int mca_btl_sm_register(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_tag_t tag,
    mca_btl_base_module_recv_cb_fn_t cbfunc,
    void* cbdata)
{
    mca_btl_sm_t* sm_btl = (mca_btl_sm_t*)btl;
    sm_btl->sm_reg[tag].cbfunc = cbfunc;
    sm_btl->sm_reg[tag].cbdata = cbdata;
    return OMPI_SUCCESS;
}
                                                                                                                 
/* 
 * Register callback function for error handling..
 */
int mca_btl_sm_register_error_cb(
        struct mca_btl_base_module_t* btl,
        mca_btl_base_module_error_cb_fn_t cbfunc)
{
    mca_btl_sm_t *sm_btl = (mca_btl_sm_t *)btl;
    sm_btl->error_cb = cbfunc;
    return OMPI_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
extern mca_btl_base_descriptor_t* mca_btl_sm_alloc(
    struct mca_btl_base_module_t* btl,
    size_t size)
{
    mca_btl_sm_frag_t* frag;
    int rc;
    if(size <= mca_btl_sm_component.eager_limit) {
        MCA_BTL_SM_FRAG_ALLOC1(frag,rc);
    } else if (size <= mca_btl_sm_component.max_frag_size) {
        MCA_BTL_SM_FRAG_ALLOC2(frag,rc);
    } else {
        return NULL;
    }

    if (frag != NULL) {
        frag->segment.seg_len = size;
    }

    return (mca_btl_base_descriptor_t*)frag;
}

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_btl_sm_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* des)
{
    mca_btl_sm_frag_t* frag = (mca_btl_sm_frag_t*)des;
    MCA_BTL_SM_FRAG_RETURN(frag);
    
    return OMPI_SUCCESS;
}


/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 */
struct mca_btl_base_descriptor_t* mca_btl_sm_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_btl_sm_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    MCA_BTL_SM_FRAG_ALLOC2(frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    if(reserve + max_data > frag->size) {
        max_data = frag->size - reserve;
    } 
    iov.iov_len = max_data;
    iov.iov_base = (IOVBASE_TYPE*)(((unsigned char*)(frag+1)) + reserve);

    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );
    if(rc < 0) {
        MCA_BTL_SM_FRAG_RETURN(frag);
        return NULL;
    }
    frag->segment.seg_len = reserve + max_data;
    *size = max_data;
    return &frag->base;
}

 
/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */

int mca_btl_sm_send(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct mca_btl_base_descriptor_t* descriptor,
    mca_btl_base_tag_t tag)
{
    mca_btl_sm_frag_t* frag = (mca_btl_sm_frag_t*)descriptor;
    int rc;

    frag->tag = tag;
    frag->type = MCA_BTL_SM_FRAG_SEND;
    frag->rc = OMPI_SUCCESS;

    /* 
     * post the descriptor in the queue - post with the relative
     * address 
     */
    MCA_BTL_SM_FIFO_WRITE(endpoint, endpoint->my_smp_rank, endpoint->peer_smp_rank, frag, rc);
    return rc;
}


