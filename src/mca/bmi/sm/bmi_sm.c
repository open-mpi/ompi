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

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "threads/mutex.h"
#include "datatype/datatype.h"
#include "include/sys/atomic.h"
#include "util/output.h"
#include "util/if.h"
#include "util/proc_info.h"
#include "util/printf.h"
#include "util/sys_info.h"
#include "class/ompi_fifo.h"
#include "class/ompi_free_list.h"
#include "mca/pml/pml.h"
#include "mca/bmi/bmi.h"
#include "mca/mpool/base/base.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "bmi_sm.h"
#include "bmi_sm_endpoint.h"
#include "bmi_sm_frag.h"
#include "bmi_sm_fifo.h"


mca_bmi_sm_t mca_bmi_sm[2] = {
    {
        {
        &mca_bmi_sm_component.super,
        0, /* bmi_first_frag_size */
        0, /* bmi_min_frag_size */
        0, /* bmi_max_frag_size */
        0, /* bmi_exclusivity */
        0, /* bmi_latency */
        0, /* bmi_bandwidth */
        0, /* bmi flags */
        mca_bmi_sm_add_procs_same_base_addr,
        mca_bmi_sm_del_procs,
        mca_bmi_sm_register,
        mca_bmi_sm_finalize,
        mca_bmi_sm_alloc,
        mca_bmi_sm_free,
        mca_bmi_sm_prepare_src,
        NULL,
        mca_bmi_sm_send, 
        NULL,  /* put */
        NULL   /* get */
        }
    },
    {
        {
        &mca_bmi_sm_component.super,
        0, /* bmi_first_frag_size */
        0, /* bmi_min_frag_size */
        0, /* bmi_max_frag_size */
        0, /* bmi_exclusivity */
        0, /* bmi_latency */
        0, /* bmi_bandwidth */
        0, /* bmi flags */
        mca_bmi_sm_add_procs,
        mca_bmi_sm_del_procs,
        mca_bmi_sm_register,
        mca_bmi_sm_finalize,
        mca_bmi_sm_alloc,  
        mca_bmi_sm_free,  
        mca_bmi_sm_prepare_src,
        NULL,
        mca_bmi_sm_send,  
        NULL, /* get function */
        NULL  /* put function */
        }
    }
};

/* track information needed to synchronise a Shared Memory BMI module */
mca_bmi_sm_module_resource_t mca_bmi_sm_module_resource;


int mca_bmi_sm_add_procs_same_base_addr(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_bmi_base_endpoint_t **peers,
    ompi_bitmap_t* reachability)
{
    int return_code=OMPI_SUCCESS;
    size_t i,j,proc,size,n_to_allocate,length;
    int n_local_procs,cnt,len, my_len;
    mca_bmi_sm_exchange_t **sm_proc_info;
    ompi_proc_t* my_proc; /* pointer to caller's proc structure */
    mca_bmi_sm_t *bmi_sm;
    ompi_fifo_t *my_fifos;
    ompi_fifo_t * volatile *fifo_tmp;
    bool same_sm_base;
    ssize_t diff;
    volatile char **tmp_ptr;

    /* initializion */
    for(i=0 ; i < nprocs ; i++ ) {
        peers[i]=NULL;
    }
    bmi_sm=(mca_bmi_sm_t *)bmi;

    /* allocate array to hold setup shared memory from all
     * other procs */
    sm_proc_info=(mca_bmi_sm_exchange_t **)
        malloc(nprocs*sizeof(mca_bmi_sm_exchange_t *));
    if( NULL == sm_proc_info ){
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    mca_bmi_sm_component.sm_proc_connect=(int *) malloc(nprocs*sizeof(int));
    if( NULL == mca_bmi_sm_component.sm_proc_connect ){
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* initialize sm_proc_info and sm_proc_connect*/
    for(proc=0 ; proc < nprocs ; proc++ ) {
        sm_proc_info[proc]=0;
        mca_bmi_sm_component.sm_proc_connect[proc]=0;
    }

    /* get pointer to my proc structure */
    my_proc=ompi_proc_local();
    if( NULL == my_proc ) {
        return_code=OMPI_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    my_len=strlen(orte_system_info.nodename);

    /* Get unique host identifier for each process in the list,
     * and idetify procs that are on this host.  Add procs on this
     * host to shared memory reachbility list.  Also, get number
     * of local procs in the prcs list. */
    n_local_procs=0;
    for( proc=0 ; proc < nprocs; proc++ ) {
        /* check to see if this is me */
        if( my_proc == procs[proc] ) {
            mca_bmi_sm_component.my_smp_rank=n_local_procs;
        }
        if( procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ) {
            continue;
        }

        return_code = mca_base_modex_recv(
                &mca_bmi_sm_component.super.bmi_version, procs[proc],
                (void**)(&(sm_proc_info[proc])), &size);
        if(return_code != OMPI_SUCCESS) {
            ompi_output(0, "mca_bmi_sm_add_procs: mca_base_modex_recv: failed with return value=%d", return_code);
            goto CLEANUP;
        }

       /* for zero length, just continue - comparison is meaningless*/
       if( 0 >= size ) {
           continue;
       }

       /* check to see if this proc is on my host */
       len=strlen((char *)(sm_proc_info[proc]));
       if( len == my_len ) {
           if( 0 == strncmp(orte_system_info.nodename,
                       (char *)(sm_proc_info[proc]),len) ) {
               struct mca_bmi_base_endpoint_t *peer = peers[proc];
#if OMPI_ENABLE_PROGRESS_THREADS == 1
               char path[PATH_MAX];
               /* int flags; */
#endif

               /* initialize the peers information */
               peer = peers[proc]=malloc(sizeof(struct mca_bmi_base_endpoint_t));
               if( NULL == peer ){
                   return_code=OMPI_ERR_OUT_OF_RESOURCE;
                   goto CLEANUP;
               }
               peer->peer_smp_rank=n_local_procs+
                   mca_bmi_sm_component.num_smp_procs;

#if OMPI_ENABLE_PROGRESS_THREADS == 1
               sprintf(path, "%s/sm_fifo.%d", orte_process_info.job_session_dir, 
                   procs[proc]->proc_name.vpid);
               peer->fifo_fd = open(path, O_WRONLY);
               if(peer->fifo_fd < 0) {
                   ompi_output(0, "mca_bmi_sm_add_procs: open(%s) failed with errno=%d\n", path, errno);
                   goto CLEANUP;
               }
#endif
               n_local_procs++;
               mca_bmi_sm_component.sm_proc_connect[proc]=SM_CONNECTED;
           }
       }
    }
    if( n_local_procs == 0) {
        return_code = OMPI_SUCCESS;
        goto CLEANUP;
    }

    /* lookup shared memory pool */
    if(NULL == mca_bmi_sm_component.sm_mpool) {
        mca_bmi_sm_component.sm_mpool =
            mca_mpool_base_module_lookup(mca_bmi_sm_component.sm_mpool_name);
                                                                                                                     
        /* Sanity check to ensure that we found it */
        if (NULL == mca_bmi_sm_component.sm_mpool) {
           return_code = OMPI_ERR_OUT_OF_RESOURCE;
           goto CLEANUP;
        }
        mca_bmi_sm_component.sm_mpool_base =
            mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool_base);
    }

    /* make sure that my_smp_rank has been defined */
    if( 0xFFFFFFFF == mca_bmi_sm_component.my_smp_rank ) {
        return_code=OMPI_ERROR;
        goto CLEANUP;
    }

    /* see if need to allocate space for extra procs */
    if(  0 > mca_bmi_sm_component.sm_max_procs ) {

        /* no limit */
        if( 0 <= mca_bmi_sm_component.sm_extra_procs ) {
            /* limit */
            mca_bmi_sm_component.sm_max_procs=n_local_procs+
                mca_bmi_sm_component.sm_extra_procs;
        } else {
            /* no limit */
            mca_bmi_sm_component.sm_max_procs=2*n_local_procs;
        }
    }
    n_to_allocate=mca_bmi_sm_component.sm_max_procs;

    /* make sure n_to_allocate is greater than 0 */

    if ( !mca_bmi_sm[0].bmi_inited ) {
        /* set the shared memory offset */
        mca_bmi_sm_component.sm_offset=(ssize_t *)
            malloc(n_to_allocate*sizeof(ssize_t));
            if(NULL == mca_bmi_sm_component.sm_offset ) {
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* create a list of peers */
        mca_bmi_sm_component.sm_peers=(struct mca_bmi_base_endpoint_t**)
            malloc(n_to_allocate*sizeof(struct mca_bmi_base_endpoint_t*));
        if(NULL == mca_bmi_sm_component.sm_peers ) {
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access */
    for( proc=0 ; proc < nprocs; proc++ ) {
        struct mca_bmi_base_endpoint_t* peer = peers[proc];
        if(NULL != peer) {
            mca_bmi_sm_component.sm_peers[peer->peer_smp_rank] = peer;
            peer->my_smp_rank=mca_bmi_sm_component.my_smp_rank;
        }
    }

    /* Allocate Shared Memory BMI process coordination
     * data structure.  This will reside in shared memory */

    /* 
     * Create backing file - only first time through 
     */
    if ( !mca_bmi_sm[0].bmi_inited ) {
        /* set file name */
        len=asprintf(&(mca_bmi_sm_component.sm_resouce_ctl_file),
                "%s/shared_mem_bmi_module.%s",orte_process_info.job_session_dir,
                orte_system_info.nodename);
        if( 0 > len ) {
            goto CLEANUP;
        }

        size=sizeof(mca_bmi_sm_module_resource_t);
        if(NULL==(mca_bmi_sm_component.mmap_file=mca_common_sm_mmap_init(size,
                        mca_bmi_sm_component.sm_resouce_ctl_file,
                        sizeof(mca_bmi_sm_module_resource_t), 8 ))) 
        {
            ompi_output(0, "mca_bmi_sm_add_procs: unable to create shared memory BMI coordinating strucure :: size %ld \n",
                    size);
            return_code=OMPI_ERROR;
            goto CLEANUP;
        }

        /* set the pointer to the shared memory control structure */
        mca_bmi_sm_component.sm_ctl_header=(mca_bmi_sm_module_resource_t *)
            mca_bmi_sm_component.mmap_file->map_seg;


        /* Allocate a fixed size pointer array for the 2-D Shared memory queues.
         * Excess slots will be allocated for future growth.  One could
         * make this array growable, but then one would need to uses mutexes
         * for any access to these queues to ensure data consistancy when
         * the array is grown */
   
        if(0 == mca_bmi_sm_component.my_smp_rank ) {
            /* allocate ompi_fifo_t strucutes for each fifo of the queue
             * pairs - one per pair of local processes */
            /* check to make sure number of local procs is within the
             * specified limits */
            if( ( 0 < mca_bmi_sm_component.sm_max_procs ) &&
                    ( n_local_procs > mca_bmi_sm_component.sm_max_procs) ) {
                return_code=OMPI_ERROR;
                goto CLEANUP;
            }

            /* allocate array of ompi_fifo_t* elements -
             * offset relative to base segement is stored, so that
             * this can be used by other procs */
            mca_bmi_sm_component.sm_ctl_header->fifo=
                mca_bmi_sm_component.sm_mpool->mpool_alloc
                (mca_bmi_sm_component.sm_mpool, n_to_allocate*sizeof(ompi_fifo_t *),
                 CACHE_LINE_SIZE, NULL);
            if ( NULL == mca_bmi_sm_component.sm_ctl_header->fifo ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            /* initiazlize the pointer array */
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_bmi_sm_component.sm_ctl_header->fifo[i]=NULL;
            }

            /*  allocate and initialize the array to hold the virtual address
             *  of the shared memory base */
            mca_bmi_sm_component.sm_ctl_header->segment_header.
                base_shared_mem_segment =  ( volatile char **)
                mca_bmi_sm_component.sm_mpool->mpool_alloc
                (mca_bmi_sm_component.sm_mpool, n_to_allocate*sizeof(char *), CACHE_LINE_SIZE, NULL);
            if ( NULL == mca_bmi_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            /* initialize the pointer array */
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_bmi_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment[i]=NULL;
            }

            /*  allocate and initialize the array of flags indicating
             *  when the virtual address of the shared memory address
             *  has been set */
            mca_bmi_sm_component.sm_ctl_header->segment_header.
                base_shared_mem_flags = ( int *)
                mca_bmi_sm_component.sm_mpool->mpool_alloc
                (mca_bmi_sm_component.sm_mpool, n_to_allocate*sizeof(int), CACHE_LINE_SIZE, NULL);
            if ( NULL == mca_bmi_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_flags ) {
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            for(i=0 ; i < n_to_allocate ; i++ ) {
                mca_bmi_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_flags[i]=0;
            }

            /* set the addresses to be a relative, so that
             * they can be used by other procs */
            mca_bmi_sm_component.sm_ctl_header->fifo=
                (volatile ompi_fifo_t **)
                ( (char *)(mca_bmi_sm_component.sm_ctl_header->fifo)-
                  (char *)(mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool)) );

                mca_bmi_sm_component.sm_ctl_header->segment_header.
                    base_shared_mem_segment=( volatile char **)
                    ( (char *)(mca_bmi_sm_component.sm_ctl_header->
                               segment_header.base_shared_mem_segment) -
                      (char *)(mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool)) );

            /* allow other procs to use this shared memory map */
            mca_bmi_sm_component.mmap_file->map_seg->seg_inited=true;

            /* memory barrier to ensure this flag is set before other
             *  flags are set */
            ompi_atomic_mb();
        }
   
        /* Note:  Need to make sure that proc 0 initializes control
         * structures before any of the other procs can progress */
        if( 0 != mca_bmi_sm_component.my_smp_rank ) 
        {
            /* spin unitl local proc 0 initializes the segment */
            while(!mca_bmi_sm_component.mmap_file->map_seg->seg_inited)
            { ; }
        }
                
        /* set the base of the shared memory segment, and flag
         * indicating that it is set */
        tmp_ptr=(volatile char **)
            ( (char *)(mca_bmi_sm_component.sm_ctl_header->segment_header.
              base_shared_mem_segment)  +
		      (long )(mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool)) );
        tmp_ptr[mca_bmi_sm_component.my_smp_rank]=
            mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool);
        /* memory barrier to ensure this flag is set before other
         *  flags are set */
        ompi_atomic_mb();

        mca_bmi_sm_component.sm_ctl_header->segment_header.
            base_shared_mem_flags[mca_bmi_sm_component.my_smp_rank]=1;

        /*
         * initialize the array of fifo's "owned" by this process
         * The virtual addresses are valid only in the sender's
         * address space - unless the base of the shared memory
         * segment is mapped at the same location in the reader's
         * virtual address space.
         */
        my_fifos=( ompi_fifo_t *)
            mca_bmi_sm_component.sm_mpool->mpool_alloc
            (mca_bmi_sm_component.sm_mpool, n_to_allocate*sizeof(ompi_fifo_t), CACHE_LINE_SIZE, NULL);
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
        fifo_tmp=(ompi_fifo_t * volatile *)
                ( (char *)(mca_bmi_sm_component.sm_ctl_header->fifo) +
                  (long)(mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool)) );
        /* RLG : need memory barrier */
        fifo_tmp[mca_bmi_sm_component.my_smp_rank]=my_fifos;

        /* cache the pointer to the 2d fifo array.  These addresses
         * are valid in the current process space */
        mca_bmi_sm_component.fifo=(ompi_fifo_t **)
            malloc(sizeof(ompi_fifo_t *)*n_to_allocate);
        if( NULL == mca_bmi_sm_component.fifo ) {
            return_code=OMPI_ERROR;
            goto CLEANUP;
        }
        mca_bmi_sm_component.fifo[mca_bmi_sm_component.my_smp_rank]=my_fifos;
    }

    /* cache the pointers to the rest of the fifo arrays */
    fifo_tmp=(ompi_fifo_t * volatile *)
        ( (char *)(mca_bmi_sm_component.sm_ctl_header->fifo) +
          (long)(mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool)) );
    for( j=mca_bmi_sm_component.num_smp_procs ; j <
            mca_bmi_sm_component.num_smp_procs+n_local_procs ; j++ ) {

        /* spin until this element is allocated */
        while ( NULL == fifo_tmp[j] )
        { ; }

        tmp_ptr=(volatile char **)
            ( (char *)mca_bmi_sm_component.sm_ctl_header->
              segment_header.base_shared_mem_segment +
              (long)mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool));
        diff= tmp_ptr[mca_bmi_sm_component.my_smp_rank]-tmp_ptr[j];
        mca_bmi_sm_component.fifo[j]=
            ( ompi_fifo_t *)( (char *)fifo_tmp[j]+diff);
        mca_bmi_sm_component.sm_offset[j]=tmp_ptr[j]-
            tmp_ptr[mca_bmi_sm_component.my_smp_rank];
                
    }

    /* initialize some of the free-lists */
    if( !mca_bmi_sm[0].bmi_inited ) {
        /* some initialization happens only the first time this routine
         * is called, i.e. when bmi_inited is false */

        /* initialize fragment descriptor free list */

        /* 
         * first fragment 
         */

        /* allocation will be for the fragment descriptor, payload buffer,
         * and padding to ensure proper alignment can be acheived */
        length=sizeof(mca_bmi_sm_frag_t)+
            mca_bmi_sm_component.fragment_alignment+
            mca_bmi_sm_component.first_fragment_size;
        ompi_free_list_init(&mca_bmi_sm_component.sm_frags1, length,
                OBJ_CLASS(mca_bmi_sm_frag1_t),
                mca_bmi_sm_component.sm_free_list_num,
                mca_bmi_sm_component.sm_free_list_max,
                mca_bmi_sm_component.sm_free_list_inc,
                mca_bmi_sm_component.sm_mpool); /* use shared-memory pool */

        length=sizeof(mca_bmi_sm_frag_t)+
            mca_bmi_sm_component.fragment_alignment+
            mca_bmi_sm_component.max_fragment_size;
        ompi_free_list_init(&mca_bmi_sm_component.sm_frags2, length,
                OBJ_CLASS(mca_bmi_sm_frag2_t),
                mca_bmi_sm_component.sm_free_list_num,
                mca_bmi_sm_component.sm_free_list_max,
                mca_bmi_sm_component.sm_free_list_inc,
                mca_bmi_sm_component.sm_mpool); /* use shared-memory pool */

        /* set up mca_bmi_sm_component.list_smp_procs_same_base_addr */
        mca_bmi_sm_component.list_smp_procs_same_base_addr=(int *)
            malloc(mca_bmi_sm_component.sm_max_procs*sizeof(int));
        if( NULL == mca_bmi_sm_component.list_smp_procs_same_base_addr ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* set up mca_bmi_sm_component.list_smp_procs_different_base_addr */
        mca_bmi_sm_component.list_smp_procs_different_base_addr=(int *)
            malloc(mca_bmi_sm_component.sm_max_procs*sizeof(int));
        if( NULL == mca_bmi_sm_component.list_smp_procs_different_base_addr ){
            return_code=OMPI_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }

        /* set flag indicating bmi has been inited */
        bmi_sm->bmi_inited=true;
    }

    /* set connectivity */
    cnt=0;
    for(proc = 0 ; proc < nprocs ; proc++ ) {

        struct mca_bmi_base_endpoint_t* peer = peers[proc];
        if(peer == NULL)
            continue;

        tmp_ptr=(volatile char **)
            ( (char *)mca_bmi_sm_component.sm_ctl_header->
              segment_header.base_shared_mem_segment +
              (long)mca_bmi_sm_component.sm_mpool->mpool_base(mca_bmi_sm_component.sm_mpool));
        same_sm_base=(tmp_ptr[peer->peer_smp_rank] ==
            tmp_ptr[mca_bmi_sm_component.my_smp_rank]);

        if( SM_CONNECTED == mca_bmi_sm_component.sm_proc_connect[proc] ) {
            if( same_sm_base ){

                /* don't count if same process */
                if( (mca_bmi_sm_component.num_smp_procs+cnt ) ==
                        mca_bmi_sm_component.my_smp_rank) {
                    cnt++;
                    continue;
                }
                /* set up the list of local processes with the same base
                 * shared memory virtual address as this process */
                mca_bmi_sm_component.list_smp_procs_same_base_addr
                    [mca_bmi_sm_component.num_smp_procs_same_base_addr]=
                    cnt;
                mca_bmi_sm_component.num_smp_procs_same_base_addr++;
                cnt++;
                /* add this proc to shared memory accessability list */
                return_code=ompi_bitmap_set_bit(reachability,proc);
                if( OMPI_SUCCESS != return_code ){
                    goto CLEANUP;
                }
            } else {
                /* set up the list of local processes with the same base
                 * shared memory virtual address as this process */
                mca_bmi_sm_component.list_smp_procs_different_base_addr
                    [mca_bmi_sm_component.num_smp_procs_different_base_addr]=
                    cnt;
                mca_bmi_sm_component.num_smp_procs_different_base_addr++;
                cnt++;
                mca_bmi_sm_component.sm_proc_connect[proc]=
                    SM_CONNECTED_DIFFERENT_BASE_ADDR;
            }
        }
    }

    /* update the local smp process count */
    mca_bmi_sm_component.num_smp_procs+=n_local_procs;

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

/* Note:: this routine assumes that mca_bmi_sm_add_procs_same_base_addr
 *  has already been called to set up data structures needed by this
 *  routine */
int mca_bmi_sm_add_procs(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs, 
    struct ompi_proc_t **procs, 
    struct mca_bmi_base_endpoint_t **peers,
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
                    mca_bmi_sm_component.sm_proc_connect[proc]) ||
                (SM_CONNECTED ==
                    mca_bmi_sm_component.sm_proc_connect[proc]) ) {
            tmp_cnt++;
        }
    }
    /* set connectivity */
    n_local_procs=0;
    for(proc = 0 ; proc < nprocs ; proc++ ) {
        if( (SM_CONNECTED_DIFFERENT_BASE_ADDR ==
                    mca_bmi_sm_component.sm_proc_connect[proc]) ||
                (SM_CONNECTED ==
                    mca_bmi_sm_component.sm_proc_connect[proc]) ) {
            n_local_procs++;
        }

        if( (SM_CONNECTED_DIFFERENT_BASE_ADDR ==
                    mca_bmi_sm_component.sm_proc_connect[proc]) ) {

            /* add this proc to shared memory accessability list */
            return_code=ompi_bitmap_set_bit(reachability,proc);
            if( OMPI_SUCCESS != return_code ){
                goto CLEANUP;
            }

            /* initialize the peers information */
            peers[proc]=malloc(sizeof(struct mca_bmi_base_endpoint_t));
            if( NULL == peers[proc] ){
                return_code=OMPI_ERR_OUT_OF_RESOURCE;
                goto CLEANUP;
            }
            peers[proc]->my_smp_rank=mca_bmi_sm_component.my_smp_rank;
            /* subtract tmp_cnt, since mca_bmi_sm_add_procs_same_base_addr
             * already added these into num_smp_procs */
            peers[proc]->peer_smp_rank=n_local_procs+
                mca_bmi_sm_component.num_smp_procs-tmp_cnt;
            n_local_procs++;
        }
    }

CLEANUP:
    /* free local memory */
    if(mca_bmi_sm_component.sm_proc_connect){
        free(mca_bmi_sm_component.sm_proc_connect);
        mca_bmi_sm_component.sm_proc_connect=NULL;
    }

    return return_code;
}

int mca_bmi_sm_del_procs(
    struct mca_bmi_base_module_t* bmi, 
    size_t nprocs,
    struct ompi_proc_t **procs, 
    struct mca_bmi_base_endpoint_t **peers)
{
    return OMPI_SUCCESS;
}


/**
 * MCA->BMI Clean up any resources held by BMI module
 * before the module is unloaded.
 *
 * @param bmi (IN)   BMI module.
 *
 * Prior to unloading a BMI module, the MCA framework will call
 * the BMI finalize method of the module. Any resources held by
 * the BMI should be released and if required the memory corresponding
 * to the BMI module freed.
 *
 */

int mca_bmi_sm_finalize(struct mca_bmi_base_module_t* bmi)
{
    return OMPI_SUCCESS;
}


/**
 * Register a callback function that is called on receipt
 * of a fragment.
 *
 * @param bmi (IN)     BMI module
 * @return             Status indicating if cleanup was successful
 *
 * When the process list changes, the PML notifies the BMI of the
 * change, to provide the opportunity to cleanup or release any
 * resources associated with the peer.
 */

int mca_bmi_sm_register(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_tag_t tag,
    mca_bmi_base_module_recv_cb_fn_t cbfunc,
    void* cbdata)
{
    mca_bmi_sm_t* sm_bmi = (mca_bmi_sm_t*)bmi;
    sm_bmi->sm_reg[tag].cbfunc = cbfunc;
    sm_bmi->sm_reg[tag].cbdata = cbdata;
    return OMPI_SUCCESS;
}
                                                                                                                 

/**
 * Allocate a segment.
 *
 * @param bmi (IN)      BMI module
 * @param size (IN)     Request segment size.
 */
extern mca_bmi_base_descriptor_t* mca_bmi_sm_alloc(
    struct mca_bmi_base_module_t* bmi,
    size_t size)
{
    mca_bmi_sm_frag_t* frag;
    int rc;
    if(size <= mca_bmi_sm_component.first_fragment_size) {
        MCA_BMI_SM_FRAG_ALLOC1(frag,rc);
    } else {
        MCA_BMI_SM_FRAG_ALLOC2(frag,rc);
    }
    return (mca_bmi_base_descriptor_t*)frag;
}
                                                                                                                   
/**
 * Return a segment allocated by this BMI.
 *
 * @param bmi (IN)      BMI module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_bmi_sm_free(
    struct mca_bmi_base_module_t* bmi,
    mca_bmi_base_descriptor_t* des)
{
    mca_bmi_sm_frag_t* frag = (mca_bmi_sm_frag_t*)des;
    if(frag->size <= mca_bmi_sm_component.first_fragment_size) {
        MCA_BMI_SM_FRAG_RETURN1(frag);
    } else {
        MCA_BMI_SM_FRAG_RETURN2(frag);
    }
    return OMPI_SUCCESS;
}


/**
 * Pack data
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */
struct mca_bmi_base_descriptor_t* mca_bmi_sm_prepare_src(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* peer,
    struct ompi_convertor_t* convertor,
    size_t reserve,
    size_t* size)
{
    mca_bmi_sm_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    uint32_t max_data = *size;
    int rc;

    MCA_BMI_SM_FRAG_ALLOC2(frag, rc);
    if(NULL == frag) {
        return NULL;
    }

    if(max_data + reserve > frag->size) {
        max_data = *size - reserve;
    } 
    iov.iov_len = max_data;
    iov.iov_base = (unsigned char*)(frag+1) + reserve;

    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data, NULL);
    if(rc < 0) {
        MCA_BMI_SM_FRAG_RETURN2(frag);
        return NULL;
    }
    *size = max_data;
    return &frag->base;
}

 
/**
 * Initiate a send to the peer.
 *
 * @param bmi (IN)      BMI module
 * @param peer (IN)     BMI peer addressing
 */

int mca_bmi_sm_send(
    struct mca_bmi_base_module_t* bmi,
    struct mca_bmi_base_endpoint_t* endpoint,
    struct mca_bmi_base_descriptor_t* descriptor,
    mca_bmi_base_tag_t tag)
{
    mca_bmi_sm_frag_t* frag = (mca_bmi_sm_frag_t*)descriptor;
    int rc;

    frag->tag = tag;
    frag->type = MCA_BMI_SM_FRAG_SEND;
    frag->rc = OMPI_SUCCESS;

    /* 
     * post the descriptor in the queue - post with the relative
     * address 
     */
    MCA_BMI_SM_FIFO_WRITE(endpoint->my_smp_rank,endpoint->peer_smp_rank, frag, rc);
    return rc;
}


