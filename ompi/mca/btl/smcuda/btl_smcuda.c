/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2009 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.  
 *                         All rights reserved. 
 * Copyright (c) 2012      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#include <errno.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */

#include "opal/sys/atomic.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "opal/util/printf.h"
#include "opal/mca/hwloc/base/base.h"
#include "orte/util/proc_info.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/btl/btl.h"
#if OMPI_CUDA_SUPPORT
#include "ompi/mca/common/cuda/common_cuda.h"
#endif /* OMPI_CUDA_SUPPORT */
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"

#if OPAL_ENABLE_FT_CR    == 1
#include "opal/mca/crs/base/base.h"
#include "opal/util/basename.h"
#include "orte/mca/sstore/sstore.h"
#include "ompi/runtime/ompi_cr.h"
#endif

#include "btl_smcuda.h"
#include "btl_smcuda_endpoint.h"
#include "btl_smcuda_frag.h"
#include "btl_smcuda_fifo.h"
#include "ompi/proc/proc.h"

mca_btl_smcuda_t mca_btl_smcuda = {
    {
        &mca_btl_smcuda_component.super,
        0, /* btl_eager_limit */
        0, /* btl_rndv_eager_limit */
        0, /* btl_max_send_size */
        0, /* btl_rdma_pipeline_send_length */
        0, /* btl_rdma_pipeline_frag_size */
        0, /* btl_min_rdma_pipeline_size */
        0, /* btl_exclusivity */
        0, /* btl_latency */
        0, /* btl_bandwidth */
        0, /* btl flags */
        0, /* btl segment size */
        mca_btl_smcuda_add_procs,
        mca_btl_smcuda_del_procs,
        NULL,
        mca_btl_smcuda_finalize,
        mca_btl_smcuda_alloc,
        mca_btl_smcuda_free,
        mca_btl_smcuda_prepare_src,
#if OMPI_CUDA_SUPPORT
        mca_btl_smcuda_prepare_dst,
#else
        NULL,
#endif /* OMPI_CUDA_SUPPORT */
        mca_btl_smcuda_send,
        mca_btl_smcuda_sendi,
        NULL,  /* put */
        NULL,  /* get -- optionally filled during initialization */
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_smcuda_register_error_cb, /* register error */
        mca_btl_smcuda_ft_event
    }
};

/*
 * calculate offset of an address from the beginning of a shared memory segment
 */
#define ADDR2OFFSET(ADDR, BASE) ((char*)(ADDR) - (char*)(BASE))

/*
 * calculate an absolute address in a local address space given an offset and
 * a base address of a shared memory segment
 */
#define OFFSET2ADDR(OFFSET, BASE) ((ptrdiff_t)(OFFSET) + (char*)(BASE))


static void *mpool_calloc(size_t nmemb, size_t size)
{
    void *buf;
    size_t bsize = nmemb * size;
    mca_mpool_base_module_t *mpool = mca_btl_smcuda_component.sm_mpool;

    buf = mpool->mpool_alloc(mpool, bsize, opal_cache_line_size, 0, NULL);

    if (NULL == buf)
        return NULL;

    memset(buf, 0, bsize);
    return buf;
}


static int smcuda_btl_first_time_init(mca_btl_smcuda_t *smcuda_btl, int n)
{
    size_t size, length, length_payload;
    char *sm_ctl_file;
    sm_fifo_t *my_fifos;
    int my_mem_node, num_mem_nodes, i;
    ompi_proc_t **procs;
    size_t num_procs;
    mca_mpool_base_resources_t res;
    mca_btl_smcuda_component_t* m = &mca_btl_smcuda_component;

    /* Assume we don't have hwloc support and fill in dummy info */
    mca_btl_smcuda_component.mem_node = my_mem_node = 0;
    mca_btl_smcuda_component.num_mem_nodes = num_mem_nodes = 1;

#if OPAL_HAVE_HWLOC
    /* If we have hwloc support, then get accurate information */
    if (NULL != opal_hwloc_topology) {
        i = opal_hwloc_base_get_nbobjs_by_type(opal_hwloc_topology,
                                               HWLOC_OBJ_NODE, 0,
                                               OPAL_HWLOC_AVAILABLE);

        /* If we find >0 NUMA nodes, then investigate further */
        if (i > 0) {
            opal_hwloc_level_t bind_level;
            unsigned int bind_index;

            /* JMS This tells me how many numa nodes are *available*,
               but it's not how many are being used *by this job*.
               Note that this is the value we've previously used (from
               the previous carto-based implementation), but it really
               should be improved to be how many NUMA nodes are being
               used *in this job*. */
            mca_btl_smcuda_component.num_mem_nodes = num_mem_nodes = i;

            /* Fill opal_hwloc_my_cpuset and find out to what level
               this process is bound (if at all) */
            opal_hwloc_base_get_local_cpuset();
            opal_hwloc_base_get_level_and_index(opal_hwloc_my_cpuset,
                                                &bind_level, &bind_index);
            if (OPAL_HWLOC_NODE_LEVEL != bind_level) {
                /* We are bound to *something* (i.e., our binding
                   level is less than "node", meaning the entire
                   machine), so discover which NUMA node this process
                   is bound */
                if (OPAL_HWLOC_NUMA_LEVEL == bind_level) {
                    mca_btl_smcuda_component.mem_node = my_mem_node = (int) bind_index;
                } else {
                    if (OPAL_SUCCESS == 
                        opal_hwloc_base_get_local_index(HWLOC_OBJ_NODE, 0, &bind_index)) {
                        mca_btl_smcuda_component.mem_node = my_mem_node = (int) bind_index;
                    } else {
                        /* Weird.  We can't figure out what NUMA node
                           we're on. :-( */
                        mca_btl_smcuda_component.mem_node = my_mem_node = -1;
                    }
                }
            }
        }
    }
#endif

    /* lookup shared memory pool */
    mca_btl_smcuda_component.sm_mpools = (mca_mpool_base_module_t **) calloc(num_mem_nodes,
                                            sizeof(mca_mpool_base_module_t*));

    /* Create one mpool.  Per discussion with George and a UTK Euro
       MPI 2010 paper, it may be beneficial to create multiple mpools.
       Leaving that for a future optimization, however. */
    /* Disable memory binding, because each MPI process will claim
       pages in the mpool for their local NUMA node */
    res.mem_node = -1;

    /* determine how much memory to create */
    /*
     * This heuristic formula mostly says that we request memory for:
     * - nfifos FIFOs, each comprising:
     *   . a sm_fifo_t structure
     *   . many pointers (fifo_size of them per FIFO)
     * - eager fragments (2*n of them, allocated in sm_free_list_inc chunks)
     * - max fragments (sm_free_list_num of them)
     *
     * On top of all that, we sprinkle in some number of
     * "opal_cache_line_size" additions to account for some
     * padding and edge effects that may lie in the allocator.
     */
    res.size =
        FIFO_MAP_NUM(n) * ( sizeof(sm_fifo_t) + sizeof(void *) * m->fifo_size + 4 * opal_cache_line_size )
        + ( 2 * n + m->sm_free_list_inc ) * ( m->eager_limit   + 2 * opal_cache_line_size )
        +           m->sm_free_list_num   * ( m->max_frag_size + 2 * opal_cache_line_size );

    /* before we multiply by n, make sure the result won't overflow */
    /* Stick that little pad in, particularly since we'll eventually
     * need a little extra space.  E.g., in mca_mpool_sm_init() in
     * mpool_sm_component.c when sizeof(mca_common_sm_module_t) is
     * added.
     */
    if ( ((double) res.size) * n > LONG_MAX - 4096 ) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    res.size *= n;
    
    /* now, create it */
    mca_btl_smcuda_component.sm_mpools[0] =
        mca_mpool_base_module_create(mca_btl_smcuda_component.sm_mpool_name,
                                     smcuda_btl, &res);
    /* Sanity check to ensure that we found it */
    if (NULL == mca_btl_smcuda_component.sm_mpools[0]) {
            return OMPI_ERR_OUT_OF_RESOURCE;
    }

    mca_btl_smcuda_component.sm_mpool = mca_btl_smcuda_component.sm_mpools[0];
#if OMPI_CUDA_SUPPORT
    /* Create a local memory pool that sends handles to the remote
     * side.  Note that the res argument is not really used, but
     * needed to satisfy function signature. */
    smcuda_btl->super.btl_mpool = mca_mpool_base_module_create("gpusm",
                                                               smcuda_btl,
                                                               &res);
    if (NULL == smcuda_btl->super.btl_mpool) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
#endif /* OMPI_CUDA_SUPPORT */

    mca_btl_smcuda_component.sm_mpool_base =
        mca_btl_smcuda_component.sm_mpools[0]->mpool_base(mca_btl_smcuda_component.sm_mpools[0]);

    /* create a list of peers */
    mca_btl_smcuda_component.sm_peers = (struct mca_btl_base_endpoint_t**)
        calloc(n, sizeof(struct mca_btl_base_endpoint_t*));
    if (NULL == mca_btl_smcuda_component.sm_peers) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Allocate Shared Memory BTL process coordination
     * data structure.  This will reside in shared memory */

    /* set file name */
    if (asprintf(&sm_ctl_file, "%s"OPAL_PATH_SEP"shared_mem_btl_module.%s",
                 orte_process_info.job_session_dir,
                 orte_process_info.nodename) < 0) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Pass in a data segment alignment of 0 to get no data
       segment (only the shared control structure) */
    size = sizeof(mca_common_sm_seg_header_t) +
        n * (sizeof(sm_fifo_t*) + sizeof(char *) + sizeof(uint16_t)) + opal_cache_line_size;
    procs = ompi_proc_world(&num_procs);
    if (!(mca_btl_smcuda_component.sm_seg =
          mca_common_sm_init(procs, num_procs, size, sm_ctl_file,
                             sizeof(mca_common_sm_seg_header_t),
                             opal_cache_line_size))) {
        opal_output(0, "mca_btl_smcuda_add_procs: unable to create shared memory "
                    "BTL coordinating strucure :: size %lu \n",
                    (unsigned long)size);
        free(procs);
        free(sm_ctl_file);
        return OMPI_ERROR;
    }
    free(procs);
    free(sm_ctl_file);

    /* check to make sure number of local procs is within the
     * specified limits */
    if(mca_btl_smcuda_component.sm_max_procs > 0 &&
       mca_btl_smcuda_component.num_smp_procs + n >
       mca_btl_smcuda_component.sm_max_procs) {
        return OMPI_ERROR;
    }

    mca_btl_smcuda_component.shm_fifo = (volatile sm_fifo_t **)mca_btl_smcuda_component.sm_seg->module_data_addr;
    mca_btl_smcuda_component.shm_bases = (char**)(mca_btl_smcuda_component.shm_fifo + n);
    mca_btl_smcuda_component.shm_mem_nodes = (uint16_t*)(mca_btl_smcuda_component.shm_bases + n);

    /* set the base of the shared memory segment */
    mca_btl_smcuda_component.shm_bases[mca_btl_smcuda_component.my_smp_rank] =
        (char*)mca_btl_smcuda_component.sm_mpool_base;
    mca_btl_smcuda_component.shm_mem_nodes[mca_btl_smcuda_component.my_smp_rank] =
        (uint16_t)my_mem_node;

    /* initialize the array of fifo's "owned" by this process */
    if(NULL == (my_fifos = (sm_fifo_t*)mpool_calloc(FIFO_MAP_NUM(n), sizeof(sm_fifo_t))))
        return OMPI_ERR_OUT_OF_RESOURCE;

    mca_btl_smcuda_component.shm_fifo[mca_btl_smcuda_component.my_smp_rank] = my_fifos;

    /* cache the pointer to the 2d fifo array.  These addresses
     * are valid in the current process space */
    mca_btl_smcuda_component.fifo = (sm_fifo_t**)malloc(sizeof(sm_fifo_t*) * n);

    if(NULL == mca_btl_smcuda_component.fifo)
        return OMPI_ERR_OUT_OF_RESOURCE;

    mca_btl_smcuda_component.fifo[mca_btl_smcuda_component.my_smp_rank] = my_fifos;

    mca_btl_smcuda_component.mem_nodes = (uint16_t *) malloc(sizeof(uint16_t) * n);
    if(NULL == mca_btl_smcuda_component.mem_nodes)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* initialize fragment descriptor free lists */

    /* allocation will be for the fragment descriptor and payload buffer */
    length = sizeof(mca_btl_smcuda_frag1_t);
    length_payload =
        sizeof(mca_btl_smcuda_hdr_t) + mca_btl_smcuda_component.eager_limit;
    i = ompi_free_list_init_new(&mca_btl_smcuda_component.sm_frags_eager, length,
                                opal_cache_line_size, OBJ_CLASS(mca_btl_smcuda_frag1_t),
                                length_payload, opal_cache_line_size,
                                mca_btl_smcuda_component.sm_free_list_num,
                                mca_btl_smcuda_component.sm_free_list_max,
                                mca_btl_smcuda_component.sm_free_list_inc,
                                mca_btl_smcuda_component.sm_mpool);
    if ( OMPI_SUCCESS != i )
        return i;

    length = sizeof(mca_btl_smcuda_frag2_t);
    length_payload =
        sizeof(mca_btl_smcuda_hdr_t) + mca_btl_smcuda_component.max_frag_size;
    i = ompi_free_list_init_new(&mca_btl_smcuda_component.sm_frags_max, length,
                                opal_cache_line_size, OBJ_CLASS(mca_btl_smcuda_frag2_t),
                                length_payload, opal_cache_line_size,
                                mca_btl_smcuda_component.sm_free_list_num,
                                mca_btl_smcuda_component.sm_free_list_max,
                                mca_btl_smcuda_component.sm_free_list_inc,
                                mca_btl_smcuda_component.sm_mpool);
    if ( OMPI_SUCCESS != i )
        return i;

    i = ompi_free_list_init_new(&mca_btl_smcuda_component.sm_frags_user, 
		    sizeof(mca_btl_smcuda_user_t),
		    opal_cache_line_size, OBJ_CLASS(mca_btl_smcuda_user_t),
		    sizeof(mca_btl_smcuda_hdr_t), opal_cache_line_size,
		    mca_btl_smcuda_component.sm_free_list_num,
		    mca_btl_smcuda_component.sm_free_list_max,
		    mca_btl_smcuda_component.sm_free_list_inc,
		    mca_btl_smcuda_component.sm_mpool);
    if ( OMPI_SUCCESS != i )
	    return i;   

    mca_btl_smcuda_component.num_outstanding_frags = 0;

    mca_btl_smcuda_component.num_pending_sends = 0;
    i = opal_free_list_init(&mca_btl_smcuda_component.pending_send_fl,
                            sizeof(btl_smcuda_pending_send_item_t),
                            OBJ_CLASS(opal_free_list_item_t),
                            16, -1, 32);
    if ( OMPI_SUCCESS != i )
        return i;

    /* set flag indicating btl has been inited */
    smcuda_btl->btl_inited = true;

    return OMPI_SUCCESS;
}

static struct mca_btl_base_endpoint_t *
create_sm_endpoint(int local_proc, struct ompi_proc_t *proc)
{
    struct mca_btl_base_endpoint_t *ep;
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    char path[PATH_MAX];
#endif

    ep = (struct mca_btl_base_endpoint_t*)
        malloc(sizeof(struct mca_btl_base_endpoint_t));
    if(NULL == ep)
        return NULL;
    ep->peer_smp_rank = local_proc + mca_btl_smcuda_component.num_smp_procs;

    OBJ_CONSTRUCT(&ep->pending_sends, opal_list_t);
    OBJ_CONSTRUCT(&ep->endpoint_lock, opal_mutex_t);
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    sprintf(path, "%s"OPAL_PATH_SEP"sm_fifo.%lu",
            orte_process_info.job_session_dir,
            (unsigned long)proc->proc_name.vpid);
    ep->fifo_fd = open(path, O_WRONLY);
    if(ep->fifo_fd < 0) {
        opal_output(0, "mca_btl_smcuda_add_procs: open(%s) failed with errno=%d\n",
                    path, errno);
        free(ep);
        return NULL;
    }
#endif
#if OMPI_CUDA_SUPPORT
    {
        mca_mpool_base_resources_t resources; /* unused, but needed */

        /* Create a remote memory pool on the endpoint.  Note that the resources
         * argument is just to satisfy the function signature.  The rcuda mpool
         * actually takes care of filling in the resources. */
        ep->mpool = mca_mpool_base_module_create("rgpusm",
                                                 NULL,
                                                 &resources);
    }
#endif /* OMPI_CUDA_SUPPORT */
    return ep;
}

static void calc_sm_max_procs(int n)
{
    /* see if need to allocate space for extra procs */
    if(0 > mca_btl_smcuda_component.sm_max_procs) {
        /* no limit */
        if(0 <= mca_btl_smcuda_component.sm_extra_procs) {
            /* limit */
            mca_btl_smcuda_component.sm_max_procs =
                n + mca_btl_smcuda_component.sm_extra_procs;
        } else {
            /* no limit */
            mca_btl_smcuda_component.sm_max_procs = 2 * n;
        }
    }
}

int mca_btl_smcuda_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t **peers,
    opal_bitmap_t* reachability)
{
    int return_code = OMPI_SUCCESS;
    int32_t n_local_procs = 0, proc, j, my_smp_rank = -1;
    ompi_proc_t* my_proc; /* pointer to caller's proc structure */
    mca_btl_smcuda_t *smcuda_btl;
    bool have_connected_peer = false;
    char **bases;
    /* initializion */

    smcuda_btl = (mca_btl_smcuda_t *)btl;

    /* get pointer to my proc structure */
    if(NULL == (my_proc = ompi_proc_local()))
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* Get unique host identifier for each process in the list,
     * and idetify procs that are on this host.  Add procs on this
     * host to shared memory reachbility list.  Also, get number
     * of local procs in the procs list. */
    for(proc = 0; proc < (int32_t)nprocs; proc++) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        /* check to see if this is me */
        if(my_proc == procs[proc]) {
            my_smp_rank = mca_btl_smcuda_component.my_smp_rank = n_local_procs++;
            continue;
        }

        /* we have someone to talk to */
        have_connected_peer = true;

        if(!(peers[proc] = create_sm_endpoint(n_local_procs, procs[proc]))) {
            return_code = OMPI_ERROR;
            goto CLEANUP;
        }
        n_local_procs++;

        /* add this proc to shared memory accessibility list */
        return_code = opal_bitmap_set_bit(reachability, proc);
        if(OMPI_SUCCESS != return_code)
            goto CLEANUP;
    }

    /* jump out if there's not someone we can talk to */
    if (!have_connected_peer)
        goto CLEANUP;

    /* make sure that my_smp_rank has been defined */
    if(-1 == my_smp_rank) {
        return_code = OMPI_ERROR;
        goto CLEANUP;
    }

    calc_sm_max_procs(n_local_procs);

    if (!smcuda_btl->btl_inited) {
        return_code =
            smcuda_btl_first_time_init(smcuda_btl, mca_btl_smcuda_component.sm_max_procs);
        if(return_code != OMPI_SUCCESS)
            goto CLEANUP;
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access and calculate reachability */
    for(proc = 0; proc < (int32_t)nprocs; proc++) {
        if(NULL == peers[proc])
            continue;
        mca_btl_smcuda_component.sm_peers[peers[proc]->peer_smp_rank] = peers[proc];
        peers[proc]->my_smp_rank = my_smp_rank;
    }

    bases = mca_btl_smcuda_component.shm_bases;

    /* initialize own FIFOs */
    /*
     * The receiver initializes all its FIFOs.  All components will
     * be allocated near the receiver.  Nothing will be local to
     * "the sender" since there will be many senders.
     */
    for(j = mca_btl_smcuda_component.num_smp_procs;
        j < mca_btl_smcuda_component.num_smp_procs + FIFO_MAP_NUM(n_local_procs); j++) {

        return_code = sm_fifo_init( mca_btl_smcuda_component.fifo_size,
                                    mca_btl_smcuda_component.sm_mpool,
                                   &mca_btl_smcuda_component.fifo[my_smp_rank][j],
                                    mca_btl_smcuda_component.fifo_lazy_free);
        if(return_code != OMPI_SUCCESS)
            goto CLEANUP;
    }

    opal_atomic_wmb();

    /* Sync with other local procs. Force the FIFO initialization to always
     * happens before the readers access it.
     */
    opal_atomic_add_32( &mca_btl_smcuda_component.sm_seg->module_seg->seg_inited, 1);
    while( n_local_procs >
           mca_btl_smcuda_component.sm_seg->module_seg->seg_inited) {
        opal_progress();
        opal_atomic_rmb();
    }

    /* coordinate with other processes */
    for(j = mca_btl_smcuda_component.num_smp_procs;
        j < mca_btl_smcuda_component.num_smp_procs + n_local_procs; j++) {
        ptrdiff_t diff;

        /* spin until this element is allocated */
        /* doesn't really wait for that process... FIFO might be allocated, but not initialized */
        opal_atomic_rmb();
        while(NULL == mca_btl_smcuda_component.shm_fifo[j]) {
            opal_progress();
            opal_atomic_rmb();
        }

        /* Calculate the difference as (my_base - their_base) */
        diff = ADDR2OFFSET(bases[my_smp_rank], bases[j]);

        /* store local address of remote fifos */
        mca_btl_smcuda_component.fifo[j] =
            (sm_fifo_t*)OFFSET2ADDR(diff, mca_btl_smcuda_component.shm_fifo[j]);

        /* cache local copy of peer memory node number */
        mca_btl_smcuda_component.mem_nodes[j] = mca_btl_smcuda_component.shm_mem_nodes[j];
    }

    /* update the local smp process count */
    mca_btl_smcuda_component.num_smp_procs += n_local_procs;

    /* make sure we have enough eager fragmnents for each process */
    return_code = ompi_free_list_resize(&mca_btl_smcuda_component.sm_frags_eager,
                                        mca_btl_smcuda_component.num_smp_procs * 2);
    if (OMPI_SUCCESS != return_code)
        goto CLEANUP;

CLEANUP:
    return return_code;
}

int mca_btl_smcuda_del_procs(
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

int mca_btl_smcuda_finalize(struct mca_btl_base_module_t* btl)
{
    return OMPI_SUCCESS;
}


/*
 * Register callback function for error handling..
 */
int mca_btl_smcuda_register_error_cb(
        struct mca_btl_base_module_t* btl,
        mca_btl_base_module_error_cb_fn_t cbfunc)
{
    mca_btl_smcuda_t *smcuda_btl = (mca_btl_smcuda_t *)btl;
    smcuda_btl->error_cb = cbfunc;
    return OMPI_SUCCESS;
}

/**
 * Allocate a segment.
 *
 * @param btl (IN)      BTL module
 * @param size (IN)     Request segment size.
 */
extern mca_btl_base_descriptor_t* mca_btl_smcuda_alloc(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_smcuda_frag_t* frag = NULL;
    int rc;
    if(size <= mca_btl_smcuda_component.eager_limit) {
        MCA_BTL_SMCUDA_FRAG_ALLOC_EAGER(frag,rc);
    } else if (size <= mca_btl_smcuda_component.max_frag_size) {
        MCA_BTL_SMCUDA_FRAG_ALLOC_MAX(frag,rc);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segment.base.seg_len = size;
        frag->base.des_flags = flags;
    }
    return (mca_btl_base_descriptor_t*)frag;
}

/**
 * Return a segment allocated by this BTL.
 *
 * @param btl (IN)      BTL module
 * @param segment (IN)  Allocated segment.
 */
extern int mca_btl_smcuda_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* des)
{
    mca_btl_smcuda_frag_t* frag = (mca_btl_smcuda_frag_t*)des;
    MCA_BTL_SMCUDA_FRAG_RETURN(frag);

    return OMPI_SUCCESS;
}


/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 */
struct mca_btl_base_descriptor_t* mca_btl_smcuda_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    mca_mpool_base_registration_t* registration,
    struct opal_convertor_t* convertor,
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_smcuda_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;
#if OMPI_CUDA_SUPPORT
    if (0 != reserve) {
#endif /* OMPI_CUDA_SUPPORT */
        if ( reserve + max_data <= mca_btl_smcuda_component.eager_limit ) {
            MCA_BTL_SMCUDA_FRAG_ALLOC_EAGER(frag,rc);
        } else {
            MCA_BTL_SMCUDA_FRAG_ALLOC_MAX(frag, rc);
        }
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }

        if( OPAL_UNLIKELY(reserve + max_data > frag->size) ) {
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base =
            (IOVBASE_TYPE*)(frag->segment.base.seg_addr.lval + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if( OPAL_UNLIKELY(rc < 0) ) {
            MCA_BTL_SMCUDA_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.base.seg_len = reserve + max_data;
#if OMPI_CUDA_SUPPORT
    } else {
        /* Normally, we are here because we have a GPU buffer and we are preparing
         * to send it.  However, we can also be there because we have received a 
         * PUT message because we are trying to send a host buffer.  Therefore,
         * we need to again check to make sure buffer is GPU.  If not, then return
         * NULL. We can just check the convertor since we have that. */
        if (!(convertor->flags & CONVERTOR_CUDA)) {
            return NULL;
        }

        MCA_BTL_SMCUDA_FRAG_ALLOC_USER(frag, rc);
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
        if( OPAL_UNLIKELY(rc < 0) ) {
           MCA_BTL_SMCUDA_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.base.seg_addr.pval = iov.iov_base;
        frag->segment.base.seg_len = max_data;
        memcpy(frag->segment.key, ((mca_mpool_common_cuda_reg_t *)registration)->memHandle,
               sizeof(((mca_mpool_common_cuda_reg_t *)registration)->memHandle) + 
               sizeof(((mca_mpool_common_cuda_reg_t *)registration)->evtHandle));
        frag->segment.memh_seg_addr.pval = registration->base;
        frag->segment.memh_seg_len = registration->bound - registration->base + 1;

    }
#endif /* OMPI_CUDA_SUPPORT */
    frag->base.des_src = &(frag->segment.base);
    frag->base.des_src_cnt = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->base.des_dst = NULL;
    frag->base.des_dst_cnt = 0;
    frag->base.des_flags = flags;
    *size = max_data;
    return &frag->base;
}

#if 0
#define MCA_BTL_SMCUDA_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(sm_frag)      \
    do {                                                                \
        char* _memory = (char*)(sm_frag)->segment.base.seg_addr.pval +  \
            (sm_frag)->segment.base.seg_len;                            \
        int* _intmem;                                                   \
        size_t align = (intptr_t)_memory & 0xFUL;                       \
        switch( align & 0x3 ) {                                         \
        case 3: *_memory = 0; _memory++;                                \
        case 2: *_memory = 0; _memory++;                                \
        case 1: *_memory = 0; _memory++;                                \
        }                                                               \
        align >>= 2;                                                    \
        _intmem = (int*)_memory;                                        \
        switch( align ) {                                               \
        case 3: *_intmem = 0; _intmem++;                                \
        case 2: *_intmem = 0; _intmem++;                                \
        case 1: *_intmem = 0; _intmem++;                                \
        }                                                               \
    } while(0)
#else
#define MCA_BTL_SMCUDA_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(sm_frag)
#endif

#if 0
        if( OPAL_LIKELY(align > 0) ) {                                  \
            align = 0xFUL - align;                                      \
            memset( _memory, 0, align );                                \
        }                                                               \

#endif

/**
 * Initiate an inline send to the peer. If failure then return a descriptor.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_smcuda_sendi( struct mca_btl_base_module_t* btl,
                      struct mca_btl_base_endpoint_t* endpoint,
                      struct opal_convertor_t* convertor,
                      void* header,
                      size_t header_size,
                      size_t payload_size,
                      uint8_t order,
                      uint32_t flags,
                      mca_btl_base_tag_t tag,
                      mca_btl_base_descriptor_t** descriptor )
{
    size_t length = (header_size + payload_size);
    mca_btl_smcuda_frag_t* frag;
    int rc;

    if ( mca_btl_smcuda_component.num_outstanding_frags * 2 > (int) mca_btl_smcuda_component.fifo_size ) {
        mca_btl_smcuda_component_progress();
    }

    /* this check should be unnecessary... turn into an assertion? */
    if( length < mca_btl_smcuda_component.eager_limit ) {

        /* allocate a fragment, giving up if we can't get one */
        /* note that frag==NULL is equivalent to rc returning an error code */
        MCA_BTL_SMCUDA_FRAG_ALLOC_EAGER(frag, rc);
        if( OPAL_UNLIKELY(NULL == frag) ) {
            *descriptor = NULL;
            return rc;
        }

        /* fill in fragment fields */
        frag->segment.base.seg_len = length;
        frag->hdr->len        = length;
        assert( 0 == (flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) );
        frag->base.des_flags = flags | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;   /* why do any flags matter here other than OWNERSHIP? */
        frag->hdr->tag = tag;
        frag->endpoint = endpoint;

        /* write the match header (with MPI comm/tag/etc. info) */
        memcpy( frag->segment.base.seg_addr.pval, header, header_size );

        /* write the message data if there is any */
        /*
          We can add MEMCHECKER calls before and after the packing.
        */
        if( payload_size ) {
            size_t max_data;
            struct iovec iov;
            uint32_t iov_count;
            /* pack the data into the supplied buffer */
            iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)frag->segment.base.seg_addr.pval + header_size);
            iov.iov_len  = max_data = payload_size;
            iov_count    = 1;

            (void)opal_convertor_pack( convertor, &iov, &iov_count, &max_data);

            assert(max_data == payload_size);
        }

        MCA_BTL_SMCUDA_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(frag);

        /* write the fragment pointer to the FIFO */
        /*
         * Note that we don't care what the FIFO-write return code is.  Even if
         * the return code indicates failure, the write has still "completed" from
         * our point of view:  it has been posted to a "pending send" queue.
         */
        OPAL_THREAD_ADD32(&mca_btl_smcuda_component.num_outstanding_frags, +1);
        MCA_BTL_SMCUDA_FIFO_WRITE(endpoint, endpoint->my_smp_rank,
                              endpoint->peer_smp_rank, (void *) VIRTUAL2RELATIVE(frag->hdr), false, true, rc);
        return OMPI_SUCCESS;
    }

    /* presumably, this code path will never get executed */
    *descriptor = mca_btl_smcuda_alloc( btl, endpoint, order,
                                    payload_size + header_size, flags);
    return OMPI_ERR_RESOURCE_BUSY;
}

/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_smcuda_send( struct mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor,
                     mca_btl_base_tag_t tag )
{
    mca_btl_smcuda_frag_t* frag = (mca_btl_smcuda_frag_t*)descriptor;
    int rc;

    if ( mca_btl_smcuda_component.num_outstanding_frags * 2 > (int) mca_btl_smcuda_component.fifo_size ) {
        mca_btl_smcuda_component_progress();
    }

    /* available header space */
    frag->hdr->len = frag->segment.base.seg_len;
    /* type of message, pt-2-pt, one-sided, etc */
    frag->hdr->tag = tag;

    MCA_BTL_SMCUDA_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(frag);

    frag->endpoint = endpoint;

    /*
     * post the descriptor in the queue - post with the relative
     * address
     */
    OPAL_THREAD_ADD32(&mca_btl_smcuda_component.num_outstanding_frags, +1);
    MCA_BTL_SMCUDA_FIFO_WRITE(endpoint, endpoint->my_smp_rank,
                          endpoint->peer_smp_rank, (void *) VIRTUAL2RELATIVE(frag->hdr), false, true, rc);
    if( OPAL_LIKELY(0 == rc) ) {
        return 1;  /* the data is completely gone */
    }
    frag->base.des_flags |= MCA_BTL_DES_SEND_ALWAYS_CALLBACK;
    /* not yet gone, but pending. Let the upper level knows that
     * the callback will be triggered when the data will be sent.
     */
    return 0;
}
#if OMPI_CUDA_SUPPORT
struct mca_btl_base_descriptor_t* mca_btl_smcuda_prepare_dst( 
        struct mca_btl_base_module_t* btl,
        struct mca_btl_base_endpoint_t* endpoint,
        struct mca_mpool_base_registration_t* registration,
        struct opal_convertor_t* convertor,
        uint8_t order,
        size_t reserve,
        size_t* size,
        uint32_t flags)
{
    int rc;
    mca_btl_smcuda_frag_t* frag;

    /* Only support GPU buffers */
    if (!(convertor->flags & CONVERTOR_CUDA)) {
        return NULL;
    }

    MCA_BTL_SMCUDA_FRAG_ALLOC_USER(frag, rc);
    if(OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }
    
    frag->segment.base.seg_len = *size;
    opal_convertor_get_current_pointer( convertor, (void**)&(frag->segment.base.seg_addr.pval) );

    frag->base.des_src = NULL;
    frag->base.des_src_cnt = 0;
    frag->base.des_dst = &frag->segment.base;
    frag->base.des_dst_cnt = 1;
    frag->base.des_flags = flags;
    return &frag->base;
}
#endif /* OMPI_CUDA_SUPPORT */


#if OMPI_CUDA_SUPPORT
int mca_btl_smcuda_get_cuda(struct mca_btl_base_module_t* btl,
                        struct mca_btl_base_endpoint_t* ep,
                        struct mca_btl_base_descriptor_t* descriptor)
{
    mca_btl_smcuda_segment_t *src_seg = (mca_btl_smcuda_segment_t *) descriptor->des_src;
    mca_btl_smcuda_segment_t *dst_seg = (mca_btl_smcuda_segment_t *) descriptor->des_dst;
    mca_mpool_common_cuda_reg_t rget_reg;
    mca_mpool_common_cuda_reg_t *reg_ptr = &rget_reg;
    int btl_ownership;
    int rc, done;
    void *remote_memory_address;
    size_t offset;
    mca_btl_smcuda_frag_t* frag = (mca_btl_smcuda_frag_t*)descriptor;
 
    /* Set to 0 for debugging since it is a list item but I am not
     * intializing it properly and it is annoying to see all the
     * garbage in the debugger.  */
    
    memset(&rget_reg, 0, sizeof(rget_reg));
    memcpy(&rget_reg.memHandle, src_seg->key, sizeof(src_seg->key));

    /* Open the memory handle to the remote memory.  If it is cached, then
     * we just retrieve it from cache and avoid a call to open the handle.  That
     * is taken care of in the memory pool.  Note that we are searching for the
     * memory based on the base address and size of the memory handle, not the
     * remote memory which may lie somewhere in the middle. This is taken care of
     * a few lines down. Note that we hand in the peer rank just for debugging
     * support. */
    rc = ep->mpool->mpool_register(ep->mpool, src_seg->memh_seg_addr.pval,
                                   src_seg->memh_seg_len, ep->peer_smp_rank,
                                   (mca_mpool_base_registration_t **)&reg_ptr);

    if (OMPI_SUCCESS != rc) {
        opal_output(0, "Failed to register remote memory, rc=%d", rc);
        return rc;
    }
    frag->registration = (mca_mpool_base_registration_t *)reg_ptr;
    frag->endpoint = ep;

    /* The registration has given us back the memory block that this
     * address lives in.  However, the base address of the block may
     * not equal the address that was used to retrieve the block.
     * Therefore, compute the offset and add it to the address of the
     * memory handle. */
    offset = (unsigned char *)src_seg->base.seg_addr.lval - reg_ptr->base.base;
    remote_memory_address = (unsigned char *)reg_ptr->base.alloc_base + offset;
    if (0 != offset) {
        opal_output(-1, "OFFSET=%d", (int)offset);
    }

    /* The remote side posted an IPC event to make sure we do not start our
     * copy until IPC event completes.  This is to ensure that the data being sent
     * is available in the sender's GPU buffer.  Therefore, do a stream synchronize
     * on the IPC event that we received.  Note that we pull it from 
     * rget_reg, not reg_ptr, as we do not cache the event. */
    mca_common_wait_stream_synchronize(&rget_reg);

    rc = mca_common_cuda_memcpy(dst_seg->base.seg_addr.pval, remote_memory_address,
                                dst_seg->base.seg_len, "mca_btl_smcuda_get",
                                (mca_btl_base_descriptor_t *)frag, &done);
    if (OMPI_SUCCESS != rc) {
        /* Out of resources can be handled by upper layers. */
        if (OMPI_ERR_OUT_OF_RESOURCE != rc) {
            opal_output(0, "Failed to cuMemcpy GPU memory, rc=%d", rc);
        }
        return rc;
    }

    if (OPAL_UNLIKELY(1 == done)) {
        /* This should only be true when experimenting with synchronous copies. */
        btl_ownership = (frag->base.des_flags & MCA_BTL_DES_FLAGS_BTL_OWNERSHIP);
        if (0 != (MCA_BTL_DES_SEND_ALWAYS_CALLBACK & frag->base.des_flags)) {
            frag->base.des_cbfunc(&mca_btl_smcuda.super, 
                                  frag->endpoint, &frag->base, 
                                  OMPI_SUCCESS);
        }

        if (btl_ownership) {
            mca_btl_smcuda_free(btl, (mca_btl_base_descriptor_t *)frag);
        }
    }

    return OMPI_SUCCESS;

}
#endif /* OMPI_CUDA_SUPPORT */

#if OPAL_ENABLE_FT_CR    == 0
int mca_btl_smcuda_ft_event(int state) {
    return OMPI_SUCCESS;
}
#else
int mca_btl_smcuda_ft_event(int state) {
    /* Notify mpool */
    if( NULL != mca_btl_smcuda_component.sm_mpool &&
        NULL != mca_btl_smcuda_component.sm_mpool->mpool_ft_event) {
        mca_btl_smcuda_component.sm_mpool->mpool_ft_event(state);
    }

    if(OPAL_CRS_CHECKPOINT == state) {
        if( NULL != mca_btl_smcuda_component.sm_seg ) {
            /* On restart we need the old file names to exist (not necessarily
             * contain content) so the CRS component does not fail when searching
             * for these old file handles. The restart procedure will make sure
             * these files get cleaned up appropriately.
             */
            orte_sstore.set_attr(orte_sstore_handle_current,
                                 SSTORE_METADATA_LOCAL_TOUCH,
                                 mca_btl_smcuda_component.sm_seg->shmem_ds.seg_name);
        }
    }
    else if(OPAL_CRS_CONTINUE == state) {
        if( orte_cr_continue_like_restart ) {
            if( NULL != mca_btl_smcuda_component.sm_seg ) {
                /* Add shared memory file */
                opal_crs_base_cleanup_append(mca_btl_smcuda_component.sm_seg->shmem_ds.seg_name, false);
            }

            /* Clear this so we force the module to re-init the sm files */
            mca_btl_smcuda_component.sm_mpool = NULL;
        }
    }
    else if(OPAL_CRS_RESTART == state ||
            OPAL_CRS_RESTART_PRE == state) {
        if( NULL != mca_btl_smcuda_component.sm_seg ) {
            /* Add shared memory file */
            opal_crs_base_cleanup_append(mca_btl_smcuda_component.sm_seg->shmem_ds.seg_name, false);
        }

        /* Clear this so we force the module to re-init the sm files */
        mca_btl_smcuda_component.sm_mpool = NULL;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
#endif /* OPAL_ENABLE_FT_CR */
