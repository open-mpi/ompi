/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
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
#include "orte/util/show_help.h"
#include "opal/util/if.h"
#include "opal/mca/carto/carto.h"
#include "opal/mca/carto/base/base.h"
#include "opal/mca/paffinity/base/base.h"
#include "opal/mca/maffinity/base/base.h"
#include "orte/util/proc_info.h"
#include "opal/util/printf.h"
#include "ompi/class/ompi_free_list.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"
#include "ompi/mca/mpool/sm/mpool_sm.h"

#if OPAL_ENABLE_FT    == 1
#include "opal/mca/crs/base/base.h"
#include "opal/util/basename.h"
#include "ompi/runtime/ompi_cr.h"
#endif

#include "btl_sm.h"
#include "btl_sm_endpoint.h"
#include "btl_sm_frag.h"
#include "btl_sm_fifo.h"
#include "ompi/proc/proc.h"

mca_btl_sm_t mca_btl_sm = {
    {
        &mca_btl_sm_component.super,
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
        mca_btl_sm_add_procs,
        mca_btl_sm_del_procs,
        NULL,
        mca_btl_sm_finalize,
        mca_btl_sm_alloc,
        mca_btl_sm_free,
        mca_btl_sm_prepare_src,
        NULL,
        mca_btl_sm_send,
        NULL /*mca_btl_sm_sendi*/,  /* send immediate */
        NULL,  /* put */
        NULL,  /* get */
        mca_btl_base_dump,
        NULL, /* mpool */
        mca_btl_sm_register_error_cb, /* register error */
        mca_btl_sm_ft_event
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
    mca_mpool_base_module_t *mpool = mca_btl_sm_component.sm_mpool;

    buf = mpool->mpool_alloc(mpool, bsize, CACHE_LINE_SIZE, 0, NULL);

    if (NULL == buf)
        return NULL;

    memset(buf, 0, bsize);
    return buf;
}

static void init_maffinity(int *my_mem_node, int *max_mem_node)
{
    static opal_carto_graph_t *topo;
    opal_value_array_t dists;
    int i, num_core, socket;
    opal_paffinity_base_cpu_set_t cpus;
    char *myslot = NULL;
    opal_carto_node_distance_t *dist;
    opal_carto_base_node_t *slot_node;

    *my_mem_node = 0;
    *max_mem_node = 1;

    if (OMPI_SUCCESS != opal_carto_base_get_host_graph(&topo, "Memory")) {
        return;
    }

     OBJ_CONSTRUCT(&dists, opal_value_array_t);
     opal_value_array_init(&dists, sizeof(opal_carto_node_distance_t));

    if (OMPI_SUCCESS != opal_paffinity_base_get_processor_info(&num_core))  {
        num_core = 100;  /* set something large */
    }

     OPAL_PAFFINITY_CPU_ZERO(cpus);
     opal_paffinity_base_get(&cpus);

     /* find core we are running on */
     for (i = 0; i < num_core; i++) {
         if (OPAL_PAFFINITY_CPU_ISSET(i, cpus)) {
             break;
         }
     }

    if (OMPI_SUCCESS != opal_paffinity_base_get_map_to_socket_core(i, &socket, &i)) {
        /* no topology info available */
        goto out;
    }
    
     asprintf(&myslot, "slot%d", socket);

     slot_node = opal_carto_base_find_node(topo, myslot);

     if(NULL == slot_node) {
         goto out;
     }

     opal_carto_base_get_nodes_distance(topo, slot_node, "Memory", &dists);
     if((*max_mem_node = opal_value_array_get_size(&dists)) < 2) {
         goto out;
     }

     dist = (opal_carto_node_distance_t *) opal_value_array_get_item(&dists, 0);
     opal_maffinity_base_node_name_to_id(dist->node->node_name, my_mem_node);
out:
     if (myslot) {
         free(myslot);
     }
     OBJ_DESTRUCT(&dists);
     opal_carto_base_free_graph(topo);
}

static int sm_btl_first_time_init(mca_btl_sm_t *sm_btl, int n)
{
    size_t size, length, length_payload;
    char *sm_ctl_file;
    mca_btl_sm_fifo_t *my_fifos;
    int my_mem_node=-1, num_mem_nodes=-1, i;
    ompi_proc_t **procs;
    size_t num_procs;

    init_maffinity(&my_mem_node, &num_mem_nodes);
    mca_btl_sm_component.mem_node = my_mem_node;
    mca_btl_sm_component.num_mem_nodes = num_mem_nodes;

    /* lookup shared memory pool */
    mca_btl_sm_component.sm_mpools = (mca_mpool_base_module_t **) calloc(num_mem_nodes,
                                            sizeof(mca_mpool_base_module_t*));

    /* create mpool for each memory node */
    for(i = 0; i < num_mem_nodes; i++) {
        mca_mpool_base_resources_t res;
        mca_btl_sm_component_t* m = &mca_btl_sm_component;

        /* disable memory binding if there is only one memory node */
        res.mem_node = (num_mem_nodes == 1) ? -1 : i;

        /* determine how much memory to create */
        /*
         * This heuristic formula mostly says that we request memory for:
         * - nfifos FIFOs, each comprising:
         *   . an mca_btl_sm_fifo_t structure
         *   . many pointers (fifo_size of them per FIFO)
         * - eager fragments (2*n of them, allocated in sm_free_list_inc chunks)
         * - max fragments (sm_free_list_num of them)
         *
         * On top of all that, we sprinkle in some number of "CACHE_LINE_SIZE"
         * additions to account for some padding and edge effects that may lie
         * in the allocator.
         */
        res.size = m->nfifos * ( sizeof(mca_btl_sm_fifo_t) + sizeof(void *) * m->fifo_size + 4 * CACHE_LINE_SIZE )
            + ( 2 * n + m->sm_free_list_inc ) * ( m->eager_limit   + 2 * CACHE_LINE_SIZE )
            +           m->sm_free_list_num   * ( m->max_frag_size + 2 * CACHE_LINE_SIZE );
        if ( ((double) res.size) * n > LONG_MAX )
            res.size  = LONG_MAX;
        else
            res.size *= n;

        /* now, create it */
        mca_btl_sm_component.sm_mpools[i] =
            mca_mpool_base_module_create(mca_btl_sm_component.sm_mpool_name,
                                         sm_btl, &res);
        /* Sanity check to ensure that we found it */
        if(NULL == mca_btl_sm_component.sm_mpools[i])
            return OMPI_ERR_OUT_OF_RESOURCE;

        if(i == my_mem_node)
            mca_btl_sm_component.sm_mpool = mca_btl_sm_component.sm_mpools[i];
    }


    mca_btl_sm_component.sm_mpool_base =
        mca_btl_sm_component.sm_mpools[0]->mpool_base(mca_btl_sm_component.sm_mpools[0]);

    /* set the shared memory offset */
    mca_btl_sm_component.sm_offset = (ptrdiff_t*)calloc(n, sizeof(ptrdiff_t));
    if(NULL == mca_btl_sm_component.sm_offset)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* create a list of peers */
    mca_btl_sm_component.sm_peers = (struct mca_btl_base_endpoint_t**)
        calloc(n, sizeof(struct mca_btl_base_endpoint_t*));
    if(NULL == mca_btl_sm_component.sm_peers)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* Allocate Shared Memory BTL process coordination
     * data structure.  This will reside in shared memory */

    /* set file name */
    if(asprintf(&sm_ctl_file, "%s"OPAL_PATH_SEP"shared_mem_btl_module.%s",
                orte_process_info.job_session_dir,
                orte_process_info.nodename) < 0)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* Pass in a data segment alignment of 0 to get no data
       segment (only the shared control structure) */
    size = sizeof(mca_common_sm_file_header_t) +
        n * (sizeof(mca_btl_sm_fifo_t*) + sizeof(char *) + sizeof(uint16_t)) + CACHE_LINE_SIZE;
    procs = ompi_proc_world(&num_procs);
    if (!(mca_btl_sm_component.mmap_file =
          mca_common_sm_mmap_init(procs, num_procs, size, sm_ctl_file,
                                  sizeof(mca_common_sm_file_header_t),
                                  CACHE_LINE_SIZE))) {
        opal_output(0, "mca_btl_sm_add_procs: unable to create shared memory "
                    "BTL coordinating strucure :: size %lu \n",
                    (unsigned long)size);
        free(procs);
        free(sm_ctl_file);
        return OMPI_ERROR;
    }
    free(procs);
    free(sm_ctl_file);

    /* set the pointer to the shared memory control structure */
    mca_btl_sm_component.sm_ctl_header =
        (mca_common_sm_file_header_t*)mca_btl_sm_component.mmap_file->map_seg;


    /* check to make sure number of local procs is within the
     * specified limits */
    if(mca_btl_sm_component.sm_max_procs > 0 &&
       mca_btl_sm_component.num_smp_procs + n >
       mca_btl_sm_component.sm_max_procs) {
        return OMPI_ERROR;
    }

    mca_btl_sm_component.shm_fifo = (mca_btl_sm_fifo_t **)mca_btl_sm_component.mmap_file->data_addr;
    mca_btl_sm_component.shm_bases = (char**)(mca_btl_sm_component.shm_fifo + n);
    mca_btl_sm_component.shm_mem_nodes = (uint16_t*)(mca_btl_sm_component.shm_bases + n);

    /* Sync with other local procs. (Do we have to?) */
    if(0 == mca_btl_sm_component.my_smp_rank) {
        mca_btl_sm_component.mmap_file->map_seg->seg_inited = true;

        /* memory barrier to ensure this flag is set before other
         *  flags are set */
        opal_atomic_wmb();
    } else {
        while(!mca_btl_sm_component.mmap_file->map_seg->seg_inited) {
            opal_atomic_rmb();
            opal_progress();
        }
    }

    /* set the base of the shared memory segment */
    mca_btl_sm_component.shm_bases[mca_btl_sm_component.my_smp_rank] =
        (char*)mca_btl_sm_component.sm_mpool_base;
    mca_btl_sm_component.shm_mem_nodes[mca_btl_sm_component.my_smp_rank] =
        (uint16_t)my_mem_node;

    /* initialize the array of fifo's "owned" by this process */
    if(NULL == (my_fifos = (mca_btl_sm_fifo_t*)mpool_calloc(FIFO_MAP_NUM(n), sizeof(mca_btl_sm_fifo_t))))
        return OMPI_ERR_OUT_OF_RESOURCE;

    mca_btl_sm_component.shm_fifo[mca_btl_sm_component.my_smp_rank] = my_fifos;

    opal_atomic_wmb();

    /* cache the pointer to the 2d fifo array.  These addresses
     * are valid in the current process space */
    mca_btl_sm_component.fifo = (mca_btl_sm_fifo_t**)malloc(sizeof(mca_btl_sm_fifo_t*) * n);

    if(NULL == mca_btl_sm_component.fifo)
        return OMPI_ERR_OUT_OF_RESOURCE;

    mca_btl_sm_component.fifo[mca_btl_sm_component.my_smp_rank] = my_fifos;

    mca_btl_sm_component.mem_nodes = (uint16_t *) malloc(sizeof(uint16_t) * n);
    if(NULL == mca_btl_sm_component.mem_nodes)
        return OMPI_ERR_OUT_OF_RESOURCE;

    /* initialize fragment descriptor free lists */

    /* allocation will be for the fragment descriptor and payload buffer */
    length = sizeof(mca_btl_sm_frag1_t);
    length_payload =
        sizeof(mca_btl_sm_hdr_t) + mca_btl_sm_component.eager_limit;
    i = ompi_free_list_init_new(&mca_btl_sm_component.sm_frags_eager, length,
                                CACHE_LINE_SIZE, OBJ_CLASS(mca_btl_sm_frag1_t),
                                length_payload, CACHE_LINE_SIZE,
                                mca_btl_sm_component.sm_free_list_num,
                                mca_btl_sm_component.sm_free_list_max,
                                mca_btl_sm_component.sm_free_list_inc,
                                mca_btl_sm_component.sm_mpool);
    if ( OMPI_SUCCESS != i )
        return i;

    length = sizeof(mca_btl_sm_frag2_t);
    length_payload =
        sizeof(mca_btl_sm_hdr_t) + mca_btl_sm_component.max_frag_size;
    i = ompi_free_list_init_new(&mca_btl_sm_component.sm_frags_max, length,
                                CACHE_LINE_SIZE, OBJ_CLASS(mca_btl_sm_frag2_t),
                                length_payload, CACHE_LINE_SIZE,
                                mca_btl_sm_component.sm_free_list_num,
                                mca_btl_sm_component.sm_free_list_max,
                                mca_btl_sm_component.sm_free_list_inc,
                                mca_btl_sm_component.sm_mpool);
    if ( OMPI_SUCCESS != i )
        return i;

    mca_btl_sm_component.num_outstanding_frags = 0;

    mca_btl_sm_component.num_pending_sends = 0;
    i = opal_free_list_init(&mca_btl_sm_component.pending_send_fl,
                            sizeof(mca_btl_sm_pending_send_item_t),
                            OBJ_CLASS(opal_free_list_item_t),
                            16, -1, 32);
    if ( OMPI_SUCCESS != i )
        return i;

    /* set flag indicating btl has been inited */
    sm_btl->btl_inited = true;

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
    ep->peer_smp_rank = local_proc + mca_btl_sm_component.num_smp_procs;

    OBJ_CONSTRUCT(&ep->pending_sends, opal_list_t);
#if OMPI_ENABLE_PROGRESS_THREADS == 1
    sprintf(path, "%s"OPAL_PATH_SEP"sm_fifo.%lu",
            orte_process_info.job_session_dir,
            (unsigned long)proc->proc_name.vpid);
    ep->fifo_fd = open(path, O_WRONLY);
    if(ep->fifo_fd < 0) {
        opal_output(0, "mca_btl_sm_add_procs: open(%s) failed with errno=%d\n",
                    path, errno);
        free(ep);
        return NULL;
    }
#endif
    return ep;
}

static void calc_sm_max_procs(int n)
{
    /* see if need to allocate space for extra procs */
    if(0 > mca_btl_sm_component.sm_max_procs) {
        /* no limit */
        if(0 <= mca_btl_sm_component.sm_extra_procs) {
            /* limit */
            mca_btl_sm_component.sm_max_procs =
                n + mca_btl_sm_component.sm_extra_procs;
        } else {
            /* no limit */
            mca_btl_sm_component.sm_max_procs = 2 * n;
        }
    }
}

int mca_btl_sm_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct ompi_proc_t **procs,
    struct mca_btl_base_endpoint_t **peers,
    ompi_bitmap_t* reachability)
{
    int return_code = OMPI_SUCCESS;
    int32_t n_local_procs = 0, proc, j,
        my_smp_rank = mca_btl_sm_component.my_smp_rank;
    ompi_proc_t* my_proc; /* pointer to caller's proc structure */
    mca_btl_sm_t *sm_btl;
    bool have_connected_peer = false;
    char **bases;
    /* initializion */

    sm_btl = (mca_btl_sm_t *)btl;

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
                 0 == (procs[proc]->proc_flags & OMPI_PROC_FLAG_LOCAL)) {
            peers[proc] = NULL;
            continue;
        }

        /* check to see if this is me */
        if(my_proc == procs[proc]) {
            my_smp_rank = mca_btl_sm_component.my_smp_rank = n_local_procs++;
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
        return_code = ompi_bitmap_set_bit(reachability, proc);
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

    if (!sm_btl->btl_inited) {
        return_code =
            sm_btl_first_time_init(sm_btl, mca_btl_sm_component.sm_max_procs);
        if(return_code != OMPI_SUCCESS)
            goto CLEANUP;
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access and calulcate reachebility */
    for(proc = 0; proc < (int32_t)nprocs; proc++) {
        if(NULL == peers[proc])
            continue;
        mca_btl_sm_component.sm_peers[peers[proc]->peer_smp_rank] = peers[proc];
        peers[proc]->my_smp_rank = my_smp_rank;
    }

    bases = mca_btl_sm_component.shm_bases;

    /* initialize own FIFOs */
    /*
     * The receiver initializes all its FIFOs.  All components will
     * be allocated near the receiver.  Nothing will be local to
     * "the sender" since there will be many senders.
     */
    for(j = mca_btl_sm_component.num_smp_procs;
        j < mca_btl_sm_component.num_smp_procs + FIFO_MAP_NUM(n_local_procs); j++) {

        return_code = sm_fifo_init( mca_btl_sm_component.fifo_size,
                                    mca_btl_sm_component.sm_mpool,
                                   &mca_btl_sm_component.fifo[my_smp_rank][j],
                                    mca_btl_sm_component.fifo_lazy_free);
        if(return_code != OMPI_SUCCESS)
            goto CLEANUP;
    }

    /* coordinate with other processes */
    for(j = mca_btl_sm_component.num_smp_procs;
        j < mca_btl_sm_component.num_smp_procs + n_local_procs; j++) {
        ptrdiff_t diff;

        /* spin until this element is allocated */
        /* doesn't really wait for that process... FIFO might be allocated, but not initialized */
        while(NULL == mca_btl_sm_component.shm_fifo[j]) {
            opal_atomic_rmb();
            opal_progress();
        }

        /* Calculate the difference as (my_base - their_base) */
        diff = ADDR2OFFSET(bases[my_smp_rank], bases[j]);
        mca_btl_sm_component.sm_offset[j] = diff;

        /* store local address of remote fifos */
        mca_btl_sm_component.fifo[j] =
            (mca_btl_sm_fifo_t*)OFFSET2ADDR(diff, mca_btl_sm_component.shm_fifo[j]);

        /* cache local copy of peer memory node number */
        mca_btl_sm_component.mem_nodes[j] = mca_btl_sm_component.shm_mem_nodes[j];
    }

    /* update the local smp process count */
    mca_btl_sm_component.num_smp_procs += n_local_procs;

    /* make sure we have enough eager fragmnents for each process */
    return_code = ompi_free_list_resize(&mca_btl_sm_component.sm_frags_eager,
                                        mca_btl_sm_component.num_smp_procs * 2);
    if (OMPI_SUCCESS != return_code)
        goto CLEANUP;

CLEANUP:
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
    struct mca_btl_base_endpoint_t* endpoint,
    uint8_t order,
    size_t size,
    uint32_t flags)
{
    mca_btl_sm_frag_t* frag = NULL;
    int rc;
    if(size <= mca_btl_sm_component.eager_limit) {
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag,rc);
    } else if (size <= mca_btl_sm_component.max_frag_size) {
        MCA_BTL_SM_FRAG_ALLOC_MAX(frag,rc);
    }

    if (OPAL_LIKELY(frag != NULL)) {
        frag->segment.seg_len = size;
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
    uint8_t order,
    size_t reserve,
    size_t* size,
    uint32_t flags)
{
    mca_btl_sm_frag_t* frag;
    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data = *size;
    int rc;

    if ( reserve + max_data <= mca_btl_sm_component.eager_limit ) {
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag,rc);
    } else {
        MCA_BTL_SM_FRAG_ALLOC_MAX(frag, rc);
    }
    if(OPAL_UNLIKELY(NULL == frag)) {
        return NULL;
    }

    if(reserve + max_data > frag->size) {
        max_data = frag->size - reserve;
    }
    iov.iov_len = max_data;
    iov.iov_base =
        (IOVBASE_TYPE*)(((unsigned char*)(frag->segment.seg_addr.pval)) +
                reserve);

    rc = ompi_convertor_pack(convertor, &iov, &iov_count, &max_data );
    if(rc < 0) {
        MCA_BTL_SM_FRAG_RETURN(frag);
        return NULL;
    }
    frag->segment.seg_len = reserve + max_data;
    frag->base.des_flags = flags;
    *size = max_data;
    return &frag->base;
}

#if 0
#define MCA_BTL_SM_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(sm_frag)          \
    do {                                                                \
        char* _memory = (char*)(sm_frag)->segment.seg_addr.pval +       \
            (sm_frag)->segment.seg_len;                                 \
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
#define MCA_BTL_SM_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(sm_frag)
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
int mca_btl_sm_sendi( struct mca_btl_base_module_t* btl,
                      struct mca_btl_base_endpoint_t* endpoint,
                      struct ompi_convertor_t* convertor,
                      void* header,
                      size_t header_size,
                      size_t payload_size,
                      uint8_t order,
                      uint32_t flags,
                      mca_btl_base_tag_t tag,
                      mca_btl_base_descriptor_t** descriptor )
{
    size_t max_data, length = (header_size + payload_size);
    mca_btl_sm_frag_t* frag;
    int rc;

    if ( mca_btl_sm_component.num_outstanding_frags * 2 > (int) mca_btl_sm_component.fifo_size ) {
        mca_btl_sm_component_progress();
    }

    /* this check should be unnecessary... turn into an assertion? */
    if( length < mca_btl_sm_component.eager_limit ) {
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag, rc);
        if( OPAL_UNLIKELY(NULL == frag) ) {
            *descriptor = NULL;
            return rc;
        }
        frag->segment.seg_len = length;
        frag->hdr->len = length;
        assert( 0 == (flags & MCA_BTL_DES_SEND_ALWAYS_CALLBACK) );
        frag->base.des_flags = flags | MCA_BTL_DES_FLAGS_BTL_OWNERSHIP;
        frag->hdr->tag = tag;
        frag->endpoint = endpoint;

        memcpy( frag->segment.seg_addr.pval, header, header_size );
        if( payload_size ) {
            struct iovec iov;
            uint32_t iov_count;
            /* pack the data into the supplied buffer */
            iov.iov_base = (IOVBASE_TYPE*)((unsigned char*)frag->segment.seg_addr.pval + header_size);
            iov.iov_len  = max_data = payload_size;
            iov_count    = 1;

            (void)ompi_convertor_pack( convertor,
                                       &iov, &iov_count, &max_data);

            assert(max_data == payload_size);
        }
        MCA_BTL_SM_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(frag);
        /*
         * post the descriptor in the queue - post with the relative
         * address
         */
        OPAL_THREAD_ADD32(&mca_btl_sm_component.num_outstanding_frags, +1);
        MCA_BTL_SM_FIFO_WRITE(endpoint, endpoint->my_smp_rank,
                              endpoint->peer_smp_rank, (void *) VIRTUAL2RELATIVE(frag->hdr), false, true, rc);
        return rc;
    }
    *descriptor = mca_btl_sm_alloc( btl, endpoint, order,
                                    payload_size + header_size, flags);
    return OMPI_ERR_RESOURCE_BUSY;
}

/**
 * Initiate a send to the peer.
 *
 * @param btl (IN)      BTL module
 * @param peer (IN)     BTL peer addressing
 */
int mca_btl_sm_send( struct mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* endpoint,
                     struct mca_btl_base_descriptor_t* descriptor,
                     mca_btl_base_tag_t tag )
{
    mca_btl_sm_frag_t* frag = (mca_btl_sm_frag_t*)descriptor;
    int rc;

    if ( mca_btl_sm_component.num_outstanding_frags * 2 > (int) mca_btl_sm_component.fifo_size ) {
        mca_btl_sm_component_progress();
    }

    /* available header space */
    frag->hdr->len = frag->segment.seg_len;
    /* type of message, pt-2-pt, one-sided, etc */
    frag->hdr->tag = tag;

    MCA_BTL_SM_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(frag);

    frag->endpoint = endpoint;

    /*
     * post the descriptor in the queue - post with the relative
     * address
     */
    OPAL_THREAD_ADD32(&mca_btl_sm_component.num_outstanding_frags, +1);
    MCA_BTL_SM_FIFO_WRITE(endpoint, endpoint->my_smp_rank,
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

#if OPAL_ENABLE_FT    == 0
int mca_btl_sm_ft_event(int state) {
    return OMPI_SUCCESS;
}
#else
int mca_btl_sm_ft_event(int state) {
    char * tmp_dir = NULL;

    /* Notify mpool */
    if( NULL != mca_btl_sm_component.sm_mpool &&
        NULL != mca_btl_sm_component.sm_mpool->mpool_ft_event) {
        mca_btl_sm_component.sm_mpool->mpool_ft_event(state);
    }

    if(OPAL_CRS_CHECKPOINT == state) {
        if( NULL != mca_btl_sm_component.mmap_file ) {
            /* On restart we need the old file names to exist (not necessarily
             * contain content) so the CRS component does not fail when searching
             * for these old file handles. The restart procedure will make sure
             * these files get cleaned up appropriately.
             */
            opal_crs_base_metadata_write_token(NULL, CRS_METADATA_TOUCH, mca_btl_sm_component.mmap_file->map_path);

            /* Record the job session directory */
            opal_crs_base_metadata_write_token(NULL, CRS_METADATA_MKDIR, orte_process_info.job_session_dir);
        }
    }
    else if(OPAL_CRS_CONTINUE == state) {
        if( ompi_cr_continue_like_restart ) {
            if( NULL != mca_btl_sm_component.mmap_file ) {
                /* Do not Add session directory on continue */

                /* Add shared memory file */
                opal_crs_base_cleanup_append(mca_btl_sm_component.mmap_file->map_path, false);
            }

            /* Clear this so we force the module to re-init the sm files */
            mca_btl_sm_component.sm_mpool = NULL;
        }
    }
    else if(OPAL_CRS_RESTART == state ||
            OPAL_CRS_RESTART_PRE == state) {
        if( NULL != mca_btl_sm_component.mmap_file ) {
            /* Add session directory */
            opal_crs_base_cleanup_append(orte_process_info.job_session_dir, true);
            tmp_dir = opal_dirname(orte_process_info.job_session_dir);
            if( NULL != tmp_dir ) {
                opal_crs_base_cleanup_append(tmp_dir, true);
                free(tmp_dir);
                tmp_dir = NULL;
            }
            /* Add shared memory file */
            opal_crs_base_cleanup_append(mca_btl_sm_component.mmap_file->map_path, false);
        }

        /* Clear this so we force the module to re-init the sm files */
        mca_btl_sm_component.sm_mpool = NULL;
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return OMPI_SUCCESS;
}
#endif /* OPAL_ENABLE_FT */
