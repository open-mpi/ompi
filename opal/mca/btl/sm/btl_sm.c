/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2011 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2014 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2007 Voltaire. All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2015 Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * Copyright (c) 2010-2012 IBM Corporation.  All rights reserved.
 * Copyright (c) 2012      Oracle and/or its affiliates.  All rights reserved.
 * Copyright (c) 2013      Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif  /* HAVE_FCNTL_H */
#include <errno.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */

#if OPAL_BTL_SM_HAVE_CMA && defined(OPAL_CMA_NEED_SYSCALL_DEFS)
#include "opal/sys/cma.h"
#endif /* OPAL_CMA_NEED_SYSCALL_DEFS */

#include "opal/sys/atomic.h"
#include "opal/class/opal_bitmap.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/util/printf.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/mca/shmem/base/base.h"
#include "opal/mca/shmem/shmem.h"

#include "opal/datatype/opal_convertor.h"
#include "opal/mca/btl/btl.h"
#include "opal/mca/mpool/base/base.h"
#include "opal/mca/mpool/sm/mpool_sm.h"

#include "opal/align.h"
#include "opal/util/sys_limits.h"

#if OPAL_ENABLE_FT_CR    == 1
#include "opal/util/basename.h"
#include "opal/mca/crs/base/base.h"
#include "opal/util/basename.h"
#include "orte/mca/sstore/sstore.h"
#include "opal/runtime/opal_cr.h"
#endif

#include "btl_sm.h"
#include "btl_sm_endpoint.h"
#include "btl_sm_frag.h"
#include "btl_sm_fifo.h"

#include "opal/util/proc.h"

mca_btl_sm_t mca_btl_sm = {
    .super = {
        .btl_component = &mca_btl_sm_component.super,
        .btl_add_procs = mca_btl_sm_add_procs,
        .btl_del_procs = mca_btl_sm_del_procs,
        .btl_finalize = mca_btl_sm_finalize,
        .btl_alloc = mca_btl_sm_alloc,
        .btl_free = mca_btl_sm_free,
        .btl_prepare_src = mca_btl_sm_prepare_src,
        .btl_send = mca_btl_sm_send,
        .btl_sendi = mca_btl_sm_sendi,
        .btl_dump = mca_btl_sm_dump,
        .btl_register_error = mca_btl_sm_register_error_cb, /* register error */
        .btl_ft_event = mca_btl_sm_ft_event
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

    buf = mpool->mpool_alloc(mpool, bsize, opal_cache_line_size, 0, NULL);

    if (NULL == buf)
        return NULL;

    memset(buf, 0, bsize);
    return buf;
}

static int
setup_mpool_base_resources(mca_btl_sm_component_t *comp_ptr,
                           mca_mpool_base_resources_t *out_res)
{
    int rc = OPAL_SUCCESS;
    int fd = -1;
    ssize_t bread = 0;

     /* Wait for the file to be created */
    while (0 != access(comp_ptr->sm_rndv_file_name, R_OK)) {
        opal_progress();
    }

    if (-1 == (fd = open(comp_ptr->sm_mpool_rndv_file_name, O_RDONLY))) {
        int err = errno;
        opal_show_help("help-mpi-btl-sm.txt", "sys call fail", true,
                       "open(2)", strerror(err), err);
        rc = OPAL_ERR_IN_ERRNO;
        goto out;
    }
    if ((ssize_t)sizeof(opal_shmem_ds_t) != (bread =
        read(fd, &out_res->bs_meta_buf, sizeof(opal_shmem_ds_t)))) {
        opal_output(0, "setup_mpool_base_resources: "
                    "Read inconsistency -- read: %lu, but expected: %lu!\n",
                    (unsigned long)bread,
                    (unsigned long)sizeof(opal_shmem_ds_t));
        rc = OPAL_ERROR;
        goto out;
    }
    if ((ssize_t)sizeof(out_res->size) != (bread =
        read(fd, &out_res->size, sizeof(size_t)))) {
        opal_output(0, "setup_mpool_base_resources: "
                    "Read inconsistency -- read: %lu, but expected: %lu!\n",
                    (unsigned long)bread,
                    (unsigned long)sizeof(opal_shmem_ds_t));
        rc = OPAL_ERROR;
        goto out;
    }

out:
    if (-1 != fd) {
        (void)close(fd);
    }
    return rc;
}

static int
sm_segment_attach(mca_btl_sm_component_t *comp_ptr)
{
    int rc = OPAL_SUCCESS;
    int fd = -1;
    ssize_t bread = 0;
    opal_shmem_ds_t *tmp_shmem_ds = calloc(1, sizeof(*tmp_shmem_ds));

    if (NULL == tmp_shmem_ds) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (-1 == (fd = open(comp_ptr->sm_rndv_file_name, O_RDONLY))) {
        int err = errno;
        opal_show_help("help-mpi-btl-sm.txt", "sys call fail", true,
                       "open(2)", strerror(err), err);
        rc = OPAL_ERR_IN_ERRNO;
        goto out;
    }
    if ((ssize_t)sizeof(opal_shmem_ds_t) != (bread =
        read(fd, tmp_shmem_ds, sizeof(opal_shmem_ds_t)))) {
        opal_output(0, "sm_segment_attach: "
                    "Read inconsistency -- read: %lu, but expected: %lu!\n",
                    (unsigned long)bread,
                    (unsigned long)sizeof(opal_shmem_ds_t));
        rc = OPAL_ERROR;
        goto out;
    }
    if (NULL == (comp_ptr->sm_seg =
                 mca_common_sm_module_attach(tmp_shmem_ds,
                                             sizeof(mca_common_sm_seg_header_t),
                                             opal_cache_line_size))) {
        /* don't have to detach here, because module_attach cleans up after
         * itself on failure. */
        opal_output(0, "sm_segment_attach: "
                    "mca_common_sm_module_attach failure!\n");
        rc = OPAL_ERROR;
    }

out:
    if (-1 != fd) {
        (void)close(fd);
    }
    if (tmp_shmem_ds) {
        free(tmp_shmem_ds);
    }
    return rc;
}

static int
sm_btl_first_time_init(mca_btl_sm_t *sm_btl,
                       int32_t my_smp_rank,
                       int n)
{
    size_t length, length_payload;
    sm_fifo_t *my_fifos;
    int my_mem_node, num_mem_nodes, i, rc;
    mca_mpool_base_resources_t *res = NULL;
    mca_btl_sm_component_t* m = &mca_btl_sm_component;

    /* Assume we don't have hwloc support and fill in dummy info */
    mca_btl_sm_component.mem_node = my_mem_node = 0;
    mca_btl_sm_component.num_mem_nodes = num_mem_nodes = 1;

#if OPAL_HAVE_HWLOC
    /* If we have hwloc support, then get accurate information */
    if (NULL != opal_hwloc_topology) {
        i = opal_hwloc_base_get_nbobjs_by_type(opal_hwloc_topology,
                                               HWLOC_OBJ_NODE, 0,
                                               OPAL_HWLOC_AVAILABLE);

        /* If we find >0 NUMA nodes, then investigate further */
        if (i > 0) {
            int numa=0, w;
            unsigned n_bound=0;
            hwloc_cpuset_t avail;
            hwloc_obj_t obj;

            /* JMS This tells me how many numa nodes are *available*,
               but it's not how many are being used *by this job*.
               Note that this is the value we've previously used (from
               the previous carto-based implementation), but it really
               should be improved to be how many NUMA nodes are being
               used *in this job*. */
            mca_btl_sm_component.num_mem_nodes = num_mem_nodes = i;

            /* if we are not bound, then there is nothing further to do */
            if (NULL != opal_process_info.cpuset) {
                /* count the number of NUMA nodes to which we are bound */
                for (w=0; w < i; w++) {
                    if (NULL == (obj = opal_hwloc_base_get_obj_by_type(opal_hwloc_topology,
                                                                       HWLOC_OBJ_NODE, 0, w,
                                                                       OPAL_HWLOC_AVAILABLE))) {
                        continue;
                    }
                    /* get that NUMA node's available cpus */
                    avail = opal_hwloc_base_get_available_cpus(opal_hwloc_topology, obj);
                    /* see if we intersect */
                    if (hwloc_bitmap_intersects(avail, opal_hwloc_my_cpuset)) {
                        n_bound++;
                        numa = w;
                    }
                }
                /* if we are located on more than one NUMA, or we didn't find
                 * a NUMA we are on, then not much we can do
                 */
                if (1 == n_bound) {
                    mca_btl_sm_component.mem_node = my_mem_node = numa;
                } else {
                    mca_btl_sm_component.mem_node = my_mem_node = -1;
                }
            }
        }
    }
#endif

    if (NULL == (res = calloc(1, sizeof(*res)))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* lookup shared memory pool */
    mca_btl_sm_component.sm_mpools =
        (mca_mpool_base_module_t **)calloc(num_mem_nodes,
                                           sizeof(mca_mpool_base_module_t *));

    /* Disable memory binding, because each MPI process will claim pages in the
     * mpool for their local NUMA node */
    res->mem_node = -1;

    if (OPAL_SUCCESS != (rc = setup_mpool_base_resources(m, res))) {
        free(res);
        return rc;
    }
    /* now that res is fully populated, create the thing */
    mca_btl_sm_component.sm_mpools[0] =
        mca_mpool_base_module_create(mca_btl_sm_component.sm_mpool_name,
                                     sm_btl, res);
    /* Sanity check to ensure that we found it */
    if (NULL == mca_btl_sm_component.sm_mpools[0]) {
        free(res);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    mca_btl_sm_component.sm_mpool = mca_btl_sm_component.sm_mpools[0];

    mca_btl_sm_component.sm_mpool_base =
        mca_btl_sm_component.sm_mpools[0]->mpool_base(mca_btl_sm_component.sm_mpools[0]);

    /* create a list of peers */
    mca_btl_sm_component.sm_peers = (struct mca_btl_base_endpoint_t**)
        calloc(n, sizeof(struct mca_btl_base_endpoint_t*));
    if (NULL == mca_btl_sm_component.sm_peers) {
        free(res);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    /* remember that node rank zero is already attached */
    if (0 != my_smp_rank) {
        if (OPAL_SUCCESS != (rc = sm_segment_attach(m))) {
            free(res);
            return rc;
        }
    }

    /* it is now safe to free the mpool resources */
    free(res);

    /* check to make sure number of local procs is within the
     * specified limits */
    if(mca_btl_sm_component.sm_max_procs > 0 &&
       mca_btl_sm_component.num_smp_procs + n >
       mca_btl_sm_component.sm_max_procs) {
        return OPAL_ERROR;
    }

    mca_btl_sm_component.shm_fifo = (volatile sm_fifo_t **)mca_btl_sm_component.sm_seg->module_data_addr;
    mca_btl_sm_component.shm_bases = (char**)(mca_btl_sm_component.shm_fifo + n);
    mca_btl_sm_component.shm_mem_nodes = (uint16_t*)(mca_btl_sm_component.shm_bases + n);

    /* set the base of the shared memory segment */
    mca_btl_sm_component.shm_bases[mca_btl_sm_component.my_smp_rank] =
        (char*)mca_btl_sm_component.sm_mpool_base;
    mca_btl_sm_component.shm_mem_nodes[mca_btl_sm_component.my_smp_rank] =
        (uint16_t)my_mem_node;

    /* initialize the array of fifo's "owned" by this process */
    if(NULL == (my_fifos = (sm_fifo_t*)mpool_calloc(FIFO_MAP_NUM(n), sizeof(sm_fifo_t))))
        return OPAL_ERR_OUT_OF_RESOURCE;

    mca_btl_sm_component.shm_fifo[mca_btl_sm_component.my_smp_rank] = my_fifos;

    /* cache the pointer to the 2d fifo array.  These addresses
     * are valid in the current process space */
    mca_btl_sm_component.fifo = (sm_fifo_t**)malloc(sizeof(sm_fifo_t*) * n);

    if(NULL == mca_btl_sm_component.fifo)
        return OPAL_ERR_OUT_OF_RESOURCE;

    mca_btl_sm_component.fifo[mca_btl_sm_component.my_smp_rank] = my_fifos;

    mca_btl_sm_component.mem_nodes = (uint16_t *) malloc(sizeof(uint16_t) * n);
    if(NULL == mca_btl_sm_component.mem_nodes)
        return OPAL_ERR_OUT_OF_RESOURCE;

    /* initialize fragment descriptor free lists */

    /* allocation will be for the fragment descriptor and payload buffer */
    length = sizeof(mca_btl_sm_frag1_t);
    length_payload =
        sizeof(mca_btl_sm_hdr_t) + mca_btl_sm_component.eager_limit;
    i = opal_free_list_init (&mca_btl_sm_component.sm_frags_eager, length,
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag1_t),
                             length_payload, opal_cache_line_size,
                             mca_btl_sm_component.sm_free_list_num,
                             mca_btl_sm_component.sm_free_list_max,
                             mca_btl_sm_component.sm_free_list_inc,
                             mca_btl_sm_component.sm_mpool, 0, NULL, NULL, NULL);
    if ( OPAL_SUCCESS != i )
        return i;

    length = sizeof(mca_btl_sm_frag2_t);
    length_payload =
        sizeof(mca_btl_sm_hdr_t) + mca_btl_sm_component.max_frag_size;
    i = opal_free_list_init (&mca_btl_sm_component.sm_frags_max, length,
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_frag2_t),
                             length_payload, opal_cache_line_size,
                             mca_btl_sm_component.sm_free_list_num,
                             mca_btl_sm_component.sm_free_list_max,
                             mca_btl_sm_component.sm_free_list_inc,
                             mca_btl_sm_component.sm_mpool, 0, NULL, NULL, NULL);
    if ( OPAL_SUCCESS != i )
        return i;

    i = opal_free_list_init (&mca_btl_sm_component.sm_frags_user,
                             sizeof(mca_btl_sm_user_t),
                             opal_cache_line_size, OBJ_CLASS(mca_btl_sm_user_t),
                             sizeof(mca_btl_sm_hdr_t), opal_cache_line_size,
                             mca_btl_sm_component.sm_free_list_num,
                             mca_btl_sm_component.sm_free_list_max,
                             mca_btl_sm_component.sm_free_list_inc,
                             mca_btl_sm_component.sm_mpool, 0, NULL, NULL, NULL);
    if ( OPAL_SUCCESS != i )
        return i;   

    mca_btl_sm_component.num_outstanding_frags = 0;

    mca_btl_sm_component.num_pending_sends = 0;
    i = opal_free_list_init(&mca_btl_sm_component.pending_send_fl,
                            sizeof(btl_sm_pending_send_item_t), 8,
                            OBJ_CLASS(opal_free_list_item_t),
                            0, 0, 16, -1, 32, NULL, 0, NULL, NULL,
                            NULL);
    if ( OPAL_SUCCESS != i )
        return i;

    /* set flag indicating btl has been inited */
    sm_btl->btl_inited = true;

    return OPAL_SUCCESS;
}

static struct mca_btl_base_endpoint_t *
create_sm_endpoint(int local_proc, struct opal_proc_t *proc)
{
    struct mca_btl_base_endpoint_t *ep;

#if OPAL_ENABLE_PROGRESS_THREADS == 1
    char path[PATH_MAX];
#endif

    ep = (struct mca_btl_base_endpoint_t*)
        malloc(sizeof(struct mca_btl_base_endpoint_t));
    if(NULL == ep)
        return NULL;
    ep->peer_smp_rank = local_proc + mca_btl_sm_component.num_smp_procs;

    OBJ_CONSTRUCT(&ep->pending_sends, opal_list_t);
    OBJ_CONSTRUCT(&ep->endpoint_lock, opal_mutex_t);
#if OPAL_ENABLE_PROGRESS_THREADS == 1
    sprintf(path, "%s"OPAL_PATH_SEP"sm_fifo.%lu",
            opal_process_info.job_session_dir,
            (unsigned long)proc->proc_name);
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

int mca_btl_sm_add_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t **peers,
    opal_bitmap_t* reachability)
{
    int return_code = OPAL_SUCCESS;
    int32_t n_local_procs = 0, proc, j, my_smp_rank = -1;
    const opal_proc_t* my_proc; /* pointer to caller's proc structure */
    mca_btl_sm_t *sm_btl;
    bool have_connected_peer = false;
    char **bases;
    /* for easy access to the mpool_sm_module */
    mca_mpool_sm_module_t *sm_mpool_modp = NULL;

    /* initializion */

    sm_btl = (mca_btl_sm_t *)btl;

    /* get pointer to my proc structure */
    if( NULL == (my_proc = opal_proc_local_get()) )
        return OPAL_ERR_OUT_OF_RESOURCE;

    /* Get unique host identifier for each process in the list,
     * and idetify procs that are on this host.  Add procs on this
     * host to shared memory reachbility list.  Also, get number
     * of local procs in the procs list. */
    for (proc = 0; proc < (int32_t)nprocs; proc++) {
        /* check to see if this proc can be reached via shmem (i.e.,
           if they're on my local host and in my job) */
        if (procs[proc]->proc_name.jobid != my_proc->proc_name.jobid ||
            !OPAL_PROC_ON_LOCAL_NODE(procs[proc]->proc_flags)) {
            peers[proc] = NULL;
            continue;
        }

        /* check to see if this is me */
        if(my_proc == procs[proc]) {
            my_smp_rank = mca_btl_sm_component.my_smp_rank = n_local_procs++;
            continue;
        }

         /* sm doesn't support heterogeneous yet... */
        if (procs[proc]->proc_arch != my_proc->proc_arch) {
            continue;
        }

        /* we have someone to talk to */
        have_connected_peer = true;

        if(!(peers[proc] = create_sm_endpoint(n_local_procs, procs[proc]))) {
            return_code = OPAL_ERROR;
            goto CLEANUP;
        }
        n_local_procs++;

        /* add this proc to shared memory accessibility list */
        return_code = opal_bitmap_set_bit(reachability, proc);
        if(OPAL_SUCCESS != return_code)
            goto CLEANUP;
    }

    /* jump out if there's not someone we can talk to */
    if (!have_connected_peer)
        goto CLEANUP;

    /* make sure that my_smp_rank has been defined */
    if (-1 == my_smp_rank) {
        return_code = OPAL_ERROR;
        goto CLEANUP;
    }

    if (!sm_btl->btl_inited) {
        return_code =
            sm_btl_first_time_init(sm_btl, my_smp_rank,
                                   mca_btl_sm_component.sm_max_procs);
        if (return_code != OPAL_SUCCESS) {
            goto CLEANUP;
        }
    }

    /* set local proc's smp rank in the peers structure for
     * rapid access and calculate reachability */
    for(proc = 0; proc < (int32_t)nprocs; proc++) {
        if(NULL == peers[proc])
            continue;
        mca_btl_sm_component.sm_peers[peers[proc]->peer_smp_rank] = peers[proc];
        peers[proc]->my_smp_rank = my_smp_rank;
    }

    bases = mca_btl_sm_component.shm_bases;
    sm_mpool_modp = (mca_mpool_sm_module_t *)mca_btl_sm_component.sm_mpool;

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
        if(return_code != OPAL_SUCCESS)
            goto CLEANUP;
    }

    opal_atomic_wmb();

    /* Sync with other local procs. Force the FIFO initialization to always
     * happens before the readers access it.
     */
    (void)opal_atomic_add_32(&mca_btl_sm_component.sm_seg->module_seg->seg_inited, 1);
    while( n_local_procs >
           mca_btl_sm_component.sm_seg->module_seg->seg_inited) {
        opal_progress();
        opal_atomic_rmb();
    }

    /* it is now safe to unlink the shared memory segment. only one process
     * needs to do this, so just let smp rank zero take care of it. */
    if (0 == my_smp_rank) {
        if (OPAL_SUCCESS !=
            mca_common_sm_module_unlink(mca_btl_sm_component.sm_seg)) {
            /* it is "okay" if this fails at this point. we have gone this far,
             * so just warn about the failure and continue. this is probably
             * only triggered by a programming error. */
            opal_output(0, "WARNING: common_sm_module_unlink failed.\n");
        }
        /* SKG - another abstraction violation here, but I don't want to add
         * extra code in the sm mpool for further synchronization. */

        /* at this point, all processes have attached to the mpool segment. so
         * it is safe to unlink it here. */
        if (OPAL_SUCCESS !=
            mca_common_sm_module_unlink(sm_mpool_modp->sm_common_module)) {
            opal_output(0, "WARNING: common_sm_module_unlink failed.\n");
        }
        if (-1 == unlink(mca_btl_sm_component.sm_mpool_rndv_file_name)) {
            opal_output(0, "WARNING: %s unlink failed.\n",
                        mca_btl_sm_component.sm_mpool_rndv_file_name);
        }
        if (-1 == unlink(mca_btl_sm_component.sm_rndv_file_name)) {
            opal_output(0, "WARNING: %s unlink failed.\n",
                        mca_btl_sm_component.sm_rndv_file_name);
        }
    }

    /* free up some space used by the name buffers */
    free(mca_btl_sm_component.sm_mpool_ctl_file_name);
    free(mca_btl_sm_component.sm_mpool_rndv_file_name);
    free(mca_btl_sm_component.sm_ctl_file_name);
    free(mca_btl_sm_component.sm_rndv_file_name);

    /* coordinate with other processes */
    for(j = mca_btl_sm_component.num_smp_procs;
        j < mca_btl_sm_component.num_smp_procs + n_local_procs; j++) {
        ptrdiff_t diff;

        /* spin until this element is allocated */
        /* doesn't really wait for that process... FIFO might be allocated, but not initialized */
        opal_atomic_rmb();
        while(NULL == mca_btl_sm_component.shm_fifo[j]) {
            opal_progress();
            opal_atomic_rmb();
        }

        /* Calculate the difference as (my_base - their_base) */
        diff = ADDR2OFFSET(bases[my_smp_rank], bases[j]);

        /* store local address of remote fifos */
        mca_btl_sm_component.fifo[j] =
            (sm_fifo_t*)OFFSET2ADDR(diff, mca_btl_sm_component.shm_fifo[j]);

        /* cache local copy of peer memory node number */
        mca_btl_sm_component.mem_nodes[j] = mca_btl_sm_component.shm_mem_nodes[j];
    }

    /* update the local smp process count */
    mca_btl_sm_component.num_smp_procs += n_local_procs;

    /* make sure we have enough eager fragmnents for each process */
    return_code = opal_free_list_resize_mt (&mca_btl_sm_component.sm_frags_eager,
                                            mca_btl_sm_component.num_smp_procs * 2);
    if (OPAL_SUCCESS != return_code)
        goto CLEANUP;

CLEANUP:
    return return_code;
}

int mca_btl_sm_del_procs(
    struct mca_btl_base_module_t* btl,
    size_t nprocs,
    struct opal_proc_t **procs,
    struct mca_btl_base_endpoint_t **peers)
{
    return OPAL_SUCCESS;
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
    return OPAL_SUCCESS;
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
    return OPAL_SUCCESS;
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
    if(size <= mca_btl_sm_component.eager_limit) {
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag);
    } else if (size <= mca_btl_sm_component.max_frag_size) {
        MCA_BTL_SM_FRAG_ALLOC_MAX(frag);
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
extern int mca_btl_sm_free(
    struct mca_btl_base_module_t* btl,
    mca_btl_base_descriptor_t* des)
{
    mca_btl_sm_frag_t* frag = (mca_btl_sm_frag_t*)des;
    MCA_BTL_SM_FRAG_RETURN(frag);

    return OPAL_SUCCESS;
}


/**
 * Pack data
 *
 * @param btl (IN)      BTL module
 */
struct mca_btl_base_descriptor_t* mca_btl_sm_prepare_src(
    struct mca_btl_base_module_t* btl,
    struct mca_btl_base_endpoint_t* endpoint,
    struct opal_convertor_t* convertor,
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

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
    mca_btl_sm_t* sm_btl = (mca_btl_sm_t*)btl; (void)sm_btl;

    if( (0 != reserve) || ( OPAL_UNLIKELY(!mca_btl_sm_component.use_knem)
                            && OPAL_UNLIKELY(!mca_btl_sm_component.use_cma)) ) {
#endif /* OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA */
        if ( reserve + max_data <= mca_btl_sm_component.eager_limit ) {
            MCA_BTL_SM_FRAG_ALLOC_EAGER(frag);
        } else {
            MCA_BTL_SM_FRAG_ALLOC_MAX(frag);
        }
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }

        if( OPAL_UNLIKELY(reserve + max_data > frag->size) ) {
            max_data = frag->size - reserve;
        }
        iov.iov_len = max_data;
        iov.iov_base =
            (IOVBASE_TYPE*)(((unsigned char*)(frag->segment.base.seg_addr.pval)) + reserve);

        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data );
        if( OPAL_UNLIKELY(rc < 0) ) {
            MCA_BTL_SM_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.base.seg_len = reserve + max_data;
#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
    } else {
#if OPAL_BTL_SM_HAVE_KNEM
        struct knem_cmd_create_region knem_cr;
        struct knem_cmd_param_iovec knem_iov;
#endif /* OPAL_BTL_SM_HAVE_KNEM */
        MCA_BTL_SM_FRAG_ALLOC_USER(frag);
        if( OPAL_UNLIKELY(NULL == frag) ) {
            return NULL;
        }
        iov.iov_len = max_data;
        iov.iov_base = NULL;
        rc = opal_convertor_pack(convertor, &iov, &iov_count, &max_data);
        if( OPAL_UNLIKELY(rc < 0) ) {
            MCA_BTL_SM_FRAG_RETURN(frag);
            return NULL;
        }
        frag->segment.base.seg_addr.lval = (uint64_t)(uintptr_t) iov.iov_base;
        frag->segment.base.seg_len = max_data;

#if OPAL_BTL_SM_HAVE_KNEM
        if (OPAL_LIKELY(mca_btl_sm_component.use_knem)) {
            knem_iov.base = (uintptr_t)iov.iov_base;
            knem_iov.len = max_data;
            knem_cr.iovec_array = (uintptr_t)&knem_iov;
            knem_cr.iovec_nr = iov_count;
            knem_cr.protection = PROT_READ;
            knem_cr.flags = KNEM_FLAG_SINGLEUSE;
            if (OPAL_UNLIKELY(ioctl(sm_btl->knem_fd, KNEM_CMD_CREATE_REGION, &knem_cr) < 0)) {
                return NULL;
            }
            frag->segment.key = knem_cr.cookie;
        }
#endif /* OPAL_BTL_SM_HAVE_KNEM */

#if OPAL_BTL_SM_HAVE_CMA
        if (OPAL_LIKELY(mca_btl_sm_component.use_cma)) {
            /* Encode the pid as the key */
            frag->segment.key = getpid();
        }
#endif /* OPAL_BTL_SM_HAVE_CMA */
    }
#endif /* OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA */

    frag->base.des_segments = &(frag->segment.base);
    frag->base.des_segment_count = 1;
    frag->base.order = MCA_BTL_NO_ORDER;
    frag->base.des_flags = flags;
    *size = max_data;
    return &frag->base;
}

#if 0
#define MCA_BTL_SM_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(sm_frag)          \
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
    mca_btl_sm_frag_t* frag;
    int rc;

    if ( mca_btl_sm_component.num_outstanding_frags * 2 > (int) mca_btl_sm_component.fifo_size ) {
        mca_btl_sm_component_progress();
    }

    /* this check should be unnecessary... turn into an assertion? */
    if( length < mca_btl_sm_component.eager_limit ) {

        /* allocate a fragment, giving up if we can't get one */
        /* note that frag==NULL is equivalent to rc returning an error code */
        MCA_BTL_SM_FRAG_ALLOC_EAGER(frag);
        if( OPAL_UNLIKELY(NULL == frag) ) {
            *descriptor = NULL;
            return OPAL_ERR_OUT_OF_RESOURCE;
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

        MCA_BTL_SM_TOUCH_DATA_TILL_CACHELINE_BOUNDARY(frag);

        /* write the fragment pointer to the FIFO */
        /*
         * Note that we don't care what the FIFO-write return code is.  Even if
         * the return code indicates failure, the write has still "completed" from
         * our point of view:  it has been posted to a "pending send" queue.
         */
        OPAL_THREAD_ADD32(&mca_btl_sm_component.num_outstanding_frags, +1);
        MCA_BTL_SM_FIFO_WRITE(endpoint, endpoint->my_smp_rank,
                              endpoint->peer_smp_rank, (void *) VIRTUAL2RELATIVE(frag->hdr), false, true, rc);
        (void)rc; /* this is safe to ignore as the message is requeued till success */
        return OPAL_SUCCESS;
    }

    if (NULL != descriptor) {
        /* presumably, this code path will never get executed */
        *descriptor = mca_btl_sm_alloc( btl, endpoint, order,
                                        payload_size + header_size, flags);
    }

    return OPAL_ERR_RESOURCE_BUSY;
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
    frag->hdr->len = frag->segment.base.seg_len;
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

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA
mca_btl_base_registration_handle_t *mca_btl_sm_register_mem (struct mca_btl_base_module_t* btl,
                                                             struct mca_btl_base_endpoint_t* endpoint,
                                                             void *base, size_t size, uint32_t flags)
{
    mca_btl_sm_registration_handle_t *handle;
    opal_free_list_item_t *item = NULL;

    item = opal_free_list_get (&mca_btl_sm_component.registration_handles);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    handle = (mca_btl_sm_registration_handle_t *) item;

#if OPAL_BTL_SM_HAVE_KNEM
    if (OPAL_LIKELY(mca_btl_sm_component.use_knem)) {
        struct knem_cmd_create_region knem_cr;
        struct knem_cmd_param_iovec knem_iov;

        knem_iov.base = (uintptr_t)base & ~(opal_getpagesize() - 1);
        knem_iov.len = OPAL_ALIGN(size + ((intptr_t) base - knem_iov.base), opal_getpagesize(), intptr_t);
        knem_cr.iovec_array = (uintptr_t)&knem_iov;
        knem_cr.iovec_nr = 1;
        knem_cr.flags = 0;
        knem_cr.protection = 0;

        if (flags & MCA_BTL_REG_FLAG_REMOTE_READ) {
            knem_cr.protection |= PROT_READ;
        }
        if (flags & MCA_BTL_REG_FLAG_REMOTE_WRITE) {
            knem_cr.protection |= PROT_WRITE;
        }

       if (OPAL_UNLIKELY(ioctl(((mca_btl_sm_t*)btl)->knem_fd, KNEM_CMD_CREATE_REGION, &knem_cr) < 0)) {
           opal_free_list_return (&mca_btl_sm_component.registration_handles, item);
           return NULL;
        }

        handle->btl_handle.data.knem.cookie = knem_cr.cookie;
        handle->btl_handle.data.knem.base_addr = knem_iov.base;
    } else
#endif
    {
        /* the pid could be included in a modex but this will work until btl/sm is
         * deleted */
        handle->btl_handle.data.pid = getpid ();
    }

    /* return the public part of the handle */
    return &handle->btl_handle;
}

int mca_btl_sm_deregister_mem (struct mca_btl_base_module_t* btl, mca_btl_base_registration_handle_t *handle)
{
    mca_btl_sm_registration_handle_t *sm_handle =
        (mca_btl_sm_registration_handle_t *)((intptr_t) handle - offsetof (mca_btl_sm_registration_handle_t, btl_handle));

#if OPAL_BTL_SM_HAVE_KNEM
    if (OPAL_LIKELY(mca_btl_sm_component.use_knem)) {
        (void) ioctl(((mca_btl_sm_t*)btl)->knem_fd, KNEM_CMD_DESTROY_REGION, &handle->data.knem.cookie);
    }
#endif

    opal_free_list_return (&mca_btl_sm_component.registration_handles, &sm_handle->super);

    return OPAL_SUCCESS;
}
#endif /* OPAL_BTL_SM_HAVE_KNEM */

#if OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA

/**
 * Initiate an synchronous get.
 */
int mca_btl_sm_get_sync (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                         uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                         mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                         int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
#if OPAL_BTL_SM_HAVE_KNEM
    mca_btl_sm_t* sm_btl = (mca_btl_sm_t*) btl;
    if (OPAL_LIKELY(mca_btl_sm_component.use_knem)) {
        struct knem_cmd_inline_copy icopy;
        struct knem_cmd_param_iovec recv_iovec;
    
        /* Fill in the ioctl data fields.  There's no async completion, so
           we don't need to worry about getting a slot, etc. */
        recv_iovec.base = (uintptr_t) local_address;
        recv_iovec.len =  size;
        icopy.local_iovec_array = (uintptr_t)&recv_iovec;
        icopy.local_iovec_nr = 1;
        icopy.remote_cookie = remote_handle->data.knem.cookie;
        icopy.remote_offset = remote_address - remote_handle->data.knem.base_addr;
        icopy.write = 0;

        /* Use the DMA flag if knem supports it *and* the segment length
           is greater than the cutoff.  Note that if the knem_dma_min
           value is 0 (i.e., the MCA param was set to 0), the segment size
           will never be larger than it, so DMA will never be used. */
        icopy.flags = 0;
        if (mca_btl_sm_component.knem_dma_min <= size) {
            icopy.flags = mca_btl_sm_component.knem_dma_flag;
        }
        /* synchronous flags only, no need to specify icopy.async_status_index */

        /* When the ioctl returns, the transfer is done and we can invoke
           the btl callback and return the frag */
        if (OPAL_UNLIKELY(0 != ioctl(sm_btl->knem_fd,
                                     KNEM_CMD_INLINE_COPY, &icopy))) {
            return OPAL_ERROR;
        }

        /* FIXME: what if icopy.current_status == KNEM_STATUS_FAILED? */
    }
#endif /* OPAL_BTL_SM_HAVE_KNEM */

#if OPAL_BTL_SM_HAVE_CMA
    if (OPAL_LIKELY(mca_btl_sm_component.use_cma)) {
        struct iovec local, remote;
        pid_t remote_pid;
        ssize_t val;

        remote_pid = remote_handle->data.pid;
        remote.iov_base = (void *) (intptr_t) remote_address;
        remote.iov_len = size;
        local.iov_base = local_address;
        local.iov_len = size;

        val = process_vm_readv(remote_pid, &local, 1, &remote, 1, 0);

        if (val != (ssize_t)size) {
            if (val < 0) {
                opal_output(0, "mca_btl_sm_get_sync: process_vm_readv failed: %i",
                            errno);
            } else {
                /* Should never get a short read from process_vm_readv */
                opal_output(0, "mca_btl_sm_get_sync: process_vm_readv short read: %i",
                            (int)val);
            }
            return OPAL_ERROR;
        }
    }
#endif /* OPAL_BTL_SM_HAVE_CMA */

    cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

    return OPAL_SUCCESS;
}

#endif /* OPAL_BTL_SM_HAVE_KNEM || OPAL_BTL_SM_HAVE_CMA */

#if OPAL_BTL_SM_HAVE_KNEM
/* No support async_get for CMA yet */

/**
 * Initiate an asynchronous get.
 */
int mca_btl_sm_get_async (mca_btl_base_module_t *btl, struct mca_btl_base_endpoint_t *endpoint, void *local_address,
                          uint64_t remote_address, mca_btl_base_registration_handle_t *local_handle,
                          mca_btl_base_registration_handle_t *remote_handle, size_t size, int flags,
                          int order, mca_btl_base_rdma_completion_fn_t cbfunc, void *cbcontext, void *cbdata)
{
    mca_btl_sm_t* sm_btl = (mca_btl_sm_t*) btl;
    mca_btl_sm_frag_t* frag;
    struct knem_cmd_inline_copy icopy;
    struct knem_cmd_param_iovec recv_iovec;
    
    /* If we have no knem slots available, fall back to synchronous */
    if (sm_btl->knem_status_num_used >=
        mca_btl_sm_component.knem_max_simultaneous) {
        return mca_btl_sm_get_sync (btl, endpoint, local_address, remote_address, local_handle,
                                    remote_handle, size, flags, order, cbfunc, cbcontext, cbdata);
    }

    /* allocate a fragment to keep track of this transaction */
    MCA_BTL_SM_FRAG_ALLOC_USER(frag);
    if (OPAL_UNLIKELY(NULL == frag)) {
        return mca_btl_sm_get_sync (btl, endpoint, local_address, remote_address, local_handle,
                                    remote_handle, size, flags, order, cbfunc, cbcontext, cbdata);
    }

    /* fill in callback data */
    frag->cb.func = cbfunc;
    frag->cb.context = cbcontext;
    frag->cb.data = cbdata;
    frag->cb.local_address = local_address;
    frag->cb.local_handle = local_handle;

    /* We have a slot, so fill in the data fields.  Bump the
       first_avail and num_used counters. */
    recv_iovec.base = (uintptr_t) local_address;
    recv_iovec.len =  size;
    icopy.local_iovec_array = (uintptr_t)&recv_iovec;
    icopy.local_iovec_nr = 1;
    icopy.write = 0;
    icopy.async_status_index = sm_btl->knem_status_first_avail++;
    if (sm_btl->knem_status_first_avail >= 
        mca_btl_sm_component.knem_max_simultaneous) {
        sm_btl->knem_status_first_avail = 0;
    }
    ++sm_btl->knem_status_num_used;
    icopy.remote_cookie = remote_handle->data.knem.cookie;
    icopy.remote_offset = remote_address - remote_handle->data.knem.base_addr;

    /* Use the DMA flag if knem supports it *and* the segment length
       is greater than the cutoff */
    icopy.flags = KNEM_FLAG_ASYNCDMACOMPLETE;
    if (mca_btl_sm_component.knem_dma_min <= size) {
        icopy.flags = mca_btl_sm_component.knem_dma_flag;
    }

    sm_btl->knem_frag_array[icopy.async_status_index] = frag;
    if (OPAL_LIKELY(0 == ioctl(sm_btl->knem_fd, 
                               KNEM_CMD_INLINE_COPY, &icopy))) {
        if (icopy.current_status != KNEM_STATUS_PENDING) {
            MCA_BTL_SM_FRAG_RETURN(frag);
            /* request completed synchronously */

            /* FIXME: what if icopy.current_status == KNEM_STATUS_FAILED? */
            cbfunc (btl, endpoint, local_address, local_handle, cbcontext, cbdata, OPAL_SUCCESS);

            --sm_btl->knem_status_num_used;
            ++sm_btl->knem_status_first_used;
            if (sm_btl->knem_status_first_used >=
                mca_btl_sm_component.knem_max_simultaneous) {
                sm_btl->knem_status_first_used = 0;
            }
        }
        return OPAL_SUCCESS;
    } else {
        return OPAL_ERROR;
    }
}
#endif /* OPAL_BTL_SM_HAVE_KNEM */

/**
 *
 */
void mca_btl_sm_dump(struct mca_btl_base_module_t* btl,
                     struct mca_btl_base_endpoint_t* endpoint,
                     int verbose)
{
    opal_list_item_t *item;
    mca_btl_sm_frag_t* frag;

    if( NULL != endpoint ) {
        mca_btl_base_err("BTL SM %p endpoint %p [smp_rank %d] [peer_rank %d]\n",
                         (void*) btl, (void*) endpoint, 
                         endpoint->my_smp_rank, endpoint->peer_smp_rank);
        for(item =  opal_list_get_first(&endpoint->pending_sends);
            item != opal_list_get_end(&endpoint->pending_sends); 
            item = opal_list_get_next(item)) {
            frag = (mca_btl_sm_frag_t*)item;
            mca_btl_base_err(" |  frag %p size %lu (hdr frag %p len %lu rank %d tag %d)\n",
                             (void*) frag, frag->size, (void*) frag->hdr->frag,
                             frag->hdr->len, frag->hdr->my_smp_rank, 
                             frag->hdr->tag);
        }
    }
}

#if OPAL_ENABLE_FT_CR    == 0
int mca_btl_sm_ft_event(int state) {
    return OPAL_SUCCESS;
}
#else
int mca_btl_sm_ft_event(int state) {
    /* Notify mpool */
    if( NULL != mca_btl_sm_component.sm_mpool &&
        NULL != mca_btl_sm_component.sm_mpool->mpool_ft_event) {
        mca_btl_sm_component.sm_mpool->mpool_ft_event(state);
    }

    if(OPAL_CRS_CHECKPOINT == state) {
        if( NULL != mca_btl_sm_component.sm_seg ) {
            /* On restart we need the old file names to exist (not necessarily
             * contain content) so the CRS component does not fail when searching
             * for these old file handles. The restart procedure will make sure
             * these files get cleaned up appropriately.
             */
            /* Disabled to get FT code compiled again
             * TODO: FIXIT soon
            orte_sstore.set_attr(orte_sstore_handle_current,
                                 SSTORE_METADATA_LOCAL_TOUCH,
                                 mca_btl_sm_component.sm_seg->shmem_ds.seg_name);
             */
        }
    }
    else if(OPAL_CRS_CONTINUE == state) {
        if (opal_cr_continue_like_restart) {
            if( NULL != mca_btl_sm_component.sm_seg ) {
                /* Add shared memory file */
                opal_crs_base_cleanup_append(mca_btl_sm_component.sm_seg->shmem_ds.seg_name, false);
            }

            /* Clear this so we force the module to re-init the sm files */
            mca_btl_sm_component.sm_mpool = NULL;
        }
    }
    else if(OPAL_CRS_RESTART == state ||
            OPAL_CRS_RESTART_PRE == state) {
        if( NULL != mca_btl_sm_component.sm_seg ) {
            /* Add shared memory file */
            opal_crs_base_cleanup_append(mca_btl_sm_component.sm_seg->shmem_ds.seg_name, false);
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

    return OPAL_SUCCESS;
}
#endif /* OPAL_ENABLE_FT_CR */
