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
/**
 * @file
 *
 * Warning: this is not for the faint of heart -- don't even bother
 * reading this source code if you don't have a strong understanding
 * of nested data structures and pointer math (remember that
 * associativity and order of C operations is *critical* in terms of
 * pointer math!).
 */

#include "ompi_config.h"

#include <stdio.h>
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#include <sys/types.h>
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif  /* HAVE_SYS_MMAN_H */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */

#include "mpi.h"
#include "opal/mca/maffinity/maffinity.h"
#include "opal/mca/maffinity/base/base.h"
#include "opal/util/os_path.h"
#include "orte/mca/ns/ns.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "ompi/proc/proc.h"
#include "coll_sm.h"


/*
 * Global variables
 */
uint32_t mca_coll_sm_iov_size = 1;


/*
 * Local functions
 */
static const struct mca_coll_base_module_1_0_0_t *
    sm_module_init(struct ompi_communicator_t *comm);
static int sm_module_finalize(struct ompi_communicator_t *comm);
static bool have_local_peers(ompi_proc_t **procs, size_t size);
static int bootstrap_init(void);
static int bootstrap_comm(ompi_communicator_t *comm);


/*
 * Local variables
 */
static bool bootstrap_inited = false;


/*
 * Linear set of collective algorithms
 */
static const mca_coll_base_module_1_0_0_t module = {

    /* Initialization / finalization functions */

    sm_module_init,
    sm_module_finalize,

    /* Collective function pointers */

    NULL,
    NULL,
    mca_coll_sm_allreduce_intra,
    NULL,
    NULL,
    NULL,
    mca_coll_sm_barrier_intra,
    mca_coll_sm_bcast_intra,
    NULL,
    NULL,
    NULL,
    mca_coll_sm_reduce_intra,
    NULL,
    NULL,
    NULL,
    NULL
};


/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.  This function is invoked exactly
 * once.
 */
int mca_coll_sm_init_query(bool enable_progress_threads,
                           bool enable_mpi_threads)
{
#if 0
    /* JMS: Arrgh.  Unfortunately, we don't have this information by
       the time this is invoked -- the GPR compound command doesn't
       fire until after coll_base_open() in ompi_mpi_init(). */

    ompi_proc_t **procs;
    size_t size;

    /* Check to see if anyone is local on this machine.  If not, don't
       bother doing anything else. */

    procs = ompi_proc_all(&size);
    if (NULL == procs || 0 == size) {
        return OMPI_ERROR;
    }
    if (!have_local_peers(procs, size)) {
        return OMPI_ERROR;
    }
    free(procs);
#endif

    /* Don't do much here because we don't really want to allocate any
       shared memory until this component is selected to be used. */

    mca_coll_sm_component.sm_data_mpool_created = false;
    mca_coll_sm_component.sm_component_setup = false;
    opal_atomic_init(&mca_coll_sm_component.sm_component_setup_lock, 0);

    /* Alles gut */

    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
const mca_coll_base_module_1_0_0_t *
mca_coll_sm_comm_query(struct ompi_communicator_t *comm, int *priority,
                       struct mca_coll_base_comm_t **data)
{
    /* See if someone has previously lazily initialized and failed */

    if (mca_coll_sm_component.sm_component_setup &&
        !mca_coll_sm_component.sm_component_setup_success) {
        return NULL;
    }
    
    /* If we're intercomm, or if there's only one process in the
       communicator, or if not all the processes in the communicator
       are not on this node, then we don't want to run */

    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm) ||
        !have_local_peers(comm->c_local_group->grp_proc_pointers,
                          ompi_comm_size(comm))) {
	return NULL;
    }

    /* If the number of processes in this communicator is larger than
       (mca_coll_sm_component.sm_control_size / sizeof(uint32_t)),
       then we can't handle it. */

    if (((unsigned) ompi_comm_size(comm)) > 
        mca_coll_sm_component.sm_control_size / sizeof(uint32_t)) {
        return NULL;
    }

    /* Get our priority */

    *priority = mca_coll_sm_component.sm_priority;
    
    /* All is good -- return a module */

    return &module;
}


/* 
 * Unquery the coll on comm
 */
int mca_coll_sm_comm_unquery(struct ompi_communicator_t *comm,
                             struct mca_coll_base_comm_t *data)
{
    return OMPI_SUCCESS;
}


/*
 * Init module on the communicator
 */
static const struct mca_coll_base_module_1_0_0_t *
sm_module_init(struct ompi_communicator_t *comm)
{
    int i, j, root;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    mca_coll_base_comm_t *data;
    size_t control_size, frag_size;
    mca_coll_sm_component_t *c = &mca_coll_sm_component;
    opal_maffinity_base_segment_t *maffinity;
    int parent, min_child, max_child, num_children;
    char *base;
    const int num_barrier_buffers = 2;

    /* Once-per-component setup.  This may happen at any time --
       during MPI_INIT or later.  So we must protect this with locks
       to ensure that only one thread in the process actually does
       this setup. */

    opal_atomic_lock(&mca_coll_sm_component.sm_component_setup_lock);
    if (!mca_coll_sm_component.sm_component_setup) {
        mca_coll_sm_component.sm_component_setup = true;

        if (OMPI_SUCCESS != bootstrap_init()) {
            mca_coll_sm_component.sm_component_setup_success = false;
            opal_atomic_unlock(&mca_coll_sm_component.sm_component_setup_lock);
            return NULL;
        }

        /* Can we get an mpool allocation?  See if there was one created
           already.  If not, try to make one. */
        
        mca_coll_sm_component.sm_data_mpool = 
            mca_mpool_base_module_lookup(mca_coll_sm_component.sm_mpool_name);
        if (NULL == mca_coll_sm_component.sm_data_mpool) {
            mca_coll_sm_component.sm_data_mpool = 
                mca_mpool_base_module_create(mca_coll_sm_component.sm_mpool_name,
                                             NULL, NULL);
            if (NULL == mca_coll_sm_component.sm_data_mpool) {
                mca_coll_sm_bootstrap_finalize();
                mca_coll_sm_component.sm_component_setup_success = false;
                opal_atomic_unlock(&mca_coll_sm_component.sm_component_setup_lock);
                return NULL;
            }
            mca_coll_sm_component.sm_data_mpool_created = true;
        } else {
            mca_coll_sm_component.sm_data_mpool_created = false;
        }
        mca_coll_sm_component.sm_component_setup_success = true;
    }
    opal_atomic_unlock(&mca_coll_sm_component.sm_component_setup_lock);

    /* Double check to see if some interleaved lazy init failed before
       we got in here */

    if (!mca_coll_sm_component.sm_component_setup_success) {
        return NULL;
    }

    /* Get some space to setup memory affinity (just easier to try to
       alloc here to handle the error case) */

    maffinity = (opal_maffinity_base_segment_t*)malloc(sizeof(opal_maffinity_base_segment_t) * 
                                                       c->sm_comm_num_segments * 3);
    if (NULL == maffinity) {
        return NULL;
    }

    /* Allocate data to hang off the communicator.  The memory we
       alloc will be laid out as follows:

       1. mca_coll_base_comm_t
       2. array of num_segments mca_coll_base_mpool_index_t instances
          (pointed to by the array in 2)
       3. array of ompi_comm_size(comm) mca_coll_sm_tree_node_t
          instances
       4. array of sm_tree_degree pointers to other tree nodes (i.e.,
          this nodes' children) for each instance of
          mca_coll_sm_tree_node_t
    */

    comm->c_coll_selected_data = data = (mca_coll_base_comm_t*)
        malloc(sizeof(mca_coll_base_comm_t) + 
               (c->sm_comm_num_segments * 
                sizeof(mca_coll_base_mpool_index_t)) +
               (size * 
                (sizeof(mca_coll_sm_tree_node_t) +
                 (sizeof(mca_coll_sm_tree_node_t*) * c->sm_tree_degree))));

    if (NULL == data) {
        return NULL;
    }
    data->mcb_data_mpool_malloc_addr = NULL;

    /* Setup #2: set the array to point immediately beyond the
       mca_coll_base_comm_t */
    data->mcb_mpool_index = (mca_coll_base_mpool_index_t*) (data + 1);
    /* Setup array of pointers for #3 */
    data->mcb_tree = (mca_coll_sm_tree_node_t*)
        (data->mcb_mpool_index + c->sm_comm_num_segments);
    /* Finally, setup the array of children pointers in the instances
       in #5 to point to their corresponding arrays in #6 */
    data->mcb_tree[0].mcstn_children = (mca_coll_sm_tree_node_t**)
        (data->mcb_tree + size);
    for (i = 1; i < size; ++i) {
        data->mcb_tree[i].mcstn_children = 
            data->mcb_tree[i - 1].mcstn_children + c->sm_tree_degree;
    }

    /* Pre-compute a tree for a given number of processes and degree.
       We'll re-use this tree for all possible values of root (i.e.,
       shift everyone's process to be the "0"/root in this tree. */
    for (root = 0; root < size; ++root) {
        parent = (root - 1) / mca_coll_sm_component.sm_tree_degree;
        num_children = mca_coll_sm_component.sm_tree_degree;
    
        /* Do we have children?  If so, how many? */
        
        if ((root * num_children) + 1 >= size) {
            /* Leaves */
            min_child = -1;
            max_child = -1;
            num_children = 0;
        } else {
            /* Interior nodes */
            min_child = root * num_children + 1;
            max_child = root * num_children + num_children;
            if (max_child >= size) {
                max_child = size - 1;
            }
            num_children = max_child - min_child + 1;
        }

        /* Save the values */
        data->mcb_tree[root].mcstn_id = root;
        if (root == 0 && parent == 0) {
            data->mcb_tree[root].mcstn_parent = NULL;
        } else {
            data->mcb_tree[root].mcstn_parent = &data->mcb_tree[parent];
        }
        data->mcb_tree[root].mcstn_num_children = num_children;
        for (i = 0; i < c->sm_tree_degree; ++i) {
            data->mcb_tree[root].mcstn_children[i] = 
                (i < num_children) ?
                &data->mcb_tree[min_child + i] : NULL;
        }
    }

    /* Bootstrap this communicator; find the shared memory in the main
       mpool that has been allocated among my peers for this
       communicator. */

    if (OMPI_SUCCESS != bootstrap_comm(comm)) {
        free(data);
        comm->c_coll_selected_data = NULL;
        return NULL;
    }

    /* Once the communicator is bootstrapped, setup the pointers into
       the data mpool area.  First, setup the barrier buffers.  There
       are 2 sets of barrier buffers (because there can never be more
       than one outstanding barrier occuring at any timie).  Setup
       pointers to my control buffers, my parents, and [the beginning
       of] my children (note that the children are contiguous, so
       having the first pointer and the num_children from the mcb_tree
       data is sufficient). */

    control_size = c->sm_control_size;
    base = (char*) (data->mcb_mpool_base + data->mcb_mpool_offset);
    data->mcb_barrier_control_me = (uint32_t*)
        (base + (rank * control_size * num_barrier_buffers * 2));
    if (data->mcb_tree[rank].mcstn_parent) {
        data->mcb_barrier_control_parent = (uint32_t*)
            (base +
             (data->mcb_tree[rank].mcstn_parent->mcstn_id * control_size * 
              num_barrier_buffers * 2));
    } else {
        data->mcb_barrier_control_parent = NULL;
    }
    if (data->mcb_tree[rank].mcstn_num_children > 0) {
        data->mcb_barrier_control_children = (uint32_t*)
            (base +
             (data->mcb_tree[rank].mcstn_children[0]->mcstn_id * control_size *
              num_barrier_buffers * 2));
    } else {
        data->mcb_barrier_control_children = NULL;
    }
    data->mcb_barrier_count = 0;

    /* Next, setup the pointer to the in-use flags.  The number of
       segments will be an even multiple of the number of in-use
       flags. */

    base += (c->sm_control_size * size * num_barrier_buffers * 2);
    data->mcb_in_use_flags = (mca_coll_sm_in_use_flag_t*) base;

    /* All things being equal, if we're rank 0, then make the in-use
       flags be local (memory affinity).  Then zero them all out so
       that they're marked as unused. */
    
    j = 0;
    if (0 == rank) {
        maffinity[j].mbs_start_addr = base;
        maffinity[j].mbs_len = c->sm_control_size * 
            c->sm_comm_num_in_use_flags;
        /* Set the op counts to 1 (actually any nonzero value will do)
           so that the first time children/leaf processes come
           through, they don't see a value of 0 and think that the
           root/parent has already set the count to their op number
           (i.e., 0 is the first op count value). */
        for (i = 0; i < mca_coll_sm_component.sm_comm_num_in_use_flags; ++i) {
            ((mca_coll_sm_in_use_flag_t *)base)[i].mcsiuf_operation_count = 1;
            ((mca_coll_sm_in_use_flag_t *)base)[i].mcsiuf_num_procs_using = 0;
        }
        D(("rank 0 zeroed in-use flags (num %d, len %d): %p - %p\n",
           c->sm_comm_num_in_use_flags,
           maffinity[j].mbs_len,
           base, base + maffinity[j].mbs_len));
        ++j;
    }

    /* Next, setup pointers to the control and data portions of the
       segments, as well as to the relevant in-use flags. */

    base += (c->sm_comm_num_in_use_flags * c->sm_control_size);
    control_size = size * c->sm_control_size;
    frag_size = size * c->sm_fragment_size;
    for (i = 0; i < c->sm_comm_num_segments; ++i) {
        data->mcb_mpool_index[i].mcbmi_control = (uint32_t*)
            (base + (i * (control_size + frag_size)));
        data->mcb_mpool_index[i].mcbmi_data = 
            (((char*) data->mcb_mpool_index[i].mcbmi_control) + 
             control_size);

        /* Memory affinity: control */

        maffinity[j].mbs_len = c->sm_control_size;
        maffinity[j].mbs_start_addr = (void *)
            (data->mcb_mpool_index[i].mcbmi_control +
             (rank * c->sm_control_size));
        ++j;

        /* Memory affinity: data */

        maffinity[j].mbs_len = c->sm_fragment_size;
        maffinity[j].mbs_start_addr = 
            data->mcb_mpool_index[i].mcbmi_data +
            (rank * c->sm_control_size);
        ++j;
    }

    /* Setup memory affinity so that the pages that belong to this
       process are local to this process */

    opal_maffinity_base_set(maffinity, j);
    free(maffinity);

    /* Zero out the control structures that belong to this process */

    memset(data->mcb_barrier_control_me, 0, 
           num_barrier_buffers * 2 * c->sm_control_size);
    for (i = 0; i < c->sm_comm_num_segments; ++i) {
        memset((void *) data->mcb_mpool_index[i].mcbmi_control, 0,
               c->sm_control_size);
    }

    /* All done */

    return &module;
}


/*
 * Finalize module on the communicator
 */
static int sm_module_finalize(struct ompi_communicator_t *comm)
{
    mca_coll_base_comm_t *data;

    /* Free the space in the data mpool and the data hanging off the
       communicator */

    data = comm->c_coll_selected_data;
    if (NULL != data) {
        /* If this was the process that allocated the space in the
           data mpool, then this is the process that frees it */

        if (NULL != data->mcb_data_mpool_malloc_addr) {
            mca_coll_sm_component.sm_data_mpool->mpool_free(mca_coll_sm_component.sm_data_mpool,
                                                       data->mcb_data_mpool_malloc_addr, NULL);
        }

        /* Now free the data hanging off the communicator */

        free(data);
    }

    return OMPI_SUCCESS;
}


static bool have_local_peers(ompi_proc_t **procs, size_t size)
{
    size_t i;

    for (i = 0; i < size; ++i) {
        if (0 == (procs[i]->proc_flags & OMPI_PROC_FLAG_LOCAL)) {
            return false;
        }
    }

    return true;
}


static int bootstrap_init(void)
{
    int i;
    size_t size;
    char *fullpath;
    mca_common_sm_mmap_t *meta;
    mca_coll_sm_bootstrap_header_extension_t *bshe;

    /* Create/open the sm coll bootstrap mmap.  Make it have enough
       space for the top-level control structure and
       sm_bootstrap_num_segments per-communicator setup struct's
       (i.e., enough for sm_bootstrap_num_segments communicators to
       simultaneously set themselves up)  */

    if (NULL == mca_coll_sm_component.sm_bootstrap_filename) {
        return OMPI_ERROR;
    }
    orte_proc_info();
    fullpath = opal_os_path( false, orte_process_info.job_session_dir,
                             mca_coll_sm_component.sm_bootstrap_filename, NULL );
    if (NULL == fullpath) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    size = 
        sizeof(mca_coll_sm_bootstrap_header_extension_t) +
        (mca_coll_sm_component.sm_bootstrap_num_segments *
         sizeof(mca_coll_sm_bootstrap_comm_setup_t)) +
        (sizeof(mca_coll_sm_bootstrap_comm_key_t) *
         mca_coll_sm_component.sm_bootstrap_num_segments);

    mca_coll_sm_component.sm_bootstrap_meta = meta =
        mca_common_sm_mmap_init(size, fullpath,
                                sizeof(mca_coll_sm_bootstrap_header_extension_t),
                                8);
    if (NULL == meta) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    free(fullpath);

    /* set the pointer to the bootstrap control structure */
    bshe = (mca_coll_sm_bootstrap_header_extension_t *) meta->map_seg;

    /* Lock the bootstrap control structure.  If it's not already
       initialized, then we're the first one in and we setup the data
       structures */

    opal_atomic_lock(&bshe->super.seg_lock);
    opal_atomic_wmb();
    if (!bshe->super.seg_inited) {
        bshe->smbhe_segments = (mca_coll_sm_bootstrap_comm_setup_t *)
            (((char *) bshe) + 
             sizeof(mca_coll_sm_bootstrap_header_extension_t) +
             (sizeof(uint32_t) * 
              mca_coll_sm_component.sm_bootstrap_num_segments));
        bshe->smbhe_keys = (mca_coll_sm_bootstrap_comm_key_t *)
            (((char *) bshe) + sizeof(*bshe));
        for (i = 0; i < mca_coll_sm_component.sm_bootstrap_num_segments; ++i) {
            bshe->smbhe_keys[i].mcsbck_cid = INT_MAX;
            memset(&bshe->smbhe_keys[i].mcsbck_rank0_name, 0,
                   sizeof(orte_process_name_t));
        }

        bshe->super.seg_inited = true;
    }
    opal_atomic_unlock(&bshe->super.seg_lock);

    /* All done */

    bootstrap_inited = true;
    return OMPI_SUCCESS;
}


static int bootstrap_comm(ompi_communicator_t *comm)
{
    int i, empty_index, err;
    bool found;
    mca_coll_sm_component_t *c = &mca_coll_sm_component;
    mca_coll_sm_bootstrap_header_extension_t *bshe;
    mca_coll_sm_bootstrap_comm_setup_t *bscs;
    mca_coll_base_comm_t *data = comm->c_coll_selected_data;
    int comm_size = ompi_comm_size(comm);
    int num_segments = c->sm_comm_num_segments;
    int num_in_use = c->sm_comm_num_in_use_flags;
    int frag_size = c->sm_fragment_size;
    int control_size = c->sm_control_size;
    orte_process_name_t *rank0;

    /* Is our CID in the CIDs array?  If not, loop until we can find
       an open slot in the array to use in the bootstrap to setup our
       communicator. */

    bshe = (mca_coll_sm_bootstrap_header_extension_t *) 
        c->sm_bootstrap_meta->map_seg;
    bscs = bshe->smbhe_segments;
    opal_atomic_lock(&bshe->super.seg_lock);
    rank0 = &(comm->c_local_group->grp_proc_pointers[0]->proc_name);
    while (1) {
        opal_atomic_wmb();
        found = false;
        empty_index = -1;
        for (i = 0; i < mca_coll_sm_component.sm_bootstrap_num_segments; ++i) {
            if (comm->c_contextid == bshe->smbhe_keys[i].mcsbck_cid &&
                ORTE_EQUAL == orte_ns.compare_fields(ORTE_NS_CMP_ALL,
                                                     rank0,
                                                     &bshe->smbhe_keys[i].mcsbck_rank0_name)) {
                found = true;
                break;
            } else if (INT_MAX == bshe->smbhe_keys[i].mcsbck_cid &&
                       -1 == empty_index) {
                empty_index = i;
            }
        }

        /* Did we find our CID? */

        if (found) {
            break;
        }

        /* Nope.  Did we find an empty slot?  If so, initialize that
           slot and its corresponding segment for our CID.  Get an
           mpool allocation big enough to handle all the shared memory
           collective stuff. */

        else if (-1 != empty_index) {
            char *tmp;
            size_t size;

            i = empty_index;
            bshe->smbhe_keys[i].mcsbck_cid = comm->c_contextid;
            /* JMS better assignment? */
            bshe->smbhe_keys[i].mcsbck_rank0_name = *rank0;

            bscs[i].smbcs_count = comm_size;

            /* Calculate how much space we need in the data mpool.
               There are several values to add:

               - size of the barrier data (2 of these):
                   - fan-in data (num_procs * control_size)
                   - fan-out data (num_procs * control_size)
               - size of the "in use" buffers:
                   - num_in_use_buffers * control_size
               - size of the message fragment area (one for each segment):
                   - control (num_procs * control_size)
                   - fragment data (num_procs * (frag_size))

               So it's:

               barrier: 2 * control_size + 2 * control_size
               in use:  num_in_use * control_size
               control: num_segments * (num_procs * control_size * 2 +
                                        num_procs * control_size)
               message: num_segments * (num_procs * frag_size)
            */

            size = 4 * control_size +
                (num_in_use * control_size) +
                (num_segments * (comm_size * control_size * 2)) +
                (num_segments * (comm_size * frag_size));

            data->mcb_data_mpool_malloc_addr = tmp =
                (char*)c->sm_data_mpool->mpool_alloc(c->sm_data_mpool, size, 
                                                     c->sm_control_size, 0, NULL);
            if (NULL == tmp) {
                /* Cleanup before returning; allow other processes in
                   this communicator to learn of the failure.  Note
                   that by definition, bscs[i].smbcs_count won't be
                   zero after the decrement (because there must be >=2
                   processes in this communicator, or the self coll
                   component would have been chosen), so we don't need
                   to do that cleanup. */
                bscs[i].smbcs_data_mpool_offset = 0;
                bscs[i].smbcs_success = false;
                --bscs[i].smbcs_count;
                opal_atomic_unlock(&bshe->super.seg_lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
            bscs[i].smbcs_success = true;

            /* Calculate the offset and put it in the bootstrap
               area */

            bscs[i].smbcs_data_mpool_offset = (size_t) 
                (tmp - 
                 ((char *) c->sm_data_mpool->mpool_base(c->sm_data_mpool)));

            break;
        }

        /* Bad luck all around -- we didn't find our CID in the array
           and there were no empty slots.  So give up the lock and let
           some other processes / threads in there to try to free up
           some slots, and then try again once we have reacquired the
           lock. */

        else {
            opal_atomic_unlock(&bshe->super.seg_lock);
            SPIN;
            opal_atomic_lock(&bshe->super.seg_lock);
        }
    }

    /* Check to see if there was an error while allocating the shared
       memory */
    if (!bscs[i].smbcs_success) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Look at the comm_setup_t section (in the data segment of the
       bootstrap) and fill in the values on our communicator */

    else {
        err = OMPI_SUCCESS;
        data->mcb_mpool_base = (unsigned char*)c->sm_data_mpool->mpool_base(c->sm_data_mpool);
        data->mcb_mpool_offset = bscs[i].smbcs_data_mpool_offset;
        data->mcb_mpool_area = data->mcb_mpool_base + data->mcb_mpool_offset;
        data->mcb_operation_count = 0;
    }

    /* If the count is now zero, then we're finished with this section
       in the bootstrap segment, and we should release it for others
       to use */

    --bscs[i].smbcs_count;
    if (0 == bscs[i].smbcs_count) {
        bscs[i].smbcs_data_mpool_offset = 0;
        bshe->smbhe_keys[i].mcsbck_cid = INT_MAX;
        memset(&bshe->smbhe_keys[i].mcsbck_rank0_name, 0,
               sizeof(orte_process_name_t));
    }

    /* All done */
    
    opal_atomic_unlock(&bshe->super.seg_lock);
    return err;
}


/*
 * This function is not static and has a prefix-rule-enabled name
 * because it gets called from the component (but may also be called
 * from above).  This is only called once -- no need for reference
 * counting or thread protection.
 */
int mca_coll_sm_bootstrap_finalize(void)
{
    mca_common_sm_mmap_t *meta;

    if (bootstrap_inited) {
        meta = mca_coll_sm_component.sm_bootstrap_meta;

        /* Free the area in the mpool that we were using */
        if (mca_coll_sm_component.sm_data_mpool_created) {
            mca_mpool_base_module_destroy(mca_coll_sm_component.sm_data_mpool);
        }

        /* Free the entire bootstrap area (no need to zero out
           anything in here -- all data structures are referencing
           within the bootstrap area, so the one top-level unmap does
           it all) */
        if (OMPI_SUCCESS == mca_common_sm_mmap_fini( meta )) {
            unlink(meta->map_path);
        }
        OBJ_RELEASE(meta);
    }

    return OMPI_SUCCESS;
}
