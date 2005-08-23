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

#include <stdio.h>
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#include <sys/mman.h>

#include "mpi.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/mpool/base/base.h"
#include "coll_sm.h"


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
    NULL, 
    NULL,
    NULL,
    NULL,
    mca_coll_sm_barrier_intra,
    mca_coll_sm_bcast_intra,
    NULL,
    NULL,
    NULL,
    NULL,
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
    int ret;
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

    /* Ok, we have local peers.  So setup the bootstrap file */

    if (OMPI_SUCCESS != (ret = bootstrap_init())) {
        return ret;
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
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        mca_coll_sm_component.sm_data_mpool_created = true;
    } else {
        mca_coll_sm_component.sm_data_mpool_created = false;
    }

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
    /* If we're intercomm, or if there's only one process in the
       communicator, or if not all the processes in the communicator
       are not on this node, then we don't want to run */

    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm) ||
        !have_local_peers(comm->c_local_group->grp_proc_pointers,
                          ompi_comm_size(comm))) {
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
    int i;
    mca_coll_base_comm_t *data;
    size_t control_size, data_size, total_size;
    mca_coll_sm_component_t *c = &mca_coll_sm_component;

    /* Allocate data to hang off the communicator.  The memory we
       alloc will be laid out as follows:

       1. mca_coll_base_comm_t
       2. array of num_segments pointers to
          mca_coll_base_mpool_index_t instances
       3. array of num_segments mca_coll_base_mpool_index_t instances
          (pointed to by the array in 2)
    */

    data = malloc(sizeof(mca_coll_base_comm_t) + 
                  (mca_coll_sm_component.sm_bootstrap_num_segments * 
                   (sizeof(mca_coll_base_mpool_index_t *) +
                    sizeof(mca_coll_base_mpool_index_t))));
    if (NULL == data) {
        return NULL;
    }
    data->mcb_data_mpool_malloc_addr = NULL;

    /* Setup #2: set the array to point immediately beyond the
       mca_coll_base_comm_t */
    data->mcb_mpool_index = (mca_coll_base_mpool_index_t**) (data + 1);
    /* Setup the first pointer in #2 to point to the first instance in
       #3 */
    data->mcb_mpool_index[0] = (mca_coll_base_mpool_index_t*)
        (((char*) data->mcb_mpool_index) +
         (mca_coll_sm_component.sm_bootstrap_num_segments * 
          sizeof(mca_coll_base_mpool_index_t *)));
    /* Now setup the rest of the pointers in #2 to point to their
       corresponding instances in #3 */
    for (i = 1; i < mca_coll_sm_component.sm_bootstrap_num_segments; ++i) {
        data->mcb_mpool_index[i] = data->mcb_mpool_index[i - 1] + 1;
    }

    comm->c_coll_selected_data = data;

    /* Bootstrap this communicator; set us up with the global mpool,
       etc. */

    if (OMPI_SUCCESS != bootstrap_comm(comm)) {
        return NULL;
    }

    /* Once the communicator is bootstrapped, setup the
       mca_coll_base_mpool_index_t pointers to point to the
       appropriate places in the mpool. */

    control_size = ompi_comm_size(comm) * c->sm_control_size;
    data_size = ompi_comm_size(comm) * c->sm_fragment_size;
    total_size = (control_size + data_size) * 2;
    for (i = 0; i < data->mcb_mpool_num_segments; ++i) {
        data->mcb_mpool_index[i]->mcbmi_control_fan_in = 
            data->mcb_mpool_base + data->mcb_mpool_offset +
            (i * total_size);
        data->mcb_mpool_index[i]->mcbmi_control_fan_out =
            data->mcb_mpool_index[i]->mcbmi_control_fan_in + control_size;
        data->mcb_mpool_index[i]->mcbmi_data_fan_in =
            data->mcb_mpool_index[i]->mcbmi_control_fan_out + control_size;
        data->mcb_mpool_index[i]->mcbmi_data_fan_out =
            data->mcb_mpool_index[i]->mcbmi_data_fan_in + data_size;
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
    asprintf(&fullpath, "%s/%s", orte_process_info.job_session_dir,
             mca_coll_sm_component.sm_bootstrap_filename);
    if (NULL == fullpath) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    size = 
        sizeof(mca_coll_sm_bootstrap_header_extension_t) +
        (mca_coll_sm_component.sm_bootstrap_num_segments *
         sizeof(mca_coll_sm_bootstrap_comm_setup_t)) +
        (sizeof(uint32_t) * mca_coll_sm_component.sm_bootstrap_num_segments);

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
        bshe->smbhe_num_segments = 
            mca_coll_sm_component.sm_bootstrap_num_segments;
        bshe->smbhe_segments = (mca_coll_sm_bootstrap_comm_setup_t *)
            (((char *) bshe) + 
             sizeof(mca_coll_sm_bootstrap_header_extension_t) +
             (sizeof(uint32_t) * 
              mca_coll_sm_component.sm_bootstrap_num_segments));
        bshe->smbhe_cids = (uint32_t *)
            (((char *) bshe) + sizeof(*bshe));
        for (i = 0; i < bshe->smbhe_num_segments; ++i) {
            bshe->smbhe_cids[i] = INT_MAX;
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

    /* Is our CID in the CIDs array?  If not, loop until we can find
       an open slot in the array to use in the bootstrap to setup our
       communicator. */

    bshe = (mca_coll_sm_bootstrap_header_extension_t *) 
        c->sm_bootstrap_meta->map_seg;
    bscs = bshe->smbhe_segments;
    opal_atomic_lock(&bshe->super.seg_lock);
    while (1) {
        opal_atomic_wmb();
        found = false;
        empty_index = -1;
        for (i = 0; i < bshe->smbhe_num_segments; ++i) {
            if (comm->c_contextid == bshe->smbhe_cids[i]) {
                found = true;
                break;
            } else if (INT_MAX == bshe->smbhe_cids[i] && -1 == empty_index) {
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
            bshe->smbhe_cids[i] = comm->c_contextid;

            bscs[i].smbcs_communicator_num_segments = 
                c->sm_communicator_num_segments;
            bscs[i].smbcs_count = ompi_comm_size(comm);

            /* Calculate how much space we need in the data mpool.
               There are several values to add (one of these for each
               segment):

               - size of the control data:
                   - fan-in data (num_procs * control size)
                   - fan-out data (num_procs * control size)
               - size of message data
                   - fan-in data (num_procs * (frag_size rounded up to
                     control size))
                   - fan-out data (num_procs * (frag_size rounded up
                     to control size))

               So it's:

               num_segs * ((num_procs * cachline * 2) + (num_procs *
                                                         frag * 2))

               Which reduces to:

               num_segs * num_procs * 2 * (control + frag)
            */

            size = c->sm_communicator_num_segments * ompi_comm_size(comm) * 2 *
                (c->sm_control_size + c->sm_fragment_size);

            data->mcb_data_mpool_malloc_addr = tmp =
                c->sm_data_mpool->mpool_alloc(c->sm_data_mpool, size, 
                                         sizeof(double), NULL);
            if (NULL == tmp) {
                /* Cleanup before returning; allow other processes in
                   this communicator to learn of the failure.  Note
                   that by definition, bscs[i].smbcs_count won't be
                   zero after the decrement (because there must be >=2
                   processes in this communicator, or the self coll
                   component would have been chosen), so we don't need
                   to do that cleanup. */
                bscs[i].smbcs_data_mpool_offset = 0;
                bscs[i].smbcs_communicator_num_segments = 0;
                --bscs[i].smbcs_count;
                opal_atomic_unlock(&bshe->super.seg_lock);
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            /* Zero it out.  We really only need to zero out the
               control structures, but zeroing out the entire this is
               easy and "good enough". */

            memset(tmp, 0, size);

            /* Calculate the offset and put it in the bootstrap
               area */

            bscs[i].smbcs_data_mpool_offset = (size_t) 
                (tmp - ((char *) c->sm_data_mpool->mpool_base(c->sm_data_mpool)));

            /* Now setup memory affinity such that various control
               data pages are local to their target processors, if
               supported. */

            /* JMS continue here */

            break;
        }

        /* Bad luck all around -- we didn't find our CID in the array
           and there were no empty slots.  So give up the lock and let
           some other processes / threads in there to try to free up
           some slots, and then try again once we have reacquired the
           lock. */

        else {
            opal_atomic_unlock(&bshe->super.seg_lock);
#ifdef HAVE_SCHED_YIELD
            sched_yield();
#endif
            opal_atomic_lock(&bshe->super.seg_lock);
        }
    }

    /* Check to see if there was an error while allocating the shared
       memory */
    if (0 == bscs[i].smbcs_communicator_num_segments) {
        err = OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Look at the comm_setup_t section (in the data segment of the
       bootstrap) and fill in the values on our communicator */

    else {
        err = OMPI_SUCCESS;
        data->mcb_mpool_base = c->sm_data_mpool->mpool_base(c->sm_data_mpool);
        data->mcb_mpool_offset = bscs[i].smbcs_data_mpool_offset;
        data->mcb_mpool_area = data->mcb_mpool_base + data->mcb_mpool_offset;
        data->mcb_mpool_num_segments = bscs[i].smbcs_communicator_num_segments;
        data->mcb_operation_count = 0;
    }

    /* If the count is now zero, then we're finished with this section
       in the bootstrap segment, and we should release it for others
       to use */

    --bscs[i].smbcs_count;
    if (0 == bscs[i].smbcs_count) {
        bscs[i].smbcs_data_mpool_offset = 0;
        bshe->smbhe_cids[i] = INT_MAX;
    }

    /* All done */
    
    opal_atomic_unlock(&bshe->super.seg_lock);
    return err;
}


/*
 * This function is not static and has a prefix-rule-enabled name
 * because it gets called from the component.  This is only called
 * once -- no need for reference counting or thread protection.
 */
int mca_coll_sm_bootstrap_finalize(void)
{
    mca_common_sm_mmap_t *meta;

    if (bootstrap_inited) {
        meta = mca_coll_sm_component.sm_bootstrap_meta;

        /* Free the area in the mpool that we were using */
        if (mca_coll_sm_component.sm_data_mpool_created) {
            /* JMS: there does not yet seem to be any opposite to
               mca_mpool_base_module_create()... */
        }

        /* Free the entire bootstrap area (no need to zero out
           anything in here -- all data structures are referencing
           within the bootstrap area, so the one top-level unmap does
           it all) */

        munmap(meta->map_seg, meta->map_size);
    }

    return OMPI_SUCCESS;
}
