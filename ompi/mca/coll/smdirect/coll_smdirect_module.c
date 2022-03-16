/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
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
#include <string.h>
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
#include "opal_stdint.h"
#include "opal/mca/hwloc/base/base.h"
#include "opal/util/os_path.h"
#include "opal/util/printf.h"

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/base.h"
#include "ompi/runtime/ompi_rte.h"
#include "ompi/proc/proc.h"
#include "coll_smdirect.h"

#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/pml/pml.h"

#include "opal/mca/smsc/base/base.h"
#include "opal/mca/smsc/smsc.h"

/*
 * Local functions
 */
static int sm_module_enable(mca_coll_base_module_t *module,
                          struct ompi_communicator_t *comm);
static int bootstrap_comm(ompi_communicator_t *comm,
                          mca_coll_smdirect_module_t *module);
static int mca_coll_smdirect_module_disable(mca_coll_base_module_t *module,
                          struct ompi_communicator_t *comm);

/*
 * Module constructor
 */
static void mca_coll_smdirect_module_construct(mca_coll_smdirect_module_t *module)
{
    module->enabled = false;
    module->sm_comm_data = NULL;
    module->previous_reduce = NULL;
    module->previous_reduce_module = NULL;
    module->super.coll_module_disable = mca_coll_smdirect_module_disable;
}

/*
 * Module destructor
 */
static void mca_coll_smdirect_module_destruct(mca_coll_smdirect_module_t *module)
{
    mca_coll_smdirect_comm_t *c = module->sm_comm_data;

    if (NULL != c) {
        /* Munmap the per-communicator shmem data segment */
        if (NULL != c->sm_bootstrap_meta) {
            /* Ignore any errors -- what are we going to do about
               them? */
            mca_common_sm_fini(c->sm_bootstrap_meta);
            OBJ_RELEASE(c->sm_bootstrap_meta);
        }
        for (int i = 0; i < module->sm_comm_data->comm_size; ++i) {
            if (NULL != module->sm_comm_data->endpoints[i]) {
                MCA_SMSC_CALL(return_endpoint, module->sm_comm_data->endpoints[i]);
            }
        }
        free(c);
    }

    /* It should always be non-NULL, but just in case */
    if (NULL != module->previous_reduce_module) {
        OBJ_RELEASE(module->previous_reduce_module);
    }

    module->enabled = false;
}

/*
 * Module disable
 */
static int mca_coll_smdirect_module_disable(mca_coll_base_module_t *module, struct ompi_communicator_t *comm)
{
    mca_coll_smdirect_module_t *sm_module = (mca_coll_smdirect_module_t*) module;
    if (NULL != sm_module->previous_reduce_module) {
	sm_module->previous_reduce = NULL;
        OBJ_RELEASE(sm_module->previous_reduce_module);
	sm_module->previous_reduce_module = NULL;
    }
    return OMPI_SUCCESS;
}


OBJ_CLASS_INSTANCE(mca_coll_smdirect_module_t,
                   mca_coll_base_module_t,
                   mca_coll_smdirect_module_construct,
                   mca_coll_smdirect_module_destruct);

/*
 * Initial query function that is invoked during MPI_INIT, allowing
 * this component to disqualify itself if it doesn't support the
 * required level of thread support.  This function is invoked exactly
 * once.
 */
int mca_coll_smdirect_init_query(bool enable_progress_threads,
                           bool enable_mpi_threads)
{
    /* if no session directory was created, then we cannot be used */
    if (NULL == ompi_process_info.job_session_dir) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    /* Don't do much here because we don't really want to allocate any
       shared memory until this component is selected to be used. */
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:sm:init_query: pick me! pick me!");
    return OMPI_SUCCESS;
}


/*
 * Invoked when there's a new communicator that has been created.
 * Look at the communicator and decide which set of functions and
 * priority we want to return.
 */
mca_coll_base_module_t *
mca_coll_smdirect_comm_query(struct ompi_communicator_t *comm, int *priority)
{
    mca_coll_smdirect_module_t *sm_module;

    /* If we're intercomm, or if there's only one process in the
       communicator, or if not all the processes in the communicator
       are not on this node, then we don't want to run */
    if (OMPI_COMM_IS_INTER(comm) || 1 == ompi_comm_size(comm) || ompi_group_have_remote_peers (comm->c_local_group)) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:comm_query (%d/%s): intercomm, comm is too small, or not all peers local; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    /* TODO: it's not clear how multiple initializations are avoided */
    if (NULL == mca_smsc) {
        int rc = mca_smsc_base_select();
        if (OPAL_SUCCESS != rc) {
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                                "coll:smdirect:comm_query (%d/%s): SM single copy component not available",
                                comm->c_contextid, comm->c_name);
            return NULL;
        }
    }

    bool smsc_can_map = mca_smsc_base_has_feature(MCA_SMSC_FEATURE_CAN_MAP);

    if (!smsc_can_map) {
        /* For now we only support mapping SMSC,
          * need to figure out whether non-mapping smsc could be beneficial */
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:smdirect:comm_query (%d/%s): SM single copy component does not support mapping peer memory",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    /* Get the priority level attached to this module. If priority is less
     * than or equal to 0, then the module is unavailable. */
    *priority = mca_coll_smdirect_component.sm_priority;
    if (mca_coll_smdirect_component.sm_priority < 0) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:comm_query (%d/%s): priority too low; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return NULL;
    }

    sm_module = OBJ_NEW(mca_coll_smdirect_module_t);
    if (NULL == sm_module) {
        return NULL;
    }

    /* All is good -- return a module */
    sm_module->super.coll_module_enable = sm_module_enable;
    sm_module->super.coll_allgather  = NULL;
    sm_module->super.coll_allgatherv = NULL;
    sm_module->super.coll_allreduce  = NULL;
    sm_module->super.coll_alltoall   = NULL;
    sm_module->super.coll_alltoallv  = NULL;
    sm_module->super.coll_alltoallw  = NULL;
    sm_module->super.coll_barrier    = NULL;
    sm_module->super.coll_bcast      = NULL;
    sm_module->super.coll_exscan     = NULL;
    sm_module->super.coll_gather     = NULL;
    sm_module->super.coll_gatherv    = NULL;
    sm_module->super.coll_reduce     = mca_coll_smdirect_reduce_intra;
    sm_module->super.coll_reduce_scatter = NULL;
    sm_module->super.coll_scan       = NULL;
    sm_module->super.coll_scatter    = NULL;
    sm_module->super.coll_scatterv   = NULL;

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:sm:comm_query (%d/%s): pick me! pick me!",
                        comm->c_contextid, comm->c_name);
    return &(sm_module->super);
}


/*
 * Init module on the communicator
 */
static int sm_module_enable(mca_coll_base_module_t *module,
                            struct ompi_communicator_t *comm)
{
    if (NULL == comm->c_coll->coll_reduce ||
        NULL == comm->c_coll->coll_reduce_module) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:enable (%d/%s): no underlying reduce; disqualifying myself",
                            comm->c_contextid, comm->c_name);
        return OMPI_ERROR;
    }

    /* We do everything lazily in ompi_coll_smdirect_enable() */
    return OMPI_SUCCESS;
}

int ompi_coll_smdirect_lazy_enable(mca_coll_base_module_t *module,
                             struct ompi_communicator_t *comm)
{
    int i, j, root, ret;
    int rank = ompi_comm_rank(comm);
    int size = ompi_comm_size(comm);
    mca_coll_smdirect_module_t *sm_module = (mca_coll_smdirect_module_t*) module;
    mca_coll_smdirect_comm_t *data = NULL;
    size_t control_size;
    mca_coll_smdirect_component_t *c = &mca_coll_smdirect_component;
    opal_hwloc_base_memory_segment_t *maffinity;
    int parent, min_child, num_children;
    unsigned char *base = NULL;

    /* Just make sure we haven't been here already */
    if (sm_module->enabled) {
        return OMPI_SUCCESS;
    }
    sm_module->enabled = true;


    /* Allocate data to hang off the communicator.  The memory we
       alloc will be laid out as follows:

       1. mca_coll_base_comm_t
       2. array of ompi_comm_size(comm) mca_coll_smdirect_tree_node_t
          instances
       3. array of sm_tree_degree pointers to other tree nodes (i.e.,
          this nodes' children) for each instance of
          mca_coll_smdirect_tree_node_t
    */

    sm_module->sm_comm_data = data = (mca_coll_smdirect_comm_t*)
        malloc(sizeof(mca_coll_smdirect_comm_t)  +
               (size *
                (sizeof(mca_coll_smdirect_tree_node_t) +
                 (sizeof(mca_coll_smdirect_tree_node_t*) * c->sm_tree_degree))));
    if (NULL == data) {
        free(maffinity);
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:enable (%d/%s): malloc failed (2)",
                            comm->c_contextid, comm->c_name);
        return OMPI_ERR_TEMP_OUT_OF_RESOURCE;
    }

    data->comm_size = size;
    data->endpoints = calloc(size, sizeof(mca_smsc_endpoint_t *));

    /* Setup array of pointers for #2 */
    data->mcb_tree = (mca_coll_smdirect_tree_node_t*) (data + 1);
    /* Finally, setup the array of children pointers in the instances
       in #5 to point to their corresponding arrays in #6 */
    data->mcb_tree[0].mcstn_children = (mca_coll_smdirect_tree_node_t**)
        (data->mcb_tree + size);
    for (i = 1; i < size; ++i) {
        data->mcb_tree[i].mcstn_children =
            data->mcb_tree[i - 1].mcstn_children + c->sm_tree_degree;
    }

    /* Pre-compute a tree for a given number of processes and degree.
       We'll re-use this tree for all possible values of root (i.e.,
       shift everyone's process to be the "0"/root in this tree. */
    for (root = 0; root < size; ++root) {
        parent = (root - 1) / mca_coll_smdirect_component.sm_tree_degree;
        num_children = mca_coll_smdirect_component.sm_tree_degree;

        /* Do we have children?  If so, how many? */

        if ((root * num_children) + 1 >= size) {
            /* Leaves */
            min_child = -1;
            num_children = 0;
        } else {
            /* Interior nodes */
            int max_child;
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


    /* Finally, setup the array of children pointers in the instances
       in #5 to point to their corresponding arrays in #6 */
    data->mcb_tree[0].mcstn_children = (mca_coll_smdirect_tree_node_t**)
        (data->mcb_tree + size);
    for (i = 1; i < size; ++i) {
        data->mcb_tree[i].mcstn_children =
            data->mcb_tree[i - 1].mcstn_children + c->sm_tree_degree;
    }

    /* Pre-compute a tree for a given number of processes and degree.
       We'll re-use this tree for all possible values of root (i.e.,
       shift everyone's process to be the "0"/root in this tree. */
    for (root = 0; root < size; ++root) {
        parent = (root - 1) / mca_coll_smdirect_component.sm_tree_degree;
        num_children = mca_coll_smdirect_component.sm_tree_degree;

        /* Do we have children?  If so, how many? */

        if ((root * num_children) + 1 >= size) {
            /* Leaves */
            min_child = -1;
            num_children = 0;
        } else {
            /* Interior nodes */
            int max_child;
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

    /* allocate space for the maximum number of children we expect */
    data->peerdata = malloc(sizeof(data->peerdata[0])*mca_coll_smdirect_component.sm_tree_degree);

    /* Attach to this communicator's shmem data segment */
    if (OMPI_SUCCESS != (ret = bootstrap_comm(comm, sm_module))) {
        free(data);
        free(maffinity);
        sm_module->sm_comm_data = NULL;
        return ret;
    }

    /* Once the communicator is bootstrapped, setup the pointers into
       the per-communicator shmem data segment.  First, setup the
       barrier buffers.  There are 2 sets of barrier buffers (because
       there can never be more than one outstanding barrier occuring
       at any timie).  Setup pointers to my control buffers, my
       parents, and [the beginning of] my children (note that the
       children are contiguous, so having the first pointer and the
       num_children from the mcb_tree data is sufficient). */
    control_size = c->sm_control_size;
    base = data->sm_bootstrap_meta->module_data_addr;

    data->procdata = (mca_coll_smdirect_procdata_t*)(((uintptr_t)base) + rank*control_size);

    data->procdata->mcsp_indata = NULL;
    data->procdata->mcsp_insize = 0;
    /* initialize the flags to something that blocks peers on first call */
    data->procdata->mcsp_op_flag.mcsiuf_operation_count = 1;
    data->procdata->mcsp_op_flag.mcsiuf_num_procs_using = 0;
    data->procdata->mcsp_segment_flag.mcsiuf_operation_count = 1;
    data->procdata->mcsp_segment_flag.mcsiuf_num_procs_using = 0;
    data->mcb_operation_count = -1;

    /* Save previous component's reduce information */
    sm_module->previous_reduce = comm->c_coll->coll_reduce;
    sm_module->previous_reduce_module = comm->c_coll->coll_reduce_module;
    OBJ_RETAIN(sm_module->previous_reduce_module);

    /* Indicate that we have successfully attached and setup */
    opal_atomic_add (&(data->sm_bootstrap_meta->module_seg->seg_inited), 1);

    /* Wait for everyone in this communicator to attach and setup */
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:sm:enable (%d/%s): waiting for peers to attach",
                        comm->c_contextid, comm->c_name);
    SPIN_CONDITION(size == data->sm_bootstrap_meta->module_seg->seg_inited);

    /* Once we're all here, remove the mmap file; it's not needed anymore */
    if (0 == rank) {
        unlink(data->sm_bootstrap_meta->shmem_ds.seg_name);
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:enable (%d/%s): removed mmap file %s",
                            comm->c_contextid, comm->c_name,
                            data->sm_bootstrap_meta->shmem_ds.seg_name);
    }

    /* All done */

    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:sm:enable (%d/%s): success!",
                        comm->c_contextid, comm->c_name);
    return OMPI_SUCCESS;
}

static int bootstrap_comm(ompi_communicator_t *comm,
                          mca_coll_smdirect_module_t *module)
{
    char *shortpath, *fullpath;
    mca_coll_smdirect_component_t *c = &mca_coll_smdirect_component;
    mca_coll_smdirect_comm_t *data = module->sm_comm_data;
    int comm_size = ompi_comm_size(comm);
    int control_size = c->sm_control_size;
    ompi_process_name_t *lowest_name = NULL;
    size_t size;
    ompi_proc_t *proc;

    /* Make the rendezvous filename for this communicators shmem data
       segment.  The CID is not guaranteed to be unique among all
       procs on this node, so also pair it with the PID of the proc
       with the lowest PMIx name to form a unique filename. */
    proc = ompi_group_peer_lookup(comm->c_local_group, 0);
    lowest_name = OMPI_CAST_RTE_NAME(&proc->super.proc_name);
    for (int i = 1; i < comm_size; ++i) {
        proc = ompi_group_peer_lookup(comm->c_local_group, i);
        if (ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL,
                                          OMPI_CAST_RTE_NAME(&proc->super.proc_name),
                                          lowest_name) < 0) {
            lowest_name = OMPI_CAST_RTE_NAME(&proc->super.proc_name);
        }
    }
    opal_asprintf(&shortpath, "coll-sm-cid-%s-name-%s.mmap", ompi_comm_print_cid (comm),
             OMPI_NAME_PRINT(lowest_name));
    if (NULL == shortpath) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:enable:bootstrap comm (%s/%s): asprintf failed",
                            ompi_comm_print_cid (comm), comm->c_name);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    fullpath = opal_os_path(false, ompi_process_info.job_session_dir,
                            shortpath, NULL);
    free(shortpath);
    if (NULL == fullpath) {
        opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                            "coll:sm:enable:bootstrap comm (%s/%s): opal_os_path failed",
                            ompi_comm_print_cid (comm), comm->c_name);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    /* Calculate how much space we need in the per-communicator shmem
       data segment. We only need one control data (single page) per process.
     */
    size = comm_size*control_size;
    opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                        "coll:sm:enable:bootstrap comm (%s/%s): attaching to %" PRIsize_t " byte mmap: %s",
                        ompi_comm_print_cid (comm), comm->c_name, size, fullpath);
    if (0 == ompi_comm_rank (comm)) {
        data->sm_bootstrap_meta = mca_common_sm_module_create_and_attach (size, fullpath, sizeof(mca_common_sm_seg_header_t), 8);
        if (NULL == data->sm_bootstrap_meta) {
            opal_output_verbose(10, ompi_coll_base_framework.framework_output,
                "coll:sm:enable:bootstrap comm (%s/%s): mca_common_sm_init_group failed",
                ompi_comm_print_cid (comm), comm->c_name);
            free(fullpath);
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        for (int i = 1 ; i < ompi_comm_size (comm) ; ++i) {
            MCA_PML_CALL(send(&data->sm_bootstrap_meta->shmem_ds, sizeof (data->sm_bootstrap_meta->shmem_ds), MPI_BYTE,
                         i, MCA_COLL_BASE_TAG_BCAST, MCA_PML_BASE_SEND_STANDARD, comm));
        }
    } else {
        opal_shmem_ds_t shmem_ds;
        MCA_PML_CALL(recv(&shmem_ds, sizeof (shmem_ds), MPI_BYTE, 0, MCA_COLL_BASE_TAG_BCAST, comm, MPI_STATUS_IGNORE));
        data->sm_bootstrap_meta = mca_common_sm_module_attach (&shmem_ds, sizeof(mca_common_sm_seg_header_t), 8);
    }

    /* All done */
    free(fullpath);
    return OMPI_SUCCESS;
}
