/* 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
/** @file */

#ifndef MCA_COLL_SM2_EXPORT_H
#define MCA_COLL_SM2_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"

BEGIN_C_DECLS

#ifdef HAVE_SCHED_YIELD
#  include <sched.h>
#  define SPIN sched_yield()
#elif defined(__WINDOWS__)
#  define SPIN SwitchToThread()
#else  /* no switch available */
#  define SPIN
#endif

/*
 * Memory Management
 * - All memory allocation will be done on a per-communictor basis
 * - The two banks of memory will be used
 * - Each bank of memory will have M buffers
 * - These buffers will be used in a cirucular buffer order
 * - Each buffer will be contigous in virtual memory, and will have page-aligned
 *     regions belonging to each process in the communicator
 * - The memory associated with each process will have a control region, and
 *     a data region.
 * - First touch will be used to enforce memory locality, and thus relies on
 *     processor affinity to be set.
 * - A non-blocking collective will be issued when all buffers in a bank have
 *     been used.  This will be completed before this bank is re-used.
 */

    /** 
     * Structure to hold the sm coll component.  First it holds the
     * base coll component, and then holds a bunch of
     * sm-coll-component-specific stuff (e.g., current MCA param
     * values). 
     */
    struct mca_coll_sm2_component_t {
        /** Base coll component */
        mca_coll_base_component_1_1_0_t super;

        /** MCA parameter: Priority of this component */
        int sm2_priority;

        /** MCA parameter: control region size (bytes), per proc */
        size_t sm2_ctl_size_per_proc;

        /** MCA parameter: control region size (bytes) actually allocated - per proc*/
        size_t sm2_ctl_size_allocated;

        /** MCA parameter: control region alignment */
        size_t sm2_ctl_alignment;

        /** MCA parameter: Max data Segment size */
        size_t sm2_max_data_seg_size;

        /** MCA parameter: Min data Segment size */
        size_t sm2_data_seg_size;

        /** MCA parameter: control data size (bytes) actually allocated - per proc*/
        size_t sm2_data_size_allocated;

        /** MCA parameter: data region alignment */
        size_t sm2_data_alignment;

        /** MCA parameter: number of memory banks */
        size_t sm2_num_mem_banks;

        /** MCA parameter: number of regions per memory bank */
        size_t sm2_num_regions_per_bank;

    };

    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm2_component_t mca_coll_sm2_component_t;


    struct mca_coll_sm2_module_t {
        /* base structure */
        mca_coll_base_module_1_1_0_t super;

        /* Shared Memory file name */
        char *coll_sm2_file_name;

        /* size of shared memory backing file */
        size_t size_sm2_backing_file;

        /* Memory pointer to shared file */
        char *shared_memory_region;

    };

    typedef struct mca_coll_sm2_module_t mca_coll_sm2_module_t;
    OBJ_CLASS_DECLARATION(mca_coll_sm2_module_t);

    
    /**
     * Global component instance
     */
    OMPI_MODULE_DECLSPEC extern mca_coll_sm2_component_t mca_coll_sm2_component;


    /*
     * coll module functions
     */

    /* query to see if the component is available for use, and can
     * satisfy the thread and progress requirements
     */
    int mca_coll_sm2_init_query(bool enable_progress_threads,
			       bool enable_mpi_threads);

    /* query to see if the module is available for use on the given
     * communicator, and if so, what it's priority is.
     */
    struct mca_coll_base_module_1_1_0_t *
    mca_coll_sm2_comm_query(struct ompi_communicator_t *comm, int *priority);

/**
 * Macro to setup flag usage
 */
#define FLAG_SETUP(flag_num, flag, data) \
    (flag) = (mca_coll_sm_in_use_flag_t*) \
        (((char *) (data)->mcb_in_use_flags) + \
        ((flag_num) * mca_coll_sm_component.sm_control_size))

/**
 * Macro to wait for the in-use flag to become idle (used by the root)
 */
#define FLAG_WAIT_FOR_IDLE(flag) \
    while (0 != (flag)->mcsiuf_num_procs_using) SPIN

/**
 * Macro to wait for a flag to indicate that it's ready for this
 * operation (used by non-root processes to know when FLAG_SET() has
 * been called)
 */
#define FLAG_WAIT_FOR_OP(flag, op) \
    while ((op) != flag->mcsiuf_operation_count) SPIN

/**
 * Macro to set an in-use flag with relevant data to claim it
 */
#define FLAG_RETAIN(flag, num_procs, op_count) \
    (flag)->mcsiuf_num_procs_using = (num_procs); \
    (flag)->mcsiuf_operation_count = (op_count)

/**
 * Macro to release an in-use flag from this process
 */
#define FLAG_RELEASE(flag) \
    opal_atomic_add(&(flag)->mcsiuf_num_procs_using, -1)

/**
 * Macro to copy a single segment in from a user buffer to a shared
 * segment
 */
#define COPY_FRAGMENT_IN(convertor, index, rank, iov, max_data) \
    (iov).iov_base = \
        (index)->mcbmi_data + \
        ((rank) * mca_coll_sm_component.sm_fragment_size); \
    (max_data) = (iov).iov_len = mca_coll_sm_component.sm_fragment_size; \
    ompi_convertor_pack(&(convertor), &(iov), &mca_coll_sm_iov_size, \
                        &(max_data) )

/**
 * Macro to copy a single segment out from a shared segment to a user
 * buffer
 */
#define COPY_FRAGMENT_OUT(convertor, src_rank, index, iov, max_data) \
    (iov).iov_base = (((char*) (index)->mcbmi_data) + \
                      ((src_rank) * mca_coll_sm_component.sm_fragment_size)); \
    ompi_convertor_unpack(&(convertor), &(iov), &mca_coll_sm_iov_size, \
                          &(max_data) )

/**
 * Macro to memcpy a fragment between one shared segment and another
 */
#define COPY_FRAGMENT_BETWEEN(src_rank, dest_rank, index, len) \
    memcpy(((index)->mcbmi_data + \
            ((dest_rank) * mca_coll_sm_component.sm_fragment_size)), \
           ((index)->mcbmi_data + \
            ((src_rank) * \
             mca_coll_sm_component.sm_fragment_size)), \
           (len))

/** 
 * Macro to tell children that a segment is ready (normalize
 * the child's ID based on the shift used to calculate the "me" node
 * in the tree).  Used in fan out opertations.
 */
#define PARENT_NOTIFY_CHILDREN(children, num_children, index, value) \
    do { \
        for (i = 0; i < (num_children); ++i) { \
            *((size_t*) \
              (((char*) index->mcbmi_control) + \
               (mca_coll_sm_component.sm_control_size * \
                (((children)[i]->mcstn_id + root) % size)))) = (value); \
        } \
    } while (0)

/**
 * Macro for childen to wait for parent notification (use real rank).
 * Save the value passed and then reset it when done.  Used in fan out
 * operations.
 */
#define CHILD_WAIT_FOR_NOTIFY(rank, index, value) \
    do { \
        uint32_t volatile *ptr = ((uint32_t*) \
                                  (((char*) index->mcbmi_control) + \
                                   ((rank) * mca_coll_sm_component.sm_control_size))); \
        while (0 == *ptr) SPIN; \
        (value) = *ptr; \
        *ptr = 0; \
    } while (0)

/**
 * Macro for children to tell parent that the data is ready in their
 * segment.  Used for fan in operations.
 */
#define CHILD_NOTIFY_PARENT(child_rank, parent_rank, index, value) \
    ((size_t volatile *) \
     (((char*) (index)->mcbmi_control) + \
      (mca_coll_sm_component.sm_control_size * \
       (parent_rank))))[(child_rank)] = (value)

/**
 * Macro for parent to wait for a specific child to tell it that the
 * data is in the child's segment.  Save the value when done.  Used
 * for fan in operations.
 */
#define PARENT_WAIT_FOR_NOTIFY_SPECIFIC(child_rank, parent_rank, index, value) \
    do { \
        size_t volatile *ptr = ((size_t volatile *) \
                                (((char*) index->mcbmi_control) + \
                                 (mca_coll_sm_component.sm_control_size * \
                                  (parent_rank)))) + child_rank; \
        while (0 == *ptr) SPIN; \
        (value) = *ptr; \
        *ptr = 0; \
    } while (0)

END_C_DECLS

#endif /* MCA_COLL_SM2_EXPORT_H */
