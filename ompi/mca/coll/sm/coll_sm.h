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
/** @file */

#ifndef MCA_COLL_SM_EXPORT_H
#define MCA_COLL_SM_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "orte/mca/ns/ns_types.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"

/*
 * Horrid debugging macro
 */
#if 0
#include <stdio.h>
#define D(foo) { printf foo ; fflush(stdout); }
#else
#define D(foo)
#endif

#ifdef HAVE_SCHED_YIELD
#  include <sched.h>
#  define SPIN sched_yield()
#elif defined(__WINDOWS__)
#  define SPIN SwitchToThread()
#else  /* no switch available */
#  define SPIN
#endif

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

    /**
     * Structure used within bootstrap mmap file for setting up a coll
     * sm component on a communicator
     */
    struct mca_coll_sm_bootstrap_comm_setup_t {
        /** Offset to be used in the data mpool for this comm's
            collective sm operations -- use this value plus the base
            of the mpool to obtain the pointer to this comm's
            mca_coll_sm_mpool_area_t */
        volatile size_t smbcs_data_mpool_offset;

        /** Number of processes in this communicator who have seen
            this value already. */
        volatile int smbcs_count;

        /** Mechanism for letting multiple processes know whether this
            allocation succeeded or not */
        volatile bool smbcs_success;
    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm_bootstrap_comm_setup_t 
        mca_coll_sm_bootstrap_comm_setup_t;

    /**
     * Structure that acts as a key for the bootstrap area -- a
     * segment in the bootstrap mpool is uniquely identified by its
     * CID and the process name of rank 0 in the group.
     */
    struct mca_coll_sm_bootstrap_comm_key_t {
        /** CID of the communicator using a bootstrap segment */
        uint32_t mcsbck_cid;
        /** Process name of rank 0 in this communicator */
        orte_process_name_t mcsbck_rank0_name;
    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm_bootstrap_comm_key_t
        mca_coll_sm_bootstrap_comm_key_t;

    /**
     * Extension to the control structure in the bootstrap mmap file
     */
    struct mca_coll_sm_bootstrap_header_extension_t {
        /** upper-level control structure */
        mca_common_sm_file_header_t super;

        /** Pointer to the start of the segments in the bootstrap area
            (map->seg_data only points to just beyond the
            mca_common_sm_file_header_t) */
        mca_coll_sm_bootstrap_comm_setup_t *smbhe_segments;

        /** Pointer to array containing
            component.sm_bootstrap_num_segments (CID, rank 0 process
            name) tuples for use in bootstrap phase -- will always
            point immediately after the end of this struct (i.e.,
            still within this header, but since it's variable size
            (set by MCA param), we can't just have it here in the
            struct.  Bonk). */
        mca_coll_sm_bootstrap_comm_key_t *smbhe_keys;
    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm_bootstrap_header_extension_t
        mca_coll_sm_bootstrap_header_extension_t;

    /** 
     * Structure to hold the sm coll component.  First it holds the
     * base coll component, and then holds a bunch of
     * sm-coll-component-specific stuff (e.g., current MCA param
     * values). 
     */
    struct mca_coll_sm_component_t {
        /** Base coll component */
        mca_coll_base_component_1_0_0_t super;

        /** MCA parameter: Priority of this component */
        int sm_priority;

        /** MCA parameter: Length of a cache line or page (in bytes) */
        int sm_control_size;

        /** MCA parameter: Name of shared memory control / bootstrap
            mmap file */
        char *sm_bootstrap_filename;

        /** MCA parameter: Number of segments in the bootstrap file
            (for use with setting up multiple comm's with sm
            components simultaneously) */
        int sm_bootstrap_num_segments;

        /** MCA parameter: Name of the mpool that this component will
            use */
        char *sm_mpool_name;

        /** MCA parameter: Number of "in use" flags in each
            communicator's area in the data mpool */
        int sm_comm_num_in_use_flags;

        /** MCA parameter: Number of segments for each communicator in
            the data mpool */
        int sm_comm_num_segments;

        /** MCA parameter: Fragment size for data */
        int sm_fragment_size;

        /** MCA parameter: Degree of tree for tree-based collectives */
        int sm_tree_degree;

        /** MCA parameter: Number of processes to use in the
            calculation of the "info" MCA parameter */
        int sm_info_comm_size;

        /******* end of MCA params ********/

        /** Size of the bootstrap area -- calculated in
            coll_sm_component.c */
        size_t sm_bootstrap_size;

        /** Size of the data mpool area -- calculated in
            coll_sm_component.c */
        size_t sm_data_mpool_size;

        /** Data mpool that will be used */
        mca_mpool_base_module_t *sm_data_mpool;

        /** Whether we ended up creating the sm mpool or whether
            someone else created it and we just found it */
        bool sm_data_mpool_created;

        /** Meta struct containing information about the bootstrap area */
        mca_common_sm_mmap_t *sm_bootstrap_meta;

        /** How many fragment segments are protected by a single
            in-use flags.  This is solely so that we can only perform
            the division once and then just use the value without
            having to re-calculate. */
        int sm_segs_per_inuse_flag;

        /** Whether the component's shared memory has been [lazily]
            initialized or not */
        bool sm_component_setup;

        /** Once the component has been lazily initialized, keep the
            state of it around */
        bool sm_component_setup_success;

        /** A lock protecting the lazy initialzation of the component
            (SINCE THERE IS NO STATIC INITIALIZER FOR
            opal_atomic_lock_t, THIS *MUST* BE THE LAST MEMBER OF THE
            STRUCT!) */
        opal_atomic_lock_t sm_component_setup_lock;

    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm_component_t mca_coll_sm_component_t;

    /**
     * Structure for representing a node in the tree
     */
    struct mca_coll_sm_tree_node_t {
        /** Arbitrary ID number, starting from 0 */
        int mcstn_id;
        /** Pointer to parent, or NULL if root */
        struct mca_coll_sm_tree_node_t *mcstn_parent;
        /** Number of children, or 0 if a leaf */
        int mcstn_num_children;
        /** Pointer to an array of children, or NULL if 0 ==
            mcstn_num_children */
        struct mca_coll_sm_tree_node_t **mcstn_children;
    };
    /**
     * Convenienve typedef
     */
    typedef struct mca_coll_sm_tree_node_t mca_coll_sm_tree_node_t;

    /**
     * Simple structure comprising the "in use" flags.  Contains two
     * members: the number of processes that are currently using this
     * set of segments and the operation number of the current
     * operation.
     */
    struct mca_coll_sm_in_use_flag_t {
        /** Number of processes currently using this set of
            segments */
        volatile uint32_t mcsiuf_num_procs_using;
        /** Must match data->mcb_count */
        volatile uint32_t mcsiuf_operation_count;
    };
    /**
     * Convenienve typedef
     */
    typedef struct mca_coll_sm_in_use_flag_t mca_coll_sm_in_use_flag_t;

    /**
     * Structure containing pointers to various arrays of data in the
     * data mpool area (one of these indexes a single segment in the
     * data mpool).  Nothing is hard-coded because all the array
     * lengths and displacements of the pointers all depend on how
     * many processes are in the communicator.
     */
    struct mca_coll_base_mpool_index_t {
        /** Pointer to beginning of control data */
        uint32_t volatile *mcbmi_control;
        /** Pointer to beginning of message fragment data */
        char *mcbmi_data;
    };
    typedef struct mca_coll_base_mpool_index_t mca_coll_base_mpool_index_t;

    /**
     * Structure for the sm coll module to hang off the communicator.
     * Contains communicator-specific information, including pointers
     * into the data mpool for this comm's sm collective operations
     * area. 
     */
    struct mca_coll_base_comm_t {
        /** If this process is the one that invoked mpool_alloc() for
            the data segment, the return value will be in here.
            Otherwise, it will be NULL (i.e., only the allocating
            process will call free). */
        void *mcb_data_mpool_malloc_addr;
        /** Base of the data mpool */
        unsigned char *mcb_mpool_base;
        /** Offset into the data mpool where this comm's operations
            area is */
        size_t mcb_mpool_offset;
        /** Pointer in the data mpool to the beginning of this comm's
            operations area (i.e., mcb_mpool_base +
            mcb_mpool_offset) */
        unsigned char *mcb_mpool_area;

        /** Pointer to my barrier control pages (odd index pages are
            "in", even index pages are "out") */
        uint32_t *mcb_barrier_control_me;

        /** Pointer to my parent's barrier control pages (will be NULL
            for communicator rank 0; odd index pages are "in", even
            index pages are "out") */
        uint32_t *mcb_barrier_control_parent;

        /** Pointers to my childrens' barrier control pages (they're
            contiguous in memory, so we only point to the base -- the
            number of children is in my entry in the mcb_tree); will
            be NULL if this process has no children (odd index pages
            are "in", even index pages are "out") */
        uint32_t *mcb_barrier_control_children;

        /** Number of barriers that we have executed (i.e., which set
            of barrier buffers to use). */
        int mcb_barrier_count;

        /** "In use" flags indicating which segments are available */
        mca_coll_sm_in_use_flag_t *mcb_in_use_flags;

        /** Array of indexes into the mpool area for control and data
            fragment passing (containing pointers to each segments
            control and data areas). */
        mca_coll_base_mpool_index_t *mcb_mpool_index;

        /** Array of graph nodes representing the tree used for
            communications */
        mca_coll_sm_tree_node_t *mcb_tree;

        /** Operation number (i.e., which segment number to use) */
        uint32_t mcb_operation_count;
    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_base_comm_t mca_coll_base_comm_t;
        
    
    /**
     * Global component instance
     */
    OMPI_MODULE_DECLSPEC extern mca_coll_sm_component_t mca_coll_sm_component;

    /*
     * coll module functions
     */

    int mca_coll_sm_init_query(bool enable_progress_threads,
                               bool enable_mpi_threads);

    const struct mca_coll_base_module_1_0_0_t *
    mca_coll_sm_comm_query(struct ompi_communicator_t *comm, int *priority,
                           struct mca_coll_base_comm_t **data);

    int mca_coll_sm_comm_unquery(struct ompi_communicator_t *comm,
                                 struct mca_coll_base_comm_t *data);

    int mca_coll_sm_bootstrap_finalize(void);

    int mca_coll_sm_allgather_intra(void *sbuf, int scount, 
                                    struct ompi_datatype_t *sdtype, 
                                    void *rbuf, int rcount, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
    int mca_coll_sm_allgatherv_intra(void *sbuf, int scount, 
                                     struct ompi_datatype_t *sdtype, 
                                     void * rbuf, int *rcounts, int *disps, 
                                     struct ompi_datatype_t *rdtype, 
                                     struct ompi_communicator_t *comm);
    int mca_coll_sm_allreduce_intra(void *sbuf, void *rbuf, int count, 
                                    struct ompi_datatype_t *dtype, 
                                    struct ompi_op_t *op, 
                                    struct ompi_communicator_t *comm);
    int mca_coll_sm_alltoall_intra(void *sbuf, int scount, 
                                   struct ompi_datatype_t *sdtype, 
                                   void* rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, 
                                   struct ompi_communicator_t *comm);
    int mca_coll_sm_alltoallv_intra(void *sbuf, int *scounts, int *sdisps, 
                                    struct ompi_datatype_t *sdtype, 
                                    void *rbuf, int *rcounts, int *rdisps, 
                                    struct ompi_datatype_t *rdtype, 
                                    struct ompi_communicator_t *comm);
    int mca_coll_sm_alltoallw_intra(void *sbuf, int *scounts, int *sdisps, 
                                    struct ompi_datatype_t **sdtypes, 
                                    void *rbuf, int *rcounts, int *rdisps, 
                                    struct ompi_datatype_t **rdtypes, 
                                    struct ompi_communicator_t *comm);
    int mca_coll_sm_barrier_intra(struct ompi_communicator_t *comm);
    int mca_coll_sm_bcast_intra(void *buff, int count, 
                                struct ompi_datatype_t *datatype,
                                int root, 
                                struct ompi_communicator_t *comm);
    int mca_coll_sm_bcast_log_intra(void *buff, int count, 
                                    struct ompi_datatype_t *datatype, 
                                    int root, 
                                    struct ompi_communicator_t *comm);
    int mca_coll_sm_exscan_intra(void *sbuf, void *rbuf, int count, 
                                 struct ompi_datatype_t *dtype, 
                                 struct ompi_op_t *op, 
                                 struct ompi_communicator_t *comm);
    int mca_coll_sm_gather_intra(void *sbuf, int scount, 
                                 struct ompi_datatype_t *sdtype, void *rbuf, 
                                 int rcount, struct ompi_datatype_t *rdtype, 
                                 int root, struct ompi_communicator_t *comm);
    int mca_coll_sm_gatherv_intra(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int *rcounts, int *disps, 
                                  struct ompi_datatype_t *rdtype, int root, 
                                  struct ompi_communicator_t *comm);
    int mca_coll_sm_reduce_intra(void *sbuf, void* rbuf, int count, 
                                 struct ompi_datatype_t *dtype, 
                                 struct ompi_op_t *op, 
                                 int root,
                                 struct ompi_communicator_t *comm);
    int mca_coll_sm_reduce_log_intra(void *sbuf, void* rbuf, int count, 
                                     struct ompi_datatype_t *dtype, 
                                     struct ompi_op_t *op, 
                                     int root, 
                                     struct ompi_communicator_t *comm);
    int mca_coll_sm_reduce_scatter_intra(void *sbuf, void *rbuf, 
                                         int *rcounts, 
                                         struct ompi_datatype_t *dtype, 
                                         struct ompi_op_t *op, 
                                         struct ompi_communicator_t *comm);
    int mca_coll_sm_scan_intra(void *sbuf, void *rbuf, int count, 
                               struct ompi_datatype_t *dtype, 
                               struct ompi_op_t *op, 
                               struct ompi_communicator_t *comm);
    int mca_coll_sm_scatter_intra(void *sbuf, int scount, 
                                  struct ompi_datatype_t *sdtype, void *rbuf, 
                                  int rcount, struct ompi_datatype_t *rdtype, 
                                  int root, struct ompi_communicator_t *comm);
    int mca_coll_sm_scatterv_intra(void *sbuf, int *scounts, int *disps, 
                                   struct ompi_datatype_t *sdtype, 
                                   void* rbuf, int rcount, 
                                   struct ompi_datatype_t *rdtype, int root, 
                                   struct ompi_communicator_t *comm);
    
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

/**
 * Global variables used in the macros (essentially constants, so
 * these are thread safe)
 */
extern uint32_t mca_coll_sm_iov_size;


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

#endif /* MCA_COLL_SM_EXPORT_H */
