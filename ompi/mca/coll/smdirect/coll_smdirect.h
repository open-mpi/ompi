/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2022 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2009 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "ompi/mca/mca.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/common/sm/common_sm.h"
#include "ompi/mca/coll/coll.h"
#include "opal/mca/smsc/smsc.h"
#include "opal/datatype/opal_datatype_internal.h"

BEGIN_C_DECLS

/* Attempt to give some sort of progress / fairness if we're blocked
   in an sm collective for a long time: call opal_progress once in a
   great while.  Use a "goto" label for expdiency to exit loops. */
#define SPIN_CONDITION_MAX 100000
#define SPIN_CONDITION(cond)                          \
  do {                                                \
      int spin_cond_i = 0;                            \
      while (!(cond)) {                               \
          if (OPAL_UNLIKELY(++spin_cond_i == SPIN_CONDITION_MAX)) {  \
              opal_progress();                        \
              spin_cond_i = 0;                        \
          }                                           \
      }                                               \
  } while (0)

    /**
     * Structure to hold the sm coll component.  First it holds the
     * base coll component, and then holds a bunch of
     * sm-coll-component-specific stuff (e.g., current MCA param
     * values).
     */
    typedef struct mca_coll_smdirect_component_t {
        /** Base coll component */
        mca_coll_base_component_2_4_0_t super;

        /** MCA parameter: Priority of this component */
        int sm_priority;

        /** MCA parameter: Length of a cache line or page (in bytes) */
        int sm_control_size;

        /** MCA parameter: Fragment size for data */
        int sm_fragment_size;

        /** MCA parameter: Degree of tree for tree-based collectives */
        int sm_tree_degree;

        /******* end of MCA params ********/
    } mca_coll_smdirect_component_t;

    /**
     * Structure for representing a node in the tree
     */
    typedef struct mca_coll_smdirect_tree_node_t {
        /** Arbitrary ID number, starting from 0 */
        int mcstn_id;
        /** Pointer to parent, or NULL if root */
        struct mca_coll_smdirect_tree_node_t *mcstn_parent;
        /** Number of children, or 0 if a leaf */
        int mcstn_num_children;
        /** Pointer to an array of children, or NULL if 0 ==
            mcstn_num_children */
        struct mca_coll_smdirect_tree_node_t **mcstn_children;
    } mca_coll_smdirect_tree_node_t;

    /**
     * Simple structure comprising the "in use" flags.  Contains two
     * members: the number of processes that are currently using this
     * set of segments and the operation number of the current
     * operation.
     */
    typedef struct mca_coll_smdirect_in_use_flag_t {
        /** Number of processes currently using this set of
            segments */
        opal_atomic_uint32_t mcsiuf_num_procs_using;
        /** Must match data->mcb_count */
        volatile uint32_t mcsiuf_operation_count;
    } mca_coll_smdirect_in_use_flag_t;

    /**
     * Structure containing pointers to various arrays of data in the
     * per-communicator shmem data segment (one of these indexes a
     * single segment in the per-communicator shmem data segment).
     * Nothing is hard-coded because all the array lengths and
     * displacements of the pointers all depend on how many processes
     * are in the communicator.
     */
    typedef struct mca_coll_smdirect_data_index_t {
        /** Pointer to beginning of control data */
        uint32_t volatile *mcbmi_control;
        /** Pointer to beginning of message fragment data */
        char *mcbmi_data;
    } mca_coll_smdirect_data_index_t;

    typedef struct mca_coll_smdirect_procdata_t {
        /** the pointer to my local input buffer */
        void *mcsp_indata;

        /** the extent of my local input buffer */
        size_t mcsp_insize;

        /** id for my readily processed segments, non-atomically monotonically increasing */
        //volatile int32_t mcsp_segment_id;

        /** flag used to wait for sync with children across operations */
        mca_coll_smdirect_in_use_flag_t mcsp_op_flag;

        /** flag used to sync with children across segments */
        mca_coll_smdirect_in_use_flag_t mcsp_segment_down_flag;

        /** flag used to sync with parent across segments */
        mca_coll_smdirect_in_use_flag_t mcsp_segment_up_flag;

    } mca_coll_smdirect_procdata_t;

    typedef struct mca_coll_smdirect_peerdata_t {
      mca_smsc_endpoint_t *endpoint;
      void *mapping_ctx; // the context used to unmap
      void *mapping_ptr; // the local pointer to access the peer's memory
      uint32_t num_children; // number of children this peer has
      mca_coll_smdirect_procdata_t *procdata; // peer's proc data
    } mca_coll_smdirect_peerdata_t;

    /**
     * Structure for the sm coll module to hang off the communicator.
     * Contains communicator-specific information, including pointers
     * into the per-communicator shmem data data segment for this
     * comm's sm collective operations area.
     */
    typedef struct mca_coll_smdirect_comm_t {
        /* Meta data that we get back from the common mmap allocation
           function */
        mca_common_sm_module_t *sm_bootstrap_meta;

        /* My local process' information accessible by other peers */
        mca_coll_smdirect_procdata_t *procdata;

        /* Operation count, i.e., which operation to execute */
        uint32_t mcb_operation_count;

        /** Array of graph nodes representing the tree used for
            communications */
        mca_coll_smdirect_tree_node_t *mcb_tree;

        /** Array of peers to use in operations */
        mca_coll_smdirect_peerdata_t *peerdata;

        /* communicator size */
        int comm_size;

        /** endpoint cache */
        mca_smsc_endpoint_t **endpoints;
    } mca_coll_smdirect_comm_t;

    /** Coll sm module */
    typedef struct mca_coll_smdirect_module_t {
        /** Base module */
        mca_coll_base_module_t super;

        /* Whether this module has been lazily initialized or not yet */
        bool enabled;

        /* Data that hangs off the communicator */
        mca_coll_smdirect_comm_t *sm_comm_data;

        /* Underlying reduce function and module */
        mca_coll_base_module_reduce_fn_t previous_reduce;
        mca_coll_base_module_t *previous_reduce_module;
    } mca_coll_smdirect_module_t;
    OBJ_CLASS_DECLARATION(mca_coll_smdirect_module_t);

    /**
     * Global component instance
     */
    OMPI_MODULE_DECLSPEC extern mca_coll_smdirect_component_t mca_coll_smdirect_component;

    /*
     * coll module functions
     */
    int mca_coll_smdirect_init_query(bool enable_progress_threads,
              bool enable_mpi_threads);

    mca_coll_base_module_t *
    mca_coll_smdirect_comm_query(struct ompi_communicator_t *comm, int *priority);

    /* Lazily enable a module (since it involves expensive/slow mmap
       allocation, etc.) */
    int ompi_coll_smdirect_lazy_enable(mca_coll_base_module_t *module,
                                  struct ompi_communicator_t *comm);

    int mca_coll_smdirect_allgather_intra(const void *sbuf, int scount,
            struct ompi_datatype_t *sdtype,
            void *rbuf, int rcount,
            struct ompi_datatype_t *rdtype,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_smdirect_allgatherv_intra(const void *sbuf, int scount,
            struct ompi_datatype_t *sdtype,
            void * rbuf, const int *rcounts, const int *disps,
            struct ompi_datatype_t *rdtype,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_allreduce_intra(const void *sbuf, void *rbuf, int count,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_alltoall_intra(const void *sbuf, int scount,
            struct ompi_datatype_t *sdtype,
            void* rbuf, int rcount,
            struct ompi_datatype_t *rdtype,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_alltoallv_intra(const void *sbuf, const int *scounts, const int *sdisps,
            struct ompi_datatype_t *sdtype,
            void *rbuf, const int *rcounts, const int *rdisps,
            struct ompi_datatype_t *rdtype,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_alltoallw_intra(const void *sbuf, const int *scounts, const int *sdisps,
            struct ompi_datatype_t * const *sdtypes,
            void *rbuf, const int *rcounts, const int *rdisps,
            struct ompi_datatype_t * const *rdtypes,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_barrier_intra(struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_bcast_intra(void *buff, int count,
            struct ompi_datatype_t *datatype,
            int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_bcast_log_intra(void *buff, int count,
            struct ompi_datatype_t *datatype,
            int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_exscan_intra(const void *sbuf, void *rbuf, int count,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_gather_intra(void *sbuf, int scount,
            struct ompi_datatype_t *sdtype, void *rbuf,
            int rcount, struct ompi_datatype_t *rdtype,
            int root, struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_gatherv_intra(void *sbuf, int scount,
            struct ompi_datatype_t *sdtype, void *rbuf,
            int *rcounts, int *disps,
            struct ompi_datatype_t *rdtype, int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_reduce_intra(const void *sbuf, void* rbuf, int count,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_reduce_log_intra(const void *sbuf, void* rbuf, int count,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_reduce_scatter_intra(const void *sbuf, void *rbuf,
            int *rcounts,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_scan_intra(const void *sbuf, void *rbuf, int count,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_scatter_intra(const void *sbuf, int scount,
            struct ompi_datatype_t *sdtype, void *rbuf,
            int rcount, struct ompi_datatype_t *rdtype,
            int root, struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);
    int mca_coll_smdirect_scatterv_intra(const void *sbuf, const int *scounts, const int *disps,
            struct ompi_datatype_t *sdtype,
            void* rbuf, int rcount,
            struct ompi_datatype_t *rdtype, int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    /**
     * Returns the size required to serialize the given datatype into a (shared)
     * memory region.
     */
    static inline
    size_t mca_coll_smdirect_serialize_ddt_size(opal_datatype_t *datatype) {
        return sizeof(*datatype) + datatype->opt_desc.used*sizeof(dt_elem_desc_t);
    }

    /* Serialize a datatype description into a provided (shared) memory region.
     * All internal pointers will point into the provided memory region.
     */
    static inline
    void mca_coll_smdirect_serialize_ddt(void* buf, opal_datatype_t *datatype) {
        unsigned char *dtype_store = (unsigned char*)buf;
        /* put the OPAL datatype right after our procdata */
        memcpy(dtype_store, &datatype->super, sizeof(opal_datatype_t));
        /* put the datatype description elements after it */
        dtype_store += sizeof(opal_datatype_t);
        opal_datatype_t *dtype_copy = (opal_datatype_t*)dtype_store;
        memcpy(dtype_store, datatype->desc.desc, datatype->desc.used*sizeof(dt_elem_desc_t));
        /* adjust datatype pointers to use the opt_desc */
        dtype_copy->desc = datatype->desc;
        dtype_copy->desc.desc = (dt_elem_desc_t*)dtype_store;
        dtype_copy->opt_desc = dtype_copy->desc;
    }

/**
 * Macro to wait for the in-use flag to become idle (used by the root)
 */
#define FLAG_WAIT_FOR_IDLE(flag) \
    SPIN_CONDITION(0 == (flag)->mcsiuf_num_procs_using)

/**
 * Macro to wait for a flag to indicate that it's ready for this
 * operation (used by non-root processes to know when FLAG_SET() has
 * been called)
 */
#define FLAG_WAIT_FOR_OP(flag, op) \
    SPIN_CONDITION((op) == (flag)->mcsiuf_operation_count)

/**
 * Macro to set an in-use flag with relevant data to claim it
 */
#define FLAG_RETAIN(flag, num_procs, op_count) \
    (flag)->mcsiuf_num_procs_using = (num_procs); \
    opal_atomic_wmb(); \
    (flag)->mcsiuf_operation_count = (op_count)

/**
 * Macro to release an in-use flag from this process
 */
#define FLAG_RELEASE(flag) \
    opal_atomic_add(&(flag)->mcsiuf_num_procs_using, -1)

/**
 * Macro to non-atomically reset an in-use flag from this process
 */
#define FLAG_RESET(flag) \
    (flag)->mcsiuf_num_procs_using = 0

/**
 * Macro to memcpy a fragment between one shared segment and another
 */
#define COPY_FRAGMENT_BETWEEN(src_rank, dest_rank, index, len) \
    memcpy(((index)->mcbmi_data + \
            ((dest_rank) * mca_coll_smdirect_component.sm_fragment_size)), \
           ((index)->mcbmi_data + \
            ((src_rank) * \
             mca_coll_smdirect_component.sm_fragment_size)), \
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
               (mca_coll_smdirect_component.sm_control_size * \
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
                                   ((rank) * mca_coll_smdirect_component.sm_control_size))); \
        SPIN_CONDITION(0 != *ptr); \
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
      (mca_coll_smdirect_component.sm_control_size * \
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
                                 (mca_coll_smdirect_component.sm_control_size * \
                                  (parent_rank)))) + child_rank; \
        SPIN_CONDITION(0 != *ptr); \
        (value) = *ptr; \
        *ptr = 0; \
    } while (0)

END_C_DECLS

#endif /* MCA_COLL_SM_EXPORT_H */
