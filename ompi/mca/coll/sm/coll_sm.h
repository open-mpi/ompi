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
/**
 * @file
 */

#ifndef MCA_COLL_SM_EXPORT_H
#define MCA_COLL_SM_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"

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
        size_t smbcs_data_mpool_offset;

        /** Number of segments in the data mpool area for this
            communicator */
        int smbcs_communicator_num_segments;

        /** Number of processes in this communicator who have seen
            this value already. */
        int smbcs_count;
    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm_bootstrap_comm_setup_t 
        mca_coll_sm_bootstrap_comm_setup_t;

    /**
     * Extension to the control structure in the bootstrap mmap file
     */
    struct mca_coll_sm_bootstrap_header_extension_t {
        /** upper-level control structure */
        mca_common_sm_file_header_t super;

        /** Number of segments in the bootstrap mmap file */
        int smbhe_num_segments;

        /** Pointer to the start of the segments in the bootstrap area
            (map->seg_data only points to just beyond the
            mca_common_sm_file_header_t) */
        mca_coll_sm_bootstrap_comm_setup_t *smbhe_segments;

        /** Pointer to array containing smhe_num_segments CIDs for use
            in bootstrap phase -- will always point immediately after
            the end of this struct (i.e., still within this header,
            but since it's variable size (set by MCA param), we can't
            just have it here in the struct.  Bonk). */
        uint32_t *smbhe_cids;
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

        /** MCA parameter: Number of segments for each communicator in
            the data mpool */
        int sm_communicator_num_segments;

        /** MCA parameter: Fragment size for data */
        int sm_fragment_size;

        /** MCA parameter: Degree of tree for tree-based collectives */
        int sm_tree_degree;

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
     * Structure containing pointers to various arrays of data in the
     * data mpool area (one of these indexes a single segment in the
     * data mpool).  Nothing is hard-coded because all the array
     * lengths and displacements of the pointers all depend on how
     * many processes are in the communicator.
     */
    struct mca_coll_base_mpool_index_t {
        /** Pointer to beginning of control data */
        uint32_t *mcbmi_control;
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
        /** Number of segments in this comm's area in the data mpool */
        int mcb_mpool_num_segments;

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

        /** Array of indexes into the mpool area for control and data
            fragment passing (containing pointers to each segments
            control and data areas). */
        mca_coll_base_mpool_index_t *mcb_mpool_index;

        /** Array of graph nodes representing the tree used for
            communications */
        mca_coll_sm_tree_node_t *mcb_tree;

        /** Operation number (i.e., which segment number to use) */
        int mcb_operation_count;
    };
    /**
     * Convenience typedef
     */
    typedef struct mca_coll_base_comm_t mca_coll_base_comm_t;
        
    
    /**
     * Global component instance
     */
    extern mca_coll_sm_component_t mca_coll_sm_component;


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

#endif /* MCA_COLL_SM_EXPORT_H */
