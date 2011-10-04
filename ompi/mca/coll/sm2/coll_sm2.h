/* 
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Cisco Systems, Inc.  All rights reserved.
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
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/common/sm/common_sm_mmap.h"
#include "ompi/request/request.h"

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
 * - At least two banks of memory will be used
 * - Each bank of memory will have M buffers (or segments)
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
        mca_coll_base_component_2_0_0_t super;

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
        int sm2_data_alignment;

        /** MCA parameter: number of memory banks */
        int sm2_num_mem_banks;

        /** MCA parameter: number of regions per memory bank */
        int sm2_num_regions_per_bank;

        /** MCA parameter: order of buffer management barrier tree */
        int order_barrier_tree;

        /** MCA parameter: order of reduction tree */
        int order_reduction_tree;

        /** MCA parameter: order of fan-out read tree */
        int order_fanout_read_tree;

        /** MCA paramenter:  number of polling loops to run while waiting
         *  for children or parent to complete their work
         */
        int n_poll_loops;

        /** MCA parameter:  message size cutoff for switching between 
         *  short and long protocol
         */
        size_t short_message_size;

        /*
         * Parameters to control methods used
         */
        /** MCA parameter:  method to force a given barrier method to be used.
         *  0 - FANIN_FAN_OUT_BARRIER_FN
         *  1 - RECURSIVE_DOUBLING_BARRIER_FN
         */
        int force_barrier;

        /** MCA parameter:  method to force a given reduce method to be used.
         * 0 - FANIN_FAN_OUT_REDUCE_FN
         * 1 - REDUCE_SCATTER_GATHER_FN
         */
        int force_reduce;

        /** MCA parameter: method to force a given allreduce method to be used.
         * 0 - FANIN_FANOUT_ALLREDUCE_FN
         * 1 - REDUCE_SCATTER_ALLGATHER_FN
         */
        int force_allreduce;

    };

    /**
     * Convenience typedef
     */
    typedef struct mca_coll_sm2_component_t mca_coll_sm2_component_t;

    /* 
     * Implemented function index list 
     */

    /* barrier */
    enum{
        FANIN_FAN_OUT_BARRIER_FN,
        RECURSIVE_DOUBLING_BARRIER_FN,
        N_BARRIER_FNS
    };

    /* reduce */
    enum{
        FANIN_REDUCE_FN,
        REDUCE_SCATTER_GATHER_FN,
        N_REDUCE_FNS
    };
    enum{
        SHORT_DATA_FN_REDUCE,
        LONG_DATA_FN_REDUCE,
        N_REDUCE_FNS_USED
    };

    /* all-reduce */
    enum{
        FANIN_FANOUT_ALLREDUCE_FN,
        REDUCE_SCATTER_ALLGATHER_FN,
        N_ALLREDUCE_FNS
    };
    enum{
        SHORT_DATA_FN_ALLREDUCE,
        LONG_DATA_FN_ALLREDUCE,
        N_ALLREDUCE_FNS_USED
    };


    /* enum for node type */
    enum{
        ROOT_NODE,
        LEAF_NODE,
        INTERIOR_NODE
    };


    /*
     * N-order tree node description
     */
    struct tree_node_t {
        /* my rank within the group */
        int my_rank;
        /* my node type - root, leaf, or interior */
        int my_node_type;
        /* number of nodes in the tree */
        int tree_size;
        /* number of parents (0/1) */
        int n_parents;
        /* number of children */
        int n_children;
        /* parent rank within the group */
        int parent_rank;
        /* chidren ranks within the group */
        int *children_ranks;
    };
    typedef struct tree_node_t tree_node_t;

    /*
     * Pair-wise data exchange
     */
    /* enum for node type */
    enum{
        EXCHANGE_NODE,
        EXTRA_NODE
    };

    struct pair_exchange_node_t {

        /* number of nodes this node will exchange data with */
        int n_exchanges;

        /* ranks of nodes involved in data exchnge */
        int *rank_exchanges;

        /* number of extra sources of data - outside largest power of 2 in
         *  this group */
        int n_extra_sources;
        
        /* rank of the extra source */
        int rank_extra_source;

        /* number of tags needed per stripe */
        int n_tags;

        /* log 2 of largest full power of 2 for this node set */
        int log_2;

        /* largest power of 2 that fits in this group */
        int n_largest_pow_2;

        /* node type */
        int node_type;

    };
    typedef struct pair_exchange_node_t pair_exchange_node_t;

    /*
     * Barrier request objects
     */

    /* shared memory data strucutures */
    struct mca_coll_sm2_nb_request_process_shared_mem_t {
        /* flag used to indicate the status of this memory region */
        volatile long long flag;
        volatile long long index;

        /* pading */
        /* Note: need to change this so it takes less memory */
        char padding[2*opal_cache_line_size-2*sizeof(long long)];
    };

    typedef struct mca_coll_sm2_nb_request_process_shared_mem_t
        mca_coll_sm2_nb_request_process_shared_mem_t;

    /* enum for phase at which the nb barrier is in */
    enum{
        NB_BARRIER_INACTIVE,
        NB_BARRIER_FAN_IN,
        NB_BARRIER_FAN_OUT,
        /* done and not started are the same for all practicle
         * purposes, as the init funtion always sets this flag
         */
        NB_BARRIER_DONE
    };

    /* forward declartion */
    struct mca_coll_sm2_module_t;

    /*
     * shared memory region descriptor
     */
    struct sm_memory_region_desc_t {

        /* pointer to control structures */
        volatile mca_coll_sm2_nb_request_process_shared_mem_t *control_region;

        /* pointer to data segment, and lower half of data segment */
        volatile char *data_segment;

    };
    typedef struct sm_memory_region_desc_t sm_memory_region_desc_t;

    /*
     * Shared memory buffer management strcucture
     */
    struct sm_work_buffer_t {
        /* pointer to segment base */
        volatile char * base_segment_address;

        /* description of how the memory segment is mapped on
         *   a per process basis
         */
        sm_memory_region_desc_t *proc_memory;

        /*
         * bank index
         */
        int bank_index;

        /*
         * first buffer in the bank - if the barrier corresponding to
         *   this bank is active when trying to allocate this buffer, 
         *   can't proceed until it complete
         */
        int index_first_buffer_in_bank;

        /* last buffer in the bank - nb barrier is started after this
         *   buffer is freed.
         */
        int index_last_buffer_in_bank;
    };
    typedef struct sm_work_buffer_t sm_work_buffer_t;

    /* process private barrier request object */
    struct mca_coll_sm2_nb_request_process_private_mem_t {
        struct ompi_request_t super;
        /* tag that will be used as unique barrier identifier */
        long long tag;

        /* barrier phase */
        int sm2_barrier_phase;
        
        /* shared memory strucuture index - will be flip-flopping between structures */
        int sm_index;

        /* this processes base address of the barrier shared memory region */
        mca_coll_sm2_nb_request_process_shared_mem_t *barrier_base_address[2];

        /* module pointer */
        struct mca_coll_sm2_module_t *coll_sm2_module;

    };
    typedef struct mca_coll_sm2_nb_request_process_private_mem_t 
        mca_coll_sm2_nb_request_process_private_mem_t;

        /* debug */
#define BARRIER_BANK_LIST_SIZE 32
        /* end debug */
    struct mca_coll_sm2_module_t {
        /* base structure */
        mca_coll_base_module_t super;

        /* size */
        int comm_size;

        /* Shared Memory file name */
        char *coll_sm2_file_name;

        /* size of shared memory backing file */
        size_t size_sm2_backing_file;

        /* Memory pointer to shared file */
        char *shared_memory_region;

        /* size of memory banks control regions */
        size_t size_mem_banks_ctl_region;

        /* Pointer to the collective buffers */
        char *collective_buffer_region;

        /* size of collective buffer region */
        size_t size_of_collective_buffer_region;

        /* pointer to memory for blocking collectives */
        char *sm_blocking_barrier_region;

        /* size of memory for blocking collectives */
        size_t size_of_blocking_barrier_region;

        /* per proc size of memory for blocking collectives */
        size_t per_proc_size_of_blocking_barrier_region;

        /* index of blocking barrier memory region to use */
        int index_blocking_barrier_memory_bank;

        /* pointers to blocking memory control regions */
        volatile mca_coll_sm2_nb_request_process_shared_mem_t ***ctl_blocking_barrier;

        /* description of allocated temp buffers - one struct per
         * buffer.  Each buffer has space "owned" by each process
         * in the group.
         */
        sm_work_buffer_t *sm_buffer_descriptor;

        /* size of memory region, per process, for memory bank management */
        size_t sm2_size_management_region_per_proc;

        /* size of each memory segment */
        size_t segment_size;

        /* size, per process, of each memory segment */
        size_t segement_size_per_process;

        /* size, per process and segment , of control region */
        size_t ctl_memory_per_proc_per_segment;

        /* size, per process and segment , of data region */
        size_t data_memory_per_proc_per_segment;

        /* data strucutures used to manage the memory buffers */
        long long num_nb_barriers_started;
        long long num_nb_barriers_completed;

        /* number of memory banks */
        int sm2_module_num_memory_banks;

        /* number of buffers per memory bank */
        int sm2_module_num_regions_per_bank;

        /* total number of working buffers */
        int sm2_module_num_buffers;

        /* allocated buffer index - local counter */
        int sm2_allocated_buffer_index;

        /* freed allocated buffer index - local counter */
        int sm2_freed_buffer_index;

        /* communicator - there is a one-to-one association between
         *  the communicator and the module
         */
        struct ompi_communicator_t *module_comm;

        /* non-blocking barrier strcutres used for mangeing the shared
         * buffers */
        tree_node_t sm_buffer_mgmt_barrier_tree;

        /* request objects for the non-blocking barrier */
        mca_coll_sm2_nb_request_process_private_mem_t *barrier_request;

        /* barrier request to progress */
        int current_request_index;

        /* unique tag used for non-blocking collectives */
        long long nb_barrier_tag;

        /* multinumial reduction tree */
        tree_node_t *reduction_tree;

        /* multinumial fan-out read tree */
        tree_node_t *fanout_read_tree;

        /* recursive-doubling tree node */
        pair_exchange_node_t recursive_doubling_tree;

        /* number of polling loops to run while waiting
         *  for children or parent to complete their work
         */
        int n_poll_loops;

        /* collective tag */
        long long collective_tag;

        /* scratch space - one int per process */
        int *scratch_space;

        /* message size cutoff for switching between short and long
         *   protocol
         */
        size_t short_message_size;

        /*
         * flag indicating if have socket layout for the procs
         */
        int have_socket_information;

        /*
         * socket index
         */
        int *socket_index;

        /*
         * number of processes per socket
         */
        int *n_procs_per_socket;

        /*
         * sockets in use
         */
        int *sockets_in_use;

        /*
         * index of my socekt within the list of sockets in use
         */
        int my_socket_group;
            
        /*
         * number of processes per socket
         */
        int **list_of_ranks_per_socket;

        /*
         * function table for variants of a given collective
         *   function.
         */
        mca_coll_base_module_barrier_fn_t barrier_functions[N_BARRIER_FNS];
        mca_coll_base_module_reduce_fn_t list_reduce_functions[N_REDUCE_FNS];
        mca_coll_base_module_reduce_fn_t reduce_functions[N_REDUCE_FNS_USED];
        mca_coll_base_module_allreduce_fn_t 
            list_allreduce_functions[N_ALLREDUCE_FNS];
        mca_coll_base_module_allreduce_fn_t 
            allreduce_functions[N_ALLREDUCE_FNS_USED];


    };

    typedef struct mca_coll_sm2_module_t mca_coll_sm2_module_t;
    OBJ_CLASS_DECLARATION(mca_coll_sm2_module_t);

    /*
     * struct for manageing the allreduce pipeline.
     */
     struct mca_coll_sm2_module_allreduce_pipeline_t {
         /* pointer to shared temporary working buffer */
         sm_work_buffer_t *shared_buffer;

         /* cached rank */
         int my_rank;

         /* cached reduction node */
         tree_node_t *my_reduction_node;

         /* cached fanout tree */
         tree_node_t *my_fanout_read_tree;


         /* staus of the buffer - determines what next to do
          *   with this data
          */
         int status;

         /*
          * number of child loops completed - needed for
          *  async progress
          */
         int n_child_loops_completed;

         /*
          * number of data-type elements to process
          */
         int count_this_stripe;

         /*
          * offset into the data type buffer, in units of data-types
          */
         int count_processed;

         /*
          * tag
          */
         long long tag;
     };
     typedef struct mca_coll_sm2_module_allreduce_pipeline_t 
         mca_coll_sm2_module_allreduce_pipeline_t;
     OBJ_CLASS_DECLARATION(mca_coll_sm2_module_allreduce_pipeline_t);

     enum {
         BUFFER_AVAILABLE,
         STARTED,
         FANIN,
         FANOUT
     };


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
    mca_coll_base_module_t *
    mca_coll_sm2_comm_query(struct ompi_communicator_t *comm, int *priority);

    /* setup an multi-nomial tree - for each node in the tree
     *  this returns it's parent, and it's children 
     */
    int setup_multinomial_tree(int tree_order, int num_nodes,
                    tree_node_t *tree_nodes);

    /* setup recursive doubleing tree node */
    int setup_recursive_doubling_tree_node(int num_nodes, int node_rank,
            pair_exchange_node_t *tree_node);

    /* non-blocking barrier - init function */
    int mca_coll_sm2_nbbarrier_intra(struct ompi_communicator_t *comm,
            mca_coll_sm2_nb_request_process_private_mem_t *request,
            mca_coll_base_module_t *module);

    /* non-blocking barrier - completion function */
    int mca_coll_sm2_nbbarrier_intra_progress(struct ompi_communicator_t *comm,
            mca_coll_sm2_nb_request_process_private_mem_t *request,
            mca_coll_base_module_t *module);

    /* allocate working buffer */
    sm_work_buffer_t *alloc_sm2_shared_buffer(mca_coll_sm2_module_t *module);

    /* free working buffer - it is assumed that buffers are released in
     * the order they are allocated.  We can assume this because each
     * communiator will have only one outstanding collective at a given
     * time, and we ensure that operations are completed in order. */
    int free_sm2_shared_buffer(mca_coll_sm2_module_t *module);

    /**
     * Shared memory blocking allreduce.
     */
    int mca_coll_sm2_allreduce_intra(void *sbuf, void *rbuf, int count,
            struct ompi_datatype_t *dtype,
            struct ompi_op_t *op,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_sm2_allreduce_intra_reducescatter_allgather(
            void *sbuf, void *rbuf, int count, struct ompi_datatype_t *dtype,
            struct ompi_op_t *op, struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_sm2_allreduce_intra_fanin_fanout(void *sbuf, void *rbuf, 
            int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op,
            struct ompi_communicator_t *comm, 
            mca_coll_base_module_t *module);

    /**
     * Shared memory blocking reduce
     */
    int mca_coll_sm2_reduce_intra(void *sbuf, void *rbuf, int count,
            struct ompi_datatype_t *dtype, struct ompi_op_t *op,
            int root, struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_sm2_reduce_intra_reducescatter_gather(void *sbuf, void *rbuf, 
            int count, struct ompi_datatype_t *dtype, struct ompi_op_t *op,
            int root, struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_sm2_reduce_intra_fanin(void *sbuf, void *rbuf, int count,
            struct ompi_datatype_t *dtype, struct ompi_op_t *op,
            int root, struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    /**
     * Shared memory blocking broadcast.
     */
    int mca_coll_sm2_bcast_intra(void *buf, int count,
            struct ompi_datatype_t *dtype, int root,
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    /**
      * Shared memory blocking barrier
      */
    int mca_coll_sm2_barrier_intra( struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_sm2_barrier_intra_fanin_fanout( 
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

    int mca_coll_sm2_barrier_intra_recursive_doubling(
            struct ompi_communicator_t *comm,
            mca_coll_base_module_t *module);

END_C_DECLS

#endif /* MCA_COLL_SM2_EXPORT_H */
