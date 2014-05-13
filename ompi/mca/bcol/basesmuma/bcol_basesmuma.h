/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2009-2013 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * Copyright (c) 2013-2014 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#ifndef MCA_BCOL_basesmuma_EXPORT_H
#define MCA_BCOL_basesmuma_EXPORT_H

#include "ompi_config.h"
#include "ompi/mca/bcol/bcol.h"
#include "ompi/mca/bcol/base/base.h"
#include "ompi/mca/mpool/mpool.h"
#include "ompi/request/request.h"
#include "ompi/proc/proc.h"
#include "ompi/patterns/net/netpatterns.h"

#include "opal/mca/mca.h"
#include "opal/util/arch.h"
#include "opal/util/argv.h"
#include "opal/datatype/opal_datatype.h"
#include "opal/util/output.h"

#include "bcol_basesmuma_smcm.h"
BEGIN_C_DECLS

struct list_data_t  {
    opal_list_item_t super;
    void *data;
};
typedef struct list_data_t list_data_t;
OBJ_CLASS_DECLARATION(list_data_t);

/*
 * Macro's for manipulating the 64 bit shared memory control bits.
 * The 64 bit field is devided into 4 bit fields
 *
 *   | 48-63: src  |  32-47: index |  16-31: flag |  0-15: sequence number |
 *
 * Only the low 16 bits of the sequence number will be put in the header
 * space.  We will use the fact that the use of the shared buffers is
 * synchronous, and get the upper 48 bits from the local process space.
 */

#define BASESMUMA_CACHE_LINE_SIZE 128

#define SHIFT_UP   <<
#define SHIFT_DOWN >>

#define SEQ_WIDTH  16
#define SEQ_BASE    0
#define FIELD_SEQ_MASK   ( ( 1 SHIFT_UP SEQ_WIDTH ) - 1 )
#define INPLACE_SEQ_MASK ( (int64_t)FIELD_SEQ_MASK SHIFT_UP SEQ_BASE)

#define FLAG_WIDTH 16
#define FLAG_BASE  16
#define FIELD_FLAG_MASK   ( ( 1 SHIFT_UP FLAG_WIDTH ) - 1 )
#define INPLACE_FLAG_MASK ( (int64_t)FIELD_FLAG_MASK SHIFT_UP FLAG_BASE)

#define INDX_WIDTH 16
#define INDX_BASE  32
#define FIELD_INDX_MASK   ( ( 1 SHIFT_UP INDX_WIDTH ) - 1 )
#define INPLACE_INDX_MASK ( (int64_t)FIELD_INDX_MASK SHIFT_UP INDX_BASE)

#define SRC_WIDTH  16
#define SRC_BASE   48
#define FIELD_SRC_MASK   ( ( 1 SHIFT_UP SRC_WIDTH ) - 1 )
#define INPLACE_SRC_MASK ( (int64_t)FIELD_SRC_MASK SHIFT_UP SRC_BASE)
/*int64_t INPLACE_SRC_MASK= ((int64_t)FIELD_SRC_MASK SHIFT_UP SRC_BASE); */


#define EXTRACT_FLAG(INPUT, OUTPUT, OUTPUT_TYPE, FIELD_BASE, FIELD_MASK) \
    OUTPUT = (OUTPUT_TYPE) ( (INPUT SHIFT_DOWN FIELD_BASE ) & FIELD_MASK )

#define STORE_FLAG(INPUT, OUTPUT, INPUT_TYPE, OUTPUT_TYPE, FIELD_BASE, INPLACE_FIELD_MASK ) \
    OUTPUT =                                                            \
        (                                                               \
         /* 3 */                                                        \
         (                                                              \
          /* 2 */                                                       \
          (                                                             \
           /* 1 - shift the input field to the proper location */       \
           (OUTPUT_TYPE)(                                               \
                         ((OUTPUT_TYPE)((INPUT_TYPE) (INPUT)))          \
                         SHIFT_UP FIELD_BASE )                          \
           /* mask off the extra bits */                                \
           & ((OUTPUT_TYPE)INPLACE_FIELD_MASK)                          \
                                                                   )    \
          /* store back to the OUTPUT field, w/o destroying other fields */ \
                                                                          ) | OUTPUT \
                                                                         )

/**
 * Structure to hold the basic shared memory bcoll component.
 */
struct mca_bcol_basesmuma_component_t {
    /** Base coll component */
    mca_bcol_base_component_2_0_0_t super;

    /* management data for collectives with no user data */

    /** MCA parameter: number of memory banks */
    int basesmuma_num_mem_banks;

    /** MCA parameter: number of regions per memory bank */
    int basesmuma_num_regions_per_bank;

    /** MCA parameter: Number of simultaneous groups supported */
    int n_groups_supported;

    /* management data for collectives with user data (ud) - the memory
     * is actually obtained at the ML level
     */

    /** MCA paramenter:  number of polling loops to run while waiting
     *  for children or parent to complete their work
     */
    int n_poll_loops;

    /* mpool size */
    size_t mpool_size;


    /* mpool inited - will use this to test whether or not the
     * shared memory has been inited
     */
    bool mpool_inited;

    /* shared memory control buffer - the control structures reside
     *   in shared memory */
    bcol_basesmuma_smcm_mmap_t *sm_ctl_structs;

    /* shared memory payload buffer
     */
    bcol_basesmuma_smcm_mmap_t *sm_payload_structs;

    /*
     * list of shared memory control structures
     */
    opal_list_t ctl_structures;


    /** opal list in which the list of peers that I am "connected" to is stored
     */
    opal_list_t sm_connections_list;

    /* opal list in which the list of payload peers that I am "connected" to
     * is stored
     */
    opal_list_t sm_payload_connections_list;

    /*
     * list of non-blocking admin barriers to progress */
    opal_mutex_t nb_admin_barriers_mutex;
    opal_list_t nb_admin_barriers;

    /*
     * order of fan-in tree
     */
    int radix_fanin;

    /*
     * order of fan-out tree
     */
    int radix_fanout;

    /*
     * Order of read tree
     */
    int radix_read_tree;

    /*
     * order of reduction fan-out tree
     */
    int order_reduction_tree;

    /*
     * K-nomial tree radix
     */
    int k_nomial_radix;

    /*
     * K-ary scatter tree radix
     */
    int scatter_kary_radix;

    /*
     * number of polling loops
     */
    int num_to_probe;

    /*
     * Portals addressing info
     * void*: because wanted to keep portal library dependencies
     * as local as possible
     */
    void *portals_info;
    bool portals_init;

    /*
     * verbosity level
     */
    int verbose;

    /*
     * control file name base string
     */
    char *clt_base_fname;

    /*
     * data file name base string
     */
    char *payload_base_fname;

    /*
     * shared memory scratch space.  This is mapped at the end of the
     * segement of memory holding the control structures.
     */
    char *my_scratch_shared_memory;

    /*
     * size of scratch memory
     */
    size_t my_scratch_shared_memory_size;

    /* the offset will be the same for all ranks */
    size_t scratch_offset_from_base_ctl_file;
};

static inline int mca_bcol_basesmuma_err(const char* fmt, ...)
{
    va_list list;
    int ret;

    va_start(list, fmt);
    ret = vfprintf(stderr, fmt, list);
    va_end(list);
    return ret;
}

#if OPAL_ENABLE_DEBUG
#define BASESMUMA_VERBOSE(level, args)                                  \
    do {                                                                \
        if(mca_bcol_basesmuma_component.verbose >= level) {             \
            mca_bcol_basesmuma_err("[%s]%s[%s:%d:%s] BCOL-BASESMUMA ",  \
                                   ompi_process_info.nodename,          \
                                   OMPI_NAME_PRINT(OMPI_PROC_MY_NAME),  \
                                   __FILE__, __LINE__, __func__);       \
            mca_bcol_basesmuma_err args;                                \
            mca_bcol_basesmuma_err("\n");                               \
        }                                                               \
    } while(0)
#else
#define BASESMUMA_VERBOSE(level, args)
#endif


/**
 * Convenience typedef */
typedef struct mca_bcol_basesmuma_component_t mca_bcol_basesmuma_component_t;

#if 0
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

    /* my rank within the group */
    int my_rank;

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
#endif
/*
 * descriptor for managing the admin nonblocking barrier routine.
 *   This is an sm internal routine, and assumes only 1 outstanding
 *   nb-barrier collective call per block.
 */
/* forward declarations */
struct mca_bcol_basesmuma_module_t;
struct sm_buffer_mgmt;

struct sm_nbbar_desc_t {
    /* make sure we can put this on a list */
    opal_list_item_t super;

    /* phase of the collective operation - needed to know how to continue
     * progressing the nb-barrier */
    int collective_phase;

    /* iteration to continue at */
    int recursive_dbl_iteration;

    /* pointer to the collective module this is associated with */
    struct mca_bcol_basesmuma_module_t *sm_module;

    /* pointer to payload/control structs buffers */
    struct sm_buffer_mgmt *coll_buff;

    /* pool index */
    int pool_index;

    /* pointer to the mca_bcol_base_memory_block_desc_t structure
     * that is actually managing this registration.
     * This is meaningful when these control structures
     * are used in conjunction with the user payload
     * data that is allocated at the ml level.
     */
    void *ml_memory_block_descriptor;

};
typedef struct sm_nbbar_desc_t sm_nbbar_desc_t;

/*
 * Barrier request objects
 */

/* shared memory data strucutures */
struct mca_bcol_basesmuma_nb_request_process_shared_mem_t {
    volatile uint64_t coll_index;
    /* flag used to indicate the status of this memory region */
    volatile uint64_t flag;
    volatile uint64_t index;

    /* pading */
    /* Note: need to change this so it takes less memory */
    char padding[BASESMUMA_CACHE_LINE_SIZE-3*sizeof(uint64_t)];
};

typedef struct mca_bcol_basesmuma_nb_request_process_shared_mem_t
mca_bcol_basesmuma_nb_request_process_shared_mem_t;

/* enum for phase at which the nb barrier is in */
enum{
    NB_BARRIER_INACTIVE,

    /* fan-in/fan-out */
    NB_BARRIER_FAN_IN,
    NB_BARRIER_FAN_OUT,

    /* recursive doubling */
    NB_PRE_PHASE,
    NB_RECURSIVE_DOUBLING,
    NB_POST_PHASE,

    /* done and not started are the same for all practicle
     * purposes, as the init funtion always sets this flag
     */
    NB_BARRIER_DONE
};



/* forward declartion */
struct mca_bcol_basesmuma_module_t;

struct mca_basesmuma_ctrl_4_hdl_t {
    int fd;
    bool status;
    volatile char buf[128];
    /*volatile char buf[OPAL_PATH_MAX];*/
};
typedef struct mca_basesmuma_ctrl_4_hdl_t mca_basesmuma_ctrl_4_hdl_t;

/* control segment for shared memory */
struct mca_bcol_basesmuma_ctl_struct_t {
    /* collective identifier */
    volatile int64_t sequence_number;
    volatile int64_t flag;
    volatile int64_t index;
    volatile int64_t offset;
    volatile int64_t offset_zip;


    /* used for non-blocking algorithms */
    int status;
    int active_requests;
    int iteration;

    int *src_ptr;

    int start;

    /* process private data */
    int starting_flag_value;

    /* experiment for large data colls */
    int n_sends;
    int length;

    /* hdl framework control structure*/
    /* no need to pad at this level anymore */
    volatile int64_t data_hdl;
    volatile mca_basesmuma_ctrl_4_hdl_t hdl_ctrl;

#ifdef __PORTALS_AVAIL__
    struct mca_bcol_basesmuma_portal_buf_addr_t portals_buf_addr;
#endif
    /* padding */
    /*char padding[BASESMUMA_CACHE_LINE_SIZE-4*sizeof(uint64_t)-3*sizeof(int)];*/
    char padding[BASESMUMA_CACHE_LINE_SIZE-6*sizeof(int64_t)-5*sizeof(int)];
};
typedef struct mca_bcol_basesmuma_ctl_struct_t mca_bcol_basesmuma_ctl_struct_t;


#define SM_BCOLS_MAX 2

/* enum for signaling flag bank, when
 * adding to this list, please keep
 * it alphabetical
 */
enum {
    ALLGATHER_FLAG,
    ALLREDUCE_FLAG,
    BARRIER_FANIN_FLAG,
    BARRIER_FANOUT_FLAG,
    BARRIER_RKING_FLAG,
    BCAST_FLAG,
    GATHER_FLAG,
    REDUCE_FLAG,
    NUM_SIGNAL_FLAGS
};


/* control region for colls with user data - shared memory */
struct mca_bcol_basesmuma_header_t {
    /* collective identifier */
    volatile int64_t sequence_number;
    volatile int8_t  flags[NUM_SIGNAL_FLAGS][SM_BCOLS_MAX];
    volatile int32_t src; /* src of bcast data for unknown root,
                             bcol id for known root
                          */
    /* starting flag - hierarchies */
    int8_t starting_flag_value[SM_BCOLS_MAX];
    int8_t ready_flag;

    /* Manju: Cached array of receive buffer offsets
     *
     * This array stores the receive buffer offsets (rbuf_offsets) of data buffer.
     * In general, we assume that sbuf_offset and rbuf_offset of
     * processes invoking the collective primitive is same. This is
     * true when the order in which processes invoke their hierarchies are
     * same.
     *
     * For some algorithms (like broadcast, reduce)  we split the ML buffer
     * and use first half as
     * source and second half as receive buffer. We swap these buffers for
     * each change when we change levels i.e., if first half is source for
     * level 1, in the level 2 of hierarchy it becomes the receive buffer.
     * For reduce algorithm, each process can invoke hierarchies
     * (primitives) in different order. For example, process 1 might have level 1 as SM
     * and level 2 as p2p, and process 2 might have different order where its
     * level 1 is p2p and level 2 SM. In this case, if in basesmuma reduce
     * algorithm, if parent assumes its rbuf_offset as child's rbuf_offset
     * it is wrong. So we cache rbuf_offset of each process so
     * it could be accessed by processes to obtain the data.
     */

    volatile int32_t roffsets[SM_BCOLS_MAX];

    /* Manju Start: Experimental ctl fields and should be removed later;
     * This is used for lmsg reduce for testing
     * during transition to HDL primitives
     */
#if 0
    int lmsg_reduce_snd_completes;
    /* There can be atmost 20 ranks in the subgroup. Since this
     * only for testing this should be good enough */
    int lmsg_reduce_peers[20];
    int lmsg_reduce_send_offsets[20];
    /* End: Experimental ctl fields */


    /* no need to pad at this level anymore */
    volatile int64_t data_hdl;
#endif
};
typedef struct mca_bcol_basesmuma_header_t mca_bcol_basesmuma_header_t;

/* data needed for large messages */
struct mca_bcol_basesmuma_large_msg_t {
    /* scatter allgather data */
    uint64_t offset;
    uint64_t n_sends;
    uint64_t length;

    /* portals data */

};
typedef struct mca_bcol_basesmuma_large_msg_t mca_bcol_basesmuma_large_msg_t;

/* payload struct */
struct mca_bcol_basesmuma_payload_t {

    /* base pointer to shared memory control structure */
    mca_bcol_basesmuma_header_t *ctl_struct;
    void *payload;

};

typedef struct mca_bcol_basesmuma_payload_t mca_bcol_basesmuma_payload_t;




/* memory bank memory management structure */
struct mem_bank_management_t {

    /* generation counter */
    uint64_t bank_gen_counter;

    /* descriptor for the non-blocking barrier.  This is
     *  used to manage this bank of memory.
     */
    sm_nbbar_desc_t nb_barrier_desc;

    /* the number of buffers that are not in use, and are
     * available.  The assumption is that the buffers are
     * recycled all at once, so are available for re-use
     * until all buffers have been made available for re-use.
     */
    volatile int available_buffers;

    /*
     * number of buffers freed */
    volatile int n_buffs_freed;

    /* mutex to ensure atomic recycling of resrouces */
    opal_mutex_t mutex;

    /* number of buffers being managed */
    int number_of_buffers;

    /* shared memory control structures */
    int index_shared_mem_ctl_structs;


};
typedef struct mem_bank_management_t mem_bank_management_t;

/* data structure for shared buffers */
struct sm_buffer_mgmt {
    /* number of buffers per process */
    int number_of_buffs;

    /* size of group */
    int size_of_group;

    /* number of memory banks */
    int num_mem_banks;

    /* number of buffers per memory bank */
    int num_buffs_per_mem_bank;

    /* log base 2 of num_buffs_per_mem_bank */
    int log2_num_buffs_per_mem_bank;

    /* log base 2 total number of buffers */
    int log2_number_of_buffs;

    /* mask - masks off the bits corresponding to buffer index */
    int mask;

    /* control buffers - these point to regions in shared memory */
    /* leading dimension is the group size - all pointers for a given
     * set of buffers appear consecutively in this array
     */
    volatile void **ctl_buffs;

    /* management data for the control structures -
     * one per bank of control structures - Will be used for
     * the payload buffers as well.
     */
    mem_bank_management_t *ctl_buffs_mgmt;

    /* data buffers - these point to regions in shared memory */
    /* leading dimension is the group size - all pointers for a given
     * set of buffers appear consecutively in this array
     */

    volatile mca_bcol_basesmuma_payload_t *data_buffs;



};
typedef struct sm_buffer_mgmt sm_buffer_mgmt;


struct mca_bcol_basesmuma_nb_coll_buff_desc_t {
    void     *data_addr;
    uint64_t     bank_index;
    uint64_t     buffer_index;
    int       active_requests;
    ompi_request_t **requests;
    int          data_src;
    int          radix_mask;
    int          radix_mask_pow;
    int          iteration;
    int          status;
    /* this is for testing */
    int                 tag;

    volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    volatile mca_bcol_basesmuma_ctl_struct_t  *my_ctl_pointer;
    volatile mca_bcol_basesmuma_ctl_struct_t  *parent_ctl_pointer;
    volatile mca_bcol_basesmuma_ctl_struct_t  *extra_partner_ctl_pointer;
};

typedef struct mca_bcol_basesmuma_nb_coll_buff_desc_t mca_bcol_basesmuma_nb_coll_buff_desc_t;

struct mca_bcol_basesmuma_local_mlmem_desc_t {

    uint32_t bank_index_for_release;
    struct mca_bcol_base_memory_block_desc_t *ml_mem_desc;
    uint32_t     num_banks;
    uint32_t     num_buffers_per_bank;
    uint32_t     size_buffer;
    uint32_t     *bank_release_counter;

    /*
     * Number of descriptors allocated is equivalent to number of ml buffers
     * (number of banks * number of buffers per bank)
     */
    mca_bcol_basesmuma_nb_coll_buff_desc_t *nb_coll_desc;
};

typedef struct mca_bcol_basesmuma_local_mlmem_desc_t mca_bcol_basesmuma_local_mlmem_desc_t;

#ifdef __PORTALS_AVAIL__
#define MAX_SM_GROUP_SIZE 32


struct portals_scatter_allgather_nb_bcast_state_t
{
    /* local variables */
    uint64_t length;
    int my_rank, src, matched;
    int src_list[MAX_SM_GROUP_SIZE];
    int group_size;
    int64_t ready_flag;
    int pow_2, pow_2_levels;
    int src_list_index;
    uint64_t fragment_size;  /* user buffer size */

    /* Input argument variables */
    void *my_userbuf;
    int64_t sequence_number;

    /* Extra source variables */
    bool secondary_root;
    int partner , extra_partner;

    /* Scatter Allgather offsets */
    uint64_t local_sg_offset , global_sg_offset , partner_offset ;

    /* Portals messaging relevant variables */
    /*
     * ptl_handle_eq_t allgather_eq_h;
     */
    ptl_handle_eq_t read_eq;
    ptl_event_t  allgather_event;
    bool msg_posted;

    /* OMPI module and component variables */
    mca_bcol_basesmuma_component_t *cs;
    struct mca_bcol_basesmuma_module_t *bcol_module;

    /* Control structure and payload variables */
    volatile mca_bcol_basesmuma_ctl_struct_t **ctl_structs;
    volatile mca_bcol_basesmuma_ctl_struct_t  *my_ctl_pointer;
    volatile mca_bcol_basesmuma_ctl_struct_t  *parent_ctl_pointer; /* scatter source */
    volatile mca_bcol_basesmuma_ctl_struct_t  *extra_partner_ctl_pointer; /* scatter source */

    int phase;
};


typedef struct portals_scatter_allgather_nb_bcast_state_t sg_state_t;
#endif

#define SM_ARRAY_INDEX(LEAD_DIM,BUF_INDEX,PROC_INDEX)   \
    ((LEAD_DIM)*(BUF_INDEX)+(PROC_INDEX))
/* debug */
#define BARRIER_BANK_LIST_SIZE 32
/* end debug */

struct mca_bcol_basesmuma_module_t {
    /* base structure */
    mca_bcol_base_module_t super;

    /* free list item with the control structures used for
     * the no user data collective operations
     */
    list_data_t *no_userdata_ctl;

    /* free list item with the control structures used for
     * the with user data collective operations
     */
    list_data_t *userdata_ctl;

    /*
     * information on sm control backing files for the subgroup
     * associated with this module.
     */
    bcol_basesmuma_smcm_proc_item_t **ctl_backing_files_info;

    /*
     * information on sm payload backing files for the subgroup
     * associated with this module.
     */
    bcol_basesmuma_smcm_proc_item_t **payload_backing_files_info;

    /*
     * buffers for the collective that do not involve user data -
     *   barrier, fanin, fanout.
     */
    sm_buffer_mgmt colls_no_user_data;

    /*
     * buffers for the collective with user data.
     */
    sm_buffer_mgmt colls_with_user_data;

    /* recursive-doubling tree node */
    netpatterns_pair_exchange_node_t recursive_doubling_tree;

    /* k-nomial gather/allgather tree */
    netpatterns_k_exchange_node_t knomial_allgather_tree;

    /* fanin tree node - root is rank 0 */
    netpatterns_tree_node_t fanin_node;

    /* fanout tree node - root is rank 0 */
    netpatterns_tree_node_t fanout_node;

    /* index of blocking barrier memory region to use */
    int index_blocking_barrier_memory_bank;

    /* comm to shared memory map */
    int *comm_to_sm_map;

    /* reduction fanout tree */
    netpatterns_tree_node_t* reduction_tree;

    /* broadcast fanout tree */
    netpatterns_tree_node_t* fanout_read_tree;

    /* scatter - k-ary tree */
    int scatter_kary_radix;
    netpatterns_tree_node_t *scatter_kary_tree;

    /* Knomial exchange tree */
    /* Currently used for only large message reduce */
    netpatterns_k_exchange_node_t knomial_exchange_tree;

    /* sequence number offset - want to make sure that we start
     *   id'ing collectives with id 0, so we can have simple
     *   resource management.
     */
    int64_t squence_number_offset;

    /* basesmuma specific header size into ml buffer
     * was calculated at ml level - it is the sum of
     * all headers from all bcols and then aligned to
     * whatever alignment was requested
     */
    uint32_t total_header_size;

    /* list of possible sources */
    int *src_list;

    /* Number of possible sources */
    int src_size;

    /* smallest power of k that is smaller
     * than or equal in size to the uma group
     */
    int pow_k_levels;

    /* size of power-of-k group */
    int pow_k;

    /* smallest power of 2 that is smaller
     * than or equal to the smuma group size
     */
    int pow_2_levels;

    /* size of power-of-2 group */
    int pow_2;

    /* pointer to the shared memory scratch array of each
     * process in the group.
     */
    void **shared_memory_scratch_space;

    /*
     * Caching information for re-entrant collectives
     */
    mca_bcol_basesmuma_local_mlmem_desc_t ml_mem;

    /*
     * Cached offsets for lmsg reduce
     */
    int **reduce_offsets;

    /*XXX:
     * Starting to explore the beauty of zero-copy for large message
     */
    struct mca_hdl_base_module_t **hdl_module;

#ifdef __PORTALS_AVAIL__
    /*
     * Store state for NB blocking functions
     */
    sg_state_t sg_state;

#endif
};

typedef struct mca_bcol_basesmuma_module_t mca_bcol_basesmuma_module_t;
OBJ_CLASS_DECLARATION(mca_bcol_basesmuma_module_t);

/* shared memory specific arguments for the bcol registration function */
typedef struct bcol_basesmuma_registration_data_t {
    char *file_name; /* filename for payload */
    void *base_addr; /* base address to be mapped */
    size_t size;     /* size of memory block to be "registered" */
    size_t size_ctl_structure;
    size_t data_seg_alignment;
    bcol_basesmuma_smcm_mmap_t *sm_mmap; /* shared memory map struct */
    mca_bcol_base_release_buff_fn_t buff_release_cb; /* buffer release
                                                      call back */
} bcol_basesmuma_registration_data_t;


enum {
    BUFFER_AVAILABLE,
    STARTED,
    FANIN,
    FANOUT
};

/* enum used for non-blocking large
 * message bcast
 */

enum {
    INIT,
    START,
    NOT_STARTED,
    SCATTER,
    ALLGATHER,
    EXTRA_RANK,
    PROBE,
    SCATTER_ROOT_WAIT,
    SCATTER_EXTRA_ROOT_WAIT,
    SCATTER_PARENT_WAIT,
    FINISHED
};

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_bcol_basesmuma_component_t mca_bcol_basesmuma_component;


/*
 * coll module functions
 */

/* query to see if the component is available for use, and can
 * satisfy the thread and progress requirements
 */
int mca_bcol_basesmuma_init_query(bool enable_progress_threads,
                                  bool enable_mpi_threads);

/* query to see if the module is available for use on the given
 * communicator, and if so, what it's priority is.
 */
mca_bcol_base_module_t **
mca_bcol_basesmuma_comm_query(mca_sbgp_base_module_t *module, int *num_modules);



/* shared memory specific memory registration function - this will be passed into the mpool */
int mca_bcol_basesmuma_register_sm(void *context_data, void *base, size_t size,
                                   void **reg);

/* shared memory specific memory deregistration function - also needed by the mpool */
int mca_bcol_basesmuma_deregister_sm(void *context_data, void *reg);

/* setup the new k_nomial tree for collectives */
int bcol_basesmuma_setup_knomial_tree(mca_bcol_base_module_t *super);

/* allocate the memory pool for the shared memory control structures */
int mca_bcol_basesmuma_allocate_pool_memory(mca_bcol_basesmuma_component_t
                                            *component);

/* initialize the internal scratch buffers and control structs that will be
   used by the module */
int base_bcol_basesmuma_setup_library_buffers(
                                              mca_bcol_basesmuma_module_t *sm_module,
                                              mca_bcol_basesmuma_component_t *cs);


/* shared memory recursive doubling initialization */
int bcol_basesmuma_rd_barrier_init(mca_bcol_base_module_t *module);

/* shared memory recusive double barrier */
int bcol_basesmuma_recursive_double_barrier(bcol_function_args_t *input_args,
                                            mca_bcol_base_function_t *c_input_args);
/* shared memory fanin */
int bcol_basesmuma_fanin_init(mca_bcol_base_module_t *super);

/* shared memory fanout */
int bcol_basesmuma_fanout_init(mca_bcol_base_module_t *super);

/* shared memory recursive k-ing non-blocking barrier */
int bcol_basesmuma_barrier_init(mca_bcol_base_module_t *super);

/* Shared memory broadcast */
int bcol_basesmuma_bcast_init(mca_bcol_base_module_t *super);

int bcol_basesmuma_bcast(bcol_function_args_t *input_args,
                         mca_bcol_base_function_t *c_input_args);

/* Shared memory non-blocking broadcast */
int bcol_basesmuma_bcast_k_nomial_anyroot(bcol_function_args_t *input_args,
                                          mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_bcast_k_nomial_knownroot(bcol_function_args_t *input_args,
                                            mca_bcol_base_function_t *c_input_args);

/* Shared memory non-blocking broadcast - Large message anyroot */
int bcol_basesmuma_binary_scatter_allgather_segment(bcol_function_args_t *input_args,
                                                    mca_bcol_base_function_t *c_input_args);

#if 0
/*FIXME: having fun here*/
int bcol_basesmuma_hdl_zerocopy_bcast(bcol_function_args_t *input_args,
                                      mca_bcol_base_function_t   *c_input_args);
#endif

int bcol_basesmuma_lmsg_bcast_k_nomial_anyroot(bcol_function_args_t *input_args,
                                               mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_lmsg_scatter_allgather_portals_bcast(bcol_function_args_t *input_args,
                                                        mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_lmsg_scatter_allgather_portals_nb_bcast(bcol_function_args_t *input_args,
                                                           mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_lmsg_scatter_allgather_portals_nb_knownroot_bcast(bcol_function_args_t *input_args,
                                                                     mca_bcol_base_function_t *c_input_args);

/*
 *  shared memory scatter
 */
int bcol_basesmuma_scatter_init(mca_bcol_base_module_t *super);

/* shared memory nonblocking scatter - known root */
int bcol_basesmuma_nb_scatter_k_array_knownroot(
                                                bcol_function_args_t *input_args,
                                                mca_bcol_base_function_t *c_input_args);

/* shared memory non-blocking k-nomial barrier init */
int bcol_basesmuma_k_nomial_barrier_init(bcol_function_args_t *input_args,
                                         struct mca_bcol_base_function_t *const_args);

/* shared memory non-blocking k-nomial barrier progress */
int bcol_basesmuma_k_nomial_barrier_progress(bcol_function_args_t *input_args,
                                             struct mca_bcol_base_function_t *const_args);

/*shared memory non-blocking k-nomial allgather init */
int bcol_basesmuma_k_nomial_allgather_init(bcol_function_args_t *input_args,
                                           struct mca_bcol_base_function_t *const_args);

/* shared memory non-blocking k-nomial allgather progress */
int bcol_basesmuma_k_nomial_allgather_progress(bcol_function_args_t *input_args,
                                               struct mca_bcol_base_function_t *const_args);

/* shared memory allgather -- selection logic api */
int bcol_basesmuma_allgather_init(mca_bcol_base_module_t *super);

/* shared memory blocking k-nomial gather */
int bcol_basesmuma_k_nomial_gather(bcol_function_args_t *input_args,
                                   mca_bcol_base_function_t *c_input_args);

/* shared memory non blocking k-nomial gather */
int bcol_basesmuma_k_nomial_gather_init(bcol_function_args_t *input_args,
                                        mca_bcol_base_function_t *c_input_args);

/* shared memory non blocking k-nomial gather progress*/
int bcol_basesmuma_k_nomial_gather_progress(bcol_function_args_t *input_args,
                                            mca_bcol_base_function_t *c_input_args);

/* shared memory init */
int bcol_basesmuma_gather_init(mca_bcol_base_module_t *super);

/* allocate shared memory control memory */
int mca_bcol_basesmuma_allocate_sm_ctl_memory(
                                              mca_bcol_basesmuma_component_t *cs);

/* Shared memory basesmuma reduce */
int bcol_basesmuma_reduce_init(mca_bcol_base_module_t *super);
int bcol_basesmuma_reduce_intra_fanin(bcol_function_args_t *input_args,
                                      mca_bcol_base_function_t *c_input_args);
int bcol_basesmuma_reduce_intra_fanin_old(bcol_function_args_t *input_args,
                                          mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_reduce_intra_reducescatter_gather(void *sbuf, void *rbuf,
                                                     int count, struct ompi_datatype_t *dtype,
                                                     struct ompi_op_t *op,
                                                     int root,
                                                     struct ompi_communicator_t *comm,
                                                     mca_coll_base_module_t *module);

/* Shared memory basesmuma allreduce */
int bcol_basesmuma_allreduce_init(mca_bcol_base_module_t *super);

int bcol_basesmuma_allreduce_intra_fanin_fanout(bcol_function_args_t *input_args,
                                                mca_bcol_base_function_t *c_input_args);

int bcol_basesmuma_allreduce_intra_recursive_doubling(bcol_function_args_t *input_args,
                                                      mca_bcol_base_function_t *c_input_args);

/* initialize non-blocking barrier for recycling the memory buffers.
 *  This is not a general purpose nb_barrier, and relies on the
 *  fact that we will have only one outstanding nb-barrier per bank
 *  at a time.
 */
int bcol_basesmuma_rd_nb_barrier_init_admin(sm_nbbar_desc_t *sm_desc);

/* admin nonblocking barrier - progress function */
int bcol_basesmuma_rd_nb_barrier_progress_admin(sm_nbbar_desc_t *sm_desc);

/* Memory syncronization registration function */
int bcol_basesmuma_memsync_init(mca_bcol_base_module_t *super);

/* smcm allgather function used to exchange file offsets. */
int bcol_basesmuma_smcm_allgather_connection(
                                             mca_bcol_basesmuma_module_t *sm_bcol_module,
                                             mca_sbgp_base_module_t *module,
                                             opal_list_t *peer_list,
                                             bcol_basesmuma_smcm_proc_item_t ***backing_files,
                                             ompi_communicator_t *comm,
                                             bcol_basesmuma_smcm_file_t input, char *base_fname,
                                             bool map_all);

/* clean up the backing files associated with a basesmuma bcol module */
int bcol_basesmuma_smcm_release_connections (mca_bcol_basesmuma_module_t *sm_bcol_module,
                                             mca_sbgp_base_module_t *sbgp_module, opal_list_t *peer_list,
                                             bcol_basesmuma_smcm_proc_item_t ***back_files);

/*
 * this function initializes the internal scratch buffers and control
 * structures that will be used by the module
 */
int base_bcol_masesmuma_setup_library_buffers(
                                              mca_bcol_basesmuma_module_t *sm_bcol_module,
                                              mca_bcol_basesmuma_component_t *sm_bcol_component);

/* get the index of the shared memory buffer to be used */
int bcol_basesmuma_get_buff_index( sm_buffer_mgmt * buff_block,
                                   uint64_t buff_id );

int bcol_basesmuma_free_buff( sm_buffer_mgmt * buff_block,
                              uint64_t buff_id );

/* bank init which is used for shared memory optimization, fall back to
 * the bank init above if this causes problems
 */
int bcol_basesmuma_bank_init_opti(struct mca_bcol_base_memory_block_desc_t *payload_block,
        uint32_t data_offset,
        mca_bcol_base_module_t *bcol_module,
        void *reg_data);

/* cleanup nb_coll_buff_desc */
void cleanup_nb_coll_buff_desc(mca_bcol_basesmuma_nb_coll_buff_desc_t **desc,
                                  uint32_t num_banks,
                                  uint32_t num_buffers_per_bank);


/* used for shared memory offset exchange */
int base_bcol_basesmuma_exchange_offsets(
                                         mca_bcol_basesmuma_module_t *sm_bcol_module,
                                         void **result_array, uint64_t mem_offset, int loop_limit,
                                         int leading_dim);


/* the progress function to be called from the opal progress function
 */
int bcol_basesmuma_progress(void);

/* Macro for initializing my shared memory control structure */
#define BASESMUMA_HEADER_INIT(my_ctl_pointer,ready_flag, seqn, bcol_id) \
    do{                                                                 \
        int i,j;                                                        \
        int8_t flag_offset = 0;                                         \
        /* setup resource recycling */                                  \
        if( (my_ctl_pointer)->sequence_number < (seqn) ) {              \
            /* Signal arrival */                                        \
            for( j = 0; j < SM_BCOLS_MAX; j++){                         \
                (my_ctl_pointer)->starting_flag_value[j]=0;             \
                for( i = 0; i < NUM_SIGNAL_FLAGS; i++){                 \
                    (my_ctl_pointer)->flags[i][j] = -1;                 \
                }                                                       \
            }                                                           \
        }                                                               \
        /* increment the starting flag by one and return */             \
        flag_offset = (my_ctl_pointer)->starting_flag_value[(bcol_id)]; \
        (ready_flag) = flag_offset + 1;                                 \
        opal_atomic_wmb();                                              \
        (my_ctl_pointer)->sequence_number = (seqn);                     \
    }while(0)

/* these are all the same, am using a single macro for all collectives */

#define IS_PEER_READY(peer, my_flag, my_sequence_number,flag_index, bcol_id) \
    (((peer)->sequence_number == (my_sequence_number) && \
      (peer)->flags[flag_index][bcol_id] >= (my_flag))? true : false )

#if 0
#define IS_AR_DATA_READY(peer, my_flag, my_sequence_number)     \
    (((peer)->sequence_number == (my_sequence_number) &&        \
      (peer)->flags[ALLREDUCE_FLAG][bcol_id] >= (my_flag)       \
      )? true : false )

#define IS_GDATA_READY(peer, my_flag, my_sequence_number)       \
    (((peer)->sequence_number == (my_sequence_number) &&        \
      (peer)->flags[GATHER_FLAG][bcol_id] == (my_flag)          \
      )? true : false )

#define IS_PEER_READY(peer, my_flag, flag_index, my_sequence_number)    \
    ((((volatile int64_t)(peer)->sequence_number > (my_sequence_number)) || \
      (((volatile int64_t)(peer)->sequence_number == (my_sequence_number)) && \
       ((peer)->flags[flag_index][bcol_id] == (my_flag)))               \
      )? true : false )

#define IS_ALLREDUCE_PEER_READY(peer, my_flag, my_sequence_number)      \
    ((((volatile int64_t)(peer)->sequence_number == (my_sequence_number)) && \
      (((peer)->flags[ALLREDUCE_FLAG][bcol_id] == (my_flag))||((peer)->flags[ALLREDUCE_FLAG][bcol_id] == (my_flag) + 1)) \
      )? true : false )
#endif

#define IS_LAST_BCOL_FUNC(ml_args)                                      \
    ((((ml_args)->n_of_this_type_in_collective ==                       \
       (ml_args)->index_of_this_type_in_collective + 1 ) )? true : false)

static inline __opal_attribute_always_inline__
size_t bcol_basesmuma_data_offset_calc(
                                       mca_bcol_basesmuma_module_t *basesmuma_module)
{
    uint32_t offset = basesmuma_module->super.header_size;
    offset = ((offset + BCOL_HEAD_ALIGN - 1) / BCOL_HEAD_ALIGN) * BCOL_HEAD_ALIGN;

    return (size_t) offset;
}


END_C_DECLS

#endif /* MCA_BCOL_basesmuma_EXPORT_H */
