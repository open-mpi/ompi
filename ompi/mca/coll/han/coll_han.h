/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2020      Bull S.A.S. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_HAN_EXPORT_H
#define MCA_COLL_HAN_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "ompi/mca/mca.h"
#include "opal/util/output.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_han_trigger.h"
#include "ompi/mca/coll/han/coll_han_dynamic.h" 

BEGIN_C_DECLS

/*
 * Today;
 * . only 2 modules available for intranode (low) level
 * . only 2 modules available for internode (up) level
 */

#define COLL_HAN_LOW_MODULES 2
#define COLL_HAN_UP_MODULES 2

typedef struct {
    uint32_t umod;
    uint32_t lmod;
    uint32_t fs;
    uint32_t ualg;
    uint32_t us;
} selection;

struct mca_bcast_argu_s {
    mca_coll_task_t *cur_task;
    void *buff;
    int seg_count;
    struct ompi_datatype_t *dtype;
    int root_low_rank;
    int root_up_rank;
    struct ompi_communicator_t *up_comm;
    struct ompi_communicator_t *low_comm;
    int num_segments;
    int cur_seg;
    int w_rank;
    int last_seg_count;
    bool noop;
};
typedef struct mca_bcast_argu_s mca_bcast_argu_t;

struct mca_reduce_argu_s {
    mca_coll_task_t *cur_task;
    void *sbuf;
    void *rbuf;
    int seg_count;
    struct ompi_datatype_t *dtype;
    struct ompi_op_t *op;
    int root_low_rank;
    int root_up_rank;
    struct ompi_communicator_t *up_comm;
    struct ompi_communicator_t *low_comm;
    int num_segments;
    int cur_seg;
    int w_rank;
    int last_seg_count;
    bool noop;
};
typedef struct mca_reduce_argu_s mca_reduce_argu_t;

struct mca_allreduce_argu_s {
    mca_coll_task_t *cur_task;
    void *sbuf;
    void *rbuf;
    int seg_count;
    struct ompi_datatype_t *dtype;
    struct ompi_op_t *op;
    int root_up_rank;
    int root_low_rank;
    struct ompi_communicator_t *up_comm;
    struct ompi_communicator_t *low_comm;
    int num_segments;
    int cur_seg;
    int w_rank;
    int last_seg_count;
    bool noop;
    ompi_request_t *req;
    int *completed;
};
typedef struct mca_allreduce_argu_s mca_allreduce_argu_t;

struct mca_scatter_argu_s {
    mca_coll_task_t *cur_task;
    void *sbuf;
    void *sbuf_inter_free;
    void *sbuf_reorder_free;
    int scount;
    struct ompi_datatype_t *sdtype;
    void *rbuf;
    int rcount;
    struct ompi_datatype_t *rdtype;
    int root;
    int root_up_rank;
    int root_low_rank;
    struct ompi_communicator_t *up_comm;
    struct ompi_communicator_t *low_comm;
    int w_rank;
    bool noop;
    ompi_request_t *req;
};
typedef struct mca_scatter_argu_s mca_scatter_argu_t;

struct mca_gather_argu_s {
    mca_coll_task_t *cur_task;
    void *sbuf;
    void *sbuf_inter_free;
    int scount;
    struct ompi_datatype_t *sdtype;
    void *rbuf;
    int rcount;
    struct ompi_datatype_t *rdtype;
    int root;
    int root_up_rank;
    int root_low_rank;
    struct ompi_communicator_t *up_comm;
    struct ompi_communicator_t *low_comm;
    int w_rank;
    bool noop;
    ompi_request_t *req;
};
typedef struct mca_gather_argu_s mca_gather_argu_t;

struct mca_allgather_argu_s {
    mca_coll_task_t *cur_task;
    void *sbuf;
    void *sbuf_inter_free;
    int scount;
    struct ompi_datatype_t *sdtype;
    void *rbuf;
    int rcount;
    struct ompi_datatype_t *rdtype;
    int root_low_rank;
    struct ompi_communicator_t *up_comm;
    struct ompi_communicator_t *low_comm;
    int w_rank;
    bool noop;
    bool is_mapbycore;
    int *topo;
    ompi_request_t *req;
};
typedef struct mca_allgather_argu_s mca_allgather_argu_t;

/**
 * Structure to hold the han coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * han-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
typedef struct mca_coll_han_component_t {
    /** Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /** MCA parameter: Priority of this component */
    int han_priority;
    /* whether output the log message */
    int han_output;
    /* segment size for bcast */
    uint32_t han_bcast_segsize;
    /* up level module for bcast */
    uint32_t han_bcast_up_module;
    /* low level module for bcast */
    uint32_t han_bcast_low_module;
    /* segment size for reduce */
    uint32_t han_reduce_segsize;
    /* up level module for reduce */
    uint32_t han_reduce_up_module;
    /* low level module for reduce */
    uint32_t han_reduce_low_module;    
    /* segment size for allreduce */
    uint32_t han_allreduce_segsize;
    /* up level module for allreduce */
    uint32_t han_allreduce_up_module;
    /* low level module for allreduce */
    uint32_t han_allreduce_low_module;
    /* up level module for allgather */
    uint32_t han_allgather_up_module;
    /* low level module for allgather */
    uint32_t han_allgather_low_module;
    /* up level module for gather */
    uint32_t han_gather_up_module;
    /* low level module for gather */
    uint32_t han_gather_low_module;
    /* up level module for scatter */
    uint32_t han_scatter_up_module;
    /* low level module for scatter */
    uint32_t han_scatter_low_module;
    /* whether enable auto tune */
    uint32_t han_auto_tune;
    /* whether we need reproducible results
     * (but disables topological optimisations)
     */
    uint32_t han_reproducible;
    /* create a 3D array
     * num_processes (n): 2 4 8 16 32 64 (6)
     * num_core (c): 2 4 8 12 (4)
     * message size (m): 1 - 4194304 (23)
     */
    uint32_t han_auto_tune_n;
    uint32_t han_auto_tune_c;
    uint32_t han_auto_tune_m;
    selection *han_auto_tuned;
    bool use_simple_algorithm[COLLCOUNT];

    /* Dynamic configuration rules */
    bool use_dynamic_file_rules;
    bool dump_dynamic_rules;
    char* dynamic_rules_filename;
    /* Dynamic rules from file */
    mca_coll_han_dynamic_rules_t dynamic_rules;
    /* Dynamic rules from mca parameter */
    COMPONENT_T mca_rules[COLLCOUNT][NB_TOPO_LVL];
    int topo_level; 

    /* Define maximum dynamic errors printed by rank 0 with a 0 verbosity level */
    int max_dynamic_errors;
} mca_coll_han_component_t;

typedef void (*previous_dummy_fn_t) (void);

/*
 * Structure used to store what is necessary for the collective operations
 * routines in case of fallback.
 */
typedef struct collective_fallback_t {
    union {
        mca_coll_base_module_allgather_fn_t allgather;
        mca_coll_base_module_allgatherv_fn_t allgatherv;
        mca_coll_base_module_allreduce_fn_t allreduce;
        mca_coll_base_module_bcast_fn_t bcast;
        mca_coll_base_module_gather_fn_t gather;
        mca_coll_base_module_reduce_fn_t reduce;
        mca_coll_base_module_scatter_fn_t scatter;
        previous_dummy_fn_t dummy;
    } previous_routine;
    mca_coll_base_module_t *previous_module;
} collective_fallback_t;

/** Coll han module */
typedef struct mca_coll_han_module_t {
    /** Base module */
    mca_coll_base_module_t super;

    /* Whether this module has been lazily initialized or not yet */
    bool enabled;

    struct ompi_communicator_t *cached_comm;
    struct ompi_communicator_t **cached_low_comms;
    struct ompi_communicator_t **cached_up_comms;
    int *cached_vranks;
    int *cached_topo;
    bool is_mapbycore;
    bool are_ppn_imbalanced;

    /* To be able to fallback when the cases are not supported */
    struct collective_fallback_t previous_routines[COLLCOUNT];

    /* To be able to fallback on reproducible algorithm */
    mca_coll_base_module_reduce_fn_t reproducible_reduce;
    mca_coll_base_module_t *reproducible_reduce_module;
    mca_coll_base_module_allreduce_fn_t reproducible_allreduce;
    mca_coll_base_module_t *reproducible_allreduce_module;

    /* Topological level of this communicator */
    int topologic_level;

    /* Collective module storage for module choice */
    mca_coll_han_collective_modules_storage_t modules_storage;
    bool storage_initialized;

    /*
     * Number of dynamic errors encountered
     * The first mca_coll_han_component.max_dynamic_errors
     * of rank 0 are printed with verbosity = 0
     */
    int dynamic_errors;

    /* Sub-communicator */
    struct ompi_communicator_t *sub_comm[NB_TOPO_LVL];
} mca_coll_han_module_t;
OBJ_CLASS_DECLARATION(mca_coll_han_module_t);

/*
 * Some defines to stick to the naming used in the other components in terms of
 * fallback routines
 */
#define previous_allgather  previous_routines[ALLGATHER].previous_routine.allgather
#define previous_allgatherv previous_routines[ALLGATHERV].previous_routine.allgatherv
#define previous_allreduce  previous_routines[ALLREDUCE].previous_routine.allreduce
#define previous_bcast      previous_routines[BCAST].previous_routine.bcast
#define previous_gather     previous_routines[GATHER].previous_routine.gather
#define previous_reduce     previous_routines[REDUCE].previous_routine.reduce
#define previous_scatter    previous_routines[SCATTER].previous_routine.scatter

#define previous_allgather_module  previous_routines[ALLGATHER].previous_module
#define previous_allgatherv_module previous_routines[ALLGATHERV].previous_module
#define previous_allreduce_module  previous_routines[ALLREDUCE].previous_module
#define previous_bcast_module      previous_routines[BCAST].previous_module
#define previous_gather_module     previous_routines[GATHER].previous_module
#define previous_reduce_module     previous_routines[REDUCE].previous_module
#define previous_scatter_module    previous_routines[SCATTER].previous_module

/**
 * Global component instance
 */
OMPI_MODULE_DECLSPEC extern mca_coll_han_component_t mca_coll_han_component;

/*
 * coll module functions
 */
int mca_coll_han_init_query(bool enable_progress_threads, bool enable_mpi_threads);

mca_coll_base_module_t *mca_coll_han_comm_query(struct ompi_communicator_t *comm, int *priority);

int han_request_free(ompi_request_t ** request);

/* Subcommunicator creation */
void mca_coll_han_comm_create(struct ompi_communicator_t *comm, mca_coll_han_module_t * han_module);
void mca_coll_han_comm_create_new(struct ompi_communicator_t *comm, mca_coll_han_module_t *han_module); 
/* Gather topology information */
int *mca_coll_han_topo_init(struct ompi_communicator_t *comm, mca_coll_han_module_t * han_module,
                            int num_topo_level);

/* Utils */
void mca_coll_han_get_ranks(int *vranks, int root, int low_size, int *root_low_rank,
                            int *root_up_rank);
uint32_t han_auto_tuned_get_n(uint32_t n);
uint32_t han_auto_tuned_get_c(uint32_t c);
uint32_t han_auto_tuned_get_m(uint32_t m);

const char* mca_coll_han_colltype_to_str(COLLTYPE_T coll);
const char* mca_coll_han_topo_lvl_to_str(TOPO_LVL_T topo_lvl);

/** Dynamic component choice */
/*
 * Get all the collective modules initialized on this communicator
 * This function must be call at the start of every selector implementation
 */
int
mca_coll_han_get_all_coll_modules(struct ompi_communicator_t *comm,
			                      mca_coll_han_module_t *han_module);

int
mca_coll_han_allgather_intra_dynamic(ALLGATHER_BASE_ARGS,
                                     mca_coll_base_module_t *module);
int
mca_coll_han_allgatherv_intra_dynamic(ALLGATHERV_BASE_ARGS,
                                      mca_coll_base_module_t *module);
int
mca_coll_han_allreduce_intra_dynamic(ALLREDUCE_BASE_ARGS,
                                     mca_coll_base_module_t *module);
int
mca_coll_han_bcast_intra_dynamic(BCAST_BASE_ARGS,
                                 mca_coll_base_module_t *module);
int
mca_coll_han_gather_intra_dynamic(GATHER_BASE_ARGS,
                                  mca_coll_base_module_t *module);
int
mca_coll_han_reduce_intra_dynamic(REDUCE_BASE_ARGS,
                                  mca_coll_base_module_t *module);
int
mca_coll_han_scatter_intra_dynamic(SCATTER_BASE_ARGS,
                                   mca_coll_base_module_t *module);

/* Bcast */
int mca_coll_han_bcast_intra_simple(void *buff,
				       int count,
				       struct ompi_datatype_t *dtype,
				       int root,
				       struct ompi_communicator_t *comm,
				       mca_coll_base_module_t *module);
void mac_coll_han_set_bcast_argu(mca_bcast_argu_t * argu, mca_coll_task_t * cur_task, void *buff,
                                 int seg_count, struct ompi_datatype_t *dtype,
                                 int root_up_rank, int root_low_rank,
                                 struct ompi_communicator_t *up_comm,
                                 struct ompi_communicator_t *low_comm,
                                 int num_segments, int cur_seg, int w_rank, int last_seg_count,
                                 bool noop);
int mca_coll_han_bcast_intra(void *buff, int count, struct ompi_datatype_t *dtype, int root,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int mca_coll_han_bcast_t0_task(void *task_argu);
int mca_coll_han_bcast_t1_task(void *task_argu);

/* Reduce */
int
mca_coll_han_reduce_intra_simple(const void *sbuf,
                                 void* rbuf,
                                 int count,
                                 struct ompi_datatype_t *dtype,
                                 ompi_op_t *op,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module);
int
mca_coll_han_reduce_reproducible_decision(struct ompi_communicator_t *comm,
                                          mca_coll_base_module_t *module);
int
mca_coll_han_reduce_reproducible(const void *sbuf,
                                 void *rbuf,
                                 int count,
                                 struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module);



void mac_coll_han_set_reduce_argu(mca_reduce_argu_t * argu, mca_coll_task_t * cur_task, 
                                  void *sbuf, 
                                  void *rbuf, int seg_count, struct ompi_datatype_t *dtype, 
                                  struct ompi_op_t *op,
                                  int root_up_rank, int root_low_rank,
                                  struct ompi_communicator_t *up_comm,
                                  struct ompi_communicator_t *low_comm,
                                  int num_segments, int cur_seg, int w_rank, int last_seg_count,
                                  bool noop);

int mca_coll_han_reduce_intra(const void *sbuf, 
                              void *rbuf,
                              int count,
                              struct ompi_datatype_t *dtype,
                              ompi_op_t* op,
                              int root,
                              struct ompi_communicator_t *comm, 
                              mca_coll_base_module_t * module);

int mca_coll_han_reduce_t0_task(void *task_argu);
int mca_coll_han_reduce_t1_task(void *task_argu);

/* Allreduce */
int
mca_coll_han_allreduce_intra_simple(const void *sbuf,
                                       void *rbuf,
                                       int count,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       mca_coll_base_module_t *module);
int
mca_coll_han_allreduce_reproducible_decision(struct ompi_communicator_t *comm,
                                             mca_coll_base_module_t *module);
int
mca_coll_han_allreduce_reproducible(const void *sbuf,
                                    void *rbuf,
                                     int count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);

void mac_coll_han_set_allreduce_argu(mca_allreduce_argu_t * argu,
                                     mca_coll_task_t * cur_task,
                                     void *sbuf,
                                     void *rbuf,
                                     int seg_count,
                                     struct ompi_datatype_t *dtype,
                                     struct ompi_op_t *op,
                                     int root_up_rank,
                                     int root_low_rank,
                                     struct ompi_communicator_t *up_comm,
                                     struct ompi_communicator_t *low_comm,
                                     int num_segments,
                                     int cur_seg,
                                     int w_rank,
                                     int last_seg_count,
                                     bool noop, ompi_request_t * req, int *completed);
int mca_coll_han_allreduce_intra(const void *sbuf,
                                 void *rbuf,
                                 int count,
                                 struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op,
                                 struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int mca_coll_han_allreduce_t0_task(void *task_argu);
int mca_coll_han_allreduce_t1_task(void *task_argu);
int mca_coll_han_allreduce_t2_task(void *task_argu);
int mca_coll_han_allreduce_t3_task(void *task_argu);

/* Scatter */
int
mca_coll_han_scatter_intra(const void *sbuf, int scount,
                            struct ompi_datatype_t *sdtype,
                            void *rbuf, int rcount,
                            struct ompi_datatype_t *rdtype,
                            int root,
                            struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int mca_coll_han_scatter_us_task(void *task_argu);
int mca_coll_han_scatter_ls_task(void *task_argu);
void mac_coll_han_set_scatter_argu(mca_scatter_argu_t * argu,
                                   mca_coll_task_t * cur_task,
                                   void *sbuf,
                                   void *sbuf_inter_free,
                                   void *sbuf_reorder_free,
                                   int scount,
                                   struct ompi_datatype_t *sdtype,
                                   void *rbuf,
                                   int rcount,
                                   struct ompi_datatype_t *rdtype,
                                   int root,
                                   int root_up_rank,
                                   int root_low_rank,
                                   struct ompi_communicator_t *up_comm,
                                   struct ompi_communicator_t *low_comm,
                                   int w_rank, bool noop, ompi_request_t * req);

/* Gather */
int
mca_coll_han_gather_intra(const void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, int rcount,
                           struct ompi_datatype_t *rdtype,
                           int root,
                           struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int mca_coll_han_gather_lg_task(void *task_argu);
int mca_coll_han_gather_ug_task(void *task_argu);
void mac_coll_han_set_gather_argu(mca_gather_argu_t * argu,
                                  mca_coll_task_t * cur_task,
                                  void *sbuf,
                                  void *sbuf_inter_free,
                                  int scount,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf,
                                  int rcount,
                                  struct ompi_datatype_t *rdtype,
                                  int root,
                                  int root_up_rank,
                                  int root_low_rank,
                                  struct ompi_communicator_t *up_comm,
                                  struct ompi_communicator_t *low_comm,
                                  int w_rank, bool noop, ompi_request_t * req);
int
mca_coll_han_gather_intra_simple(const void *sbuf, int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf, int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     int root,
                                     struct ompi_communicator_t *comm,
                                     mca_coll_base_module_t *module);
/* reordering after gather, for unordered ranks */
void
ompi_coll_han_reorder_gather(const void *sbuf,
                                     void *rbuf, int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     struct ompi_communicator_t *comm,
                                     int * topo);



/* Allgather */
int
mca_coll_han_allgather_intra(const void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int mca_coll_han_allgather_lg_task(void *task_argu);
int mca_coll_han_allgather_uag_task(void *task_argu);
int mca_coll_han_allgather_lb_task(void *task_argu);
void mac_coll_han_set_allgather_argu(mca_allgather_argu_t * argu,
                                     mca_coll_task_t * cur_task,
                                     void *sbuf,
                                     void *sbuf_inter_free,
                                     int scount,
                                     struct ompi_datatype_t *sdtype,
                                     void *rbuf,
                                     int rcount,
                                     struct ompi_datatype_t *rdtype,
                                     int root_low_rank,
                                     struct ompi_communicator_t *up_comm,
                                     struct ompi_communicator_t *low_comm,
                                     int w_rank,
                                     bool noop, bool is_mapbycore, int *topo, ompi_request_t * req);
int
mca_coll_han_allgather_intra_simple(const void *sbuf, int scount,
                                        struct ompi_datatype_t *sdtype,
                                        void* rbuf, int rcount,
                                        struct ompi_datatype_t *rdtype,
                                        struct ompi_communicator_t *comm,
                                        mca_coll_base_module_t *module);

END_C_DECLS
#endif                          /* MCA_COLL_HAN_EXPORT_H */
