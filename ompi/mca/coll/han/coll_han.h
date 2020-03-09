/*
 * Copyright (c) 2018-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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
#include "ompi/mca/coll/coll.h"
#include "ompi/communicator/communicator.h"
#include "ompi/include/mpi.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "opal/util/info.h"
#include "ompi/op/op.h"
#include "opal/runtime/opal_progress.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "coll_han_trigger.h"

BEGIN_C_DECLS typedef struct {
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
    /* create a 3D array
     * num_processes (n): 2 4 8 16 32 64 (6)
     * num_core (c): 2 4 8 12 (4)
     * message size (m): 1 - 4194304 (23)
     */
    uint32_t han_auto_tune_n;
    uint32_t han_auto_tune_c;
    uint32_t han_auto_tune_m;
    selection *han_auto_tuned;
} mca_coll_han_component_t;

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
} mca_coll_han_module_t;
OBJ_CLASS_DECLARATION(mca_coll_han_module_t);

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

/* Gather topology information */
int mca_coll_han_pow10_int(int pow_value);
int mca_coll_han_hostname_to_number(char *hostname, int size);
void mca_coll_han_topo_get(int *topo, struct ompi_communicator_t *comm, int num_topo_level);
void mca_coll_han_topo_sort(int *topo, int start, int end, int size, int level, int num_topo_level);
bool mca_coll_han_topo_is_mapbycore(int *topo, struct ompi_communicator_t *comm,
                                    int num_topo_level);
int *mca_coll_han_topo_init(struct ompi_communicator_t *comm, mca_coll_han_module_t * han_module,
                            int num_topo_level);
void mca_coll_han_topo_print(int *topo, struct ompi_communicator_t *comm, int num_topo_level);

/* Utils */
void mca_coll_han_get_ranks(int *vranks, int root, int low_size, int *root_low_rank,
                            int *root_up_rank);
uint32_t han_auto_tuned_get_n(uint32_t n);
uint32_t han_auto_tuned_get_c(uint32_t c);
uint32_t han_auto_tuned_get_m(uint32_t m);


/* Bcast */
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

/* Gatter */
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

END_C_DECLS
#endif                          /* MCA_COLL_HAN_EXPORT_H */
