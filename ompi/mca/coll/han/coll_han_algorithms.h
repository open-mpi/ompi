/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020-2022 Bull S.A.S. All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** Han algorithms, for han algorithm selection. This contains the
 * helper functions for the selection mechanism and declarations of
 * collective algorithm.
 *
 * All algorithms need to be added to the
 * 'mca_coll_han_available_algorithms' array in in
 * coll_han_algorithms.c. After mca_han_algorithm_info_init() various info on
 * id/name/functions are filled in component.algorithm_info[], 'default'
 * algorithm with #0 is added, and they are selectable in the
 * component.
 *
 * Algorithms which are selectable can be queried in ompi_info, see for
 * 'coll_han_use_<COLLNAME>_algorithm:enumerator'
 *
 */

#ifndef MCA_COLL_HAN_ALGORITHMS_H
#define MCA_COLL_HAN_ALGORITHMS_H

/* use this pointer type instead of void* to avoid warnings as it is
 * not legal to convert function pointers to void*
 *
 * note: alternatively we could use a union of function types, but
 * then it is heavy to declare available algorithms as nested
 * datastructure
 */
typedef void (*fnptr_t)(void);
// Han algorithms, data declarations per collective
// structure used to declare an array of algorithms: {name, fn}
typedef struct mca_coll_han_algorithm_value_s {
    char* name;
    fnptr_t fn;
} mca_coll_han_algorithm_value_t;

// datastructure generated from previous by mca_han_init_algorithm_info()
typedef struct mca_coll_han_collective_algorithm_info_s {
    mca_base_var_enum_value_t* enum_values;
} mca_coll_han_collective_algorithm_info_t;

// initialise before using algorithms id name fn var_enum_value
int
mca_coll_han_init_algorithms(void);
int
mca_coll_han_free_algorithms(void);

int
mca_coll_han_algorithm_name_to_id(COLLTYPE_T coll_id, const char* algorithm_name);
int
mca_coll_han_algorithm_id_is_valid(int coll_id, int algorithm_id);
fnptr_t
mca_coll_han_algorithm_id_to_fn(int coll_id, int algorithm_id);
char*
mca_coll_han_algorithm_id_to_name(int coll_id, int algorithm_id);

/**
 * Available han algorithms
 *
 * They must be added to 'mca_coll_han_available_algorithms'
 * in coll_han_algorithms.c
 */

int mca_coll_han_barrier_intra_simple(struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module);
/* Bcast */
int mca_coll_han_bcast_intra_simple(void *buff,
                                    int count,
                                    struct ompi_datatype_t *dtype,
                                    int root,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);
int mca_coll_han_bcast_intra(void *buff, int count, struct ompi_datatype_t *dtype, int root,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t * module);

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

int mca_coll_han_reduce_intra(const void *sbuf,
                              void *rbuf,
                              int count,
                              struct ompi_datatype_t *dtype,
                              ompi_op_t* op,
                              int root,
                              struct ompi_communicator_t *comm,
                              mca_coll_base_module_t * module);

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

int mca_coll_han_allreduce_intra(const void *sbuf,
                                 void *rbuf,
                                 int count,
                                 struct ompi_datatype_t *dtype,
                                 struct ompi_op_t *op,
                                 struct ompi_communicator_t *comm, mca_coll_base_module_t * module);

/* Scatter */
int
mca_coll_han_scatter_intra(const void *sbuf, int scount,
                           struct ompi_datatype_t *sdtype,
                           void *rbuf, int rcount,
                           struct ompi_datatype_t *rdtype,
                           int root,
                           struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int
mca_coll_han_scatter_intra_simple(const void *sbuf, int scount,
                                  struct ompi_datatype_t *sdtype,
                                  void *rbuf, int rcount,
                                  struct ompi_datatype_t *rdtype,
                                  int root,
                                  struct ompi_communicator_t *comm,
                                  mca_coll_base_module_t * module);

/* Gather */
int
mca_coll_han_gather_intra(const void *sbuf, int scount,
                          struct ompi_datatype_t *sdtype,
                          void *rbuf, int rcount,
                          struct ompi_datatype_t *rdtype,
                          int root,
                          struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int
mca_coll_han_gather_intra_simple(const void *sbuf, int scount,
                                 struct ompi_datatype_t *sdtype,
                                 void *rbuf, int rcount,
                                 struct ompi_datatype_t *rdtype,
                                 int root,
                                 struct ompi_communicator_t *comm,
                                 mca_coll_base_module_t *module);

/* Gatherv */
int
mca_coll_han_gatherv_intra(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                           void *rbuf, const int *rcounts, const int *displs,
                           struct ompi_datatype_t *rdtype, int root,
                           struct ompi_communicator_t *comm, mca_coll_base_module_t *module);

/* Allgather */
int
mca_coll_han_allgather_intra(const void *sbuf, int scount,
                             struct ompi_datatype_t *sdtype,
                             void *rbuf, int rcount,
                             struct ompi_datatype_t *rdtype,
                             struct ompi_communicator_t *comm, mca_coll_base_module_t * module);
int
mca_coll_han_allgather_intra_simple(const void *sbuf, int scount,
                                    struct ompi_datatype_t *sdtype,
                                    void* rbuf, int rcount,
                                    struct ompi_datatype_t *rdtype,
                                    struct ompi_communicator_t *comm,
                                    mca_coll_base_module_t *module);

#endif
