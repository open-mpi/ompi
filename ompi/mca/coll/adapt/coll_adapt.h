/*
 * Copyright (c) 2014-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */


#ifndef MCA_COLL_ADAPT_EXPORT_H
#define MCA_COLL_ADAPT_EXPORT_H

#include "ompi_config.h"

#include "mpi.h"
#include "opal/mca/mca.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_base_topo.h"

BEGIN_C_DECLS

typedef struct mca_coll_adapt_module_t mca_coll_adapt_module_t;

typedef enum {
    OMPI_COLL_ADAPT_ALGORITHM_TUNED = 0,
    OMPI_COLL_ADAPT_ALGORITHM_BINOMIAL,
    OMPI_COLL_ADAPT_ALGORITHM_IN_ORDER_BINOMIAL,
    OMPI_COLL_ADAPT_ALGORITHM_BINARY,
    OMPI_COLL_ADAPT_ALGORITHM_PIPELINE,
    OMPI_COLL_ADAPT_ALGORITHM_CHAIN,
    OMPI_COLL_ADAPT_ALGORITHM_LINEAR,
    OMPI_COLL_ADAPT_ALGORITHM_COUNT /* number of algorithms, keep last! */
} ompi_coll_adapt_algorithm_t;

/*
 * Structure to hold the adapt coll component.  First it holds the
 * base coll component, and then holds a bunch of
 * adapt-coll-component-specific stuff (e.g., current MCA param
 * values).
 */
typedef struct mca_coll_adapt_component_t {
    /* Base coll component */
    mca_coll_base_component_2_0_0_t super;

    /* MCA parameter: Priority of this component */
    int adapt_priority;

    /* MCA parameter: Output stream and verbose level */
    int adapt_output;
    int adapt_verbose;

    /* MCA parameter: Maximum number of segment in context free list */
    int adapt_context_free_list_max;

    /* MCA parameter: Minimum number of segment in context free list */
    int adapt_context_free_list_min;

    /* MCA parameter: Increasement number of segment in context free list */
    int adapt_context_free_list_inc;

    /* Bcast MCA parameter */
    int adapt_ibcast_algorithm;
    size_t adapt_ibcast_segment_size;
    int adapt_ibcast_max_send_requests;
    int adapt_ibcast_max_recv_requests;
    bool adapt_ibcast_synchronous_send;
    /* Bcast free list */
    opal_free_list_t *adapt_ibcast_context_free_list;

    /* Reduce MCA parameter */
    int adapt_ireduce_algorithm;
    size_t adapt_ireduce_segment_size;
    int adapt_ireduce_max_send_requests;
    int adapt_ireduce_max_recv_requests;
    int adapt_inbuf_free_list_min;
    int adapt_inbuf_free_list_max;
    int adapt_inbuf_free_list_inc;
    bool adapt_ireduce_synchronous_send;

    /* Reduce free list */
    opal_free_list_t *adapt_ireduce_context_free_list;

} mca_coll_adapt_component_t;

/*
 * Structure used to store what is necessary for the collective operations
 * routines in case of fallback.
 */
typedef struct mca_coll_adapt_collective_fallback_s {
    union {
        mca_coll_base_module_reduce_fn_t   reduce;
        mca_coll_base_module_ireduce_fn_t ireduce;
    } previous_routine;
    mca_coll_base_module_t *previous_module;
} mca_coll_adapt_collective_fallback_t;


typedef enum mca_coll_adapt_colltype {
    ADAPT_REDUCE  = 0,
    ADAPT_IREDUCE = 1,
    ADAPT_COLLCOUNT
} mca_coll_adapt_colltype_t;

/*
 * Some defines to stick to the naming used in the other components in terms of
 * fallback routines
 */
#define previous_reduce     previous_routines[ADAPT_REDUCE].previous_routine.reduce
#define previous_ireduce    previous_routines[ADAPT_IREDUCE].previous_routine.ireduce

#define previous_reduce_module     previous_routines[ADAPT_REDUCE].previous_module
#define previous_ireduce_module    previous_routines[ADAPT_IREDUCE].previous_module


/* Coll adapt module per communicator*/
struct mca_coll_adapt_module_t {
    /* Base module */
    mca_coll_base_module_t super;

    /* To be able to fallback when the cases are not supported */
    struct mca_coll_adapt_collective_fallback_s previous_routines[ADAPT_COLLCOUNT];

    /* cached topologies */
    opal_list_t *topo_cache;

    /* Whether this module has been lazily initialized or not yet */
    bool adapt_enabled;
};
OBJ_CLASS_DECLARATION(mca_coll_adapt_module_t);

/* Global component instance */
OMPI_MODULE_DECLSPEC extern mca_coll_adapt_component_t mca_coll_adapt_component;

/* ADAPT module functions */
int ompi_coll_adapt_init_query(bool enable_progress_threads, bool enable_mpi_threads);
mca_coll_base_module_t * ompi_coll_adapt_comm_query(struct ompi_communicator_t *comm, int *priority);

/* ADAPT request free */
int ompi_coll_adapt_request_free(ompi_request_t **request);

#endif /* MCA_COLL_ADAPT_EXPORT_H */
