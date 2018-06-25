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

BEGIN_C_DECLS typedef struct mca_coll_adapt_module_t mca_coll_adapt_module_t;

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

    /* MCA parameter: Output verbose level */
    int adapt_output;

    /* MCA parameter: Maximum number of segment in context free list */
    int adapt_context_free_list_max;

    /* MCA parameter: Minimum number of segment in context free list */
    int adapt_context_free_list_min;

    /* MCA parameter: Increasment number of segment in context free list */
    int adapt_context_free_list_inc;

    /* Bcast MCA parameter */
    int adapt_ibcast_algorithm;
    size_t adapt_ibcast_segment_size;
    int adapt_ibcast_max_send_requests;
    int adapt_ibcast_max_recv_requests;
    /* Bcast free list */
    opal_free_list_t *adapt_ibcast_context_free_list;
    _Atomic int32_t adapt_ibcast_context_free_list_enabled;

    /* Reduce MCA parameter */
    int adapt_ireduce_algorithm;
    size_t adapt_ireduce_segment_size;
    int adapt_ireduce_max_send_requests;
    int adapt_ireduce_max_recv_requests;
    int adapt_inbuf_free_list_min;
    int adapt_inbuf_free_list_max;
    int adapt_inbuf_free_list_inc;

    /* Reduce free list */
    opal_free_list_t *adapt_ireduce_context_free_list;
    _Atomic int32_t adapt_ireduce_context_free_list_enabled;

} mca_coll_adapt_component_t;

/* Coll adapt module per communicator*/
struct mca_coll_adapt_module_t {
    /* Base module */
    mca_coll_base_module_t super;

    /* Whether this module has been lazily initialized or not yet */
    bool enabled;
    /* Pointer to mca_coll_adapt_component */
    mca_coll_adapt_component_t *adapt_component;
};
OBJ_CLASS_DECLARATION(mca_coll_adapt_module_t);

/* Global component instance */
OMPI_MODULE_DECLSPEC extern mca_coll_adapt_component_t mca_coll_adapt_component;

/* ADAPT module functions */
int mca_coll_adapt_init_query(bool enable_progress_threads, bool enable_mpi_threads);

mca_coll_base_module_t *mca_coll_adapt_comm_query(struct ompi_communicator_t *comm, int *priority);

/* Free ADAPT quest */
int adapt_request_free(ompi_request_t ** request);

#endif                          /* MCA_COLL_ADAPT_EXPORT_H */
