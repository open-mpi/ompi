/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2012-2020 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COLL_FTAGREE_EXPORT_H
#define MCA_COLL_FTAGREE_EXPORT_H

#include "ompi_config.h"

#include "opal/mca/mca.h"

#include "opal/class/opal_bitmap.h"
#include "opal/class/opal_free_list.h"

#include "ompi/mca/coll/coll.h"
#include "ompi/mca/coll/base/coll_tags.h"
#include "ompi/request/request.h"
#include "ompi/group/group.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"

BEGIN_C_DECLS

/* Globally exported variables */

OMPI_DECLSPEC extern const mca_coll_base_component_2_4_0_t
mca_coll_ftagree_component;
extern int mca_coll_ftagree_priority;

enum mca_coll_ftagree_algorithm_t {
    COLL_FTAGREE_NOFT = 0,
    COLL_FTAGREE_EARLY_RETURNING   = 1,
    COLL_FTAGREE_EARLY_TERMINATION = 2
};
typedef enum mca_coll_ftagree_algorithm_t mca_coll_ftagree_algorithm_t;

extern mca_coll_ftagree_algorithm_t mca_coll_ftagree_algorithm;
extern int mca_coll_ftagree_era_rebuild;

/* Define this to enable testing random failures in various
 * places in Agree. This can be used to harden the agreement 
 * against failures that happen at various places between
 * message events.
 */
#if !defined(OPAL_ENABLE_DEBUG)
#undef FTAGREE_DEBUG_FAILURE_INJECT
#endif
#if defined(FTAGREE_DEBUG_FAILURE_INJECT)
extern double mca_coll_ftagree_debug_inject_proba;
#endif

/*
 * Base agreement structure
 * Individual agreement algorithms will extend this struct as needed
 */
struct mca_coll_ftagree_t {
    /* This is a general object */
    opal_object_t super;

    /* Agreement Sequence Number */
    int agreement_seq_num;
};
typedef struct mca_coll_ftagree_t mca_coll_ftagree_t;
OBJ_CLASS_DECLARATION(mca_coll_ftagree_t);

struct mca_coll_ftagree_module_t {
    mca_coll_base_module_t super;

    /* Array of requests */
    ompi_request_t **mccb_reqs;
    int mccb_num_reqs;

    /* Array of statuses */
    ompi_status_public_t *mccb_statuses;
    int mccb_num_statuses;

    /* Pointer to the agreement structure */
    mca_coll_ftagree_t *agreement_info;
};
typedef struct mca_coll_ftagree_module_t mca_coll_ftagree_module_t;
OBJ_CLASS_DECLARATION(mca_coll_ftagree_module_t);

/*
 * API functions
 */
int mca_coll_ftagree_init_query(bool enable_progress_threads,
                                bool enable_mpi_threads);
mca_coll_base_module_t
*mca_coll_ftagree_comm_query(struct ompi_communicator_t *comm,
                             int *priority);

int mca_coll_ftagree_module_enable(mca_coll_base_module_t *module,
                                   struct ompi_communicator_t *comm);

/*
 * Agreement algorithms
 */

/* Early termination algorithm */
int
mca_coll_ftagree_eta_intra(     void* contrib,
                                int dt_count,
                                ompi_datatype_t *dt,
                                ompi_op_t *op,
                                ompi_group_t **group, bool grp_update,
                                ompi_communicator_t* comm,
                                mca_coll_base_module_t *module);
/* Early returning algorithm */
int
mca_coll_ftagree_era_intra(     void* contrib,
                                int dt_count,
                                ompi_datatype_t *dt,
                                ompi_op_t *op,
                                ompi_group_t **group, bool grp_update,
                                ompi_communicator_t* comm,
                                mca_coll_base_module_t *module);
int mca_coll_ftagree_iera_intra(void* contrib,
                                int dt_count,
                                ompi_datatype_t *dt,
                                ompi_op_t *op,
                                ompi_group_t **group, bool grp_update,
                                ompi_communicator_t* comm,
                                ompi_request_t **request,
                                mca_coll_base_module_t *module);
int mca_coll_ftagree_era_inter( void* contrib,
                                int dt_count,
                                ompi_datatype_t *dt,
                                ompi_op_t *op,
                                ompi_group_t **group, bool grp_update,
                                ompi_communicator_t* comm,
                                mca_coll_base_module_t *module);

/*
 * Utility functions
 */
static inline void mca_coll_ftagree_free_reqs(ompi_request_t ** reqs,
                                              int count)
{
    int i;
    for (i = 0; i < count; ++i) {
        if( OMPI_REQUEST_INVALID != reqs[i]->req_state ) {
            ompi_request_free(&reqs[i]);
        }
    }
}

END_C_DECLS

#endif /* MCA_COLL_FTAGREE_EXPORT_H */
