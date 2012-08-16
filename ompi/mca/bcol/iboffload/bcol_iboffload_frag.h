/*
 * Copyright (c) 2009-2012 Oak Ridge National Laboratory.  All rights reserved.
 * Copyright (c) 2009-2012 Mellanox Technologies.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_BCOL_IBOFFLOAD_FRAG_H
#define MCA_BCOL_IBOFFLOAD_FRAG_H

#include "ompi_config.h"

#include <infiniband/verbs.h>

#include "opal/datatype/opal_convertor.h"

#include "ompi/mca/mpool/mpool.h"
#include "ompi/class/ompi_free_list.h"

#include "bcol_iboffload.h"

BEGIN_C_DECLS

/* forward declarations */
struct mca_bcol_iboffload_collreq_t;

struct mca_bcol_iboffload_reg_t {
    mca_mpool_base_registration_t base;
    struct ibv_mr *mr;
};
typedef struct mca_bcol_iboffload_reg_t mca_bcol_iboffload_reg_t;

typedef enum {
    MCA_BCOL_IBOFFLOAD_NONE_OWNER = -1,
    MCA_BCOL_IBOFFLOAD_DUMMY_OWNER,
    MCA_BCOL_IBOFFLOAD_BCOL_OWNER,
    MCA_BCOL_IBOFFLOAD_ML_OWNER
} frag_type;

typedef enum {
    MCA_BCOL_IBOFFLOAD_SEND_FRAG,
    MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML,
    MCA_BCOL_IBOFFLOAD_SEND_FRAG_ML_CALC,
    MCA_BCOL_IBOFFLOAD_SEND_FRAG_CONVERT,
    MCA_BCOL_IBOFFLOAD_SEND_FRAG_CALC,
    MCA_BCOL_IBOFFLOAD_SEND_FRAG_DUMMY
} send_frag_type;

struct mca_bcol_iboffload_frag_t {
    ompi_free_list_item_t super;

    struct mca_bcol_iboffload_frag_t *next;
    struct mca_bcol_iboffload_reg_t *registration;

    struct ibv_sge sg_entry;

    frag_type type;

    int ref_counter;
    int qp_index;
};
typedef struct mca_bcol_iboffload_frag_t mca_bcol_iboffload_frag_t;
OBJ_CLASS_DECLARATION(mca_bcol_iboffload_frag_t);

/* The same fragment maybe shared by multiple task.
 * In order to manage right release and allocation flow
 * we use reference counter on each fragment and the follow
 * wrapper allocation and release function that hides
 * the counter */

#define IBOFFLOAD_SET_SINGLE_FRAG_ON_TASK(fragment, task)           \
    do {                                                            \
        ++((fragment)->ref_counter);                                \
        (task)->frag = (fragment);                                  \
    } while(0)

#define IBOFFLOAD_SET_FRAGS_ON_TASK(fragment, task)                 \
    do {                                                            \
        struct mca_bcol_iboffload_frag_t *temp_frag = fragment;     \
        while (NULL != temp_frag) {                                 \
            ++(temp_frag->ref_counter);                             \
            temp_frag = temp_frag->next;                            \
        }                                                           \
        (task)->frag = fragment;                                    \
    } while(0)

/* function declarations */
mca_bcol_iboffload_frag_t *
mca_bcol_iboffload_get_send_frag(struct mca_bcol_iboffload_collreq_t *coll_request,
                                 uint32_t destination, int qp_index, size_t len,
                                 size_t src_offset, int buff_index, int send_frag_type);

void
mca_bcol_iboffload_frag_init(ompi_free_list_item_t* item, void* ctx);
void
mca_bcol_iboffload_ml_frag_init(ompi_free_list_item_t* item, void* ctx);

static inline __opal_attribute_always_inline__
mca_bcol_iboffload_frag_t* mca_bcol_iboffload_get_ml_empty_frag(
                    mca_bcol_iboffload_module_t *iboffload,
                    int qp_index)
{
    /* local variables */
    int rc;

    ompi_free_list_item_t *item;
    mca_bcol_iboffload_frag_t *frag;

    mca_bcol_iboffload_component_t *cm = &mca_bcol_iboffload_component;

    /* Get frag from free list */
    OMPI_FREE_LIST_GET(&cm->ml_frags_free, item, rc);
    if (OPAL_UNLIKELY(NULL == item)) {
        return NULL;
    }

    frag = (mca_bcol_iboffload_frag_t *) item;

    frag->qp_index = qp_index;
    frag->next = NULL;

    return frag;
}

static inline __opal_attribute_always_inline__
mca_bcol_iboffload_frag_t* mca_bcol_iboffload_get_ml_frag(
                    mca_bcol_iboffload_module_t *iboffload,
                    int qp_index, size_t len, uint32_t lkey, uint64_t addr)
{
    /* local variables */
    mca_bcol_iboffload_frag_t *frag;

    IBOFFLOAD_VERBOSE(10, ("Call for get ML frag - addr 0x%x", addr));

    frag = mca_bcol_iboffload_get_ml_empty_frag(iboffload, qp_index);

    frag->sg_entry.addr = addr;
    frag->sg_entry.lkey = lkey;
    frag->sg_entry.length = len;

    IBOFFLOAD_VERBOSE(10, ("Setting ml frag lkey %u, "
                           "addr %p, qp_index %d, send value - %lf",
                            frag->sg_entry.lkey, frag->sg_entry.addr,
                            qp_index, *(double *) frag->sg_entry.addr));

    return frag;
}

END_C_DECLS

#endif
