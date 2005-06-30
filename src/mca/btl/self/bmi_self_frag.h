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
#ifndef MCA_BMI_SELF_SEND_FRAG_H
#define MCA_BMI_SELF_SEND_FRAG_H

#include <sys/types.h>
#include "ompi_config.h"
#include "class/ompi_free_list.h"
#include "bmi_self.h"


/**
 * shared memory send fragment derived type.
 */
struct mca_bmi_self_frag_t {
    mca_bmi_base_descriptor_t base;
    mca_bmi_base_segment_t segment;
    struct mca_bmi_base_endpoint_t *endpoint;
    size_t size;
};
typedef struct mca_bmi_self_frag_t mca_bmi_self_frag_t;
typedef struct mca_bmi_self_frag_t mca_bmi_self_frag_eager_t;
typedef struct mca_bmi_self_frag_t mca_bmi_self_frag_send_t;
typedef struct mca_bmi_self_frag_t mca_bmi_self_frag_rdma_t;

OBJ_CLASS_DECLARATION(mca_bmi_self_frag_eager_t);
OBJ_CLASS_DECLARATION(mca_bmi_self_frag_send_t);
OBJ_CLASS_DECLARATION(mca_bmi_self_frag_rdma_t);


#define MCA_BMI_SELF_FRAG_ALLOC_EAGER(frag, rc)                              \
{                                                                            \
    ompi_list_item_t* item;                                                  \
    OMPI_FREE_LIST_WAIT(&mca_bmi_self_component.self_frags_eager, item, rc); \
    frag = (mca_bmi_self_frag_t*)item;                                       \
}

#define MCA_BMI_SELF_FRAG_RETURN_EAGER(frag)                                 \
{                                                                            \
    OMPI_FREE_LIST_RETURN(&mca_bmi_self_component.self_frags_eager, (ompi_list_item_t*)(frag)); \
}

#define MCA_BMI_SELF_FRAG_ALLOC_SEND(frag, rc)                               \
{                                                                            \
    ompi_list_item_t* item;                                                  \
    OMPI_FREE_LIST_WAIT(&mca_bmi_self_component.self_frags_send, item, rc);  \
    frag = (mca_bmi_self_frag_t*)item;                                       \
}

#define MCA_BMI_SELF_FRAG_RETURN_SEND(frag)                                  \
{                                                                            \
    OMPI_FREE_LIST_RETURN(&mca_bmi_self_component.self_frags_send, (ompi_list_item_t*)(frag)); \
}

#define MCA_BMI_SELF_FRAG_ALLOC_RDMA(frag, rc)                               \
{                                                                            \
    ompi_list_item_t* item;                                                  \
    OMPI_FREE_LIST_WAIT(&mca_bmi_self_component.self_frags_rdma, item, rc);  \
    frag = (mca_bmi_self_frag_t*)item;                                       \
}

#define MCA_BMI_SELF_FRAG_RETURN_RDMA(frag)                                  \
{                                                                            \
    OMPI_FREE_LIST_RETURN(&mca_bmi_self_component.self_frags_rdma, (ompi_list_item_t*)(frag)); \
}

#endif

