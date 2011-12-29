/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011      NVIDIA Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MCA_COMMON_CUDA_H
#define OMPI_MCA_COMMON_CUDA_H
#include "ompi/mca/btl/btl.h"

struct mca_mpool_rcuda_reg_t {
    mca_mpool_base_registration_t base;
    uint64_t memHandle[8];
    uint64_t evtHandle[8];
    uint64_t event;
};
typedef struct mca_mpool_rcuda_reg_t mca_mpool_rcuda_reg_t;


OMPI_DECLSPEC void mca_common_cuda_register(void *ptr, size_t amount, char *msg);

OMPI_DECLSPEC void mca_common_cuda_unregister(void *ptr, char *msg);

OMPI_DECLSPEC void mca_common_wait_stream_synchronize(mca_mpool_rcuda_reg_t *rget_reg);

OMPI_DECLSPEC int mca_common_cuda_memcpy(void *dst, void *src, size_t amount, char *msg,
                                         struct mca_btl_base_descriptor_t *, int *done);

OMPI_DECLSPEC int progress_one_cuda_event(struct mca_btl_base_descriptor_t **);

OMPI_DECLSPEC int mca_common_cuda_memhandle_matches(mca_mpool_rcuda_reg_t *new_reg,
                                                    mca_mpool_rcuda_reg_t *old_reg);

OMPI_DECLSPEC void mca_common_cuda_construct_event_and_handle(uint64_t **event, void **handle);
OMPI_DECLSPEC void mca_common_cuda_destruct_event(uint64_t *event);

OMPI_DECLSPEC int cuda_getmemhandle(void *base, size_t, mca_mpool_base_registration_t *newreg,
                                    mca_mpool_base_registration_t *hdrreg);
OMPI_DECLSPEC int cuda_ungetmemhandle(void *reg_data, mca_mpool_base_registration_t *reg);
OMPI_DECLSPEC int cuda_openmemhandle(void *base, size_t size, mca_mpool_base_registration_t *newreg,
                                     mca_mpool_base_registration_t *hdrreg);
OMPI_DECLSPEC int cuda_closememhandle(void *reg_data, mca_mpool_base_registration_t *reg);


#endif /* OMPI_MCA_COMMON_CUDA_H */
