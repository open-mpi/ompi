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

OMPI_DECLSPEC void mca_common_cuda_init(void);

OMPI_DECLSPEC void mca_common_cuda_register(void *ptr, size_t amount, char *msg);

OMPI_DECLSPEC void mca_common_cuda_unregister(void *ptr, char *msg);

#endif /* OMPI_MCA_COMMON_CUDA_H */
