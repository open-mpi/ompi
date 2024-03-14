/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2013 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2011-2015 NVIDIA Corporation.  All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/* Implements a progress engine based accelerator asynchronous copy implementation */

#ifndef OMPI_PML_OB1_ACCELERATOR_H
#define OMPI_PML_OB1_ACCELERATOR_H

#include "opal/mca/accelerator/accelerator.h"
#include "opal/mca/btl/btl.h"

OPAL_DECLSPEC int mca_pml_ob1_record_htod_event(char *msg, struct mca_btl_base_descriptor_t *frag);
OPAL_DECLSPEC opal_accelerator_stream_t *mca_pml_ob1_get_dtoh_stream(void);
OPAL_DECLSPEC opal_accelerator_stream_t *mca_pml_ob1_get_htod_stream(void);
OPAL_DECLSPEC int mca_pml_ob1_progress_one_htod_event(struct mca_btl_base_descriptor_t **);
OPAL_DECLSPEC int mca_pml_ob1_accelerator_init(void);
OPAL_DECLSPEC void mca_pml_ob1_accelerator_fini(void);

#endif /* OMPI_PML_OB1_ACCELERATOR_H */
