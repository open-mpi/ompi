/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2021      Google, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OPAL_MCA_SMSC_CMA_SMSC_CMA_INTERNAL_H
#define OPAL_MCA_SMSC_CMA_SMSC_CMA_INTERNAL_H

#include "opal/mca/smsc/cma/smsc_cma.h"

struct mca_smsc_cma_modex_t {
    pid_t pid;
    ino_t user_ns_id;
};

typedef struct mca_smsc_cma_modex_t mca_smsc_cma_modex_t;

struct mca_smsc_cma_endpoint_t {
    mca_smsc_endpoint_t super;
    pid_t pid;
};

typedef struct mca_smsc_cma_endpoint_t mca_smsc_cma_endpoint_t;

OBJ_CLASS_DECLARATION(mca_smsc_cma_endpoint_t);

extern mca_smsc_module_t mca_smsc_cma_module;
extern mca_smsc_component_t mca_smsc_cma_component;

ino_t mca_smsc_cma_get_user_ns_id(void);

#endif /* OPAL_MCA_SMSC_CMA_SMSC_CMA_INTERNAL_H */
