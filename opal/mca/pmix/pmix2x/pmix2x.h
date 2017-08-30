/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PMIX_PMIX2X_H
#define MCA_PMIX_PMIX2X_H

#include "opal_config.h"

#include "opal/mca/pmix/pmix.h"

BEGIN_C_DECLS

#ifdef OPAL_C_HAVE_VISIBILITY
#define PMIX_HAVE_VISIBILITY 1
#else
#undef PMIX_HAVE_VISIBILITY
#endif

OPAL_DECLSPEC opal_pmix_base_component_t mca_pmix_pmix2x_component;

END_C_DECLS

#endif /* MCA_PMIX_EXTERNAL_H */
