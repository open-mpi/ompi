/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_BFROPS_PMIX4_H
#define PMIX_BFROPS_PMIX4_H

#include "src/mca/bfrops/bfrops.h"

BEGIN_C_DECLS

/* the component must be visible data for the linker to find it */
 PMIX_EXPORT extern pmix_bfrops_base_component_t mca_bfrops_v4_component;

extern pmix_bfrops_module_t pmix_bfrops_pmix4_module;

END_C_DECLS

#endif /* PMIX_BFROPS_PMIX4_H */
