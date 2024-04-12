/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, Inc.  All rights reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef PRTE_MCA_ERRMGR_BASE_H
#define PRTE_MCA_ERRMGR_BASE_H

/*
 * includes
 */
#include "prte_config.h"
#include "constants.h"

#include "src/class/pmix_list.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/mca.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_errmgr_base_framework;
/* select a component */
PRTE_EXPORT int prte_errmgr_base_select(void);

END_C_DECLS

#endif
