/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef PRTE_BACKTRACE_BASE_H
#define PRTE_BACKTRACE_BASE_H

#include "prte_config.h"

#include "src/mca/base/pmix_mca_base_framework.h"

#include "src/mca/prtebacktrace/prtebacktrace.h"

/*
 * Global functions for MCA overall backtrace open and close
 */

BEGIN_C_DECLS

PRTE_EXPORT extern pmix_mca_base_framework_t prte_prtebacktrace_base_framework;

END_C_DECLS
#endif /* PRTE_BASE_BACKTRACE_H */
