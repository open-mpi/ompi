/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2008 Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * @file
 *
 * Interface into the PRTE Run Time Environment
 */
#ifndef PRTE_RUNTIME_INTERNALS_H
#define PRTE_RUNTIME_INTERNALS_H

#include "prte_config.h"

BEGIN_C_DECLS

/**
 * Init the PRTE datatype support
 */
PRTE_EXPORT int prte_dt_init(void);

PRTE_EXPORT void prte_preload_default_mca_params(void);

END_C_DECLS

#endif /* PRTE_RUNTIME_INTERNALS_H */
