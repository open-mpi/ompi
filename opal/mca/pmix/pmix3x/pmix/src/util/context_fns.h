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
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file:
 *
 */

#ifndef _PMIX_CONTEXT_FNS_H_
#define _PMIX_CONTEXT_FNS_H_

#include "pmix_config.h"
#include "include/pmix_common.h"

BEGIN_C_DECLS

PMIX_EXPORT int pmix_util_check_context_app(pmix_app_t *app, char **env);

PMIX_EXPORT int pmix_util_check_context_cwd(pmix_app_t *app);

END_C_DECLS
#endif
