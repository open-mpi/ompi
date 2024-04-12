/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#ifndef PMIX_INIT_UTIL_H
#define PMIX_INIT_UTIL_H

#include "src/include/pmix_config.h"
#include "pmix_common.h"

BEGIN_C_DECLS

PMIX_EXPORT extern const char* pmix_tool_basename;
PMIX_EXPORT extern const char* pmix_tool_version;
PMIX_EXPORT extern const char* pmix_tool_org;
PMIX_EXPORT extern const char* pmix_tool_msg;

PMIX_EXPORT int pmix_init_util(pmix_info_t info[], size_t ninfo, char *helpdir);
PMIX_EXPORT int pmix_finalize_util(void);

PMIX_EXPORT void pmix_expose_param(char *param);

END_C_DECLS

#endif /* PMIX_INIT_UTIL_H */
