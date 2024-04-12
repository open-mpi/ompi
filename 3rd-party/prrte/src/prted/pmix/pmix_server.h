/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef _PMIX_SERVER_H_
#define _PMIX_SERVER_H_

#include "prte_config.h"

#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_globals.h"

BEGIN_C_DECLS

PRTE_EXPORT int pmix_server_init(void);
PRTE_EXPORT void pmix_server_start(void);
PRTE_EXPORT void pmix_server_finalize(void);
PRTE_EXPORT void pmix_server_register_params(void);

PRTE_EXPORT int prte_pmix_server_register_nspace(prte_job_t *jdata);

PRTE_EXPORT void prte_pmix_server_clear(pmix_proc_t *pname);

PRTE_EXPORT void pmix_server_notify_spawn(pmix_nspace_t jobid, int room, pmix_status_t ret);

END_C_DECLS

#endif /* PMIX_SERVER_H_ */
