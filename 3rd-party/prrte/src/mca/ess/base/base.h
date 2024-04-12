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
 * Copyright (c) 2011-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_ESS_BASE_H
#define MCA_ESS_BASE_H

#include "prte_config.h"
#include "types.h"

#include "src/mca/base/pmix_mca_base_framework.h"
#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"

#include "src/mca/ess/ess.h"

BEGIN_C_DECLS

/*
 * MCA Framework
 */
PRTE_EXPORT extern pmix_mca_base_framework_t prte_ess_base_framework;
/**
 * Select a ess module
 */
PRTE_EXPORT int prte_ess_base_select(void);

/*
 * stdout/stderr buffering control parameter
 */
PRTE_EXPORT extern int prte_ess_base_std_buffering;

PRTE_EXPORT extern int prte_ess_base_num_procs;
PRTE_EXPORT extern char *prte_ess_base_nspace;
PRTE_EXPORT extern char *prte_ess_base_vpid;
PRTE_EXPORT extern pmix_list_t prte_ess_base_signals;

/*
 * Internal helper functions used by components
 */
PRTE_EXPORT int prte_ess_env_get(void);

PRTE_EXPORT int prte_ess_base_std_prolog(void);

PRTE_EXPORT int prte_ess_base_prted_setup(void);
PRTE_EXPORT int prte_ess_base_prted_finalize(void);

PRTE_EXPORT int prte_ess_base_setup_signals(char *signals);

/* Detect whether or not this proc is bound - if not,
 * see if it should bind itself
 */
PRTE_EXPORT int prte_ess_base_proc_binding(void);

/*
 * Put functions
 */
PRTE_EXPORT int prte_ess_env_put(int32_t num_procs, int32_t num_local_procs, char ***env);

/* read a bootstrap configuration file */
PRTE_EXPORT int prte_ess_base_bootstrap(void);

typedef struct {
    pmix_list_item_t super;
    char *signame;
    int signal;
    bool can_forward;
} prte_ess_base_signal_t;
PMIX_CLASS_DECLARATION(prte_ess_base_signal_t);

END_C_DECLS

#endif
