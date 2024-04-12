/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2016-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#ifndef IOF_BASE_SETUP_H_
#define IOF_BASE_SETUP_H_

#include "prte_config.h"
#include "types.h"

#include "src/mca/iof/base/base.h"

struct prte_iof_base_io_conf_t {
    int usepty;
    bool connect_stdin;

    /* private - callers should not modify these fields */
    int p_stdin[2];
    int p_stdout[2];
    int p_stderr[2];
};
typedef struct prte_iof_base_io_conf_t prte_iof_base_io_conf_t;

/**
 * Do pre-fork IOF setup tasks
 *
 * Do all stdio forwarding that must be done before fork() is called.
 * This might include creating pipes or ptys or similar work.
 */
PRTE_EXPORT int prte_iof_base_setup_prefork(prte_iof_base_io_conf_t *opts);

PRTE_EXPORT int prte_iof_base_setup_child(prte_iof_base_io_conf_t *opts, char ***env);

PRTE_EXPORT int prte_iof_base_setup_parent(const pmix_proc_t *name, prte_iof_base_io_conf_t *opts);

#endif
