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
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
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
 * Data server for PRTE
 */
#ifndef PRTE_DATA_SERVER_H
#define PRTE_DATA_SERVER_H

#include "prte_config.h"
#include "types.h"

#include "src/rml/rml_types.h"
#include "src/pmix/pmix-internal.h"

BEGIN_C_DECLS

#define PRTE_PMIX_PUBLISH_CMD    0x01
#define PRTE_PMIX_LOOKUP_CMD     0x02
#define PRTE_PMIX_UNPUBLISH_CMD  0x03
#define PRTE_PMIX_PURGE_PROC_CMD 0x04

/* provide hooks to startup and finalize the data server */
PRTE_EXPORT int prte_data_server_init(void);
PRTE_EXPORT void prte_data_server_finalize(void);

/* provide hook for the non-blocking receive */
PRTE_EXPORT void prte_data_server(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                  prte_rml_tag_t tag, void *cbdata);

END_C_DECLS

#endif /* PRTE_DATA_SERVER_H */
