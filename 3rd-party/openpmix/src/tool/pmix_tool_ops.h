/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 */

#ifndef PMIX_TOOL_OPS_H
#define PMIX_TOOL_OPS_H

#include "src/include/pmix_config.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_types.h"

PMIX_EXPORT pmix_status_t pmix_tool_relay_op(pmix_cmd_t cmd, pmix_peer_t *peer, pmix_buffer_t *bfr,
                                             uint32_t tag);

#endif // PMIX_TOOL_OPS_H
