/*
 * Copyright (c) 2012      Los Alamos National Security, LLC.
 *                         All rights reserved
 * Copyright (c) 2018-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PRRtE_FILEM_RAW_EXPORT_H
#define PRRtE_FILEM_RAW_EXPORT_H

#include "prte_config.h"

#include "src/class/pmix_object.h"
#include "src/event/event-internal.h"
#include "src/mca/mca.h"

#include "src/mca/filem/filem.h"

BEGIN_C_DECLS

PRTE_MODULE_EXPORT extern prte_filem_base_component_t prte_mca_filem_raw_component;
PRTE_EXPORT extern prte_filem_base_module_t prte_filem_raw_module;

extern bool prte_filem_raw_flatten_trees;

#define PRTE_FILEM_RAW_CHUNK_MAX 16384

/* local classes */
typedef struct {
    pmix_list_item_t super;
    pmix_list_t xfers;
    int32_t status;
    prte_filem_completion_cbfunc_t cbfunc;
    void *cbdata;
} prte_filem_raw_outbound_t;
PMIX_CLASS_DECLARATION(prte_filem_raw_outbound_t);

typedef struct {
    pmix_list_item_t super;
    prte_event_t ev;
    int fd;
    prte_filem_raw_outbound_t *outbound;
    prte_app_idx_t app_idx;
    bool pending;
    char *src;
    char *file;
    int32_t type;
    int32_t nchunk;
    int status;
    pmix_rank_t nrecvd;
} prte_filem_raw_xfer_t;
PMIX_CLASS_DECLARATION(prte_filem_raw_xfer_t);

typedef struct {
    pmix_list_item_t super;
    prte_app_idx_t app_idx;
    prte_event_t ev;
    bool pending;
    int fd;
    char *file;
    char *top;
    char *fullpath;
    int32_t type;
    char **link_pts;
    pmix_list_t outputs;
} prte_filem_raw_incoming_t;
PMIX_CLASS_DECLARATION(prte_filem_raw_incoming_t);

typedef struct {
    pmix_list_item_t super;
    int numbytes;
    unsigned char data[PRTE_FILEM_RAW_CHUNK_MAX];
} prte_filem_raw_output_t;
PMIX_CLASS_DECLARATION(prte_filem_raw_output_t);

END_C_DECLS

#endif /* PRRtE_FILEM_RAW_EXPORT_H */
