/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#ifndef PART_ONESIDED_MATCH_H
#define PART_ONESIDED_MATCH_H

#include "ompi_config.h"
#include "ompi/mca/part/onesided/part_onesided_request.h"
#include "ompi/communicator/communicator.h"
#include "opal/mutex.h"

BEGIN_C_DECLS

typedef struct {
    int src;
    int dst;
    int tag;
    int ctx;
} part_onesided_match_key_t;

typedef struct {
    mca_part_onesided_request_t *req;
    uint64_t id;
} part_onesided_match_entry_t;

typedef struct {
    opal_list_t list;
    opal_mutex_t lock;
} part_onesided_match_table_t;

/* API */
void mca_part_onesided_match_init(void);
void mca_part_onesided_match_finalize(void);

int mca_part_onesided_match_register(mca_part_onesided_request_t *req, 
                                    int peer, int tag, int ctx);
int mca_part_onesided_match_handle_init_msg(mca_part_onesided_request_t *req,
                                           int peer, int tag, int ctx,
                                           uint64_t remote_id, 
                                           void *metadata, size_t meta_size);

END_C_DECLS

#endif
