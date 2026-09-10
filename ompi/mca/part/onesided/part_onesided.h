/*
 * Copyright (c) 2026 Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * $HEADER$
 */

#ifndef PART_ONESIDED_H
#define PART_ONESIDED_H

#include "ompi_config.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/onesided/part_onesided_control.h"

BEGIN_C_DECLS

struct ompi_part_onesided_t {
    mca_part_base_module_t super;
    int verbose;
    bool require_rdma;
    int max_outstanding_puts;
};
typedef struct ompi_part_onesided_t ompi_part_onesided_t;
extern ompi_part_onesided_t ompi_part_onesided;

/* Control Message API */
int mca_part_onesided_send_ctrl(struct mca_btl_base_endpoint_t *endpoint,
                               mca_btl_base_module_t *btl,
                               part_onesided_ctrl_type_t type,
                               uint64_t req_id,
                               void *payload, size_t payload_size);
void mca_part_onesided_recv_ctrl(void *payload, size_t payload_size, 
                                 struct mca_btl_base_endpoint_t *endpoint);
int mca_part_onesided_ctrl_progress(void);

END_C_DECLS

#endif
