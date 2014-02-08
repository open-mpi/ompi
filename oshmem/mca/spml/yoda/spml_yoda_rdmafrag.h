/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 *  @file
 */

#ifndef MCA_SPML_YODA_RDMAFRAG_H
#define MCA_SPML_YODA_RDMAFRAG_H

#include "ompi/mca/btl/btl.h"
#include "opal/types.h"
#include "opal/util/arch.h"
#include "oshmem/proc/proc.h"

BEGIN_C_DECLS

typedef enum {
    MCA_SPML_YODA_RDMA_PUT,
    MCA_SPML_YODA_RDMA_GET
} mca_spml_yoda_rdma_state_t;

typedef union mca_spml_yoda_segment_t {
    mca_btl_base_segment_t base_seg;
} mca_spml_yoda_segment_t;

struct mca_spml_yoda_rdma_frag_t {
    mca_spml_yoda_segment_t rdma_segs[2];
    mca_btl_base_segment_t *btl_seg; /* save pointer to btl allocated descriptor segment */
    void *rdma_req;
    int allocated;
    int use_send;
    int size;
};

typedef struct mca_spml_yoda_rdma_frag_t mca_spml_yoda_rdma_frag_t;
END_C_DECLS
#endif

