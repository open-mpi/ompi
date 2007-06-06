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
 * Copyright (c) 2006-2007 Voltaire All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>

#include "opal/mca/base/mca_base_param.h"
#include "ompi/mca/btl/btl.h"
#include "ompi/mca/btl/base/base.h"


int mca_btl_base_param_register(mca_base_component_t *version,
        mca_btl_base_module_t *module)
{
    int value, err = 0;
#define REG_INT(N, H, D, L, T) \
    mca_base_param_reg_int(version, N, H, false, false, D, &value); \
    if(value < (L)) \
        err = -1; \
    else \
        D = (T)value;

    REG_INT("exclusivity", "BTL exclusivity (must be >= 0)",
            module->btl_exclusivity, 0, uint32_t);

    REG_INT("flags", "BTL flags, SEND=1, PUT=2, GET=4", module->btl_flags,
            0, uint32_t);

    REG_INT("eager_limit", "Eager send limit, in bytes (must be >= 1)",
            module->btl_eager_limit, 1, size_t);

    REG_INT("min_send_size", "Maximum send size, in bytes (must be >= 1)",
            module->btl_min_send_size, 1, size_t);

    REG_INT("max_send_size", "Maximum send size, in bytes (must be >= 1)",
             module->btl_max_send_size, 1, size_t);

    if(module->btl_flags & MCA_BTL_FLAGS_PUT) {
        mca_base_param_reg_int(version, "min_rdma_size", "", true, false,
                0, &value);
        if(value != 0) {
            opal_output(0, "min_rdma_size parameter is deprecated. Please use "
                    "rdma_pipeline_offset instead\n");
            module->btl_rdma_pipeline_offset = (size_t)value;
        }

        REG_INT("rdma_pipeline_offset", "Offset the pipeline protocol starts "
            "using RDMA from (must be >= 0)", module->btl_rdma_pipeline_offset,
            0, size_t);

        mca_base_param_reg_int(version, "max_rdma_size", "", true, false,
                0, &value);
        if(value != 0) {
            opal_output(0, "max_rdma_size parameter is deprecated. Please use "
                    "rdma_pipeline_frag_size instead\n");
            module->btl_rdma_pipeline_frag_size = (size_t)value;
        }

        REG_INT("rdma_pipeline_frag_size", "The size of the chunk the data "
            "RDMAed by pipeline protocol (must be >= 1)",
            module->btl_rdma_pipeline_frag_size, 1, size_t);

        REG_INT("min_rdma_pipeline_size", "Packets smaller than this value will"
            " not be subject for pipeline protocol "
            "(must be >= 0, 0 means the same as rdma_pipeline_offset)",
            module->btl_min_rdma_pipeline_size, 0, size_t);
    if(module->btl_min_rdma_pipeline_size == 0)
        module->btl_min_rdma_pipeline_size = module->btl_rdma_pipeline_offset;
    }

    REG_INT("bandwidth", "Approximate maximum bandwidth of interconnect"
            "(must be >= 1)", module->btl_bandwidth, 1, uint32_t);

    REG_INT("latency", "Approximate latency of interconnect (must be >= 0)",
            module->btl_latency, 0, uint32_t);

    return err;
}
