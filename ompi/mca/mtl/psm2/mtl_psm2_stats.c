/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2006-2017 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "psm2.h"
#include "ompi/communicator/communicator.h"
#include "ompi/message/message.h"

#include "opal/mca/base/mca_base_pvar.h"

struct ompi_mtl_psm2_name_descs
{
    char *name;
    char *desc;
    ptrdiff_t offset;
};

const struct ompi_mtl_psm2_name_descs name_descs[PSM2_MQ_NUM_STATS] =
{
    { "rx_user_bytes", "Bytes received into a matched user buffer", 
      offsetof(struct psm2_mq_stats, rx_user_bytes) },
    { "rx_user_num", "Messages received into a matched user buffer",
      offsetof(struct psm2_mq_stats, rx_user_num) },
    { "rx_sys_bytes", "Bytes received into an unmatched system buffer",
      offsetof(struct psm2_mq_stats, rx_sys_bytes) },
    { "rx_sys_num", "Messages received into an unmatched system buffer",
      offsetof(struct psm2_mq_stats, rx_sys_num) },
    { "tx_num", "Total Messages transmitted (shm and hfi)",
      offsetof(struct psm2_mq_stats, tx_num) },
    { "tx_eager_num", "Messages transmitted eagerly",
      offsetof(struct psm2_mq_stats, tx_eager_num) },
    { "tx_eager_bytes", "Bytes transmitted eagerl",
      offsetof(struct psm2_mq_stats, tx_eager_bytes) },
    { "tx_rndv_num", "Messages transmitted using expected TID mechanism",
      offsetof(struct psm2_mq_stats, tx_rndv_num) },
    { "tx_rndv_bytes", "Bytes transmitted using expected TID mechanism",
      offsetof(struct psm2_mq_stats, tx_rndv_bytes) },
    { "tx_shm_num", "Messages transmitted (shm only)",
      offsetof(struct psm2_mq_stats, tx_shm_num) },
    { "rx_shm_num", "Messages received through shm",
      offsetof(struct psm2_mq_stats, rx_shm_num) },
    { "rx_sysbuf_num", "Number of system buffers allocated",
      offsetof(struct psm2_mq_stats, rx_sysbuf_num) },
    { "rx_sysbuf_bytes", "Bytes allocated for system buffers",
      offsetof(struct psm2_mq_stats, rx_sysbuf_bytes) },
};
 
static int mca_mtl_psm2_get_stats(const mca_base_pvar_t *pvar, void *value, void *obj)
{
    psm2_mq_stats_t stats;
    int index = (int)(intptr_t) pvar->ctx;

    psm2_mq_get_stats(ompi_mtl_psm2.mq, &stats);

    *(uint64_t *)value = *(uint64_t *)((uint8_t *)&stats + name_descs[index].offset); 

    return OMPI_SUCCESS;
}


int ompi_mtl_psm2_register_pvars(void)
{
    int i;

    /* PSM2 MQ performance variables */
    for (i = 0 ; i < PSM2_MQ_NUM_STATS; ++i) {
        (void) mca_base_component_pvar_register (&mca_mtl_psm2_component.super.mtl_version, 
                                                 name_descs[i].name, name_descs[i].desc,
                                                 OPAL_INFO_LVL_4, MCA_BASE_PVAR_CLASS_COUNTER,
                                                 MCA_BASE_VAR_TYPE_UNSIGNED_LONG,
                                                 NULL, MCA_BASE_VAR_BIND_NO_OBJECT,
                                                 MCA_BASE_PVAR_FLAG_READONLY | MCA_BASE_PVAR_FLAG_CONTINUOUS,
                                                 mca_mtl_psm2_get_stats, NULL, NULL,
                                                 (void *) (intptr_t) i);
    }
    return OMPI_SUCCESS;
}
