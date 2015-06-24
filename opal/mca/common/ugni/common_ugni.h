/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "opal/util/output.h"
#include "opal/util/proc.h"
#include "opal/class/opal_list.h"
#include "opal/include/opal/prefetch.h"
#include "opal_stdint.h"

#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <assert.h>
#include <sys/time.h>
#include <gni_pub.h>

#include "common_ugni_ep.h"

#if !defined(MPI_COMMON_UGNI_H)
#define MPI_COMMON_UGNI_H

struct opal_common_ugni_modex_t {
    uint32_t addr;
    int      id;
    gni_mem_handle_t irq_memhndl;
};
typedef struct opal_common_ugni_modex_t opal_common_ugni_modex_t;

struct opal_common_ugni_device_t {
    opal_object_t    super;

    gni_nic_handle_t dev_handle;

    /* Minor number of the Gemini NIC */
    int32_t          dev_id;
    uint32_t         dev_pe_addr;
    uint32_t         dev_addr;
    uint32_t         dev_cpu_id;

    size_t           dev_ep_count;
    opal_mutex_t     dev_lock;
    gni_mem_handle_t smsg_irq_mhndl;
    void             *btl_ctx;
};
typedef struct opal_common_ugni_device_t opal_common_ugni_device_t;

struct opal_common_ugni_module_t {
    /* protection tag */
    uint8_t ptag;

    /* unique id for this process assigned by the system */
    uint32_t cookie;

    /* communication domain handle */
    gni_cdm_handle_t cd_handle;

    /* device count. to be used if we have more than 1 common per ugni device */
    int device_count;
    opal_common_ugni_device_t *devices;

    int rdma_max_retries;
};
typedef struct opal_common_ugni_module_t opal_common_ugni_module_t;

struct opal_common_ugni_post_desc_t {
    gni_post_descriptor_t base;

    opal_common_ugni_endpoint_t *endpoint;
    int tries;
};
typedef struct opal_common_ugni_post_desc_t opal_common_ugni_post_desc_t;

extern opal_common_ugni_module_t opal_common_ugni_module;
extern mca_base_component_t opal_common_ugni_component;

static inline int
opal_common_rc_ugni_to_opal (gni_return_t rc)
{
    int codes[] = {OPAL_SUCCESS,
                   OPAL_ERR_RESOURCE_BUSY,
                   OPAL_ERR_BAD_PARAM,
                   OPAL_ERR_OUT_OF_RESOURCE,
                   OPAL_ERR_TIMEOUT,
                   OPAL_ERR_PERM,
                   OPAL_ERROR,
                   OPAL_ERR_BAD_PARAM,
                   OPAL_ERR_BAD_PARAM,
                   OPAL_ERR_NOT_FOUND,
                   OPAL_ERR_VALUE_OUT_OF_BOUNDS,
                   OPAL_ERROR,
                   OPAL_ERR_NOT_SUPPORTED,
                   OPAL_ERR_OUT_OF_RESOURCE};
    return codes[rc];
}

/*
 * Initialize uGNI communication domain and device(s).
 */
int opal_common_ugni_init (void);

/*
 * Finalize uGNI communication domain and device(s).
 */
int opal_common_ugni_fini (void);

#endif /* MPI_COMMON_UGNI_H */
