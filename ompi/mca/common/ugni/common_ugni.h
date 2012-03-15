/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2011      UT-Battelle, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/util/output.h"
#include "opal/class/opal_list.h"
#include "opal/include/opal/prefetch.h"
#include "opal_stdint.h"

#include "ompi/include/ompi/constants.h"
#include "ompi/runtime/ompi_module_exchange.h"
#include "ompi/proc/proc.h"

#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <assert.h>
#include <sys/time.h>
#include <gni_pub.h>

#include "common_ugni_ep.h"

#if !defined(MPI_COMMON_UGNI_H)
#define MPI_COMMON_UGNI_H

struct ompi_common_ugni_modex_t {
    uint32_t addr;
    int      id;
};
typedef struct ompi_common_ugni_modex_t ompi_common_ugni_modex_t;

struct ompi_common_ugni_device_t {
    opal_object_t    super;
 
    gni_nic_handle_t dev_handle;

    /* Minor number of the Gemini NIC */
    int32_t          dev_id;
    uint32_t         dev_pe_addr;
    uint32_t         dev_addr;
    uint32_t         dev_cpu_id;

    gni_cq_handle_t  dev_local_cq;

    size_t                      dev_ep_count;
    ompi_common_ugni_endpoint_t **dev_eps;
    void *btl_ctx;
};
typedef struct ompi_common_ugni_device_t ompi_common_ugni_device_t;

struct ompi_common_ugni_module_t {
    /* protection tag */
    uint8_t ptag;

    /* unique id for this process assigned by the system */
    uint32_t cookie;

    /* communication domain handle */
    gni_cdm_handle_t cd_handle;

    /* device count. to be used if we have more than 1 common per ugni device */
    int device_count;
    ompi_common_ugni_device_t *devices;

    int local_cq_size;

    int rdma_max_retries;
};
typedef struct ompi_common_ugni_module_t ompi_common_ugni_module_t;

struct ompi_common_ugni_post_desc_t {
    gni_post_descriptor_t base;

    ompi_common_ugni_endpoint_t *endpoint;
    int tries;

    /* NTH: callback function for this post. this may change in the future */
    void (*cbfunc) (struct ompi_common_ugni_post_desc_t *, int);
};
typedef struct ompi_common_ugni_post_desc_t ompi_common_ugni_post_desc_t;

extern ompi_common_ugni_module_t ompi_common_ugni_module;
extern mca_base_component_t ompi_common_ugni_component;

static inline int
ompi_common_rc_ugni_to_ompi (gni_return_t rc)
{
    int codes[] = {OMPI_SUCCESS,
                   OMPI_ERR_RESOURCE_BUSY,
                   OMPI_ERR_BAD_PARAM,
                   OMPI_ERR_OUT_OF_RESOURCE,
                   OMPI_ERR_TIMEOUT,
                   OMPI_ERR_PERM,
                   OMPI_ERROR,
                   OMPI_ERR_BAD_PARAM,
                   OMPI_ERR_BAD_PARAM,
                   OMPI_ERR_NOT_FOUND,
                   OMPI_ERR_VALUE_OUT_OF_BOUNDS,
                   OMPI_ERROR,
                   OMPI_ERR_NOT_SUPPORTED,
                   OMPI_ERR_OUT_OF_RESOURCE};
    return codes[rc];
}

/*
 * Initialize uGNI communication domain and device(s).
 */
int ompi_common_ugni_init (void);

/*
 * Finalize uGNI communication domain and device(s).
 */
int ompi_common_ugni_fini (void);

extern void mca_btl_ugni_local_smsg_complete (void *, uint32_t, int);

static inline int
ompi_common_ugni_process_completed_post (ompi_common_ugni_device_t *dev,
                                         gni_cq_handle_t cq_handle) {
    ompi_common_ugni_post_desc_t *desc;
    gni_return_t rc = GNI_RC_NOT_DONE;
    gni_cq_entry_t event_data = 0;
    uint32_t recoverable = 1;

    rc = GNI_CqGetEvent (cq_handle, &event_data);
    if (GNI_RC_NOT_DONE == rc) {
        return 0;
    }

    if (OPAL_UNLIKELY((GNI_RC_SUCCESS != rc && !event_data) || GNI_CQ_OVERRUN(event_data))) {
        /* TODO -- need to handle overrun -- how do we do this without an event?
           will the event eventually come back? Ask Cray */
        OPAL_OUTPUT((-1, "post error! cq overrun = %d", (int)GNI_CQ_OVERRUN(event_data)));
        assert (0);
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    /* local SMS completion */
    if (GNI_CQ_GET_TYPE(event_data) == GNI_CQ_EVENT_TYPE_SMSG) {
        uint32_t msg_id = GNI_CQ_GET_MSG_ID(event_data);

        assert (GNI_CQ_STATUS_OK(event_data));

        if ((uint32_t)-1 == msg_id) {
            /* nothing to do */
            return 1;
        }

        /* inform the btl of local smsg completion */
        mca_btl_ugni_local_smsg_complete (dev->btl_ctx, msg_id,
                                          GNI_CQ_STATUS_OK(event_data) ? OMPI_SUCCESS : OMPI_ERROR);

        return 1;
    }

    rc = GNI_GetCompleted (cq_handle, event_data, (gni_post_descriptor_t **) &desc);
    if (OPAL_UNLIKELY(GNI_RC_SUCCESS != rc)) {
        OPAL_OUTPUT((-1, "Error in GNI_GetComplete %s", gni_err_str[rc]));
        return ompi_common_rc_ugni_to_ompi (rc);
    }

    if (OPAL_UNLIKELY(!GNI_CQ_STATUS_OK(event_data))) {
        (void) GNI_CqErrorRecoverable (event_data, &recoverable);

        if (OPAL_UNLIKELY(!recoverable ||
                          ++desc->tries >= ompi_common_ugni_module.rdma_max_retries)) {            
            OPAL_OUTPUT((-1, "giving up on descriptor %p", (void *) desc));
            /* give up */
            desc->cbfunc (desc, OMPI_ERROR);

            return OMPI_ERROR;
        }

        /* repost transaction */
        if (GNI_POST_RDMA_PUT == desc->base.type ||
            GNI_POST_RDMA_GET == desc->base.type) {
            rc = GNI_PostRdma (desc->endpoint->ep_handle, &desc->base);
        } else {
            rc = GNI_PostFma (desc->endpoint->ep_handle, &desc->base);
        }

        return ompi_common_rc_ugni_to_ompi (rc);
    }

    desc->cbfunc (desc, OMPI_SUCCESS);

    return 1;
}

static inline int ompi_common_ugni_progress (void) {
    ompi_common_ugni_device_t *dev;
    int count, i;

    for (i = 0, count = 0 ; i < ompi_common_ugni_module.device_count ; ++i) {
        dev = ompi_common_ugni_module.devices + i;
        /* progress fma/local smsg completions */
        count += ompi_common_ugni_process_completed_post (dev, dev->dev_local_cq);
    }

    return count;
}

#endif /* MPI_COMMON_UGNI_H */
