/*
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/memheap/memheap.h"
#include "oshmem/proc/proc.h"
#include "atomic_ucx.h"

/*
 * Initial query function that is invoked during initialization, allowing
 * this module to indicate what level of thread support it provides.
 */
int mca_atomic_ucx_init(bool enable_progress_threads, bool enable_threads)
{
    return OSHMEM_SUCCESS;
}

int mca_atomic_ucx_finalize(void)
{
    return OSHMEM_SUCCESS;
}

mca_atomic_base_module_t *
mca_atomic_ucx_query(int *priority)
{
    mca_atomic_ucx_module_t *module;

    *priority = mca_atomic_ucx_component.priority;

    module = OBJ_NEW(mca_atomic_ucx_module_t);
    if (module) {
        module->super.atomic_fadd = mca_atomic_ucx_fadd;
        module->super.atomic_cswap = mca_atomic_ucx_cswap;
        return &(module->super);
    }

    return NULL ;
}

void mca_atomic_ucx_complete_cb(void *request, ucs_status_t status)
{
}

ucs_status_t mca_atomic_ucx_wait_request(ucs_status_ptr_t request)
{
    ucs_status_t status;

    /* check for request completed or failed */
    if (UCS_OK == request) {
        return UCS_OK;
    } else if (UCS_PTR_IS_ERR(request)) {
        return UCS_PTR_STATUS(request);
    }

    while (UCS_INPROGRESS == (status = ucp_request_check_status(request))) {
        ucp_worker_progress(mca_spml_self->ucp_worker);
    }
    ucp_request_free(request);
    return status;
}
