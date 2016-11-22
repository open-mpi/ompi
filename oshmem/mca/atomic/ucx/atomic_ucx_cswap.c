/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include <stdio.h>
#include <stdlib.h>

#include "oshmem/constants.h"
#include "oshmem/mca/atomic/atomic.h"
#include "oshmem/mca/atomic/base/base.h"
#include "oshmem/runtime/runtime.h"

#include "atomic_ucx.h"

int mca_atomic_ucx_cswap(void *target,
                         void *prev,
                         const void *cond,
                         const void *value,
                         size_t nlong,
                         int pe)
{
    ucs_status_t status;
    spml_ucx_mkey_t *ucx_mkey;
    uint64_t rva;

    ucx_mkey = mca_spml_ucx_get_mkey(pe, target, (void *)&rva); 
    if (NULL == cond) {
        switch (nlong) {
            case 4:
                status = ucp_atomic_swap32(mca_spml_self->ucp_peers[pe].ucp_conn, 
                        *(uint32_t *)value, rva, ucx_mkey->rkey, prev);
                break;
            case 8:
                status = ucp_atomic_swap64(mca_spml_self->ucp_peers[pe].ucp_conn, 
                        *(uint64_t *)value, rva, ucx_mkey->rkey, prev);
                break;
            default:
                goto err_size;
        }
    }
    else {
        switch (nlong) {
            case 4:
                status = ucp_atomic_cswap32(mca_spml_self->ucp_peers[pe].ucp_conn, 
                        *(uint32_t *)cond, *(uint32_t *)value, rva, ucx_mkey->rkey, prev);
                break;
            case 8:
                status = ucp_atomic_cswap64(mca_spml_self->ucp_peers[pe].ucp_conn, 
                        *(uint64_t *)cond, *(uint64_t *)value, rva, ucx_mkey->rkey, prev);
                break;
            default:
                goto err_size;
        }
    }

    return ucx_status_to_oshmem(status);

err_size:
    ATOMIC_ERROR("[#%d] Type size must be 1/2/4 or 8 bytes.", my_pe);
    return OSHMEM_ERROR;
}


