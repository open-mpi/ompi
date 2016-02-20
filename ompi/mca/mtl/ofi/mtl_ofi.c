/*
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "mtl_ofi.h"

OMPI_DECLSPEC extern mca_mtl_ofi_component_t mca_mtl_ofi_component;

mca_mtl_ofi_module_t ompi_mtl_ofi = {
    {
        8191,        /* max cid - 2^13 - 1 */
        (1UL << 30), /* max tag value - must allow negatives */
        0,           /* request reserve space */
        0,           /* flags */

        ompi_mtl_ofi_add_procs,
        ompi_mtl_ofi_del_procs,
        ompi_mtl_ofi_finalize,

        ompi_mtl_ofi_send,
        ompi_mtl_ofi_isend,
        ompi_mtl_ofi_irecv,
        ompi_mtl_ofi_iprobe,
        ompi_mtl_ofi_imrecv,
        ompi_mtl_ofi_improbe,

        ompi_mtl_ofi_cancel,
        ompi_mtl_ofi_add_comm,
        ompi_mtl_ofi_del_comm
    },
    0,
    0,
    NULL,
    NULL
};

int
ompi_mtl_ofi_add_procs(struct mca_mtl_base_module_t *mtl,
                       size_t nprocs,
                       struct ompi_proc_t** procs)
{
    int ret = OMPI_SUCCESS;
    size_t i;
    size_t size;
    size_t namelen;
    int count = 0;
    char *ep_name = NULL;
    char *ep_names = NULL;
    fi_addr_t *fi_addrs = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;

    namelen = ompi_mtl_ofi.epnamelen;

    /**
     * Create array of EP names.
     */
    ep_names = malloc(nprocs * namelen);
    if (NULL == ep_names) {
        ret = OMPI_ERROR;
        goto bail;
    }

    /**
     * Create array of fi_addrs.
     */
    fi_addrs = malloc(nprocs * sizeof(fi_addr_t));
    if (NULL == fi_addrs) {
        ret = OMPI_ERROR;
        goto bail;
    }

    /**
     * Retrieve the processes' EP names from modex.
     */
    for (i = 0; i < nprocs; ++i) {
        OFI_COMPAT_MODEX_RECV(ret,
                              &mca_mtl_ofi_component.super.mtl_version,
                              procs[i],
                              (void**)&ep_name,
                              &size);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: modex_recv failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto bail;
        }
        memcpy(&ep_names[i*namelen], ep_name, namelen);
    }

    /**
     * Map the EP names to fi_addrs.
     */
    count = fi_av_insert(ompi_mtl_ofi.av, ep_names, nprocs, fi_addrs, 0, NULL);
    if ((count < 0) || (nprocs != (size_t)count)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_av_insert failed: %d\n",
                            __FILE__, __LINE__, count);
        ret = OMPI_ERROR;
        goto bail;
    }

    /**
     * Store the fi_addrs within the endpoint objects.
     */
    for (i = 0; i < nprocs; ++i) {
        endpoint = OBJ_NEW(mca_mtl_ofi_endpoint_t);
        if (NULL == endpoint) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: mtl/ofi: could not allocate endpoint"
                                " structure\n",
                                __FILE__, __LINE__);
            ret = OMPI_ERROR;
            goto bail;
        }

        endpoint->mtl_ofi_module = &ompi_mtl_ofi;
        endpoint->peer_fiaddr = fi_addrs[i];

        /* FIXME: What happens if this endpoint already exists? */
        procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
    }

    ret = OMPI_SUCCESS;

bail:
    if (fi_addrs)
        free(fi_addrs);

    if (ep_names)
        free(ep_names);

    return ret;
}

int
ompi_mtl_ofi_del_procs(struct mca_mtl_base_module_t *mtl,
                       size_t nprocs,
                       struct ompi_proc_t** procs)
{
    int ret;
    size_t i;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;

    for (i = 0 ; i < nprocs ; ++i) {
        if (NULL != procs[i] &&
            NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL]) {
            endpoint = procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
            ret = fi_av_remove(ompi_mtl_ofi.av, &endpoint->peer_fiaddr, 1, 0);
            if (ret) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "%s:%d: fi_av_remove failed: %s\n", __FILE__, __LINE__, fi_strerror(errno));
                return ret;
            }
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = NULL;
            OBJ_RELEASE(endpoint);
        }
    }

    return OMPI_SUCCESS;
}
