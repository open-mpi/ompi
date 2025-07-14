/*
 * Copyright (c) 2025      Amazon.com, Inc. or its affiliates.  All Rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"

#include "mtl_ofi.h"

static int
ompi_mtl_ofi_reg_mem(void *reg_data, void *base, size_t size,
                     mca_rcache_base_registration_t *reg)
{
    int ret;
    struct fi_mr_attr attr = {0};
    struct iovec iov = {0};
    ompi_mtl_ofi_reg_t *mtl_reg = (ompi_mtl_ofi_reg_t *)reg;
    int dev_id;
    uint64_t flags;

    iov.iov_base = base;
    iov.iov_len = size;
    attr.mr_iov = &iov;
    attr.iov_count = 1;
    attr.access = FI_SEND | FI_RECV;
    attr.offset = 0;
    attr.context = NULL;

#if  OPAL_OFI_HAVE_FI_MR_IFACE
    if (OPAL_LIKELY(NULL != base)) {
        ret = opal_accelerator.check_addr(base, &dev_id, &flags);
        if (ret < 0) {
            return ret;
        } else if (ret > 0 ) {
            if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "cuda")) {
                attr.iface = FI_HMEM_CUDA;
                opal_accelerator.get_device(&attr.device.cuda);
#if OPAL_OFI_HAVE_FI_HMEM_ROCR
	    } else if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "rocm")) {
                attr.iface = FI_HMEM_ROCR;
                opal_accelerator.get_device(&attr.device.cuda);
#endif
#if OPAL_OFI_HAVE_FI_HMEM_ZE
            } else if (0 == strcmp(opal_accelerator_base_selected_component.base_version.mca_component_name, "ze")) {
                attr.iface = FI_HMEM_ZE;
                opal_accelerator.get_device(&attr.device.ze);
#endif
            } else {
                return OPAL_ERROR;
            }
        }
    }
#endif

    ret = fi_mr_regattr(ompi_mtl_ofi.domain, &attr, 0, &mtl_reg->ofi_mr);
    if (0 != ret) {
        opal_show_help("help-mtl-ofi.txt", "Buffer Memory Registration Failed", true,
                       opal_accelerator_base_selected_component.base_version.mca_component_name,
                       base, size, fi_strerror(-ret), ret);
        mtl_reg->ofi_mr = NULL;
        return OPAL_ERR_OUT_OF_RESOURCE;
    }

    mtl_reg->mem_desc = fi_mr_desc(mtl_reg->ofi_mr);

    return OPAL_SUCCESS;
}


static int
ompi_mtl_ofi_dereg_mem(void *reg_data, mca_rcache_base_registration_t *reg)
{
    ompi_mtl_ofi_reg_t *mtl_reg = (ompi_mtl_ofi_reg_t *)reg;
    int ret;

    if (mtl_reg->ofi_mr != NULL) {
        ret = fi_close(&mtl_reg->ofi_mr->fid);
        if (0 != ret) {
            opal_output_verbose(1, opal_common_ofi.output,
                                "%s: error unpinning memory mr=%p: %s",
                                __func__, (void *)mtl_reg->ofi_mr,
                                fi_strerror(-ret));
            return OPAL_ERROR;
        }
    }

    return OPAL_SUCCESS;
}


int
ompi_mtl_ofi_rcache_init(void)
{
    mca_rcache_base_resources_t rcache_resources;
    char *tmp;

    if (NULL != ompi_mtl_ofi.rcache) {
        return OMPI_SUCCESS;
    }

    tmp = strdup("mtl-ofi");
    rcache_resources.cache_name = tmp;
    rcache_resources.reg_data = NULL;
    rcache_resources.sizeof_reg = sizeof(ompi_mtl_ofi_reg_t);
    rcache_resources.register_mem = ompi_mtl_ofi_reg_mem;
    rcache_resources.deregister_mem = ompi_mtl_ofi_dereg_mem;

    ompi_mtl_ofi.rcache = mca_rcache_base_module_create("grdma", &ompi_mtl_ofi, &rcache_resources);
    free(tmp);

    if (NULL == ompi_mtl_ofi.rcache) {
        /* something when horribly wrong */
        opal_output_verbose(1, opal_common_ofi.output,
                            "creating rcache failed");
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}
