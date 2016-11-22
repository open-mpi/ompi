/*
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef OMPI_MTL_OFI_ENDPOINT_H
#define OMPI_MTL_OFI_ENDPOINT_H

BEGIN_C_DECLS

extern int ompi_mtl_ofi_add_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

OBJ_CLASS_DECLARATION(mca_mtl_ofi_endpoint_t);

/**
 * An abstraction that represents a connection to a endpoint process.
 * An instance of mca_mtl_ofi_endpoint_t is associated with each process
 * and MTL pair at startup. However, connections to the endpoint
 * are established dynamically on an as-needed basis:
 */

struct mca_mtl_ofi_endpoint_t {
    opal_list_item_t super;

    /** MTL instance that created this connection */
    struct mca_mtl_ofi_module_t *mtl_ofi_module;

    /** The peer's fi_addr */
    fi_addr_t peer_fiaddr;
};

typedef struct mca_mtl_ofi_endpoint_t  mca_mtl_ofi_endpoint_t;

static inline mca_mtl_ofi_endpoint_t *ompi_mtl_ofi_get_endpoint (struct mca_mtl_base_module_t* mtl, ompi_proc_t *ompi_proc)
{
    if (OPAL_UNLIKELY(NULL == ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL])) {
        ompi_mtl_ofi_add_procs(mtl, 1, &ompi_proc);
    }

    return ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
}

END_C_DECLS
#endif
