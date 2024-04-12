/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2015-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2019-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/**
 * This file is a simple set of wrappers around the selected PRTE DL
 * component (it's a compile-time framework with, at most, a single
 * component; see prtedl.h for details).
 */

#include "prte_config.h"

#include "src/include/constants.h"

#include "src/mca/prtedl/base/base.h"

int prte_dl_open(const char *fname, bool use_ext, bool private_namespace, prte_dl_handle_t **handle,
                 char **err_msg)
{
    *handle = NULL;

    if (NULL != prte_prtedl && NULL != prte_prtedl->open) {
        return prte_prtedl->open(fname, use_ext, private_namespace, handle, err_msg);
    }

    return PRTE_ERR_NOT_SUPPORTED;
}

int prte_dl_lookup(prte_dl_handle_t *handle, const char *symbol, void **ptr, char **err_msg)
{
    if (NULL != prte_prtedl && NULL != prte_prtedl->lookup) {
        return prte_prtedl->lookup(handle, symbol, ptr, err_msg);
    }

    return PRTE_ERR_NOT_SUPPORTED;
}

int prte_dl_close(prte_dl_handle_t *handle)
{
    if (NULL != prte_prtedl && NULL != prte_prtedl->close) {
        return prte_prtedl->close(handle);
    }

    return PRTE_ERR_NOT_SUPPORTED;
}

int prte_dl_foreachfile(const char *search_path,
                        int (*cb_func)(const char *filename, void *context), void *context)
{
    if (NULL != prte_prtedl && NULL != prte_prtedl->foreachfile) {
        return prte_prtedl->foreachfile(search_path, cb_func, context);
    }

    return PRTE_ERR_NOT_SUPPORTED;
}
